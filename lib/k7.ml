(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Follows VHS v0.12.1's lexer/lexer.go and parser/parser.go (MIT, Charm). *)

type duration = Ms of int

type key =
  | Enter
  | Tab
  | Space
  | Backspace
  | Delete
  | Insert
  | Escape
  | Up
  | Down
  | Left
  | Right
  | Page_up
  | Page_down
  | Scroll_up
  | Scroll_down
  | Ctrl of char
  | Alt of char

type wait = Prompt | Line of string | Screen of string

type action =
  | Type of string
  | Key of key
  | Sleep of duration
  | Wait of duration option * wait
  | Hidden of action list
  | Raw of string

let type' s = Type s
let key k = Key k
let sleep d = Sleep d
let wait ?timeout w = Wait (timeout, w)
let hidden actions = Hidden actions
let raw line = Raw line

type shell =
  | Bash
  | Zsh
  | Fish
  | Powershell
  | Pwsh
  | Cmd
  | Nushell
  | Osh
  | Xonsh

type setting =
  | Shell of shell
  | Font_size of int
  | Font_family of string
  | Width of int
  | Height of int
  | Columns of int
  | Rows of int
  | Theme of string
  | Padding of int
  | Typing_speed of duration
  | Framerate of int

type error =
  | Executable_not_found of string
  | Failed of {
      vhs : string;
      status : Eio.Process.exit_status;
      output : string;
    }
  | Io of string
  | Unsupported of string
  | Timeout of { wait : string; line : int; last : string }
  | Exited of {
      status : Eio.Process.exit_status;
      wait : string;
      line : int;
      last : string;
    }
  | Invalid of string

type t = {
  outputs : string list;
  settings : setting list;
  actions : action list;
  source : string list option;
}

let ( let* ) = Result.bind
let err_unsupported fmt = Fmt.kstr (fun s -> Error (Unsupported s)) fmt
let err_invalid fmt = Fmt.kstr (fun s -> Error (Invalid s)) fmt

let rec iter_result f = function
  | [] -> Ok ()
  | x :: rest ->
      let* () = f x in
      iter_result f rest

let has_control = Tape_syntax.has_control
let quote_bit = function '"' -> 1 | '\'' -> 2 | '`' -> 4 | _ -> 0

(* A VHS string ends at its quote, a newline or a NUL and has no escapes. *)
let quotable_problem s =
  if has_control s then Some "holds a control character"
  else if String.fold_left (fun m c -> m lor quote_bit c) 0 s = 7 then
    Some "holds all three quote characters"
  else None

let check_quotable what s =
  match quotable_problem s with
  | None -> Ok ()
  | Some why -> Fmt.error "%s %a: %s" what Quote.pp s why

(* The largest Go [time.Duration], in whole milliseconds. *)
let max_ms = 9_223_372_036_854

let check_ms what n =
  if n < 0 then Fmt.error "%s%dms: negative duration" what n
  else if n > max_ms then
    Fmt.error "%s%dms: beyond Go's duration range (%dms)" what n max_ms
  else Ok ()

let check_type s =
  if not (String.is_valid_utf_8 s) then
    Fmt.error "Type %a: invalid UTF-8" Quote.pp s
  else if has_control ~except:"\t\n" s then
    Fmt.error "Type %a: holds a control character other than tab and newline"
      Quote.pp s
  else Ok ()

let check_chord name valid c =
  if Char.Ascii.is_letter c || String.contains valid c then Ok ()
  else Fmt.error "%s+%s: not a VHS key" name (Char.escaped c)

(* VHS lexes a bare [m] or [s] as a time unit; [Ctrl] takes them quoted, but
   [Alt] then presses nothing. *)
let check_key = function
  | Ctrl c -> check_chord "Ctrl" {|@[\]^-.|} c
  | Alt (('m' | 's') as c) -> Fmt.error "Alt+%c: VHS cannot press it" c
  | Alt c -> check_chord "Alt" "[]." c
  | _ -> Ok ()

let wait_name = function
  | Prompt -> "Wait"
  | Line _ -> "Wait+Line"
  | Screen _ -> "Wait+Screen"

let check_wait timeout w =
  let* () =
    match timeout with
    | None -> Ok ()
    | Some (Ms 0) -> Fmt.error "Wait@0ms: the timeout must be positive"
    | Some (Ms n) -> check_ms "Wait@" n
  in
  match w with
  | Prompt -> Ok ()
  | (Line re | Screen re) when has_control re ->
      Fmt.error "%s %a: holds a control character" (wait_name w) Quote.pp re
  | Line re | Screen re ->
      Regexp_syntax.check re
      |> Result.map_error (fun e ->
          Fmt.str "%s %a: %s" (wait_name w) Quote.pp re e)

(* VHS hands an unknown extension to ffmpeg, which then fails. *)
let extensions =
  [ ".gif"; ".mp4"; ".webm"; ".txt"; ".ascii"; ".test"; ".png"; ".svg" ]

(* VHS writes a [.svg] output as a GIF, so it never reaches VHS. *)
let is_svg path = Tape_syntax.extension path = ".svg"

let output_problem path =
  if Option.is_some (quotable_problem path) then quotable_problem path
  else if String.starts_with ~prefix:"-" path then
    Some "starts with -, which ffmpeg reads as an option"
  else if String.ends_with ~suffix:"/" path then None
  else
    let known = "not .gif, .mp4, .webm, .txt, .ascii, .test, .png or .svg" in
    match Tape_syntax.extension path with
    | "" -> Some Tape_syntax.no_extension
    | e when List.mem e extensions -> None
    (* Case-sensitive, as in VHS. *)
    | e when List.mem (String.lowercase_ascii e) extensions ->
        Some (known ^ "; extensions are lower case")
    | _ -> Some known

let check_output path =
  Option.fold ~none:(Ok ())
    ~some:(Fmt.error "Output %a: %s" Quote.pp path)
    (output_problem path)

(* VHS keeps the last output of each kind; [record] writes one SVG. *)
let kind path =
  if String.ends_with ~suffix:"/" path then "frames"
  else
    match Tape_syntax.extension path with
    | ".mp4" -> "MP4"
    | ".webm" -> "WebM"
    | ".txt" | ".ascii" | ".test" -> "text"
    | ".png" -> "frames"
    | ".svg" -> "SVG"
    | _ -> "GIF"

(* [seen] holds at most one path per kind, so this is linear. *)
let check_kinds paths =
  let rec go seen = function
    | [] -> Ok ()
    | q :: rest -> (
        match List.assoc_opt (kind q) seen with
        | Some p when kind q = "SVG" ->
            Fmt.error "Output %a: a tape has one SVG output, not also %a"
              Quote.pp p Quote.pp q
        | Some p ->
            Fmt.error "Output %a: VHS writes only the last %s output, %a"
              Quote.pp p (kind q) Quote.pp q
        | None -> go ((kind q, q) :: seen) rest)
  in
  go [] paths

let rec raw_commands actions =
  List.concat_map
    (function
      | Raw line -> Result.to_list (Tape_syntax.check line)
      | Hidden actions -> raw_commands actions
      | Type _ | Key _ | Sleep _ | Wait _ -> [])
    actions

let raw_outputs actions =
  List.filter_map
    (function Tape_syntax.Output p -> Some p | _ -> None)
    (raw_commands actions)

let raw_screenshots actions =
  List.filter_map
    (function Tape_syntax.Screenshot p -> Some p | _ -> None)
    (raw_commands actions)

let shared outputs shots =
  let rec go seen = function
    | [] -> None
    | p :: rest -> if List.mem p seen then Some p else go (p :: seen) rest
  in
  go outputs shots

let check_screenshots outputs shots =
  match shared outputs shots with
  | None -> Ok ()
  | Some p ->
      Fmt.error "Screenshot %a: the path of an output or another screenshot"
        Quote.pp p

(* VHS refuses a frame sized by both pixels and cells in one direction. *)
let check_cells settings =
  let sets p = List.exists p settings in
  let height = sets (function Height _ -> true | _ -> false)
  and width = sets (function Width _ -> true | _ -> false) in
  iter_result
    (function
      | Rows n when height ->
          Fmt.error "Set Rows %d: VHS cannot set both Height and Rows" n
      | Columns n when width ->
          Fmt.error "Set Columns %d: VHS cannot set both Width and Columns" n
      | _ -> Ok ())
    settings

(* The constructor that writes each setting [Set] can also write, but
   [Shell], which VHS applies wherever it is. *)
let constructors =
  [
    ("FontSize", "Font_size");
    ("FontFamily", "Font_family");
    ("Width", "Width");
    ("Height", "Height");
    ("Columns", "Columns");
    ("Rows", "Rows");
    ("Theme", "Theme");
    ("Padding", "Padding");
    ("Framerate", "Framerate");
  ]

(* VHS applies [Set], but [Set Shell], and [Require] only before the first
   other command. *)
let check_raw ~started line =
  if has_control line then
    Fmt.error "raw line %a: holds a control character" Quote.pp line
  else
    match Tape_syntax.check line with
    | Error e -> Fmt.error "raw line %a: %s" Quote.pp line e
    | Ok (Hide | Show) ->
        Fmt.error "raw line %a: use hidden instead" Quote.pp line
    | Ok (Blank | Set ("TypingSpeed", _)) -> Ok started
    | Ok (Output path) when is_svg path ->
        Fmt.error "raw line %a: VHS cannot write .svg, give it in outputs"
          Quote.pp line
    | Ok (Output path) -> (
        match output_problem path with
        | None -> Ok started
        | Some why -> Fmt.error "raw line %a: %s" Quote.pp line why)
    | Ok (Set ("Shell", _)) ->
        Fmt.error "raw line %a: use the setting Shell instead" Quote.pp line
    | Ok (Set _ | Require) when started ->
        Fmt.error "raw line %a: VHS ignores it after the first action" Quote.pp
          line
    | Ok (Set (name, _)) when List.mem_assoc name constructors ->
        Fmt.error "raw line %a: use the setting %s instead" Quote.pp line
          (List.assoc name constructors)
    | Ok (Set _ | Require) -> Ok false
    | Ok Other -> (
        match Tape_syntax.tokens line with
        | Keyword "Env" :: (Str k | Keyword k) :: _ -> (
            match Tape_syntax.env_name_problem k with
            | Some why -> Fmt.error "raw line %a: %s" Quote.pp line why
            | None -> Ok true)
        | _ -> Ok true)
    | Ok (Screenshot _) -> Ok true

let rec is_empty = function
  | Hidden actions -> List.for_all is_empty actions
  | Type s -> s = ""
  | Key _ | Sleep _ | Wait _ | Raw _ -> false

let records = function
  | Type s -> s <> ""
  | Key _ | Sleep _ | Wait _ -> true
  | Hidden _ -> false
  | Raw line -> (
      match Tape_syntax.check line with
      | Ok (Screenshot _ | Other) -> true
      | _ -> false)

let rec check_actions ~started = function
  | [] -> Ok started
  | a :: rest ->
      let* started =
        match a with
        | Type s ->
            let* () = check_type s in
            Ok (started || s <> "")
        | Key k ->
            let* () = check_key k in
            Ok true
        | Sleep (Ms n) ->
            let* () = check_ms "Sleep " n in
            Ok true
        | Wait (timeout, w) ->
            let* () = check_wait timeout w in
            Ok true
        | Hidden actions as a ->
            check_actions ~started:(started || not (is_empty a)) actions
        | Raw line -> check_raw ~started line
      in
      check_actions ~started rest

(* Go's [unicode.IsSpace]. *)
let is_space u =
  let c = Uchar.to_int u in
  (c >= 0x09 && c <= 0x0d)
  || (c >= 0x2000 && c <= 0x200a)
  || List.mem c
       [ 0x20; 0x85; 0xa0; 0x1680; 0x2028; 0x2029; 0x202f; 0x205f; 0x3000 ]

let is_blank s =
  let rec go i =
    i >= String.length s
    ||
    let d = String.get_utf_8_uchar s i in
    is_space (Uchar.utf_decode_uchar d) && go (i + Uchar.utf_decode_length d)
  in
  go 0

(* VHS reads a theme that starts with [{] as JSON, and a blank one as its
   default theme. *)
let check_theme t =
  let* () = check_quotable "Set Theme" t in
  if is_blank t || Themes.mem t then Ok ()
  else if String.starts_with ~prefix:"{" t then
    match Palette.of_json t with
    | Ok _ | Error (`Colour _) -> Ok ()
    | Error (`Json msg) ->
        Fmt.error "Set Theme %a: VHS cannot read it: %s" Quote.pp t msg
    | Error (`Unread msg) ->
        Fmt.error
          "Set Theme %a: this library cannot read it, though VHS does: %s"
          Quote.pp t msg
  else Fmt.error "Set Theme %a: not a VHS theme" Quote.pp t

(* VHS fonts are set by browser code that quotes the list with [']. *)
let check_setting = function
  | Shell _ -> Ok ()
  | Font_size n when n <= 0 -> Fmt.error "Set FontSize %d: not positive" n
  | Framerate n when n <= 0 -> Fmt.error "Set Framerate %d: not positive" n
  | Columns n when n <= 0 -> Fmt.error "Set Columns %d: not positive" n
  | Rows n when n <= 0 -> Fmt.error "Set Rows %d: not positive" n
  | Padding n when n < 0 -> Fmt.error "Set Padding %d: negative" n
  | Font_family f when String.exists (fun c -> c = '\'' || c = '\\') f ->
      Fmt.error "Set FontFamily %a: holds a single quote or a backslash"
        Quote.pp f
  | Font_family f -> check_quotable "Set FontFamily" f
  | Theme t -> check_theme t
  | Typing_speed (Ms n) -> check_ms "Set TypingSpeed " n
  | Font_size _ | Framerate _ | Padding _ | Width _ | Height _ | Columns _
  | Rows _ ->
      Ok ()

let raw_set actions name =
  List.fold_left
    (fun v -> function
      | Tape_syntax.Set (n, a) when n = name -> Some a | _ -> v)
    None (raw_commands actions)

(* VHS refuses a frame smaller than twice the padding. Halving the size,
   unlike doubling the padding, cannot overflow. *)
let check_padding ~padding ~width ~height =
  let p = Option.value padding ~default:60 in
  let check name size default =
    let n = Option.value size ~default in
    if n >= 0 && p <= n / 2 then Ok ()
    else
      match size with
      | Some n ->
          Fmt.error "Set %s %d: below twice the %spadding (%d)" name n
            (if padding = None then "default " else "")
            p
      | None ->
          Fmt.error "Set Padding %d: above half the default %s (%d)" p
            (String.lowercase_ascii name)
            default
  in
  let* () = check "Width" width 1200 in
  check "Height" height 600

let sum ns =
  let add a n =
    let s = Int64.add a n in
    (* A sum overflows iff its terms share a sign the sum does not have. *)
    if
      Int64.compare a 0L < 0 = (Int64.compare n 0L < 0)
      && Int64.compare s 0L < 0 <> (Int64.compare a 0L < 0)
    then None
    else Some s
  in
  List.fold_left (fun acc n -> Option.bind acc (fun a -> add a n)) (Some 0L) ns

(* VHS checks the frame in wrapping 64-bit arithmetic, which a huge margin
   passes: a sum that overflows does not fit. *)
(* The longest side of a viewport the browser VHS drives accepts. *)
let max_viewport = 10_000_000L

(* VHS measures the cells in the viewport doubled up to five times; a cell
   takes at least a pixel. *)
let check_measured ~border name cells size default bar =
  match cells with
  | None -> Ok ()
  | Some n ->
      let n = Int64.of_int n in
      let side =
        Int64.sub
          (Int64.of_int (Option.value size ~default))
          (Int64.add border bar)
      in
      let most = Int64.mul 32L side in
      if Int64.compare n most <= 0 then Ok ()
      else
        Fmt.error
          "Set %s %Ld: VHS measures at most %Ld %s, one a pixel in 32 times \
           the browser's viewport"
          name n most
          (String.lowercase_ascii name)

(* VHS's viewport is the frame less twice the padding, twice the margin
   unless [MarginFill] is empty, and the window bar; vhs panics if the
   browser refuses a side. *)
let check_viewport actions ~padding ~margin ~bar ~width ~height ~columns ~rows =
  let p = Int64.of_int (Option.value padding ~default:60) in
  let m = if raw_set actions "MarginFill" = Some "" then 0L else margin in
  let check name size default border =
    let n = Int64.of_int (Option.value size ~default) in
    let fits =
      match sum [ p; p; m; m; border ] with
      | Some b ->
          Int64.compare b n <= 0
          && Int64.compare b (Int64.sub n max_viewport) >= 0
      | None -> false
    in
    if fits then Ok ()
    else
      let culprit =
        match size with
        | Some n -> Fmt.str "%s %d" name n
        | None when Int64.equal margin 0L -> Fmt.str "Padding %Ld" p
        | None -> Fmt.str "Margin %Ld" margin
      in
      Fmt.error
        "Set %s: the browser's viewport, the %s less the padding, margin and \
         window bar, must be 0 to %Ld pixels"
        culprit
        (String.lowercase_ascii name)
        max_viewport
  in
  let* () = check "Width" width 1200 0L in
  let* () = check "Height" height 600 bar in
  let cells = check_measured ~border:(Int64.mul 2L (Int64.add p m)) in
  let* () = cells "Columns" columns width 1200 0L in
  cells "Rows" rows height 600 bar

let last settings f = List.find_map f (List.rev settings)

let check_size settings actions =
  let last = last settings in
  let padding = last (function Padding n -> Some n | _ -> None) in
  let width = last (function Width n -> Some n | _ -> None) in
  let height = last (function Height n -> Some n | _ -> None) in
  let columns = last (function Columns n -> Some n | _ -> None) in
  let rows = last (function Rows n -> Some n | _ -> None) in
  let int name default =
    Option.value ~default (Option.bind (raw_set actions name) Tape_syntax.atoi)
  in
  let margin = int "Margin" 0L in
  let window_bar = raw_set actions "WindowBar" in
  let bar =
    match window_bar with
    | None | Some "" -> 0L
    | Some _ -> int "WindowBarSize" 30L
  in
  let* () =
    if Int64.equal margin 0L && Int64.equal bar 0L then
      check_padding ~padding ~width ~height
    else
      let p = Int64.of_int (Option.value padding ~default:60) in
      let min_width = sum [ p; p; margin; margin ] in
      let min_height = Option.bind min_width (fun m -> sum [ m; bar ]) in
      let w = Option.value width ~default:1200 in
      let h = Option.value height ~default:600 in
      let fits size = function
        | Some m -> Int64.compare (Int64.of_int size) m >= 0
        | None -> false
      in
      if fits w min_width && fits h min_height then Ok ()
      else
        let culprit =
          if Int64.equal margin 0L then
            "WindowBar " ^ Option.value window_bar ~default:""
          else Fmt.str "Margin %Ld" margin
        in
        match (min_width, min_height) with
        | Some mw, Some mh ->
            Fmt.error
              "Set %s: the frame needs at least %Ld x %Ld pixels, not %d x %d"
              culprit mw mh w h
        | _ -> Fmt.error "Set %s: the frame size overflows 64 bits" culprit
  in
  check_viewport actions ~padding ~margin ~bar ~width ~height ~columns ~rows

(* VHS encodes an MP4 in yuv420p, which ffmpeg refuses for an odd width or
   height. *)
let check_mp4 settings paths =
  let last name f =
    Option.map (fun n -> (name, n)) (List.find_map f (List.rev settings))
  in
  let odd = function
    | Some (name, n) when n land 1 <> 0 ->
        Fmt.error "Set %s %d: odd, which an MP4 output cannot have" name n
    | _ -> Ok ()
  in
  if List.exists (fun p -> kind p = "MP4") paths then
    let* () = odd (last "Width" (function Width n -> Some n | _ -> None)) in
    odd (last "Height" (function Height n -> Some n | _ -> None))
  else Ok ()

let check_outputs outputs = iter_result check_output outputs

let check_records actions =
  if List.exists records actions then Ok ()
  else Fmt.error "no action outside hidden, so VHS records no frame"

(* The largest side of a frame each encoder of ffmpeg 9 takes: GIF's
   16-bit sizes, libx264's and libvpx-vp9's. *)
let encoders = [ ("GIF", 65535); ("MP4", 16384); ("WebM", 65536) ]

(* ffmpeg's [av_image_check_size2] refuses a frame unless
   (8 width + 1024) (height + 128) is below 2{^31}. *)
let check_encoders settings paths =
  let last = last settings in
  let width =
    Option.value ~default:1200 (last (function Width n -> Some n | _ -> None))
  and height =
    Option.value ~default:600 (last (function Height n -> Some n | _ -> None))
  in
  iter_result
    (fun (k, limit) ->
      if not (List.exists (fun p -> kind p = k) paths) then Ok ()
      else if width > limit then
        Fmt.error "Set Width %d: a %s is at most %d pixels wide" width k limit
      else if height > limit then
        Fmt.error "Set Height %d: a %s is at most %d pixels high" height k limit
      else if
        Int64.compare
          (Int64.mul
             (Int64.of_int ((8 * width) + 1024))
             (Int64.of_int (height + 128)))
          2147483647L
        >= 0
      then
        Fmt.error
          "Set Width %d: a %d x %d frame has more pixels than ffmpeg takes"
          width width height
      else Ok ())
    encoders

let check_paths settings paths =
  let* () = check_kinds paths in
  let* () = check_mp4 settings paths in
  check_encoders settings paths

let check_tape ~settings ~outputs actions =
  let* () = check_outputs outputs in
  let* () = iter_result check_setting settings in
  let* _started = check_actions ~started:false actions in
  let* () = check_records actions in
  let* () = check_cells settings in
  let* () = check_size settings actions in
  let outputs' = outputs @ raw_outputs actions in
  let* () = check_paths settings outputs' in
  let* () = check_screenshots outputs' (raw_screenshots actions) in
  Ok { outputs; settings; actions; source = None }

let tape ?(settings = []) ~outputs actions =
  Result.map_error
    (fun msg -> Invalid msg)
    (check_tape ~settings ~outputs actions)

let shell_name = function
  | Bash -> "bash"
  | Zsh -> "zsh"
  | Fish -> "fish"
  | Powershell -> "powershell"
  | Pwsh -> "pwsh"
  | Cmd -> "cmd"
  | Nushell -> "nu"
  | Osh -> "osh"
  | Xonsh -> "xonsh"

let key_name = function
  | Enter -> "Enter"
  | Tab -> "Tab"
  | Space -> "Space"
  | Backspace -> "Backspace"
  | Delete -> "Delete"
  | Insert -> "Insert"
  | Escape -> "Escape"
  | Up -> "Up"
  | Down -> "Down"
  | Left -> "Left"
  | Right -> "Right"
  | Page_up -> "PageUp"
  | Page_down -> "PageDown"
  | Scroll_up -> "ScrollUp"
  | Scroll_down -> "ScrollDown"
  | Ctrl (('m' | 's') as c) -> "Ctrl+\"" ^ String.make 1 c ^ "\""
  | Ctrl c -> "Ctrl+" ^ String.make 1 c
  | Alt c -> "Alt+" ^ String.make 1 c

let add_quoted b s ~first ~last ~quotes =
  let q =
    if quotes land 1 = 0 then '"' else if quotes land 2 = 0 then '\'' else '`'
  in
  Buffer.add_char b q;
  Buffer.add_substring b s first (last - first);
  Buffer.add_char b q

let add_string b s =
  let quotes = String.fold_left (fun m c -> m lor quote_bit c) 0 s in
  add_quoted b s ~first:0 ~last:(String.length s) ~quotes

let add_text b s ~first ~last =
  let line start stop quotes =
    Buffer.add_string b "Type ";
    add_quoted b s ~first:start ~last:stop ~quotes;
    Buffer.add_char b '\n'
  in
  let rec go start i quotes =
    if i = last then line start i quotes
    else
      let bit = quote_bit s.[i] in
      if bit = 0 || quotes land bit <> 0 then go start (i + 1) quotes
      else if quotes lor bit = 7 then (
        line start i quotes;
        go i (i + 1) bit)
      else go start (i + 1) (quotes lor bit)
  in
  if first < last then go first first 0

let add_type b s =
  let n = String.length s in
  let rec go start i =
    if i = n then add_text b s ~first:start ~last:i
    else
      match s.[i] with
      | ('\t' | '\n') as c ->
          add_text b s ~first:start ~last:i;
          Buffer.add_string b (if c = '\t' then "Tab\n" else "Enter\n");
          go (i + 1) (i + 1)
      | _ -> go start (i + 1)
  in
  go 0 0

let add_line b s =
  Buffer.add_string b s;
  Buffer.add_char b '\n'

let add_ms b n =
  Buffer.add_string b (Int.to_string n);
  Buffer.add_string b "ms"

let rec add_action ~hidden b = function
  | Type s -> add_type b s
  | Key k -> add_line b (key_name k)
  | Sleep (Ms n) ->
      Buffer.add_string b "Sleep ";
      add_ms b n;
      Buffer.add_char b '\n'
  | Wait (timeout, w) ->
      Buffer.add_string b (wait_name w);
      Option.iter
        (fun (Ms n) ->
          Buffer.add_char b '@';
          add_ms b n)
        timeout;
      (match w with
      | Prompt -> ()
      | Line re | Screen re ->
          Buffer.add_char b ' ';
          Regexp_syntax.write b re);
      Buffer.add_char b '\n'
  | Hidden actions when hidden -> List.iter (add_action ~hidden b) actions
  | Hidden actions when List.for_all is_empty actions -> ()
  | Hidden actions ->
      Buffer.add_string b "Hide\n";
      List.iter (add_action ~hidden:true b) actions;
      Buffer.add_string b "Show\n"
  | Raw line -> add_line b line

let setting_line = function
  | Shell sh -> ("Shell", "\"" ^ shell_name sh ^ "\"")
  | Font_size n -> ("FontSize", Int.to_string n)
  | Font_family f -> ("FontFamily", f)
  | Width n -> ("Width", Int.to_string n)
  | Height n -> ("Height", Int.to_string n)
  | Columns n -> ("Columns", Int.to_string n)
  | Rows n -> ("Rows", Int.to_string n)
  | Theme t -> ("Theme", t)
  | Padding n -> ("Padding", Int.to_string n)
  | Typing_speed (Ms n) -> ("TypingSpeed", Int.to_string n ^ "ms")
  | Framerate n -> ("Framerate", Int.to_string n)

let add_setting b s =
  let name, value = setting_line s in
  Buffer.add_string b "Set ";
  Buffer.add_string b name;
  Buffer.add_char b ' ';
  (match s with
  | Font_family _ | Theme _ -> add_string b value
  | _ -> Buffer.add_string b value);
  Buffer.add_char b '\n'

let to_string t =
  let b = Buffer.create 4096 in
  List.iter
    (fun path ->
      Buffer.add_string b "Output ";
      add_string b path;
      Buffer.add_char b '\n')
    (List.filter (Fun.negate is_svg) t.outputs);
  List.iter (add_setting b) t.settings;
  List.iter (add_action ~hidden:false b) t.actions;
  Buffer.contents b

let pp ppf t = Fmt.string ppf (to_string t)
let shells = [ Bash; Zsh; Fish; Powershell; Pwsh; Cmd; Nushell; Osh; Xonsh ]

let setting_of_line line name arg =
  let int f =
    match Tape_syntax.atoi arg with
    | Some n
      when Int64.compare n (Int64.of_int max_int) <= 0
           && Int64.compare n (Int64.of_int min_int) >= 0 ->
        Ok (Some (f (Int64.to_int n)))
    | _ -> Fmt.error "%a: out of range" Quote.pp line
  in
  match name with
  | "Shell" -> (
      match List.find_opt (fun sh -> shell_name sh = arg) shells with
      | Some sh -> Ok (Some (Shell sh))
      | None -> Fmt.error "%a: not a shell VHS starts" Quote.pp line)
  | "FontSize" -> int (fun n -> Font_size n)
  | "FontFamily" -> Ok (Some (Font_family arg))
  | "Width" -> int (fun n -> Width n)
  | "Height" -> int (fun n -> Height n)
  | "Columns" -> int (fun n -> Columns n)
  | "Rows" -> int (fun n -> Rows n)
  | "Theme" -> Ok (Some (Theme arg))
  | "Padding" -> int (fun n -> Padding n)
  | "Framerate" -> int (fun n -> Framerate n)
  | "TypingSpeed" -> (
      let d =
        match Tape_syntax.tokens line with
        | [ _; _; Num n; Keyword (("ms" | "s" | "m") as u) ] -> Some (n ^ u)
        | [ _; _; Num n ] -> Some (n ^ "s")
        | _ -> None
      in
      match Option.bind d Tape_syntax.duration with
      | Some ns when Int64.rem ns 1_000_000L = 0L ->
          Ok (Some (Typing_speed (Ms (Int64.to_int (Int64.div ns 1_000_000L)))))
      | _ -> Ok None)
  | _ -> Ok None

let lines src =
  List.map
    (fun l ->
      if String.ends_with ~suffix:"\r" l then
        String.sub l 0 (String.length l - 1)
      else l)
    (String.split_on_char '\n' src)

type reading = {
  mutable read_settings : setting list;
  mutable read_outputs : string list;
  mutable read_actions : action list;
  mutable hidden : action list option;
  mutable started : bool;
  mutable printed : bool;
}

let add r a =
  match r.hidden with
  | Some h -> r.hidden <- Some (a :: h)
  | None -> r.read_actions <- a :: r.read_actions

let show r =
  Option.iter
    (fun h -> r.read_actions <- Hidden (List.rev h) :: r.read_actions)
    r.hidden;
  r.hidden <- None

let is_env line =
  match Tape_syntax.tokens line with Keyword "Env" :: _ -> true | _ -> false

(* [started] ends the settings, as in VHS; [printed], the outputs. *)
let read_line r n line =
  let raw ~started ~printed =
    add r (Raw line);
    r.started <- r.started || started;
    r.printed <- r.printed || printed;
    Ok ()
  in
  match Tape_syntax.check line with
  | Error e -> Fmt.error "line %d: %a: %s" n Quote.pp line e
  | Ok (Output p) when not r.printed ->
      r.read_outputs <- p :: r.read_outputs;
      Ok ()
  | Ok (Set (name, arg)) when not r.started -> (
      match setting_of_line line name arg with
      | Error e -> Fmt.error "line %d: %s" n e
      | Ok (Some s) ->
          r.read_settings <- s :: r.read_settings;
          Ok ()
      | Ok None -> raw ~started:false ~printed:false)
  | Ok Hide ->
      if r.hidden = None then r.hidden <- Some [];
      r.started <- true;
      r.printed <- true;
      Ok ()
  | Ok Show ->
      show r;
      Ok ()
  | Ok (Blank | Set _ | Require | Output _) -> raw ~started:false ~printed:false
  | Ok Other when is_env line -> raw ~started:true ~printed:false
  | Ok (Screenshot _ | Other) -> raw ~started:true ~printed:true

let of_string src =
  let r =
    {
      read_settings = [];
      read_outputs = [];
      read_actions = [];
      hidden = None;
      started = false;
      printed = false;
    }
  in
  let rec go n = function
    | [] -> Ok ()
    | line :: rest ->
        let* () = read_line r n line in
        go (n + 1) rest
  in
  match go 1 (lines src) with
  | Error msg -> Error (Invalid msg)
  | Ok () ->
      show r;
      let actions =
        match r.read_actions with Raw "" :: rest -> rest | actions -> actions
      in
      tape ~settings:(List.rev r.read_settings)
        ~outputs:(List.rev r.read_outputs) (List.rev actions)
      |> Result.map (fun t -> { t with source = Some (lines src) })

let replaces s s' =
  match (s, s') with
  | (Width _ | Columns _), (Width _ | Columns _)
  | (Height _ | Rows _), (Height _ | Rows _) ->
      true
  | _ -> fst (setting_line s) = fst (setting_line s')

let with_settings settings t =
  let kept =
    List.filter
      (fun s' -> not (List.exists (fun s -> replaces s s') settings))
      t.settings
  in
  tape ~settings:(kept @ settings) ~outputs:t.outputs t.actions
  |> Result.map (fun t' -> { t' with source = t.source })

let with_outputs outputs t =
  tape ~settings:t.settings ~outputs t.actions
  |> Result.map (fun t' -> { t' with source = t.source })

let utf_8 s =
  let b = Buffer.create (String.length s) in
  let rec go i =
    if i < String.length s then (
      let d = String.get_utf_8_uchar s i in
      let u = Uchar.utf_decode_uchar d in
      let c = Uchar.to_int u in
      if c < 0x80 || c > 0x9f then Buffer.add_utf_8_uchar b u;
      go (i + Uchar.utf_decode_length d))
  in
  go 0;
  Unicode.strip_format (Buffer.contents b)

(* A terminal would run the escape sequences of what the report prints. *)
let printable s =
  let n = String.length s in
  let b = Buffer.create n in
  let c1 i =
    let d = String.get_utf_8_uchar s i in
    match Uchar.to_int (Uchar.utf_decode_uchar d) with
    | c when c >= 0x80 && c <= 0x9f ->
        Some (Char.chr (c - 0x40), i + Uchar.utf_decode_length d)
    | _ -> None
  in
  let rec text i =
    if i < n then
      match (s.[i], c1 i) with
      | '\027', _ when i + 1 < n -> escape s.[i + 1] (i + 2)
      | _, Some (f, j) -> escape f j
      | ('\r' | '\n'), _ ->
          Buffer.add_char b '\n';
          text (i + 1)
      | '\t', _ ->
          Buffer.add_char b ' ';
          text (i + 1)
      | c, _ when c < ' ' || c = '\x7f' -> text (i + 1)
      | c, _ ->
          Buffer.add_char b c;
          text (i + 1)
  and escape f i =
    match f with
    | '[' -> csi i
    | ']' | 'P' | 'X' | '^' | '_' -> control_string i
    | ' ' .. '/' -> escape_final i
    | _ -> text i
  and csi i =
    if i < n then
      match s.[i] with '@' .. '~' -> text (i + 1) | _ -> csi (i + 1)
  and escape_final i =
    if i < n then
      match s.[i] with ' ' .. '/' -> escape_final (i + 1) | _ -> text (i + 1)
  (* ST is ESC \ or its C1 form. *)
  and control_string i =
    if i < n then
      match (s.[i], c1 i) with
      | '\007', _ -> text (i + 1)
      | '\027', _ when i + 1 < n && s.[i + 1] = '\\' -> text (i + 2)
      | _, Some ('\\', j) -> text j
      | _ -> control_string (i + 1)
  in
  text 0;
  utf_8 (Buffer.contents b)

let last_lines = 10
let one_line s = String.concat " " (String.split_on_char '\n' (printable s))

let pp_last ppf = function
  | "" -> ()
  | last -> Fmt.pf ppf "; last value was: %s" (one_line last)

(* Eio's statuses carry OCaml's signal numbers; one OCaml has no system
   number for is printed as it is. *)
let signal_number n = try Sys.signal_to_int n with Invalid_argument _ -> n

let pp_error ppf = function
  | Executable_not_found exe ->
      Fmt.pf ppf "executable not found: %s" (one_line exe)
  | Failed { vhs; status; output } ->
      let vhs = one_line vhs in
      (match status with
      | `Exited n -> Fmt.pf ppf "%s exited with code %d" vhs n
      | `Signaled n ->
          Fmt.pf ppf "%s was killed by signal %d" vhs (signal_number n));
      let lines =
        String.split_on_char '\n' (printable output)
        |> List.map String.trim
        |> List.filter (( <> ) "")
      in
      List.iter (Fmt.pf ppf "@\n  %s")
        (List.drop (max 0 (List.length lines - last_lines)) lines)
  | Io msg -> Fmt.string ppf (one_line msg)
  | Unsupported why -> Fmt.string ppf (one_line why)
  | Timeout { wait; line; last } ->
      Fmt.pf ppf
        "timed out waiting for line %d, %s%a; give the wait a longer ~timeout \
         or the tape a Set WaitTimeout"
        line (one_line wait) pp_last last
  | Exited { status; wait; line; last } ->
      (match status with
      | `Exited n -> Fmt.pf ppf "the shell exited (status %d)" n
      | `Signaled n ->
          Fmt.pf ppf "the shell was killed by signal %d" (signal_number n));
      Fmt.pf ppf " while waiting for line %d, %s%a" line (one_line wait) pp_last
        last
  | Invalid msg -> Fmt.string ppf (one_line msg)

let fs_message path = function
  | Eio.Fs.Not_found _ -> path ^ ": no such file or directory"
  | Eio.Fs.Permission_denied _ -> path ^ ": permission denied"
  | Eio.Fs.Already_exists _ -> path ^ ": already exists"
  | Eio.Fs.Not_native msg -> path ^ ": " ^ msg
  | e -> Fmt.str "%s: %a" path Eio.Exn.pp_err (Eio.Fs.E e)

let io_message ~cwd = function
  | Eio.Process.E (Permission_denied exe) -> exe ^ ": permission denied"
  | Eio.Process.E (Executable_format_error exe) -> exe ^ ": not an executable"
  | Eio.Fs.E e ->
      fs_message (Option.value (Eio.Path.native cwd) ~default:(snd cwd)) e
  | err -> Fmt.str "%a" Eio.Exn.pp_err err

(* vhs uploads if [VHS_PUBLISH] is true, go-rod reads [rod], and an
   inherited [PS1], [PROMPT] or [PROMPT_COMMAND] replaces the prompt that
   [Prompt] waits for. *)
let cleared = [ "VHS_PUBLISH"; "rod"; "PS1"; "PROMPT"; "PROMPT_COMMAND" ]
let grace = 10.

let environment proc =
  Eio.Process.environment proc
  |> Eio.Process.Env.override (List.map (fun v -> (v, None)) cleared)

let run_vhs ~vhs ~clock ~proc ~cwd ~cancelled ~sink tape =
  Eio.Switch.run @@ fun sw ->
  let stdin, feed = Eio.Process.pipe ~sw proc in
  let env = environment proc in
  let child =
    Eio.Process.spawn ~sw proc ~env
      ~cwd:(cwd :> Eio.Fs.dir_ty Eio.Path.t)
      ~stdin ~stdout:sink ~stderr:sink [ vhs; "-q" ]
  in
  Eio.Flow.close stdin;
  Eio.Fiber.fork ~sw (fun () ->
      (try Eio.Flow.copy_string tape feed with Eio.Io _ -> ());
      Eio.Flow.close feed);
  Eio.Fiber.first
    (fun () -> Eio.Process.await child)
    (fun () ->
      Eio.Promise.await cancelled;
      Eio.Process.signal child Sys.sigterm;
      Eio.Time.Mono.sleep clock grace;
      Eio.Process.signal child Sys.sigkill;
      Eio.Process.await child)

module Tail = struct
  type t = Buffer.t

  let kept = 65536

  let single_write b bufs =
    List.iter (fun c -> Buffer.add_string b (Cstruct.to_string c)) bufs;
    if Buffer.length b > 2 * kept then (
      let tail = Buffer.sub b (Buffer.length b - kept) kept in
      Buffer.clear b;
      Buffer.add_string b tail);
    Cstruct.lenv bufs

  let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src

  let contents b =
    let n = Buffer.length b in
    if n <= kept then Buffer.contents b else Buffer.sub b (n - kept) kept
end

let render_vhs ?stop ~vhs ~clock ~proc ~cwd t =
  let output = Buffer.create 256 in
  let sink = Eio.Resource.T (output, Eio.Flow.Pi.sink (module Tail)) in
  let tape = to_string t in
  let cancelled, cancel = Eio.Promise.create () in
  let finished, finish = Eio.Promise.create () in
  (* Protected, so that [vhs] is reaped even when the caller is cancelled. *)
  let supervise () =
    Eio.Cancel.protect @@ fun () ->
    Eio.Promise.resolve finish
      (match run_vhs ~vhs ~clock ~proc ~cwd ~cancelled ~sink tape with
      | status -> Ok status
      | exception (Eio.Io _ as e) -> Error e)
  in
  let wait () =
    let stopped () =
      match stop with
      | None -> Eio.Fiber.await_cancel ()
      | Some p -> Eio.Promise.await p
    in
    match
      Eio.Fiber.first
        (fun () -> Some (Eio.Promise.await finished))
        (fun () ->
          stopped ();
          None)
    with
    | Some outcome -> outcome
    | None ->
        Eio.Promise.resolve cancel ();
        Eio.Promise.await finished
    | exception (Eio.Cancel.Cancelled _ as e) ->
        ignore (Eio.Promise.try_resolve cancel ());
        raise e
  in
  match
    Eio.Switch.run @@ fun sw ->
    Eio.Fiber.fork ~sw supervise;
    wait ()
  with
  | Ok (`Exited 0) -> Ok ()
  | Ok status -> Error (Failed { vhs; status; output = Tail.contents output })
  | Error (Eio.Io (Eio.Process.E (Executable_not_found exe), _)) ->
      Error (Executable_not_found exe)
  | Error (Eio.Io (err, _)) -> Error (Io (io_message ~cwd err))
  | Error e -> raise e

let refuse ~by ~other kinds t =
  match
    List.find_opt
      (fun p -> List.mem (kind p) kinds)
      (t.outputs @ raw_outputs t.actions)
  with
  | None -> Ok ()
  | Some p ->
      let what =
        match kind p with
        | "frames" -> "PNG frames"
        | "SVG" -> "an SVG image"
        | "MP4" -> "an MP4 video"
        | "WebM" -> "a WebM video"
        | k -> "a " ^ k
      in
      err_unsupported "not recorded %s: Output %a: %s, which K7.%s records" by
        Quote.pp p what other

let in_dir dir path =
  if Filename.is_relative path then Filename.concat dir path else path

let dir_problem ~create dir path =
  let file = in_dir dir path in
  let shown d =
    let k = String.length file - String.length path in
    if k >= 0 && k <= String.length d && String.length d > k then
      String.sub d k (String.length d - k)
    else d
  in
  let rec nearest d =
    let up = Filename.dirname d in
    if Sys.file_exists d || up = d then d else nearest up
  in
  let parent = Filename.dirname file in
  let d = if create then nearest parent else parent in
  match Unix.stat d with
  | { st_kind = S_DIR; _ } -> (
      match Unix.access d [ W_OK; X_OK ] with
      | () -> None
      | exception Unix.Unix_error _ ->
          Fmt.kstr Option.some "%s: cannot write in %s" path (shown d))
  | _ -> Fmt.kstr Option.some "%s: %s is not a directory" path (shown d)
  | exception Unix.Unix_error _ ->
      Fmt.kstr Option.some "%s: no directory %s" path (shown d)

let check_dirs cwd paths =
  match Eio.Path.native cwd with
  | None | (exception Failure _) -> Ok ()
  | Some dir -> (
      match
        List.find_map (fun (create, p) -> dir_problem ~create dir p) paths
      with
      | None -> Ok ()
      | Some msg -> Error (Io msg))

let mkdirs dir path =
  let file = in_dir dir path in
  let rec go d =
    if not (Sys.file_exists d || Filename.dirname d = d) then (
      go (Filename.dirname d);
      try Unix.mkdir d 0o755 with Unix.Unix_error (EEXIST, _, _) -> ())
  in
  go (Filename.dirname file)

(* VHS creates the directories of its outputs, but not those of PNG frames
   or screenshots. *)
let creates path = kind path <> "frames"
let all_outputs t = t.outputs @ raw_outputs t.actions

let check_has_output t =
  if all_outputs t = [] then
    Error (Invalid "no output: the tape names no file to write")
  else Ok ()

let render ?stop ?(vhs = "vhs") ~clock ~proc ~cwd t =
  let* () = check_has_output t in
  let* () = refuse ~by:"by vhs" ~other:"record" [ "SVG" ] t in
  let* () =
    match
      List.find_opt
        (fun p -> Tape_syntax.extension p = ".svg")
        (raw_screenshots t.actions)
    with
    | None -> Ok ()
    | Some p ->
        err_unsupported
          "not recorded by vhs: Screenshot %a: an SVG image, which K7.record \
           takes"
          Quote.pp p
  in
  let* () =
    check_dirs cwd
      (List.map (fun p -> (creates p, p)) (t.outputs @ raw_outputs t.actions)
      @ List.map (fun p -> (false, p)) (raw_screenshots t.actions))
  in
  render_vhs ?stop ~vhs ~clock ~proc ~cwd t

(* The [n]th line of [to_string t] is the [k]th of its text there, and so
   in the source of [of_string]: an action's lines keep their text and order. *)
let source_line tape n =
  match tape with
  | Some ({ source = Some src; _ } as t) -> (
      let printed = String.split_on_char '\n' (to_string t) in
      match List.nth_opt printed (n - 1) with
      | None -> n
      | Some l ->
          let rec find i k = function
            | [] -> n
            | l' :: rest when l' = l ->
                if k = 1 then i else find (i + 1) (k - 1) rest
            | _ :: rest -> find (i + 1) k rest
          in
          find 1 (List.length (List.filter (( = ) l) (List.take n printed))) src
      )
  | _ -> n

let native_error ?(verb = "recorded") ?tape : Native.error -> error = function
  | Unsupported why -> Unsupported ("not " ^ verb ^ " natively: " ^ why)
  | Timeout { wait; line; last } ->
      Timeout { wait; line = source_line tape line; last }
  | Exited { status; wait; line; last } ->
      Exited { status; wait; line = source_line tape line; last }
  | Executable_not_found exe -> Executable_not_found exe
  | Io msg -> Io msg
  | Invalid msg -> Invalid msg

let native ?verb ?source lines =
  Result.map_error (native_error ?verb) (Native.of_lines ?source lines)

(* As under vhs, paths are relative to [dir], not confined to [cwd]. *)
let with_file dir path flags f =
  let file = in_dir dir path in
  let error e =
    Error (path ^ ": " ^ String.uncapitalize_ascii (Unix.error_message e))
  in
  match Unix.openfile file (O_CLOEXEC :: flags) 0o644 with
  | exception Unix.Unix_error (e, _, _) -> error e
  | fd -> (
      match Fun.protect ~finally:(fun () -> Unix.close fd) (fun () -> f fd) with
      | x -> Ok x
      | exception Unix.Unix_error (e, _, _) -> error e)

let max_load = 10 * 1024 * 1024

(* Opened non-blocking, so a FIFO is refused rather than waited on. *)
let load dir path : (string, Native.error) result =
  let too_large = Error (Native.Invalid (path ^ ": larger than 10 MiB")) in
  let read fd =
    let stat = Unix.fstat fd in
    match stat.st_kind with
    | S_REG when stat.st_size > max_load -> too_large
    | S_REG ->
        let b = Buffer.create 4096 and chunk = Bytes.create 65536 in
        let rec go () =
          if Buffer.length b > max_load then too_large
          else
            match Unix.read fd chunk 0 (Bytes.length chunk) with
            | 0 -> Ok (Buffer.contents b)
            | n ->
                Buffer.add_subbytes b chunk 0 n;
                go ()
        in
        go ()
    | _ -> Error (Native.Invalid (path ^ ": not a regular file"))
  in
  match with_file dir path [ O_RDONLY; O_NONBLOCK ] read with
  | Ok r -> r
  | Error msg -> Error (Native.Io msg)

let save dir path contents =
  with_file dir path [ O_WRONLY; O_CREAT; O_TRUNC ] @@ fun fd ->
  let rec go off =
    if off < String.length contents then
      go
        (off
        + Unix.write_substring fd contents off (String.length contents - off))
  in
  go 0

let record ?stop ~clock ~proc ~cwd t =
  let* () = check_has_output t in
  let* () =
    refuse ~by:"natively" ~other:"render" [ "GIF"; "MP4"; "WebM"; "frames" ] t
  in
  let* dir =
    Option.to_result ~none:(Io "the directory is not a native path")
      (Eio.Path.native cwd)
  in
  let* tape =
    native ~source:(load dir) (String.split_on_char '\n' (to_string t))
  in
  let* () =
    Result.map_error
      (fun msg -> Invalid msg)
      (check_screenshots
         (t.outputs @ raw_outputs t.actions)
         (Native.stills tape))
  in
  let svgs = List.filter is_svg t.outputs
  and texts = Option.to_list (Native.text_output tape) in
  let* () =
    check_dirs cwd
      (List.map (fun p -> (true, p)) (svgs @ texts)
      @ List.map (fun p -> (false, p)) (Native.stills tape))
  in
  let* { svg; text; stills } =
    Result.map_error
      (native_error ~verb:"recorded" ~tape:t)
      (Native.play ?interrupt:stop ~clock ~env:(environment proc) ~cwd:dir tape)
  in
  let save ?(create = false) contents path =
    match if create then mkdirs dir path with
    | () -> Result.map_error (fun msg -> Io msg) (save dir path contents)
    | exception Unix.Unix_error (e, _, _) ->
        Error
          (Io (path ^ ": " ^ String.uncapitalize_ascii (Unix.error_message e)))
  in
  let* () = iter_result (save ~create:true svg) svgs in
  let* () = iter_result (save ~create:true text) texts in
  iter_result (fun (path, s) -> save s path) stills

let needs_vhs t =
  List.exists
    (fun p -> List.mem (kind p) [ "GIF"; "MP4"; "WebM"; "frames" ])
    (t.outputs @ raw_outputs t.actions)
  || List.exists
       (fun p -> Tape_syntax.extension p <> ".svg")
       (raw_screenshots t.actions)

let play ?stop ?vhs ~clock ~proc ~cwd t =
  if needs_vhs t then render ?stop ?vhs ~clock ~proc ~cwd t
  else record ?stop ~clock ~proc ~cwd t

let check_times output =
  let rec go prev = function
    | [] -> Ok ()
    | (Ms ms, _) :: rest ->
        let* () = check_ms "time " ms in
        if ms < prev then Fmt.error "time %dms: before %dms" ms prev
        else go ms rest
  in
  Result.map_error (fun msg -> Invalid msg) (go 0 output)

let terminal settings =
  let* () =
    Result.map_error
      (fun msg -> Invalid msg)
      (let* () = iter_result check_setting settings in
       let* () = check_cells settings in
       check_size settings [])
  in
  let b = Buffer.create 256 in
  List.iter (function Shell _ -> () | s -> add_setting b s) settings;
  native ~verb:"drawn" (String.split_on_char '\n' (Buffer.contents b))

let svg ?(settings = []) output =
  let* tape = terminal settings in
  let* () = check_times output in
  Ok (Native.replay tape (List.map (fun (Ms ms, s) -> (ms, s)) output))

let check_env env =
  let bad (k, v) =
    match Tape_syntax.env_name_problem k with
    | Some why -> Some why
    | None when String.contains v '\000' -> Some "a NUL byte"
    | None -> None
  in
  match
    List.find_map (fun kv -> Option.map (fun why -> (kv, why)) (bad kv)) env
  with
  | None -> Ok ()
  | Some ((k, v), why) -> err_invalid "env %a: %s" Quote.pp (k ^ "=" ^ v) why

let is_c1 s i =
  s.[i] = '\xc2'
  && i + 1 < String.length s
  && Char.code s.[i + 1] < 0xa0
  && Char.code s.[i + 1] >= 0x80

let ansi_c_quote s =
  let b = Buffer.create (String.length s + 3) in
  Buffer.add_string b "$'";
  String.iteri
    (fun i c ->
      match c with
      | '\'' | '\\' ->
          Buffer.add_char b '\\';
          Buffer.add_char b c
      | '\n' -> Buffer.add_string b {|\n|}
      | '\t' -> Buffer.add_string b {|\t|}
      | c
        when Char.code c < 0x20
             || c = '\x7f' || is_c1 s i
             || (i > 0 && is_c1 s (i - 1)) ->
          Buffer.add_string b {|\x|};
          Buffer.add_char b "0123456789ABCDEF".[Char.code c lsr 4];
          Buffer.add_char b "0123456789ABCDEF".[Char.code c land 15]
      | c -> Buffer.add_char b c)
    s;
  Buffer.add_char b '\'';
  Buffer.contents b

let shell_word s =
  let safe c =
    Char.Ascii.is_alphanum c
    || String.contains "_@%+=:,./-" c
    || Char.code c >= 0x80
  in
  if has_control s then ansi_c_quote s
  else if s <> "" && String.for_all safe s then s
  else "'" ^ String.concat {|'\''|} (String.split_on_char '\'' s) ^ "'"

let command_line = function
  | "" -> ""
  | show -> "\027[38;2;90;86;224m$ \027[0m" ^ show ^ "\r\n"

let run ?(settings = []) ?(env = []) ?show ?stop ~clock ~proc ~cwd argv =
  let* () = if argv = [] then Error (Invalid "no command to run") else Ok () in
  let* () = check_env env in
  let show =
    Option.value show ~default:(String.concat " " (List.map shell_word argv))
  in
  let* () =
    if has_control show then
      err_invalid "show %a: holds a control character" Quote.pp show
    else Ok ()
  in
  let* tape = terminal settings in
  let* dir =
    Option.to_result ~none:(Io "the directory is not a native path")
      (Eio.Path.native cwd)
  in
  Result.map_error native_error
    (Native.run ?interrupt:stop ~clock ~env:(environment proc) ~vars:env
       ~cwd:dir ~line:(command_line show) tape argv)
