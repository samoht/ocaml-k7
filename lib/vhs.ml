type duration = Ms of int | S of float

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

type setting =
  | Shell of string
  | Font_size of int
  | Font_family of string
  | Width of int
  | Height of int
  | Theme of string
  | Padding of int
  | Typing_speed of duration
  | Framerate of int
  | Custom of string * string

type wait = Prompt | Line of string | Screen of string
type item = string

let invalid fn fmt =
  Fmt.kstr (fun msg -> invalid_arg ("Vhs." ^ fn ^ ": " ^ msg)) fmt

let quotes = [ '"'; '\''; '`' ]
let is_quote c = List.mem c quotes
let is_control c = Char.code c < 0x20 || Char.code c = 0x7f

(* A VHS string has no escapes and ends at a newline. *)
let line fn s =
  if String.exists is_control s then invalid fn "%S holds a control character" s;
  s

let quoted fn s =
  match List.find_opt (fun q -> not (String.contains s q)) quotes with
  | Some q -> Fmt.str "%c%s%c" q (line fn s) q
  | None -> invalid fn "%S holds all three VHS quote characters" s

let word fn s =
  if s = "" || String.contains s ' ' then invalid fn "%S is not a single word" s;
  line fn s

let natural fn n =
  if n < 0 then invalid fn "%d is negative" n;
  string_of_int n

(* [quotable_chunks s] cuts [s] so that no chunk holds all three quotes. *)
let quotable_chunks s =
  let chunks = ref [] and start = ref 0 and seen = ref [] in
  String.iteri
    (fun i c ->
      if is_quote c && not (List.mem c !seen) then
        if List.length !seen = 2 then begin
          chunks := String.sub s !start (i - !start) :: !chunks;
          start := i;
          seen := [ c ]
        end
        else seen := c :: !seen)
    s;
  List.rev (String.sub s !start (String.length s - !start) :: !chunks)

let typeable s =
  not (String.exists (fun c -> is_control c && c <> '\n' && c <> '\t') s)

(* [type_lines s] are the VHS commands typing [s]: text runs become [Type]
   commands and newlines and tabs become key presses. *)
let type_lines s =
  if not (typeable s) then invalid "type'" "%S holds a control character" s;
  let lines = ref [] and start = ref 0 in
  let text stop =
    if stop > !start then
      let chunks = quotable_chunks (String.sub s !start (stop - !start)) in
      List.iter
        (fun c -> lines := ("Type " ^ quoted "type'" c) :: !lines)
        chunks
  in
  String.iteri
    (fun i c ->
      if c = '\n' || c = '\t' then begin
        text i;
        lines := (if c = '\n' then "Enter" else "Tab") :: !lines;
        start := i + 1
      end)
    s;
  text (String.length s);
  match !lines with [] -> [ {|Type ""|} ] | lines -> List.rev lines

(* VHS reads a plain decimal: no exponent, no sign. *)
let decimal s =
  let s = Fmt.str "%.9f" (Float.abs s) in
  let rec last i =
    match s.[i] with '0' -> last (i - 1) | '.' -> i - 1 | _ -> i
  in
  String.sub s 0 (last (String.length s - 1) + 1)

let duration fn = function
  | Ms n -> natural fn n ^ "ms"
  | S s when Float.is_nan s || s < 0. || s = Float.infinity ->
      invalid fn "invalid duration %gs" s
  | S s -> decimal s ^ "s"

let is_letter = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false

let chord name valid c =
  if not (is_letter c || String.contains valid c) then
    invalid "key" "%s+%C is not a VHS key" name c;
  Fmt.str "%s+%c" name c

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
  | Ctrl c -> chord "Ctrl" {|@[\]^-.|} c
  | Alt c -> chord "Alt" "[]." c

let setting = function
  | Shell shell -> "Shell " ^ quoted "set" shell
  | Font_size n -> "FontSize " ^ natural "set" n
  | Font_family font -> "FontFamily " ^ quoted "set" font
  | Width n -> "Width " ^ natural "set" n
  | Height n -> "Height " ^ natural "set" n
  | Theme theme -> "Theme " ^ quoted "set" theme
  | Padding n -> "Padding " ^ natural "set" n
  | Typing_speed d -> "TypingSpeed " ^ duration "set" d
  | Framerate n -> "Framerate " ^ natural "set" n
  | Custom (name, value) -> word "set" name ^ " " ^ line "set" value

(* A slash ends a VHS regexp and only VHS 0.11 and later read [\/] as an
   escape, so a slash, escaped or not, is written as RE2's [\x2f]. *)
let regexp re =
  let n = String.length re in
  let b = Buffer.create (n + 8) in
  let rec go i =
    if i < n then
      match re.[i] with
      | '/' ->
          Buffer.add_string b {|\x2f|};
          go (i + 1)
      | '\\' when i + 1 = n -> invalid "wait" "%S ends with a backslash" re
      | '\\' when re.[i + 1] = '/' ->
          Buffer.add_string b {|\x2f|};
          go (i + 2)
      | '\\' ->
          Buffer.add_string b (String.sub re i 2);
          go (i + 2)
      | c ->
          Buffer.add_char b c;
          go (i + 1)
  in
  go 0;
  "/" ^ line "wait" (Buffer.contents b) ^ "/"

let has_extension path =
  match String.rindex_opt path '.' with
  | Some i ->
      i < String.length path - 1 && not (String.contains_from path i '/')
  | None -> false

let comment s = if s = "" then "#" else "# " ^ line "comment" s
let blank () = ""
let require program = "Require " ^ quoted "require" program

let output path =
  if not (has_extension path || String.ends_with ~suffix:"/" path) then
    invalid "output" "%S has no extension and does not end with a slash" path;
  "Output " ^ quoted "output" path

let is_env_name s =
  s <> ""
  && (not (s.[0] >= '0' && s.[0] <= '9'))
  && String.for_all
       (function
         | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true | _ -> false)
       s

let env name value =
  if not (is_env_name name) then
    invalid "env" "%S is not an environment variable name" name;
  "Env " ^ name ^ " " ^ quoted "env" value

let set s = "Set " ^ setting s
let type' s = String.concat "\n" (type_lines s)
let key k = key_name k
let enter () = "Enter"
let sleep d = "Sleep " ^ duration "sleep" d

let wait ?timeout w =
  let timeout =
    Option.fold ~none:"" ~some:(fun d -> "@" ^ duration "wait" d) timeout
  in
  match w with
  | Prompt -> "Wait" ^ timeout
  | Line re -> "Wait+Line" ^ timeout ^ " " ^ regexp re
  | Screen re -> "Wait+Screen" ^ timeout ^ " " ^ regexp re

let hide () = "Hide"
let show () = "Show"

let screenshot path =
  if not (String.ends_with ~suffix:".png" path) then
    invalid "screenshot" "%S does not end with .png" path;
  "Screenshot " ^ quoted "screenshot" path

let source path = "Source " ^ quoted "source" path
let raw s = line "raw" s
let pp = Fmt.(list ~sep:nop (string ++ any "\n"))

module Cram = struct
  type t = string list list

  let err_control l =
    Fmt.kstr (fun e -> Error e) "%S holds a control character" l

  let drop n s = String.sub s n (String.length s - n)

  let parse src =
    let chomp l =
      let n = String.length l in
      if n > 0 && l.[n - 1] = '\r' then String.sub l 0 (n - 1) else l
    in
    let lines = List.map chomp (String.split_on_char '\n' src) in
    let steps = ref [] in
    let cmd = ref [] in
    let flush () =
      if !cmd <> [] then steps := List.rev !cmd :: !steps;
      cmd := []
    in
    List.iter
      (fun line ->
        if String.starts_with ~prefix:"  $ " line then begin
          flush ();
          cmd := [ drop 4 line ]
        end
        else if !cmd <> [] && String.starts_with ~prefix:"  > " line then
          cmd := drop 4 line :: !cmd
        else flush ())
      lines;
    flush ();
    let steps = List.rev !steps in
    match List.find_opt (fun l -> not (typeable l)) (List.concat steps) with
    | None -> Ok steps
    | Some l -> err_control l

  (* Source the setup scripts, then clear, all hidden: the [clear] must come
     after the [source] lines so their echoes are wiped from the scrollback
     before [Show] resumes capture, otherwise they leak into the recording. *)
  let setup_block = function
    | [] -> []
    | scripts ->
        hide ()
        :: List.concat_map
             (fun s -> [ type' ("source " ^ Filename.quote s); enter () ])
             scripts
        @ [ type' "clear"; enter (); show () ]

  let step_items ~pause step =
    List.concat_map (fun line -> [ type' line; enter () ]) step
    @ [ wait Prompt; sleep pause ]

  let items ?(setup = []) ?(pause = S 1.) steps =
    setup_block setup @ List.concat_map (step_items ~pause) steps
end

type error =
  | Executable_not_found of string
  | Failed of { vhs : string; status : [ `Exited of int | `Signaled of int ] }
  | Io of string

let pp_error ppf = function
  | Executable_not_found exe -> Fmt.pf ppf "executable not found: %s" exe
  | Failed { vhs; status = `Exited n } ->
      Fmt.pf ppf "%s exited with code %d" vhs n
  | Failed { vhs; status = `Signaled n } ->
      Fmt.pf ppf "%s was killed by signal %d" vhs n
  | Io msg -> Fmt.string ppf msg

let render ~proc ?(vhs = "vhs") items =
  let stdin = Fmt.kstr Eio.Flow.string_source "%a" pp items in
  try Ok (Eio.Process.run proc ~stdin [ vhs ]) with
  | Eio.Exn.Io (Eio.Process.E (Executable_not_found exe), _) ->
      Error (Executable_not_found exe)
  | Eio.Exn.Io (Eio.Process.E (Child_error status), _) ->
      Error (Failed { vhs; status })
  | Eio.Exn.Io _ as exn -> Error (Io (Printexc.to_string exn))
