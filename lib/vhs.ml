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

type wait = Prompt | Line of string | Screen of string
type item = string

let invalid fn fmt =
  Fmt.kstr (fun msg -> invalid_arg ("Vhs." ^ fn ^ ": " ^ msg)) fmt

let quotes = [ '"'; '\''; '`' ]
let is_quote c = List.mem c quotes
let is_control c = Char.code c < 0x20 || Char.code c = 0x7f

(* A VHS string has no escapes and ends at a newline. *)
let has_control s = String.exists is_control s

let line fn s =
  if has_control s then invalid fn "%S holds a control character" s;
  s

let quoted fn s =
  match List.find_opt (fun q -> not (String.contains s q)) quotes with
  | Some q -> Fmt.str "%c%s%c" q (line fn s) q
  | None -> invalid fn "%S holds all three VHS quote characters" s

let natural fn n =
  if n < 0 then invalid fn "%d is negative" n;
  string_of_int n

(* [quotable_chunks s] cuts [s] so that no chunk holds all three quotes. *)
let quotable_chunks s =
  let rec cut chunks start seen i =
    let chunk () = String.sub s start (i - start) in
    if i = String.length s then List.rev (chunk () :: chunks)
    else
      let c = s.[i] in
      if (not (is_quote c)) || List.mem c seen then cut chunks start seen (i + 1)
      else if List.length seen = 2 then cut (chunk () :: chunks) i [ c ] (i + 1)
      else cut chunks start (c :: seen) (i + 1)
  in
  cut [] 0 [] 0

let duration fn (Ms n) = natural fn n ^ "ms"
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
let blank = ""
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

let type' s =
  quotable_chunks (line "type'" s)
  |> List.map (fun c -> "Type " ^ quoted "type'" c)
  |> String.concat "\n"

let key k = key_name k
let sleep d = "Sleep " ^ duration "sleep" d

let wait ?timeout w =
  let timeout =
    Option.fold ~none:"" ~some:(fun d -> "@" ^ duration "wait" d) timeout
  in
  match w with
  | Prompt -> "Wait" ^ timeout
  | Line re -> "Wait+Line" ^ timeout ^ " " ^ regexp re
  | Screen re -> "Wait+Screen" ^ timeout ^ " " ^ regexp re

let hide = "Hide"
let show = "Show"

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

  let chomp l =
    if String.ends_with ~suffix:"\r" l then String.sub l 0 (String.length l - 1)
    else l

  let flush steps = function [] -> steps | cmd -> List.rev cmd :: steps

  let add (steps, cmd) line =
    if String.starts_with ~prefix:"  $ " line then
      (flush steps cmd, [ drop 4 line ])
    else if cmd <> [] && String.starts_with ~prefix:"  > " line then
      (steps, drop 4 line :: cmd)
    else (flush steps cmd, [])

  let parse src =
    let lines = List.map chomp (String.split_on_char '\n' src) in
    let steps, cmd = List.fold_left add ([], []) lines in
    let steps = List.rev (flush steps cmd) in
    match List.find_opt has_control (List.concat steps) with
    | None -> Ok steps
    | Some l -> err_control l

  (* Source the setup scripts, then clear, all hidden: the [clear] must come
     after the [source] lines so their echoes are wiped from the scrollback
     before [Show] resumes capture, otherwise they leak into the recording. *)
  let sh_quote s =
    "'" ^ String.concat {|'\''|} (String.split_on_char '\'' s) ^ "'"

  let setup_block = function
    | [] -> []
    | scripts ->
        hide
        :: List.concat_map
             (fun s -> [ type' ("source " ^ sh_quote s); key Enter ])
             scripts
        @ [ type' "clear"; key Enter; show ]

  let step_items ~pause step =
    List.concat_map (fun line -> [ type' line; key Enter ]) step
    @ [ wait Prompt; sleep pause ]

  let items ?(setup = []) ?(pause = Ms 1000) steps =
    setup_block setup @ List.concat_map (step_items ~pause) steps
end

type error =
  | Executable_not_found of string
  | Failed of {
      vhs : string;
      status : [ `Exited of int | `Signaled of int ];
      stderr : string;
    }
  | Io of string

let pp_error ppf = function
  | Executable_not_found exe -> Fmt.pf ppf "executable not found: %s" exe
  | Failed { vhs; status; stderr } ->
      let lines =
        String.split_on_char '\n' stderr
        |> List.map String.trim
        |> List.filter (( <> ) "")
      in
      (match status with
      | `Exited n -> Fmt.pf ppf "%s exited with code %d" vhs n
      | `Signaled n -> Fmt.pf ppf "%s was killed by signal %d" vhs n);
      if lines <> [] then Fmt.pf ppf ": %s" (String.concat " " lines)
  | Io msg -> Fmt.string ppf msg

let io_message = function
  | Eio.Process.E (Permission_denied exe) -> exe ^ ": permission denied"
  | Eio.Process.E (Executable_format_error exe) -> exe ^ ": not an executable"
  | err -> Fmt.str "%a" Eio.Exn.pp_err err

let render ~proc ?(vhs = "vhs") items =
  let stdin = Fmt.kstr Eio.Flow.string_source "%a" pp items in
  let stderr = Buffer.create 256 in
  try
    Ok
      (Eio.Process.run proc ~stdin
         ~stderr:(Eio.Flow.buffer_sink stderr)
         [ vhs ])
  with
  | Eio.Exn.Io (Eio.Process.E (Executable_not_found exe), _) ->
      Error (Executable_not_found exe)
  | Eio.Exn.Io (Eio.Process.E (Child_error status), _) ->
      Error (Failed { vhs; status; stderr = Buffer.contents stderr })
  | Eio.Exn.Io (err, _) -> Error (Io (io_message err))
