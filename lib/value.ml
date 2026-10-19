type duration = Ms of int | S of float

type key =
  | Enter
  | Tab
  | Space
  | Backspace
  | Escape
  | Left
  | Right
  | Up
  | Down
  | Ctrl of string
  | Alt of string
  | Ctrl_alt of string
  | Raw_key of string

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

type wait = Wait of string | Wait_screen of string | Wait_line of string

type command =
  | Type of string
  | Key of key
  | Sleep of duration
  | Wait_for of wait
  | Hide
  | Show
  | Screenshot of string option
  | Source of string
  | Raw_command of string

type item =
  | Comment of string
  | Blank
  | Require of string
  | Output of string
  | Env of string * string
  | Set of setting
  | Command of command
  | Raw_line of string

type t = item list

let tape items = items
let items t = t
let comment s = Comment s
let blank () = Blank
let require s = Require s
let output s = Output s
let env k v = Env (k, v)
let set s = Set s
let command c = Command c
let raw_line s = Raw_line s
let type' s = Command (Type s)
let key k = Command (Key k)
let enter () = key Enter
let sleep d = Command (Sleep d)
let wait w = Command (Wait_for w)
let hide () = Command Hide
let show () = Command Show
let screenshot ?path () = Command (Screenshot path)
let source path = Command (Source path)
let raw_command s = Command (Raw_command s)
let quotes = [ '"'; '\''; '`' ]
let is_quote c = List.mem c quotes
let is_control c = Char.code c < 0x20 || Char.code c = 0x7f

(* A VHS string has no escapes and ends at a newline, so it is delimited by
   a quote it lacks and cannot hold a control character. *)
let quote_string s =
  if String.exists is_control s then
    Fmt.invalid_arg "Vhs.Value: %S holds a control character" s;
  match List.find_opt (fun q -> not (String.contains s q)) quotes with
  | Some q -> Fmt.str "%c%s%c" q s q
  | None ->
      Fmt.invalid_arg "Vhs.Value: %S holds all three VHS quote characters" s

let pp_quoted ppf s = Fmt.string ppf (quote_string s)

let pp_line ppf s =
  if String.exists (fun c -> c = '\n' || c = '\r') s then
    Fmt.invalid_arg "Vhs.Value: %S holds a line break" s;
  Fmt.string ppf s

let pp_word ppf s =
  if s = "" || String.exists (fun c -> c = ' ' || is_control c) s then
    Fmt.invalid_arg "Vhs.Value: %S is not a single word" s;
  Fmt.string ppf s

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

(* [type_lines s] are the VHS commands typing [s]: text runs become [Type]
   commands and newlines and tabs become key presses. *)
let type_lines s =
  let lines = ref [] and start = ref 0 in
  let text stop =
    if stop > !start then
      let chunks = quotable_chunks (String.sub s !start (stop - !start)) in
      List.iter (fun c -> lines := ("Type " ^ quote_string c) :: !lines) chunks
  in
  String.iteri
    (fun i c ->
      match c with
      | '\n' | '\t' ->
          text i;
          lines := (if c = '\n' then "Enter" else "Tab") :: !lines;
          start := i + 1
      | c when is_control c ->
          Fmt.invalid_arg "Vhs.Value: Type %S holds a control character" s
      | _ -> ())
    s;
  text (String.length s);
  match !lines with [] -> [ {|Type ""|} ] | lines -> List.rev lines

let pp_type ppf s = Fmt.(list ~sep:(any "\n") string) ppf (type_lines s)

(* VHS reads a plain decimal: no exponent, no sign. *)
let decimal s =
  let s = Fmt.str "%.9f" (Float.abs s) in
  let rec last i =
    match s.[i] with '0' -> last (i - 1) | '.' -> i - 1 | _ -> i
  in
  String.sub s 0 (last (String.length s - 1) + 1)

let pp_duration ppf = function
  | Ms n when n < 0 -> Fmt.invalid_arg "Vhs.Value: negative duration %dms" n
  | Ms n -> Fmt.pf ppf "%dms" n
  | S s when Float.is_nan s || s < 0. || s = Float.infinity ->
      Fmt.invalid_arg "Vhs.Value: invalid duration %gs" s
  | S s -> Fmt.pf ppf "%ss" (decimal s)

let pp_key ppf = function
  | Enter -> Fmt.string ppf "Enter"
  | Tab -> Fmt.string ppf "Tab"
  | Space -> Fmt.string ppf "Space"
  | Backspace -> Fmt.string ppf "Backspace"
  | Escape -> Fmt.string ppf "Escape"
  | Left -> Fmt.string ppf "Left"
  | Right -> Fmt.string ppf "Right"
  | Up -> Fmt.string ppf "Up"
  | Down -> Fmt.string ppf "Down"
  | Ctrl key -> Fmt.pf ppf "Ctrl+%a" pp_word key
  | Alt key -> Fmt.pf ppf "Alt+%a" pp_word key
  | Ctrl_alt key -> Fmt.pf ppf "Ctrl+Alt+%a" pp_word key
  | Raw_key key -> pp_line ppf key

let pp_setting ppf = function
  | Shell shell -> Fmt.pf ppf "Shell %a" pp_quoted shell
  | Font_size n -> Fmt.pf ppf "FontSize %d" n
  | Font_family font -> Fmt.pf ppf "FontFamily %a" pp_quoted font
  | Width n -> Fmt.pf ppf "Width %d" n
  | Height n -> Fmt.pf ppf "Height %d" n
  | Theme theme -> Fmt.pf ppf "Theme %a" pp_quoted theme
  | Padding n -> Fmt.pf ppf "Padding %d" n
  | Typing_speed d -> Fmt.pf ppf "TypingSpeed %a" pp_duration d
  | Framerate n -> Fmt.pf ppf "Framerate %d" n
  | Custom (name, value) -> Fmt.pf ppf "%a %a" pp_word name pp_line value

(* A slash ends a VHS regexp and only VHS 0.11 and later read [\/] as an
   escape, so a slash, escaped or not, is written as RE2's [\x2f]. *)
let pp_regexp ppf re =
  let b = Buffer.create (String.length re + 8) in
  let rec loop i =
    if i < String.length re then
      match re.[i] with
      | '/' ->
          Buffer.add_string b {|\x2f|};
          loop (i + 1)
      | '\\' when i + 1 < String.length re && re.[i + 1] = '/' ->
          Buffer.add_string b {|\x2f|};
          loop (i + 2)
      | '\\' when i + 1 < String.length re ->
          Buffer.add_string b (String.sub re i 2);
          loop (i + 2)
      | c ->
          Buffer.add_char b c;
          loop (i + 1)
  in
  loop 0;
  Fmt.pf ppf "/%a/" pp_line (Buffer.contents b)

let pp_wait ppf = function
  | Wait re -> Fmt.pf ppf "Wait %a" pp_regexp re
  | Wait_screen re -> Fmt.pf ppf "Wait+Screen %a" pp_regexp re
  | Wait_line re -> Fmt.pf ppf "Wait+Line %a" pp_regexp re

let pp_command ppf = function
  | Type s -> pp_type ppf s
  | Key k -> pp_key ppf k
  | Sleep d -> Fmt.pf ppf "Sleep %a" pp_duration d
  | Wait_for w -> pp_wait ppf w
  | Hide -> Fmt.string ppf "Hide"
  | Show -> Fmt.string ppf "Show"
  | Screenshot None -> Fmt.string ppf "Screenshot"
  | Screenshot (Some path) -> Fmt.pf ppf "Screenshot %a" pp_quoted path
  | Source path -> Fmt.pf ppf "Source %a" pp_quoted path
  | Raw_command line -> pp_line ppf line

let pp_item ppf = function
  | Comment "" -> Fmt.string ppf "#"
  | Comment s -> Fmt.pf ppf "# %a" pp_line s
  | Blank -> ()
  | Require s -> Fmt.pf ppf "Require %a" pp_word s
  | Output path -> Fmt.pf ppf "Output %a" pp_quoted path
  | Env (key, value) -> Fmt.pf ppf "Env %a %a" pp_word key pp_quoted value
  | Set setting -> Fmt.pf ppf "Set %a" pp_setting setting
  | Command command -> pp_command ppf command
  | Raw_line line -> pp_line ppf line

let pp ppf items = List.iter (fun item -> Fmt.pf ppf "%a\n" pp_item item) items
let to_string t = Fmt.str "%a" pp t
let equal a b = String.equal (to_string a) (to_string b)
