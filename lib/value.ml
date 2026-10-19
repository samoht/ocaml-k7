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

let pp_duration ppf = function
  | Ms n -> Fmt.pf ppf "%dms" n
  | S s -> Fmt.pf ppf "%gs" s

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
  | Ctrl key -> Fmt.pf ppf "Ctrl+%s" key
  | Alt key -> Fmt.pf ppf "Alt+%s" key
  | Ctrl_alt key -> Fmt.pf ppf "Ctrl+Alt+%s" key
  | Raw_key key -> Fmt.string ppf key

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
  | Custom (name, value) -> Fmt.pf ppf "%s %s" name value

let pp_wait ppf = function
  | Wait pattern -> Fmt.pf ppf "Wait /%s/" pattern
  | Wait_screen pattern -> Fmt.pf ppf "Wait+Screen /%s/" pattern
  | Wait_line pattern -> Fmt.pf ppf "Wait+Line /%s/" pattern

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
  | Raw_command line -> Fmt.string ppf line

let pp_item ppf = function
  | Comment "" -> Fmt.string ppf "#"
  | Comment s -> Fmt.pf ppf "# %s" s
  | Blank -> ()
  | Require s -> Fmt.pf ppf "Require %s" s
  | Output path -> Fmt.pf ppf "Output %a" pp_quoted path
  | Env (key, value) -> Fmt.pf ppf "Env %s %a" key pp_quoted value
  | Set setting -> Fmt.pf ppf "Set %a" pp_setting setting
  | Command command -> pp_command ppf command
  | Raw_line line -> Fmt.string ppf line

let pp ppf items = List.iter (fun item -> Fmt.pf ppf "%a\n" pp_item item) items
let to_string t = Fmt.str "%a" pp t
let equal a b = String.equal (to_string a) (to_string b)
