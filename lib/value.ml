module Meta = Loc.Meta

type 'a node = 'a * Meta.t
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
  | Type of string node
  | Key of key node
  | Sleep of duration node
  | Wait_for of wait node
  | Hide of unit node
  | Show of unit node
  | Screenshot of string option node
  | Source of string node
  | Raw_command of string node

type item =
  | Comment of string node
  | Blank of unit node
  | Require of string node
  | Output of string node
  | Env of (string * string) node
  | Set of setting node
  | Command of command node
  | Raw_line of string node

type t = Tape of item list node

let node ?(meta = Meta.none) v = (v, meta)
let tape ?meta items = Tape (node ?meta items)
let comment ?meta s = Comment (node ?meta s)
let blank ?meta () = Blank (node ?meta ())
let require ?meta s = Require (node ?meta s)
let output ?meta s = Output (node ?meta s)
let env ?meta k v = Env (node ?meta (k, v))
let set ?meta s = Set (node ?meta s)
let command ?meta c = Command (node ?meta c)
let raw_line ?meta s = Raw_line (node ?meta s)
let type' ?meta s = command ?meta (Type (node ?meta s))
let key ?meta k = command ?meta (Key (node ?meta k))
let enter ?meta () = key ?meta Enter
let sleep ?meta d = command ?meta (Sleep (node ?meta d))
let wait ?meta w = command ?meta (Wait_for (node ?meta w))
let hide ?meta () = command ?meta (Hide (node ?meta ()))
let show ?meta () = command ?meta (Show (node ?meta ()))
let screenshot ?meta ?path () = command ?meta (Screenshot (node ?meta path))
let source ?meta path = command ?meta (Source (node ?meta path))
let raw_command ?meta s = command ?meta (Raw_command (node ?meta s))

let add_hex_escape b code =
  let hex = "0123456789abcdef" in
  Buffer.add_string b "\\x";
  Buffer.add_char b hex.[code lsr 4];
  Buffer.add_char b hex.[code land 0xf]

let contains s needle =
  let rec loop i =
    i < String.length s && (Char.equal s.[i] needle || loop (i + 1))
  in
  loop 0

(* VHS types the contents of a double-quoted string verbatim: a backslash is
   typed as a backslash, not treated as an escape introducer. So we must not
   double backslashes here -- a command such as [--format 'a\tb'] has to reach
   the shell with a single backslash. The [invalid_arg] in {!quote_string}
   guarantees this branch never sees a literal double quote, and single-line
   tape commands never carry raw control bytes; those are hex-escaped
   defensively all the same. *)
let escape_double_quoted s =
  let b = Buffer.create (String.length s + 8) in
  Buffer.add_char b '"';
  String.iter
    (fun c ->
      let code = Char.code c in
      if code < 0x20 || code = 0x7f then add_hex_escape b code
      else Buffer.add_char b c)
    s;
  Buffer.add_char b '"';
  Buffer.contents b

let single_quoted s = "'" ^ s ^ "'"

let quote_string s =
  if contains s '"' then
    if contains s '\'' then
      invalid_arg
        "Vhs.Value.quote_string: VHS strings cannot contain both quote styles"
    else single_quoted s
  else escape_double_quoted s

let pp_quoted ppf s = Fmt.string ppf (quote_string s)

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
  | Type (s, _) -> Fmt.pf ppf "Type %a" pp_quoted s
  | Key (k, _) -> pp_key ppf k
  | Sleep (d, _) -> Fmt.pf ppf "Sleep %a" pp_duration d
  | Wait_for (w, _) -> pp_wait ppf w
  | Hide _ -> Fmt.string ppf "Hide"
  | Show _ -> Fmt.string ppf "Show"
  | Screenshot (None, _) -> Fmt.string ppf "Screenshot"
  | Screenshot (Some path, _) -> Fmt.pf ppf "Screenshot %a" pp_quoted path
  | Source (path, _) -> Fmt.pf ppf "Source %a" pp_quoted path
  | Raw_command (line, _) -> Fmt.string ppf line

let pp_item ppf = function
  | Comment ("", _) -> Fmt.string ppf "#"
  | Comment (s, _) -> Fmt.pf ppf "# %s" s
  | Blank _ -> ()
  | Require (s, _) -> Fmt.pf ppf "Require %s" s
  | Output (path, _) -> Fmt.pf ppf "Output %a" pp_quoted path
  | Env ((key, value), _) -> Fmt.pf ppf "Env %s %a" key pp_quoted value
  | Set (setting, _) -> Fmt.pf ppf "Set %a" pp_setting setting
  | Command (command, _) -> pp_command ppf command
  | Raw_line (line, _) -> Fmt.string ppf line

let pp ppf (Tape (items, _)) =
  List.iter (fun item -> Fmt.pf ppf "%a\n" pp_item item) items

let to_string t = Fmt.str "%a" pp t
let equal a b = String.equal (to_string a) (to_string b)
