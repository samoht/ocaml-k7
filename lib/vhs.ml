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

type item =
  | Comment of string
  | Blank
  | Require of string
  | Output of string
  | Env of string * string
  | Set of setting
  | Type of string
  | Key of key
  | Sleep of duration
  | Wait_for of duration option * wait
  | Hide
  | Show
  | Screenshot of string
  | Source of string
  | Raw of string

type t = item list

let tape items = items
let comment s = Comment s
let blank () = Blank
let require s = Require s
let output s = Output s
let env k v = Env (k, v)
let set s = Set s
let type' s = Type s
let key k = Key k
let enter () = Key Enter
let sleep d = Sleep d
let wait ?timeout w = Wait_for (timeout, w)
let hide () = Hide
let show () = Show
let screenshot path = Screenshot path
let source path = Source path
let raw s = Raw s

(* Raised by the printers on an invalid tape; [pp] and [validate] turn it
   into [Invalid_argument] and [Error]. *)
exception Invalid of string

let fail_invalid fmt = Fmt.kstr (fun msg -> raise (Invalid msg)) fmt
let quotes = [ '"'; '\''; '`' ]
let is_quote c = List.mem c quotes
let is_control c = Char.code c < 0x20 || Char.code c = 0x7f

(* A VHS string has no escapes and ends at a newline, so it is delimited by
   a quote it lacks and cannot hold a control character. *)
let check_controls s =
  if String.exists is_control s then
    fail_invalid "%S holds a control character" s

let quote_string s =
  check_controls s;
  match List.find_opt (fun q -> not (String.contains s q)) quotes with
  | Some q -> Fmt.str "%c%s%c" q s q
  | None -> fail_invalid "%S holds all three VHS quote characters" s

let pp_quoted ppf s = Fmt.string ppf (quote_string s)

let pp_line ppf s =
  check_controls s;
  Fmt.string ppf s

let pp_word ppf s =
  if s = "" || String.exists (fun c -> c = ' ' || is_control c) s then
    fail_invalid "%S is not a single word" s;
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
          fail_invalid "Type %S holds a control character" s
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
  | Ms n when n < 0 -> fail_invalid "negative duration %dms" n
  | Ms n -> Fmt.pf ppf "%dms" n
  | S s when Float.is_nan s || s < 0. || s = Float.infinity ->
      fail_invalid "invalid duration %gs" s
  | S s -> Fmt.pf ppf "%ss" (decimal s)

let is_letter = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false

let pp_chord name valid ppf c =
  if not (is_letter c || String.contains valid c) then
    fail_invalid "%s+%C is not a VHS key" name c;
  Fmt.pf ppf "%s+%c" name c

let pp_key ppf = function
  | Enter -> Fmt.string ppf "Enter"
  | Tab -> Fmt.string ppf "Tab"
  | Space -> Fmt.string ppf "Space"
  | Backspace -> Fmt.string ppf "Backspace"
  | Delete -> Fmt.string ppf "Delete"
  | Insert -> Fmt.string ppf "Insert"
  | Escape -> Fmt.string ppf "Escape"
  | Up -> Fmt.string ppf "Up"
  | Down -> Fmt.string ppf "Down"
  | Left -> Fmt.string ppf "Left"
  | Right -> Fmt.string ppf "Right"
  | Page_up -> Fmt.string ppf "PageUp"
  | Page_down -> Fmt.string ppf "PageDown"
  | Scroll_up -> Fmt.string ppf "ScrollUp"
  | Scroll_down -> Fmt.string ppf "ScrollDown"
  | Ctrl c -> pp_chord "Ctrl" {|@[\]^-.|} ppf c
  | Alt c -> pp_chord "Alt" "[]." ppf c

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

let pp_wait ppf timeout w =
  let scope, re =
    match w with
    | Prompt -> ("", None)
    | Line re -> ("+Line", Some re)
    | Screen re -> ("+Screen", Some re)
  in
  Fmt.pf ppf "Wait%s%a%a" scope
    Fmt.(option (any "@" ++ pp_duration))
    timeout
    Fmt.(option (any " " ++ pp_regexp))
    re

let pp_item ppf = function
  | Comment "" -> Fmt.string ppf "#"
  | Comment s -> Fmt.pf ppf "# %a" pp_line s
  | Blank -> ()
  | Require s -> Fmt.pf ppf "Require %a" pp_word s
  | Output path -> Fmt.pf ppf "Output %a" pp_quoted path
  | Env (key, value) -> Fmt.pf ppf "Env %a %a" pp_word key pp_quoted value
  | Set setting -> Fmt.pf ppf "Set %a" pp_setting setting
  | Type s -> pp_type ppf s
  | Key k -> pp_key ppf k
  | Sleep d -> Fmt.pf ppf "Sleep %a" pp_duration d
  | Wait_for (timeout, w) -> pp_wait ppf timeout w
  | Hide -> Fmt.string ppf "Hide"
  | Show -> Fmt.string ppf "Show"
  | Screenshot path -> Fmt.pf ppf "Screenshot %a" pp_quoted path
  | Source path -> Fmt.pf ppf "Source %a" pp_quoted path
  | Raw line -> pp_line ppf line

let print t =
  try Ok (String.concat "" (List.map (Fmt.str "%a\n" pp_item) t))
  with Invalid msg -> Error msg

let validate t = Result.map ignore (print t)

let pp ppf t =
  match print t with
  | Ok s -> Fmt.string ppf s
  | Error msg -> invalid_arg ("Vhs.pp: " ^ msg)

module Cram = struct
  type step = {
    command : string list;
    output : string list;
    exit_code : int option;
  }

  type t = step list

  let drop n s = String.sub s n (String.length s - n)

  (* [  [N]] exit-code annotation: two-space indent, then [N] on its own. *)
  let exit_code_of_output line =
    if
      String.length line >= 3
      && Char.equal line.[0] '['
      && Char.equal line.[String.length line - 1] ']'
    then int_of_string_opt (String.sub line 1 (String.length line - 2))
    else None

  let parse src =
    let chomp l =
      let n = String.length l in
      if n > 0 && l.[n - 1] = '\r' then String.sub l 0 (n - 1) else l
    in
    let lines = List.map chomp (String.split_on_char '\n' src) in
    let steps = ref [] in
    let cmd = ref []
    and out = ref []
    and exit = ref None
    and open_ = ref false in
    let flush () =
      if !open_ then
        steps :=
          { command = List.rev !cmd; output = List.rev !out; exit_code = !exit }
          :: !steps;
      cmd := [];
      out := [];
      exit := None;
      open_ := false
    in
    List.iter
      (fun line ->
        if String.starts_with ~prefix:"  $ " line then begin
          flush ();
          open_ := true;
          cmd := [ drop 4 line ]
        end
        else if !open_ && String.starts_with ~prefix:"  > " line then
          cmd := drop 4 line :: !cmd
        else if !open_ && String.starts_with ~prefix:"  " line then begin
          let content = drop 2 line in
          match exit_code_of_output content with
          | Some _ as code -> exit := code
          | None -> out := content :: !out
        end
        else flush ())
      lines;
    flush ();
    List.rev !steps

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
    List.concat_map (fun line -> [ type' line; enter () ]) step.command
    @ [ wait Prompt; sleep pause ]

  let to_tape ?(settings = []) ?(setup_scripts = []) ?(pause = S 1.)
      ?(final_pause = S 3.) ~output (steps : t) =
    let last = List.length steps - 1 in
    let items =
      (Output output :: List.map set settings)
      @ setup_block setup_scripts
      @ List.concat
          (List.mapi
             (fun index step ->
               let pause = if index = last then final_pause else pause in
               step_items ~pause step)
             steps)
    in
    tape items

  let pp ppf steps =
    List.iter
      (fun { command; output; exit_code } ->
        List.iteri
          (fun i -> Fmt.pf ppf "  %s %s@\n" (if i = 0 then "$" else ">"))
          command;
        List.iter (Fmt.pf ppf "  %s@\n") output;
        Option.iter (Fmt.pf ppf "  [%d]@\n") exit_code)
      steps
end

type error =
  | Invalid_tape of string
  | Executable_not_found of string
  | Failed of { vhs : string; status : [ `Exited of int | `Signaled of int ] }
  | Io of string

let pp_error ppf = function
  | Invalid_tape msg -> Fmt.pf ppf "invalid tape: %s" msg
  | Executable_not_found exe -> Fmt.pf ppf "executable not found: %s" exe
  | Failed { vhs; status = `Exited n } ->
      Fmt.pf ppf "%s exited with code %d" vhs n
  | Failed { vhs; status = `Signaled n } ->
      Fmt.pf ppf "%s was killed by signal %d" vhs n
  | Io msg -> Fmt.string ppf msg

let render ~proc ?(vhs = "vhs") t =
  match print t with
  | Error msg -> Error (Invalid_tape msg)
  | Ok tape -> (
      try
        Ok (Eio.Process.run proc ~stdin:(Eio.Flow.string_source tape) [ vhs ])
      with
      | Eio.Exn.Io (Eio.Process.E (Executable_not_found exe), _) ->
          Error (Executable_not_found exe)
      | Eio.Exn.Io (Eio.Process.E (Child_error status), _) ->
          Error (Failed { vhs; status })
      | Eio.Exn.Io _ as exn -> Error (Io (Printexc.to_string exn)))
