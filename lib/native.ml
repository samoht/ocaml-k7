(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The commands follow VHS's command.go and the geometry its vhs.go, at
   v0.12.1 (MIT, Charm). *)

type error =
  | Unsupported of string
  | Timeout of { wait : string; last : string }
  | Not_found of string
  | Io of string

open Tape_syntax

exception Stop of error

let stop e = raise_notrace (Stop e)

let unsupported line fmt =
  Fmt.kstr (fun why -> stop (Unsupported (line ^ ": " ^ why))) fmt

(* Commands *)

type scope = Line | Screen

type command =
  | Type of { speed : int option; text : string }
  | Key of { speed : int option; count : int; key : string }
  | Ctrl of lexeme list
  | Alt of lexeme
  | Shift of lexeme
  | Sleep of int
  | Wait of {
      line : string;
      scope : scope;
      timeout : int option;
      pattern : Regexp_match.t option;
    }
  | Hide
  | Show
  | Set_typing_speed of int
  | Copy of string
  | Paste
  | Output of string
  | Env

let keys =
  [ "Enter"; "Tab"; "Space"; "Backspace"; "Delete"; "Insert"; "Escape"; "Up";
    "Down"; "Left"; "Right"; "PageUp"; "PageDown"; "ScrollUp"; "ScrollDown" ]
[@@ocamlformat "disable"]

let ms_of_ns ns = Int64.to_int (Int64.div ns 1_000_000L)

(* parseTime: a number and an optional unit, seconds by default. *)
let time line = function
  | Num n :: Keyword (("ms" | "s" | "m") as u) :: rest -> (n ^ u, rest)
  | Num n :: rest -> (n ^ "s", rest)
  | _ -> unsupported line "expected a duration"

let ms line s =
  match duration s with
  | Some ns -> ms_of_ns ns
  | None -> unsupported line "%s is not a duration" s

let speed line = function
  | Sym '@' :: rest ->
      let t, rest = time line rest in
      (Some (ms line t), rest)
  | rest -> (None, rest)

let strings tokens =
  String.concat " "
    (List.filter_map (function Str s -> Some s | _ -> None) tokens)

let lit = function
  | Keyword s | Str s | Pattern s | Json_value s | Num s -> s
  | Sym c -> String.make 1 c

let compile line re =
  match Regexp_match.compile re with
  | Ok r -> r
  | Error e -> unsupported line "%s" e

(* Settings *)

type config = {
  shell : string;
  font_size : int;
  font_family : string;
  width : int;
  height : int;
  padding : int;
  margin : int;
  margin_fill : int;
  border_radius : int;
  window_bar : string;
  bar_size : int;
  letter_spacing : float;
  line_height : float;
  rows : int;
  columns : int;
  palette : Palette.t;
  typing_speed : int;
  framerate : int;
  playback_speed : float;
  loop_offset : float;
  wait_timeout : int;
  wait_pattern : Regexp_match.t;
  cursor_blink : bool;
}

let default_font =
  "JetBrains Mono,DejaVu Sans Mono,Menlo,Bitstream Vera Sans \
   Mono,Inconsolata,Roboto Mono,Hack,Consolas,ui-monospace,monospace,Apple \
   Symbols"

let default_config () =
  {
    shell = "bash";
    font_size = 22;
    font_family = default_font;
    width = 1200;
    height = 600;
    padding = 60;
    margin = 0;
    margin_fill = Palette.default.background;
    border_radius = 0;
    window_bar = "";
    bar_size = 30;
    letter_spacing = 1.;
    line_height = 1.;
    rows = 0;
    columns = 0;
    palette = Palette.default;
    typing_speed = 50;
    framerate = 50;
    playback_speed = 1.;
    loop_offset = 0.;
    wait_timeout = 15_000;
    wait_pattern = compile "" ">$";
    cursor_blink = true;
  }

(* Sizes beyond this many pixels or cells are refused, which also keeps the
   frame arithmetic from overflowing. *)
let max_size = 100_000

let int_value line s =
  match int_of_string_opt s with
  | Some n when n >= 0 && n <= max_size -> n
  | Some _ -> unsupported line "%s is not between 0 and %d" s max_size
  | None -> unsupported line "%s is not an integer" s

let float_value line s =
  match float_of_string_opt s with
  | Some f when Float.is_finite f -> f
  | _ -> unsupported line "%s is not a number" s

let is_hex_colour s =
  String.length s = 7
  && s.[0] = '#'
  && String.for_all
       (function '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true | _ -> false)
       (String.sub s 1 6)

let set line c name args =
  let value () =
    match args with v :: _ -> lit v | [] -> unsupported line "no value"
  in
  match (name, args) with
  | "Shell", [ Str (("bash" | "zsh") as shell) ] -> { c with shell }
  | "Shell", _ -> unsupported line "only bash and zsh play natively"
  | "FontSize", _ -> { c with font_size = int_value line (value ()) }
  | "FontFamily", _ -> { c with font_family = value () ^ ",monospace" }
  | "Width", _ -> { c with width = int_value line (value ()) }
  | "Height", _ -> { c with height = int_value line (value ()) }
  | "Padding", _ -> { c with padding = int_value line (value ()) }
  | "Margin", _ -> { c with margin = int_value line (value ()) }
  | "MarginFill", _ when is_hex_colour (value ()) ->
      { c with margin_fill = int_of_string ("0x" ^ String.sub (value ()) 1 6) }
  | "MarginFill", _ -> unsupported line "an image margin needs vhs"
  | "BorderRadius", _ -> { c with border_radius = int_value line (value ()) }
  | "LetterSpacing", _ ->
      { c with letter_spacing = float_value line (value ()) }
  | "LineHeight", _ -> { c with line_height = float_value line (value ()) }
  | "Rows", _ -> { c with rows = int_value line (value ()) }
  | "Columns", _ -> { c with columns = int_value line (value ()) }
  | "Theme", _ -> (
      let name = value () in
      if String.trim name = "" then { c with palette = Palette.default }
      else if name.[0] = '{' then
        match Palette.of_json name with
        | Ok palette -> { c with palette }
        | Error (`Json msg) -> unsupported line "VHS cannot read it: %s" msg
        | Error (`Colour c) ->
            unsupported line "%s is a colour only a browser reads" c
      else
        match Palette.find name with
        | Some palette -> { c with palette }
        | None -> unsupported line "not a VHS theme")
  | "TypingSpeed", Num n :: rest ->
      let u =
        match rest with Keyword (("ms" | "s") as u) :: _ -> u | _ -> "s"
      in
      { c with typing_speed = ms line (n ^ u) }
  | "Framerate", _ -> { c with framerate = max 1 (int_value line (value ())) }
  | "PlaybackSpeed", _ ->
      { c with playback_speed = float_value line (value ()) }
  | "LoopOffset", _ ->
      { c with loop_offset = float_value line (value ()) /. 100. }
  | "WaitTimeout", _ -> { c with wait_timeout = ms line (fst (time line args)) }
  | "WaitPattern", _ -> { c with wait_pattern = compile line (value ()) }
  | "CursorBlink", _ -> { c with cursor_blink = value () = "true" }
  | "WindowBar", _ -> (
      match value () with
      | ("" | "Colorful" | "ColorfulRight" | "Rings" | "RingsRight") as bar ->
          { c with window_bar = bar }
      | _ -> unsupported line "not a bar style of VHS")
  | "WindowBarSize", _ -> { c with bar_size = int_value line (value ()) }
  | _ -> unsupported line "not a setting this module knows"

(* Geometry: xterm.js fits whole cells of JetBrains Mono, VHS's first font,
   in the viewport, less its 10px padding and 15px scrollbar. Chrome rounds
   the ascent and the descent of the font. *)

let round f = int_of_float (Float.round f)
let max_cells = 2_000
let even n = if n mod 2 = 0 then n else n + 1

let style c =
  let fs = float c.font_size in
  let ascent = round (1.02 *. fs) and descent = round (0.3 *. fs) in
  let char_height = ascent + descent in
  let cell_width = max 1 ((c.font_size * 6 / 10) + round c.letter_spacing) in
  let cell_height = max 1 (int_of_float (float char_height *. c.line_height)) in
  let frame = 2 * (c.padding + c.margin) in
  let bar =
    match c.window_bar with
    | "" -> None
    | style ->
        Some
          Svg.
            {
              rings = String.starts_with ~prefix:"Rings" style;
              right = String.ends_with ~suffix:"Right" style;
              size = c.bar_size;
            }
  in
  let bar_height = match bar with Some b -> b.size | None -> 0 in
  let cols =
    if c.columns > 0 then c.columns
    else max 2 ((c.width - frame - 25) / cell_width)
  in
  let rows =
    if c.rows > 0 then c.rows
    else max 1 ((c.height - frame - bar_height - 10) / cell_height)
  in
  if cols > max_cells || rows > max_cells then
    Fmt.kstr
      (fun why -> stop (Unsupported why))
      "a %d x %d terminal is beyond %d cells a side" cols rows max_cells;
  let width =
    if c.columns > 0 then even ((cols * cell_width) + 25 + frame) else c.width
  in
  let height =
    if c.rows > 0 then even ((rows * cell_height) + 10 + frame + bar_height)
    else c.height
  in
  Svg.
    {
      width;
      height;
      x = c.margin + c.padding;
      y = c.margin + bar_height + c.padding;
      cols;
      rows;
      cell_width;
      cell_height;
      baseline = ((cell_height - char_height) / 2) + ascent;
      font_size = c.font_size;
      font_family = c.font_family;
      palette = c.palette;
      margin = c.margin;
      margin_fill = c.margin_fill;
      radius = c.border_radius;
      bar;
      blink = c.cursor_blink;
      loop_offset = 0;
      speed = (if c.playback_speed > 0. then c.playback_speed else 1.);
    }

(* Tapes *)

type t = {
  config : config;
  look : Svg.t;
  actions : command list;
  env : (string * string) list;
  requires : string list;
  text : string option;  (** The text output of the settings. *)
  dumps : bool;  (** Whether the text output starts with the first action. *)
}

let is_text path =
  match extension path with ".txt" | ".ascii" | ".test" -> true | _ -> false

let wait_command line tokens =
  let scope, rest =
    match tokens with
    | Sym '+' :: Str "Screen" :: rest -> (Screen, rest)
    | Sym '+' :: Str "Line" :: rest -> (Line, rest)
    | rest -> (Line, rest)
  in
  let timeout, rest = speed line rest in
  let pattern =
    match rest with Pattern p :: _ -> Some (compile line p) | _ -> None
  in
  Wait { line; scope; timeout; pattern }

(* The settings, environment and requirements a line adds, or its action. *)
let read line t =
  match tokens line with
  | [] -> t
  | Keyword "Output" :: p :: _ when t.actions = [] ->
      if is_text (lit p) then { t with text = Some (lit p) } else t
  | Keyword "Output" :: p :: _ ->
      { t with actions = Output (lit p) :: t.actions }
  | Keyword "Set" :: Keyword "TypingSpeed" :: args when t.actions <> [] ->
      let c = set line t.config "TypingSpeed" args in
      { t with actions = Set_typing_speed c.typing_speed :: t.actions }
  | Keyword "Set" :: Keyword name :: args ->
      { t with config = set line t.config name args }
  | Keyword "Env" :: k :: v :: _ ->
      { t with env = (lit k, lit v) :: t.env; actions = Env :: t.actions }
  | Keyword "Require" :: p :: _ -> { t with requires = lit p :: t.requires }
  | tokens ->
      let action =
        match tokens with
        | Keyword "Type" :: rest ->
            let speed, rest = speed line rest in
            Type { speed; text = strings rest }
        | Keyword k :: rest when List.mem k keys ->
            let speed, rest = speed line rest in
            let count =
              match rest with Num n :: _ -> int_value line n | _ -> 1
            in
            Key { speed; count; key = k }
        | Keyword "Ctrl" :: rest -> Ctrl (List.filter (( <> ) (Sym '+')) rest)
        | Keyword "Alt" :: Sym '+' :: k :: _ -> Alt k
        | Keyword "Shift" :: Sym '+' :: k :: _ -> Shift k
        | Keyword "Sleep" :: rest -> Sleep (ms line (fst (time line rest)))
        | Keyword "Wait" :: rest -> wait_command line rest
        | [ Keyword "Hide" ] -> Hide
        | [ Keyword "Show" ] -> Show
        | Keyword "Copy" :: rest -> Copy (strings rest)
        | [ Keyword "Paste" ] -> Paste
        | Keyword "Screenshot" :: _ -> unsupported line "a screenshot needs vhs"
        | Keyword "Source" :: _ -> unsupported line "sourcing a tape needs vhs"
        | _ -> unsupported line "not a command this module knows"
      in
      { t with actions = action :: t.actions }

let of_lines lines =
  let config = default_config () in
  let empty =
    {
      config;
      look = style config;
      actions = [];
      env = [];
      requires = [];
      text = None;
      dumps = false;
    }
  in
  match
    let t = List.fold_left (fun t line -> read line t) empty lines in
    { t with look = style t.config }
  with
  | t ->
      let actions = List.rev t.actions in
      let later =
        List.find_map
          (function Output p when is_text p -> Some p | _ -> None)
          actions
      in
      Ok
        {
          t with
          actions;
          env = List.rev t.env;
          text = (match t.text with Some _ -> t.text | None -> later);
          dumps = Option.is_some t.text;
        }
  | exception Stop e -> Error e

(* Timelines *)

type timeline = {
  quantum : int;
  mutable frames : (int * Screen.t) list;  (** Latest first. *)
}

(* A screen replaces one of the same frame, and merges into an equal one
   before it. *)
let add tl ms screen =
  let q = ms / tl.quantum * tl.quantum in
  match tl.frames with
  | (t, _) :: rest when q <= t -> (
      match rest with
      | (_, s) :: _ when Screen.equal s screen -> tl.frames <- rest
      | _ -> tl.frames <- (t, screen) :: rest)
  | (_, s) :: _ when Screen.equal s screen -> ()
  | frames -> tl.frames <- (q, screen) :: frames

let svg t tl ~end_ms =
  let c = t.config and s = t.look in
  let frames = List.rev tl.frames in
  let last = match tl.frames with (t, _) :: _ -> t | [] -> 0 in
  let duration = max (last + tl.quantum) end_ms in
  let duration = (duration + tl.quantum - 1) / tl.quantum * tl.quantum in
  let loop_offset = round (c.loop_offset *. float duration) mod duration in
  Svg.to_string { s with loop_offset } ~duration frames

let timeline c = { quantum = max 1 (1000 / c.framerate); frames = [] }
let text_output t = t.text

let replay t output =
  let s = t.look in
  let term = Screen.terminal ~rows:s.rows ~cols:s.cols in
  let tl = timeline t.config in
  add tl 0 (Screen.of_terminal term);
  let screen = ref (Screen.of_terminal term) in
  let end_ms =
    List.fold_left
      (fun _ (ms, bytes) ->
        Screen.feed term bytes;
        screen := Screen.of_terminal ~previous:!screen term;
        Screen.clear_dirty term;
        add tl ms !screen;
        ms)
      0 output
  in
  svg t tl ~end_ms

(* Keys: the bytes xterm sends. *)

let arrow vte c = if Vte.cursor_key_mode vte then "\027O" ^ c else "\027[" ^ c

let key_bytes vte = function
  | "Enter" -> "\r"
  | "Tab" -> "\t"
  | "Space" -> " "
  | "Backspace" -> "\x7f"
  | "Delete" -> "\027[3~"
  | "Insert" -> "\027[2~"
  | "Escape" -> "\027"
  | "Up" -> arrow vte "A"
  | "Down" -> arrow vte "B"
  | "Right" -> arrow vte "C"
  | "Left" -> arrow vte "D"
  | "PageUp" -> "\027[5~"
  | "PageDown" -> "\027[6~"
  | _ -> ""

(* Control with a character: caret notation, where there is one. *)
let ctrl_char c =
  match c with
  | 'a' .. 'z' -> String.make 1 (Char.chr (Char.code c - 96))
  | 'A' .. 'Z' | '@' | '[' | '\\' | ']' | '^' ->
      String.make 1 (Char.chr (Char.code c land 0x1f))
  | '-' -> "\x1f"
  | c -> String.make 1 c

let ctrl_bytes args =
  let alt = List.mem (Keyword "Alt") args in
  let key = function
    | Keyword "Enter" -> "\r"
    | Keyword "Space" -> "\x00"
    | Keyword "Backspace" -> "\x08"
    | Keyword "Up" -> "\027[1;5A"
    | Keyword "Down" -> "\027[1;5B"
    | Keyword "Right" -> "\027[1;5C"
    | Keyword "Left" -> "\027[1;5D"
    | Keyword ("Alt" | "Shift") -> ""
    | Str s when String.length s = 1 -> ctrl_char s.[0]
    | Sym c -> ctrl_char c
    | _ -> ""
  in
  (if alt then "\027" else "") ^ String.concat "" (List.map key args)

let modified ~shift = function
  | Keyword "Enter" -> "\r"
  | Keyword "Tab" -> if shift then "\027[Z" else "\t"
  | Str s -> if shift then String.uppercase_ascii s else s
  | t -> lit t

(* Playing *)

type state = {
  term : Screen.terminal;
  pty : Pty.t;
  clock : Eio.Time.Mono.ty Eio.Resource.t;
  tl : timeline;
  changed : Eio.Condition.t;
  mutable live : Screen.t;  (** The screen without scrolling. *)
  mutable shown : Screen.t;
  mutable offset : int;
  mutable hidden : bool;
  mutable logical : int;  (** The tape's time at the start of the action. *)
  mutable started : Mtime.t;  (** When the action started. *)
  mutable cap : int option;  (** The tape's duration of the action. *)
  mutable typing_speed : int;
  mutable clipboard : string;
  text : Buffer.t;
  mutable dumps : bool;  (** Whether a command adds its screen to [text]. *)
}

let now st = Eio.Time.Mono.now st.clock
let vte st = Screen.vte st.term

let elapsed st =
  Int64.to_int
    (Int64.div
       (Mtime.Span.to_uint64_ns (Mtime.span st.started (now st)))
       1_000_000L)

let stamp st =
  st.logical
  + match st.cap with Some c -> min c (elapsed st) | None -> elapsed st

let record st = if not st.hidden then add st.tl (stamp st) st.shown

let refresh st =
  if Screen.is_dirty st.term then (
    st.live <- Screen.of_terminal ~previous:st.live st.term;
    st.shown <-
      (if st.offset = 0 then st.live
       else Screen.of_terminal ~offset:st.offset st.term);
    Screen.clear_dirty st.term;
    record st);
  Eio.Condition.broadcast st.changed

let fd st = Pty.file_descr st.pty

let rec reader st buf =
  match Pty.read st.pty buf 0 (Bytes.length buf) with
  | 0 -> Eio.Fiber.await_cancel ()
  | n ->
      Screen.feed st.term (Bytes.sub_string buf 0 n);
      refresh st;
      reader st buf
  | exception Unix.Unix_error ((EAGAIN | EWOULDBLOCK | EINTR), _, _) ->
      Eio_unix.await_readable (fd st);
      reader st buf
  | exception Unix.Unix_error _ -> Eio.Fiber.await_cancel ()

let write st s =
  let rec go off =
    if off < String.length s then
      match Pty.write_string st.pty s off (String.length s - off) with
      | n -> go (off + n)
      | exception Unix.Unix_error ((EAGAIN | EWOULDBLOCK | EINTR), _, _) ->
          Eio_unix.await_writable (fd st);
          go off
      | exception Unix.Unix_error _ -> ()
  in
  if st.offset > 0 then (
    st.offset <- 0;
    st.shown <- st.live;
    record st);
  go 0

(* [step st ms] runs an action the tape gives [ms] milliseconds. *)
let step st ms =
  st.started <- now st;
  st.cap <- Some ms;
  Eio.Time.Mono.sleep st.clock (float ms /. 1000.);
  if not st.hidden then st.logical <- st.logical + ms;
  st.started <- now st;
  st.cap <- Some 0

let press st ?speed s =
  write st s;
  step st (Option.value speed ~default:st.typing_speed)

let type_text st ?speed text =
  let rec go i =
    if i < String.length text then (
      let n = Uchar.utf_decode_length (String.get_utf_8_uchar text i) in
      press st ?speed (String.sub text i n);
      go (i + n))
  in
  go 0

let scroll st ?speed ~count dir =
  for _ = 1 to count do
    st.offset <- max 0 (min (Screen.history st.term) (st.offset + dir));
    st.shown <-
      (if st.offset = 0 then st.live
       else Screen.of_terminal ~offset:st.offset st.term);
    record st;
    step st (Option.value speed ~default:st.typing_speed)
  done

let within st seconds f =
  Eio.Fiber.first
    (fun () ->
      f ();
      true)
    (fun () ->
      Eio.Time.Mono.sleep st.clock seconds;
      false)

(* VHS reads as many lines as the screen has rows, from the top of the
   buffer. *)
let top st =
  Screen.buffer st.term st.live
  |> Array.to_list
  |> List.filteri (fun i _ -> i < Vte.rows (vte st))

let wait_text st = function
  | Line ->
      let row = fst (Vte.cursor_pos (vte st)) in
      if st.offset = 0 then Screen.line st.live row
      else
        (* VHS reads the line of the cursor's row on the viewport, which a
           scroll moves up. *)
        let lines = Screen.buffer st.term st.live
        and i = Screen.history st.term + row - st.offset in
        if i >= 0 && i < Array.length lines then lines.(i) else ""
  | Screen -> String.concat "\n" (top st)

let wait st c ~line ~scope ~timeout ~pattern =
  let re = Option.value pattern ~default:c.wait_pattern in
  let timeout = Option.value timeout ~default:c.wait_timeout in
  st.started <- now st;
  st.cap <- None;
  let holds () = Regexp_match.matches re (wait_text st scope) in
  let rec loop () =
    if not (holds ()) then (
      Eio.Condition.await_no_mutex st.changed;
      loop ())
  in
  if not (within st (float timeout /. 1000.) loop) then
    stop (Timeout { wait = line; last = wait_text st scope });
  if not st.hidden then st.logical <- st.logical + elapsed st;
  st.started <- now st;
  st.cap <- Some 0

(* VHS's text output: the lines {!top} reads after each command it records,
   each followed by a newline, then a line of 80 box-drawing characters. *)
let separator = String.concat "" (List.init 80 (fun _ -> "\xe2\x94\x80"))

let dump st =
  if st.dumps && not st.hidden then (
    List.iter
      (fun l ->
        Buffer.add_string st.text l;
        Buffer.add_char st.text '\n')
      (top st);
    Buffer.add_string st.text separator;
    Buffer.add_char st.text '\n')

let run_command st c = function
  | Type { speed; text } -> type_text st ?speed text
  | Key { speed; count; key = "ScrollUp" } -> scroll st ?speed ~count 1
  | Key { speed; count; key = "ScrollDown" } -> scroll st ?speed ~count (-1)
  | Key { speed; count; key } ->
      for _ = 1 to count do
        press st ?speed (key_bytes (vte st) key)
      done
  | Ctrl args -> press st (ctrl_bytes args)
  | Alt k -> press st ("\027" ^ modified ~shift:false k)
  | Shift k -> press st (modified ~shift:true k)
  | Sleep ms -> step st ms
  | Wait { line; scope; timeout; pattern } ->
      wait st c ~line ~scope ~timeout ~pattern
  | Hide -> st.hidden <- true
  | Show ->
      st.hidden <- false;
      record st
  | Set_typing_speed ms -> st.typing_speed <- ms
  | Copy s -> st.clipboard <- s
  | Paste -> write st st.clipboard
  | Output path -> if is_text path then st.dumps <- true
  | Env -> ()

let run st c command =
  run_command st c command;
  dump st

let on_path env exe =
  if String.contains exe '/' then Sys.file_exists exe
  else
    let path = Option.value (Eio.Process.Env.get_opt "PATH" env) ~default:"" in
    List.exists
      (fun dir -> dir <> "" && Sys.file_exists (Filename.concat dir exe))
      (String.split_on_char ':' path)

(* SIGTERM, then the hang-up of closing the terminal, then SIGKILL a second
   later. *)
let close clock pty =
  let pid = Pty.pid pty in
  Pty.close ~wait:false pty;
  Option.iter
    (fun pid ->
      let rec reap n =
        match Unix.waitpid [ WNOHANG ] pid with
        | 0, _ when n > 0 ->
            Eio.Time.Mono.sleep clock 0.01;
            reap (n - 1)
        | 0, _ -> (
            (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
            try ignore (Unix.waitpid [] pid) with Unix.Unix_error _ -> ())
        | _ -> ()
        | exception Unix.Unix_error _ -> ()
      in
      reap 100)
    pid

(* The shells VHS starts, from its shell.go: the environment it gives
   them before the tape's, their arguments, and their prompts. *)
let shell = function
  | "zsh" ->
      ( [ ("PROMPT", "%F{#5B56E0}> %F{reset_color}") ],
        [ "--histnostore"; "--no-rcs" ] )
  | _ ->
      ( [
          ("PS1", {|\[\e[38;2;90;86;224m\]> \[\e[0m\]|});
          ("BASH_SILENCE_DEPRECATION_WARNING", "1");
        ],
        [ "--noprofile"; "--norc"; "--login"; "+o"; "history" ] )

let spawn ~env ~cwd t s =
  let prog = t.config.shell in
  let vars, args = shell prog in
  let env =
    Eio.Process.Env.override
      (List.map
         (fun (k, v) -> (k, Some v))
         (vars @ [ ("TERM", "xterm-256color") ] @ t.env))
      env
  in
  List.iter
    (fun exe -> if not (on_path env exe) then stop (Not_found exe))
    (prog :: t.requires);
  match
    Pty.spawn ~env ~cwd
      ~winsize:{ rows = s.Svg.rows; cols = s.cols; xpixel = 0; ypixel = 0 }
      ~prog ~args ()
  with
  | pty -> pty
  | exception Unix.Unix_error (e, f, _) ->
      Fmt.kstr (fun msg -> stop (Io msg)) "%s: %s" f (Unix.error_message e)

(* The first output, the prompt, arrives before the first action, as it
   does in VHS, which starts a browser in the meantime. *)
let await_prompt st =
  let rec loop () =
    if Screen.text st.live = String.make (Vte.rows (vte st) - 1) '\n' then (
      Eio.Condition.await_no_mutex st.changed;
      loop ())
  in
  ignore (within st 5. loop)

let play ~clock ~env ~cwd t =
  let clock = (clock :> Eio.Time.Mono.ty Eio.Resource.t) in
  match
    let s = t.look in
    let pty = spawn ~env ~cwd t s in
    Fun.protect ~finally:(fun () ->
        Eio.Cancel.protect (fun () -> close clock pty))
    @@ fun () ->
    Pty.set_nonblock pty;
    let term = Screen.terminal ~rows:s.rows ~cols:s.cols in
    let screen = Screen.of_terminal term in
    let st =
      {
        term;
        pty;
        clock;
        tl = timeline t.config;
        changed = Eio.Condition.create ();
        live = screen;
        shown = screen;
        offset = 0;
        hidden = true;
        logical = 0;
        started = Eio.Time.Mono.now clock;
        cap = Some 0;
        typing_speed = t.config.typing_speed;
        clipboard = "";
        text = Buffer.create 4096;
        dumps = t.dumps;
      }
    in
    Eio.Fiber.first
      (fun () -> reader st (Bytes.create 65536))
      (fun () ->
        await_prompt st;
        st.hidden <- false;
        st.started <- now st;
        record st;
        List.iter (run st t.config) t.actions);
    (svg t st.tl ~end_ms:st.logical, Buffer.contents st.text)
  with
  | r -> Ok r
  | exception Stop e -> Error e
