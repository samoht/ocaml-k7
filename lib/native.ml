(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Follows VHS v0.12.1's command.go and vhs.go (MIT, Charm). *)

type error =
  | Executable_not_found of string
  | Io of string
  | Unsupported of string
  | Timeout of {
      wait : string;
      line : int;
      file : string option;
      last : string;
    }
  | Stuck of { wait : string; line : int; file : string option; last : string }
  | Exited of {
      status : Eio.Process.exit_status;
      wait : string;
      line : int;
      file : string option;
      last : string;
    }
  | Invalid of string

open Tape_syntax

exception Stop of error

let stop e = raise_notrace (Stop e)

let unsupported line fmt =
  Fmt.kstr (fun why -> stop (Unsupported (line ^ ": " ^ why))) fmt

type scope = Line | Screen

type command =
  | Type of { speed : Mtime.Span.t option; text : string }
  | Key of { speed : Mtime.Span.t option; count : int; key : string }
  | Ctrl of lexeme list
  | Alt of lexeme
  | Shift of lexeme
  | Sleep of Mtime.Span.t
  | Wait of {
      line : string;
      at : int;
      file : string option;
      scope : scope;
      timeout : Mtime.Span.t option;
      pattern : Regexp_match.t option;
    }
  | Hide
  | Show
  | Set_typing_speed of Mtime.Span.t
  | Copy of string
  | Paste
  | Env
  | Screenshot of string

let keys =
  [ "Enter"; "Tab"; "Space"; "Backspace"; "Delete"; "Insert"; "Escape"; "Up";
    "Down"; "Left"; "Right"; "PageUp"; "PageDown"; "ScrollUp"; "ScrollDown" ]
[@@ocamlformat "disable"]

let ms_of_span d =
  Int64.to_int (Int64.unsigned_div (Mtime.Span.to_uint64_ns d) 1_000_000L)

let ( +! ) a b =
  let d = Mtime.Span.add a b in
  if Mtime.Span.compare d a < 0 then Mtime.Span.max_span else d

let min_span a b = if Mtime.Span.compare a b <= 0 then a else b

(* VHS's parseTime: seconds without a unit. *)
let time line = function
  | Num n :: Keyword (("ms" | "s" | "m") as u) :: rest -> (n ^ u, rest)
  | Num n :: rest -> (n ^ "s", rest)
  | _ -> unsupported line "expected a duration"

let span line s =
  match duration s with
  | Some ns -> Mtime.Span.of_uint64_ns ns
  | None -> unsupported line "%s is not a duration" s

let speed line = function
  | Sym '@' :: rest ->
      let t, rest = time line rest in
      (Some (span line t), rest)
  | rest -> (None, rest)

let strings tokens =
  String.concat " "
    (List.filter_map (function Str s -> Some s | _ -> None) tokens)

let lit = function
  | Keyword s | Str s | Pattern s | Json_value s | Num s -> s
  | Sym c -> String.make 1 c

type config = {
  shell : string;
  font_size : int;
  font_family : string;
  width : int;
  height : int;
  padding : int;
  margin : int;
  margin_fill : Svg.fill option;
  border_radius : int;
  window_bar : string;
  bar_size : int;
  letter_spacing : float;
  line_height : float;
  rows : int;
  columns : int;
  palette : Palette.t;
  typing_speed : Mtime.Span.t;
  framerate : int;
  playback_speed : float;
  loop_offset : float;
  wait_timeout : Mtime.Span.t;
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
    margin_fill = Some (Colour Palette.default.background);
    border_radius = 0;
    window_bar = "";
    bar_size = 30;
    letter_spacing = 1.;
    line_height = 1.;
    rows = 0;
    columns = 0;
    palette = Palette.default;
    typing_speed = Mtime.Span.(50 * ms);
    framerate = 50;
    playback_speed = 1.;
    loop_offset = 0.;
    wait_timeout = Mtime.Span.(15 * s);
    wait_pattern = Regexp_match.compile ">$";
    cursor_blink = true;
  }

(* Larger sizes are refused, so the frame arithmetic cannot overflow. *)
let max_size = 100_000

let int_value line s =
  match int_of_string_opt s with
  | Some n when n >= 0 && n <= max_size -> n
  | Some _ -> unsupported line "%s is not between 0 and %d" s max_size
  | None -> unsupported line "%s is not an integer" s

let float_value line ?(min = -.float max_size) s =
  match float_of_string_opt s with
  | Some f when f >= min && f <= float max_size -> f
  | Some _ -> unsupported line "%s is not between %g and %d" s min max_size
  | None -> unsupported line "%s is not a number" s

(* Keeps the animation's times within Go's duration range. *)
let min_speed = 0.001

(* The shortest a frame plays, in milliseconds, whatever the speed. *)
let min_frame = 10
let quantum c = max 1 (1000 / c.framerate)

let playback_speed c =
  if c.playback_speed <= 0. then 1.
  else
    Float.min c.playback_speed
      (Float.max 1. (float (quantum c) /. float min_frame))

let is_hex_colour s =
  String.length s = 7
  && s.[0] = '#'
  && String.for_all Char.Ascii.is_hex_digit (String.sub s 1 6)

let set line c name args =
  let value () =
    match args with v :: _ -> lit v | [] -> unsupported line "no value"
  in
  match (name, args) with
  | "Shell", [ Str (("bash" | "zsh" | "fish") as shell) ] -> { c with shell }
  | "Shell", _ -> unsupported line "only bash, zsh and fish play natively"
  | "FontSize", _ -> { c with font_size = int_value line (value ()) }
  | "FontFamily", _ -> { c with font_family = value () ^ ",monospace" }
  | "Width", _ -> { c with width = int_value line (value ()) }
  | "Height", _ -> { c with height = int_value line (value ()) }
  | "Padding", _ -> { c with padding = int_value line (value ()) }
  | "Margin", _ -> { c with margin = int_value line (value ()) }
  | "MarginFill", _ when is_hex_colour (value ()) ->
      let c' = int_of_string ("0x" ^ String.sub (value ()) 1 6) in
      { c with margin_fill = Some (Colour c') }
  | "MarginFill", _ when value () = "" -> { c with margin_fill = None }
  | "BorderRadius", _ -> { c with border_radius = int_value line (value ()) }
  | "LetterSpacing", _ ->
      { c with letter_spacing = float_value line (value ()) }
  | "LineHeight", _ ->
      { c with line_height = float_value line ~min:0. (value ()) }
  | "Rows", _ -> { c with rows = int_value line (value ()) }
  | "Columns", _ -> { c with columns = int_value line (value ()) }
  | "Theme", _ -> (
      let name = value () in
      if String.trim name = "" then { c with palette = Palette.default }
      else if name.[0] = '{' then
        match Palette.of_json name with
        | Ok palette -> { c with palette }
        | Error (`Json msg) -> unsupported line "VHS cannot read it: %s" msg
        | Error (`Unread msg) ->
            unsupported line "this library cannot read it, though VHS does: %s"
              msg
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
      { c with typing_speed = span line (n ^ u) }
  | "Framerate", _ -> (
      match int_of_string_opt (value ()) with
      | Some n when n >= 0 -> { c with framerate = max 1 n }
      | Some _ -> unsupported line "%s is negative" (value ())
      | None -> unsupported line "%s is not an integer" (value ()))
  | "PlaybackSpeed", _ ->
      let s = float_value line (value ()) in
      if s > 0. && s < min_speed then
        unsupported line "a speed below %g" min_speed
      else { c with playback_speed = s }
  | "LoopOffset", _ -> (
      match float_of_string_opt (value ()) with
      | Some f when Float.is_finite f ->
          let f = Float.rem (f /. 100.) 1. in
          { c with loop_offset = (if f < 0. then f +. 1. else f) }
      | _ -> unsupported line "%s is not a number" (value ()))
  | "WaitTimeout", _ ->
      { c with wait_timeout = span line (fst (time line args)) }
  | "WaitPattern", _ ->
      { c with wait_pattern = Regexp_match.compile (value ()) }
  | "CursorBlink", _ -> { c with cursor_blink = value () = "true" }
  | "WindowBar", _ -> (
      match value () with
      | ("" | "Colorful" | "ColorfulRight" | "Rings" | "RingsRight") as bar ->
          { c with window_bar = bar }
      | _ -> unsupported line "not a bar style of VHS")
  | "WindowBarSize", _ -> { c with bar_size = int_value line (value ()) }
  | _ -> unsupported line "not a setting this module knows"

(* xterm.js fits whole cells of JetBrains Mono, VHS's first font, in the
   viewport less 10px padding and a 15px scrollbar; Chrome rounds the font's
   ascent and descent. *)

let round f = int_of_float (Float.round f)
let max_cells = 2_000
let even n = if n mod 2 = 0 then n else n + 1

let bar c =
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

let check_cells ~cols ~rows =
  if cols > max_cells || rows > max_cells then
    Fmt.kstr
      (fun why -> stop (Unsupported why))
      "a %d x %d terminal is beyond %d cells a side" cols rows max_cells

let check_pixels ~width ~height =
  if width > max_size || height > max_size then
    Fmt.kstr
      (fun why -> stop (Unsupported why))
      "a %d x %d image is beyond %d pixels a side" width height max_size

let style c =
  let fs = float c.font_size in
  let ascent = round (1.02 *. fs) and descent = round (0.3 *. fs) in
  let char_height = ascent + descent in
  let cell_width = max 1 ((c.font_size * 6 / 10) + round c.letter_spacing) in
  let cell_height = max 1 (int_of_float (float char_height *. c.line_height)) in
  (* VHS draws no margin without a fill. *)
  let margin = if c.margin_fill = None then 0 else c.margin in
  let frame = 2 * (c.padding + margin) in
  let bar = bar c in
  let bar_height = match bar with Some b -> b.size | None -> 0 in
  let cols =
    if c.columns > 0 then c.columns
    else max 2 ((c.width - frame - 25) / cell_width)
  in
  let rows =
    if c.rows > 0 then c.rows
    else max 1 ((c.height - frame - bar_height - 10) / cell_height)
  in
  check_cells ~cols ~rows;
  let width =
    if c.columns > 0 then even ((cols * cell_width) + 25 + frame) else c.width
  in
  let height =
    if c.rows > 0 then even ((rows * cell_height) + 10 + frame + bar_height)
    else c.height
  in
  check_pixels ~width ~height;
  Svg.
    {
      width;
      height;
      x = margin + c.padding;
      y = margin + bar_height + c.padding;
      cols;
      rows;
      cell_width;
      cell_height;
      baseline = ((cell_height - char_height) / 2) + ascent;
      font_size = c.font_size;
      font_family = c.font_family;
      palette = c.palette;
      margin;
      margin_fill =
        Option.value c.margin_fill ~default:(Colour Palette.default.background);
      radius = c.border_radius;
      bar;
      blink = c.cursor_blink;
      loop_offset = 0;
      speed = playback_speed c;
    }

type t = {
  config : config;
  look : Svg.look;
  actions : command list;
  env : (string * string) list;
  requires : string list;
  text : string option;
}

let is_text path =
  match extension path with ".txt" | ".ascii" | ".test" -> true | _ -> false

let wait_command ~at ~file line tokens =
  let scope, rest =
    match tokens with
    | Sym '+' :: Str "Screen" :: rest -> (Screen, rest)
    | Sym '+' :: Str "Line" :: rest -> (Line, rest)
    | rest -> (Line, rest)
  in
  let timeout, rest = speed line rest in
  let pattern =
    match rest with
    | Pattern p :: _ -> Some (Regexp_match.compile p)
    | _ -> None
  in
  Wait { line; at; file; scope; timeout; pattern }

let sourced ~source line path =
  let invalid fmt =
    Fmt.kstr (fun why -> stop (Invalid (line ^ ": " ^ why))) fmt
  in
  let src =
    match source path with
    | Ok "" -> invalid "VHS refuses an empty tape"
    | Ok src -> src
    | Error e -> stop e
  in
  String.split_on_char '\n' src
  |> List.mapi (fun i l -> (i + 1, l))
  |> List.filter_map (fun (at, l) ->
      if has_control ~except:"\t\r" l then
        invalid "%a holds a control character" Quote.pp l
      else
        match check l with
        | Error e -> invalid "%s: %s" l e
        | Ok (Output _) -> None
        | Ok _ when List.mem (Keyword "Source") (tokens l) ->
            invalid "VHS refuses a nested Source"
        | Ok _ -> Some (at, l))

let base64 s =
  let alphabet =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  in
  let n = String.length s in
  let b = Buffer.create (((n + 2) / 3 * 4) + 1) in
  let byte i = if i < n then Char.code s.[i] else 0 in
  let rec go i =
    if i < n then (
      let v = (byte i lsl 16) lor (byte (i + 1) lsl 8) lor byte (i + 2) in
      let chars = min 4 (n - i + 1) in
      for k = 0 to 3 do
        if k < chars then
          Buffer.add_char b alphabet.[(v lsr (18 - (6 * k))) land 63]
        else Buffer.add_char b '='
      done;
      go (i + 3))
  in
  go 0;
  Buffer.contents b

let image_type data =
  let at i prefix =
    String.length data >= i + String.length prefix
    && String.sub data i (String.length prefix) = prefix
  in
  if at 0 "\x89PNG\r\n\x1a\n" then Some "image/png"
  else if at 0 "\xff\xd8\xff" then Some "image/jpeg"
  else if at 0 "GIF87a" || at 0 "GIF89a" then Some "image/gif"
  else if at 0 "RIFF" && at 8 "WEBP" then Some "image/webp"
  else if at 0 "BM" then Some "image/bmp"
  else None

let data_uri ~source line path =
  match source path with
  | Ok data -> (
      match image_type data with
      | Some mime -> "data:" ^ mime ^ ";base64," ^ base64 data
      | None ->
          stop (Invalid (line ^ ": not a PNG, JPEG, GIF, WebP or BMP image")))
  | Error e -> stop e

let rec read ~source ?file ~at line t =
  match tokens line with
  | [] -> t
  | Keyword "Source" :: p :: _ ->
      let file = lit p in
      List.fold_left
        (fun t (at, l) -> read ~source ~file ~at l t)
        t
        (sourced ~source line file)
  | Keyword "Output" :: p :: _ ->
      if is_text (lit p) then { t with text = Some (lit p) } else t
  | Keyword "Set" :: Keyword "TypingSpeed" :: args when t.actions <> [] ->
      let c = set line t.config "TypingSpeed" args in
      { t with actions = Set_typing_speed c.typing_speed :: t.actions }
  | Keyword ("Set" | "Require") :: _ when t.actions <> [] -> t
  | [ Keyword "Set"; Keyword "MarginFill"; Str p ] when p <> "" && p.[0] <> '#'
    ->
      let fill = Svg.Image (data_uri ~source line p) in
      { t with config = { t.config with margin_fill = Some fill } }
  | Keyword "Set" :: Keyword name :: args ->
      { t with config = set line t.config name args }
  | Keyword "Env" :: k :: v :: _ -> (
      match Tape_syntax.env_name_problem (lit k) with
      | Some why -> stop (Invalid (line ^ ": " ^ why))
      | None ->
          { t with env = (lit k, lit v) :: t.env; actions = Env :: t.actions })
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
              match rest with
              | Num n :: _ -> (
                  match int_of_string_opt n with
                  | Some n when n >= 0 -> n
                  | _ -> unsupported line "%s is not a repeat count" n)
              | _ -> 1
            in
            Key { speed; count; key = k }
        | Keyword "Ctrl" :: rest ->
            Ctrl (List.filter (function Sym '+' -> false | _ -> true) rest)
        | Keyword "Alt" :: Sym '+' :: k :: _ -> Alt k
        | Keyword "Shift" :: Sym '+' :: k :: _ -> Shift k
        | Keyword "Sleep" :: rest -> Sleep (span line (fst (time line rest)))
        | Keyword "Wait" :: rest -> wait_command ~at ~file line rest
        | [ Keyword "Hide" ] -> Hide
        | [ Keyword "Show" ] -> Show
        | Keyword "Copy" :: rest -> Copy (strings rest)
        | [ Keyword "Paste" ] -> Paste
        | Keyword "Screenshot" :: p :: _ ->
            if extension (lit p) = ".svg" then Screenshot (lit p)
            else unsupported line "a PNG image, which the Vhs engine takes"
        | _ -> unsupported line "not a command this module knows"
      in
      { t with actions = action :: t.actions }

let of_lines ?(source = fun path -> Error (Io (path ^ ": no file to read")))
    lines =
  let config = default_config () in
  let empty =
    {
      config;
      look = style config;
      actions = [];
      env = [];
      requires = [];
      text = None;
    }
  in
  match
    let t, _ =
      List.fold_left
        (fun (t, at) line -> (read ~source ~at line t, at + 1))
        (empty, 1) lines
    in
    { t with look = style t.config }
  with
  | t -> Ok { t with actions = List.rev t.actions; env = List.rev t.env }
  | exception Stop e -> Error e

type timeline = {
  quantum : int;
  mutable frames : (int * Screen.t) list;  (** Latest first. *)
}

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
  Svg.image { s with loop_offset } ~duration frames

let timeline c = { quantum = quantum c; frames = [] }

type recording = {
  svg : string;
  text : string;
  stills : (string * string) list;
}

let text_output (t : t) = t.text

let stills t =
  List.filter_map (function Screenshot p -> Some p | _ -> None) t.actions

let size t = (t.look.cols, t.look.rows)
let palette t = t.look.palette

(* As terminals, synchronized output ends after 150ms. *)
let sync_timeout = 150
let sync_span = Mtime.Span.(sync_timeout * ms)

let replay t events =
  let s = t.look in
  let term = Screen.terminal ~rows:s.rows ~cols:s.cols in
  let tl = timeline t.config in
  let screen = ref (Screen.of_terminal term) in
  add tl 0 !screen;
  let sync = ref None in
  let show ms =
    if !sync = None then (
      screen := Screen.of_terminal ~previous:!screen term;
      Screen.clear_dirty term;
      add tl ms !screen)
  in
  let expire ms =
    match !sync with
    | Some since when ms >= since + sync_timeout ->
        sync := None;
        show (since + sync_timeout)
    | _ -> ()
  in
  let synchronize ms on =
    if not on then sync := None;
    show ms;
    if on && !sync = None then sync := Some ms
  in
  let end_ms =
    List.fold_left
      (fun _ (ms, event) ->
        expire ms;
        (match (event : Asciicast.event) with
        | Output bytes ->
            Screen.feed ~sync:(synchronize ms) term bytes;
            ignore (Screen.take_replies term)
        | Resize (cols, rows) ->
            Screen.resize term ~rows:(min rows s.rows) ~cols:(min cols s.cols)
        | Exit _ -> ());
        show ms;
        ms)
      0 events
  in
  expire max_int;
  (svg t tl ~end_ms, !screen)

(* The bytes xterm.js sends. *)

let arrow term c = if Screen.cursor_keys term then "\027O" ^ c else "\027[" ^ c

let key_bytes term = function
  | "Enter" -> "\r"
  | "Tab" -> "\t"
  | "Space" -> " "
  | "Backspace" -> "\x7f"
  | "Delete" -> "\027[3~"
  | "Insert" -> "\027[2~"
  | "Escape" -> "\027"
  | "Up" -> arrow term "A"
  | "Down" -> arrow term "B"
  | "Right" -> arrow term "C"
  | "Left" -> arrow term "D"
  | "PageUp" -> "\027[5~"
  | "PageDown" -> "\027[6~"
  | _ -> ""

let ctrl_char c =
  match c with
  | 'a' .. 'z' -> String.make 1 (Char.chr (Char.code c - 96))
  | 'A' .. 'Z' | '@' | '[' | '\\' | ']' | '^' ->
      String.make 1 (Char.chr (Char.code c land 0x1f))
  | '-' -> "\x1f"
  | c -> String.make 1 c

(* xterm.js: [CSI 1 ; m X] with [m] one more than Shift 1 + Alt 2 + Control
   4 for an arrow; Alt prefixes ESC to the other keys. *)
let ctrl_bytes args =
  let alt = List.mem (Keyword "Alt") args
  and shift = List.mem (Keyword "Shift") args in
  let arrow c =
    Fmt.str "\027[1;%d%c"
      (1 + 4 + (if alt then 2 else 0) + if shift then 1 else 0)
      c
  in
  let key = function
    | Keyword "Enter" -> "\r"
    | Keyword "Space" -> "\x00"
    | Keyword "Backspace" -> "\x08"
    | Keyword "Up" -> arrow 'A'
    | Keyword "Down" -> arrow 'B'
    | Keyword "Right" -> arrow 'C'
    | Keyword "Left" -> arrow 'D'
    | Keyword ("Alt" | "Shift") -> ""
    | Str s when String.length s = 1 -> ctrl_char s.[0]
    | Sym c -> ctrl_char c
    | _ -> ""
  in
  let is_arrow = function
    | Keyword ("Up" | "Down" | "Right" | "Left") -> true
    | _ -> false
  in
  (if alt && not (List.exists is_arrow args) then "\027" else "")
  ^ String.concat "" (List.map key args)

let modified ~shift = function
  | Keyword "Enter" -> "\r"
  | Keyword "Tab" -> if shift then "\027[Z" else "\t"
  | Str s -> if shift then String.uppercase_ascii s else s
  | t -> lit t

(* k7 keeps [slave] open: matrix 0.1.0's [Pty.spawn] closes it, so macOS loses
   the output of a program that exits at once. *)
type terminal = { pty : Pty.t; slave : Pty.t; pid : int }

type state = {
  term : Screen.terminal;
  pty : Pty.t;
  pid : int;
  clock : Eio.Time.Mono.ty Eio.Resource.t;
  tl : timeline;
  changed : Eio.Condition.t;
  mutable live : Screen.t;
  mutable shown : Screen.t;
  mutable pending : bool;  (** [live] is not shown yet. *)
  mutable sync : Mtime.Span.t option;
      (** When synchronized output started, which holds [live] back. *)
  mutable offset : int;
  mutable hidden : bool;
  mutable logical : Mtime.Span.t;
  mutable started : Mtime.t;
  mutable cap : Mtime.Span.t option;
  mutable typing_speed : Mtime.Span.t;
  mutable clipboard : string;
  text : Buffer.t;
  dumps : bool;
  mutable still : string option;
  mutable stills : (string * Screen.t) list;
  mutable submitted : int option;
  mutable exited : Eio.Process.exit_status option;
  stuck : string -> bool;
  since : Buffer.t;
  mutable captured : (int * string) list option;  (** Latest first. *)
}

let now st = Eio.Time.Mono.now st.clock
let elapsed st = Mtime.span st.started (now st)

let stamp st =
  st.logical
  +! match st.cap with Some c -> min_span c (elapsed st) | None -> elapsed st

let record ?at st =
  if not st.hidden then (
    add st.tl
      (ms_of_span (match at with Some d -> d | None -> stamp st))
      st.shown;
    Option.iter
      (fun path ->
        st.stills <- (path, st.shown) :: List.remove_assoc path st.stills;
        st.still <- None)
      st.still)

let show ?at st =
  if st.sync = None then (
    st.pending <- false;
    st.shown <-
      (if st.offset = 0 then st.live
       else Screen.of_terminal ~offset:st.offset st.term);
    record ?at st)
  else st.pending <- true

let refresh st =
  if Screen.is_dirty st.term then (
    st.live <- Screen.of_terminal ~previous:st.live st.term;
    Screen.clear_dirty st.term;
    st.pending <- true);
  if st.pending then show st;
  Eio.Condition.broadcast st.changed

let expire st =
  match st.sync with
  | Some since
    when not (Mtime.Span.is_shorter (stamp st) ~than:(since +! sync_span)) ->
      st.sync <- None;
      if st.pending then show ~at:(since +! sync_span) st
  | _ -> ()

let synchronize st on =
  if not on then st.sync <- None;
  refresh st;
  if on && st.sync = None then st.sync <- Some (stamp st)

let fd st = Pty.file_descr st.pty

let send st s =
  let rec go off =
    if off < String.length s then
      match Pty.write_string st.pty s off (String.length s - off) with
      | n -> go (off + n)
      | exception Unix.Unix_error ((EAGAIN | EWOULDBLOCK | EINTR), _, _) ->
          Eio_unix.await_writable (fd st);
          go off
      | exception Unix.Unix_error _ -> ()
  in
  go 0

let is_line_feed c = c = '\n' || c = '\x0b' || c = '\x0c'

let settle st output =
  match st.submitted with
  | Some row
    when Screen.cursor_row st.term <> row || String.exists is_line_feed output
    ->
      st.submitted <- None
  | _ -> ()

let exit_status : Unix.process_status -> Eio.Process.exit_status = function
  | WEXITED c -> `Exited c
  | WSIGNALED s | WSTOPPED s -> `Signaled s

let exited st =
  Option.is_some st.exited
  ||
  match Unix.waitpid [ WNOHANG ] st.pid with
  | 0, _ -> false
  | _, s ->
      st.exited <- Some (exit_status s);
      true
  | exception Unix.Unix_error _ -> true

let closed st =
  Eio.Condition.broadcast st.changed;
  Eio.Fiber.await_cancel ()

let kept = 65536

(* What the shell wrote since the last wait held, at least its last [kept]
   bytes. *)
let since st output =
  if Buffer.length st.since > 2 * kept then (
    let tail = Buffer.sub st.since (Buffer.length st.since - kept) kept in
    Buffer.clear st.since;
    Buffer.add_string st.since tail);
  Buffer.add_string st.since output

let last_output st =
  let n = Buffer.length st.since in
  if n <= kept then Buffer.contents st.since
  else Buffer.sub st.since (n - kept) kept

let feed st buf n =
  let output = Bytes.sub_string buf 0 n in
  Option.iter
    (fun c -> st.captured <- Some ((ms_of_span (stamp st), output) :: c))
    st.captured;
  since st output;
  expire st;
  Screen.feed ~sync:(synchronize st) st.term output;
  settle st output;
  send st (Screen.take_replies st.term);
  refresh st

(* In real time: [st.clock] gives the recording's times, and a clock that
   stands still must not stop the reader. *)
let poll () = Eio_unix.sleep 0.02

let reader st buf =
  Pty_reader.run ~read:(Pty.read st.pty)
    ~ready:(fun () ->
      Eio.Fiber.first (fun () -> Eio_unix.await_readable (fd st)) poll)
    ~pause:poll
    ~exited:(fun () -> exited st)
    ~feed:(feed st) buf

let write st s =
  if st.offset > 0 then (
    st.offset <- 0;
    show st);
  if String.exists (fun c -> c = '\r' || c = '\n') s then
    st.submitted <- Some (Screen.cursor_row st.term);
  send st s

let step st d =
  st.started <- now st;
  st.cap <- Some d;
  Eio.Time.Mono.sleep_span st.clock d;
  if not st.hidden then st.logical <- st.logical +! d;
  st.started <- now st;
  st.cap <- Some Mtime.Span.zero

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
    show st;
    step st (Option.value speed ~default:st.typing_speed)
  done

let within st d f =
  Eio.Fiber.first
    (fun () ->
      f ();
      true)
    (fun () ->
      Eio.Time.Mono.sleep_span st.clock d;
      false)

(* VHS reads as many lines as the screen has rows, from the buffer's top. *)
let top st =
  Screen.buffer st.term st.live
  |> Array.to_list
  |> List.take (Array.length (Screen.rows st.live))

let wait_text st = function
  | Line ->
      let row = Screen.cursor_row st.term in
      if st.offset = 0 then Screen.line st.live row
      else
        (* VHS reads the cursor's row on the viewport. *)
        let lines = Screen.buffer st.term st.live
        and i = Screen.history st.term + row - st.offset in
        if i >= 0 && i < Array.length lines then lines.(i) else ""
  | Screen -> String.concat "\n" (top st)

let wait st c ~line ~at ~file ~scope ~timeout ~pattern =
  let re = Option.value pattern ~default:c.wait_pattern in
  let timeout = Option.value timeout ~default:c.wait_timeout in
  st.started <- now st;
  st.cap <- None;
  (* The prompt a command was typed at is not the one that follows it. *)
  let holds () =
    let h = Regexp_match.matches re (wait_text st scope) in
    match (scope, st.submitted) with
    | Line, Some _ ->
        if not h then st.submitted <- None;
        false
    | _ -> h
  in
  let rec loop () =
    if not (holds ()) then
      match st.exited with
      | Some status ->
          stop
            (Exited
               {
                 status;
                 wait = line;
                 line = at;
                 file;
                 last = wait_text st scope;
               })
      | None when st.stuck (last_output st) ->
          stop
            (Stuck { wait = line; line = at; file; last = wait_text st scope })
      | None ->
          Eio.Condition.await_no_mutex st.changed;
          loop ()
  in
  if not (within st timeout loop) then
    stop (Timeout { wait = line; line = at; file; last = wait_text st scope });
  Buffer.clear st.since;
  if not st.hidden then st.logical <- st.logical +! elapsed st;
  st.started <- now st;
  st.cap <- Some Mtime.Span.zero

(* VHS's text output: the {!top} lines after each command, then 80 box
   characters. *)
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
        press st ?speed (key_bytes st.term key)
      done
  | Ctrl args -> press st (ctrl_bytes args)
  | Alt k -> press st ("\027" ^ modified ~shift:false k)
  | Shift k -> press st (modified ~shift:true k)
  | Sleep d -> step st d
  | Wait { line; at; file; scope; timeout; pattern } ->
      wait st c ~line ~at ~file ~scope ~timeout ~pattern
  | Hide -> st.hidden <- true
  | Show ->
      st.hidden <- false;
      record st
  | Set_typing_speed d -> st.typing_speed <- d
  | Copy s -> st.clipboard <- s
  | Paste -> write st st.clipboard
  | Screenshot path ->
      st.still <- Some path;
      record st
  | Env -> ()

let run st c command =
  expire st;
  run_command st c command;
  dump st

let is_executable file =
  match Unix.stat file with
  | { st_kind = S_REG; _ } -> (
      match Unix.access file [ X_OK ] with
      | () -> true
      | exception Unix.Unix_error _ -> false)
  | _ -> false
  | exception Unix.Unix_error _ -> false

(* The child runs in [cwd], so a relative path stays relative. As execvp,
   a file that cannot run is found when no other one can, so that exec
   reports why. *)
let resolve ~cwd env exe =
  let in_cwd p = if Filename.is_relative p then Filename.concat cwd p else p in
  if String.contains exe '/' then
    if Sys.file_exists (in_cwd exe) then Some exe else None
  else
    let dirs =
      match Eio.Process.Env.get_opt "PATH" env with
      | None -> []
      | Some path -> String.split_on_char ':' path
    in
    let candidates =
      List.filter_map
        (fun dir ->
          let p = Filename.concat (if dir = "" then "." else dir) exe in
          if Sys.file_exists (in_cwd p) then Some p else None)
        dirs
    in
    match List.find_opt (fun p -> is_executable (in_cwd p)) candidates with
    | Some p -> Some p
    | None -> List.nth_opt candidates 0

let close_terminal (t : terminal) =
  Pty.close t.pty;
  Pty.close t.slave

(* The program leads its own process group, so its descendants get the
   signals too. Once the program is reaped its pid may be reused, but not
   as the id of a group while the group has a member. *)
let stop_terminal clock ~reaped (t : terminal) =
  let reaped = ref reaped in
  let rec reap flags =
    if not !reaped then
      match Unix.waitpid flags t.pid with
      | 0, _ -> ()
      | exception Unix.Unix_error (EINTR, _, _) -> reap flags
      | _ | (exception Unix.Unix_error _) -> reaped := true
  in
  let signal s =
    match Unix.kill (-t.pid) s with
    | () -> true
    | exception Unix.Unix_error _ ->
        (if not !reaped then
           try Unix.kill t.pid s with Unix.Unix_error _ -> ());
        false
  in
  let running = signal Sys.sigterm in
  close_terminal t;
  let rec wait n =
    reap [ WNOHANG ];
    if n > 0 && (signal 0 || not !reaped) then (
      Eio.Time.Mono.sleep clock 0.01;
      wait (n - 1))
  in
  if running || not !reaped then (
    wait 100;
    if signal Sys.sigkill then wait 100);
  reap []

external spawn :
  Unix.file_descr ->
  string ->
  string array ->
  string array ->
  string ->
  (int, Unix.error * string) result = "native_spawn"

let lower e = String.uncapitalize_ascii (Unix.error_message e)
let unix_error e f = Fmt.kstr (fun s -> Io s) "%s: %s" f (Unix.error_message e)

let check_argv argv =
  if List.exists (fun a -> String.contains a '\000') argv then
    Error (unix_error EINVAL "execve")
  else Ok ()

let child_error ~cwd ~path ~prog e f =
  match (e, f) with
  | Unix.ENOENT, "execve" -> Executable_not_found prog
  | EACCES, "execve" when try Sys.is_directory path with Sys_error _ -> false ->
      Io (Fmt.str "%s: is a directory" prog)
  | e, "chdir" -> Io (Fmt.str "%s: %s" cwd (lower e))
  | e, "execve" -> Io (Fmt.str "%s: %s" prog (lower e))
  | e, f -> Io (Fmt.str "%s: %s" f (lower e))

let open_terminal ~env ~cwd ~winsize ~path prog args =
  let argv = Array.of_list (prog :: args)
  and env = Eio.Process.Env.to_array env in
  let pty, slave = Pty.open_pty ~winsize () in
  let close_terminal () =
    Pty.close pty;
    Pty.close slave
  in
  match
    Unix.set_close_on_exec (Pty.file_descr pty);
    Unix.set_close_on_exec (Pty.file_descr slave);
    spawn (Pty.file_descr slave) path argv env cwd
  with
  | Ok pid -> { pty; slave; pid }
  | Error (e, f) ->
      close_terminal ();
      stop (child_error ~cwd ~path ~prog e f)
  | exception e ->
      close_terminal ();
      raise e

(* VHS's shell.go. *)
let shell = function
  | "zsh" ->
      ( [ ("PROMPT", "%F{#5B56E0}> %F{reset_color}") ],
        [ "--histnostore"; "--no-rcs" ] )
  | "fish" ->
      ( [],
        [
          "--login";
          "--no-config";
          "--private";
          "-C";
          "function fish_greeting; end";
          "-C";
          {|function fish_prompt; set_color 5B56E0; echo -n "> "; set_color normal; end|};
        ] )
  | _ ->
      ( [
          ("PS1", {|\[\e[38;2;90;86;224m\]> \[\e[0m\]|});
          ("BASH_SILENCE_DEPRECATION_WARNING", "1");
        ],
        [ "--noprofile"; "--norc"; "--login"; "+o"; "history" ] )

let term = ("TERM", "xterm-256color")

let start ~env ~vars ~cwd t ~requires prog args =
  let env =
    Eio.Process.Env.override (List.map (fun (k, v) -> (k, Some v)) vars) env
  in
  let find exe =
    match resolve ~cwd env exe with
    | Some path -> path
    | None -> stop (Executable_not_found exe)
  in
  let path = find prog in
  List.iter
    (fun exe ->
      match resolve ~cwd env exe with
      | Some p
        when is_executable
               (if Filename.is_relative p then Filename.concat cwd p else p) ->
          ()
      | _ -> stop (Executable_not_found exe))
    requires;
  let s = t.look in
  match
    open_terminal ~env ~cwd
      ~winsize:{ rows = s.Svg.rows; cols = s.cols; xpixel = 0; ypixel = 0 }
      ~path prog args
  with
  | terminal -> terminal
  | exception Unix.Unix_error (e, f, _) -> stop (unix_error e f)

let spawn ~env ~vars ~cwd t =
  let prog = t.config.shell in
  let prompt, args = shell prog in
  start ~env
    ~vars:((term :: vars) @ prompt @ t.env)
    ~cwd t ~requires:t.requires prog args

let first_prompt = "the shell's first prompt"

(* The prompt arrives before the first action, as in VHS. *)
let await_prompt st c =
  let blank () =
    Screen.text st.live
    = String.make (Array.length (Screen.rows st.live) - 1) '\n'
  in
  let rec loop () =
    match st.exited with
    | Some status ->
        stop
          (Exited
             { status; wait = first_prompt; line = 0; file = None; last = "" })
    | None when blank () ->
        Eio.Condition.await_no_mutex st.changed;
        loop ()
    | None -> ()
  in
  if not (within st c.wait_timeout loop) then
    stop (Timeout { wait = first_prompt; line = 0; file = None; last = "" });
  Buffer.clear st.since

let state ?(stuck = fun _ -> false) t ~clock ({ pty; pid; _ } : terminal) =
  let s = t.look in
  let term = Screen.terminal ~rows:s.rows ~cols:s.cols in
  let screen = Screen.of_terminal term in
  {
    term;
    pty;
    pid;
    clock;
    tl = timeline t.config;
    changed = Eio.Condition.create ();
    live = screen;
    shown = screen;
    pending = false;
    sync = None;
    offset = 0;
    hidden = true;
    logical = Mtime.Span.zero;
    started = Eio.Time.Mono.now clock;
    cap = Some Mtime.Span.zero;
    typing_speed = t.config.typing_speed;
    clipboard = "";
    text = Buffer.create 4096;
    dumps = Option.is_some t.text;
    still = None;
    stills = [];
    submitted = None;
    exited = None;
    stuck;
    since = Buffer.create 4096;
    captured = None;
  }

let recording t st =
  let still (path, screen) =
    ( path,
      Svg.image
        { t.look with blink = false }
        ~duration:st.tl.quantum
        [ (0, screen) ] )
  in
  {
    svg = svg t st.tl ~end_ms:(ms_of_span st.logical);
    text = Buffer.contents st.text;
    stills = List.rev_map still st.stills;
  }

let with_terminal ?stuck ~clock t (terminal : terminal) f =
  let st = state ?stuck t ~clock terminal in
  let finally () =
    stop_terminal clock ~reaped:(Option.is_some st.exited) terminal
  in
  Fun.protect ~finally:(fun () -> Eio.Cancel.protect finally) @@ fun () ->
  Pty.set_nonblock terminal.pty;
  f st

let until interrupt f =
  match interrupt with
  | None -> f ()
  | Some p -> Eio.Fiber.first f (fun () -> Eio.Promise.await p)

let play ?interrupt ?stuck ~clock ~env ~vars ~cwd t =
  let clock = (clock :> Eio.Time.Mono.ty Eio.Resource.t) in
  match
    with_terminal ?stuck ~clock t (spawn ~env ~vars ~cwd t) @@ fun st ->
    Eio.Fiber.first
      (fun () ->
        reader st (Bytes.create 65536);
        closed st)
      (fun () ->
        until interrupt @@ fun () ->
        await_prompt st t.config;
        st.hidden <- false;
        st.started <- now st;
        record st;
        List.iter (run st t.config) t.actions;
        expire st);
    recording t st
  with
  | r -> Ok r
  | exception Stop e -> Error e

(* As [stop_terminal], without hanging up, so that the reader reads what the
   program prints until it exits. *)
let terminate clock (t : terminal) =
  let signal s = try Unix.kill (-t.pid) s with Unix.Unix_error _ -> () in
  signal Sys.sigterm;
  Eio.Time.Mono.sleep clock 1.;
  signal Sys.sigkill;
  Eio.Fiber.await_cancel ()

let capture ?interrupt ~clock ~env ~vars ~cwd ~line t argv =
  let clock = (clock :> Eio.Time.Mono.ty Eio.Resource.t) in
  match
    let prog, args =
      match argv with
      | prog :: args -> (prog, args)
      | [] -> stop (Invalid "no command")
    in
    let terminal =
      start ~env ~vars:(term :: vars) ~cwd t ~requires:[] prog args
    in
    with_terminal ~clock t terminal @@ fun st ->
    st.hidden <- false;
    st.cap <- None;
    st.started <- now st;
    Screen.feed st.term line;
    refresh st;
    st.captured <- Some [];
    Eio.Fiber.first
      (fun () -> reader st (Bytes.create 65536))
      (fun () ->
        match interrupt with
        | None -> Eio.Fiber.await_cancel ()
        | Some p ->
            Eio.Promise.await p;
            terminate clock terminal);
    let output = List.rev (Option.value st.captured ~default:[]) in
    match st.exited with
    | Some status -> (output, ms_of_span (stamp st), status)
    | None -> stop (Io "the program's exit status is lost")
  with
  | r -> Ok r
  | exception Stop e -> Error e
