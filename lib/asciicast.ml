(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type theme = {
  foreground : string;
  background : string;
  palette : string list;
  cursor : string option;
  cursor_accent : string option;
}

type event = Output of string | Resize of int * int | Exit of int

type t = {
  columns : int;
  rows : int;
  command : string option;
  theme : theme option;
  events : (int * event) list;
}

let ( let* ) = Result.bind

let pp_event ppf = function
  | Output s -> Fmt.Dump.string ppf s
  | Resize (c, r) -> Fmt.pf ppf "resize %dx%d" c r
  | Exit n -> Fmt.pf ppf "exit %d" n

let pp ppf t =
  Fmt.pf ppf "@[<v>%dx%d%a%a@]" t.columns t.rows
    Fmt.(option (any " $ " ++ string))
    t.command
    Fmt.(list ~sep:nop (cut ++ pair ~sep:(any " ") (int ++ any "ms") pp_event))
    t.events

type value = Number of float | String of string

let value =
  Jsont.any ~kind:"number or string"
    ~dec_number:(Jsont.map ~dec:(fun x -> Number x) Jsont.number)
    ~dec_string:(Jsont.map ~dec:(fun s -> String s) Jsont.string)
    ()

let version =
  Jsont.Object.map ~kind:"asciicast header" Fun.id
  |> Jsont.Object.mem "version" value ~enc:Fun.id
  |> Jsont.Object.finish

type raw_theme = {
  fg : string;
  bg : string;
  colours : string;
  cursor : string option;
  accent : string option;
}

let is_colour s =
  String.length s = 7
  && s.[0] = '#'
  && String.for_all Char.Ascii.is_hex_digit (String.sub s 1 6)

(* A member asciicast v3 does not define: one that is not a colour is
   ignored, as an unknown member is. *)
let extension =
  Jsont.map ~kind:"colour"
    ~dec:(function Jsont.String (s, _) when is_colour s -> Some s | _ -> None)
    Jsont.json

let theme =
  Jsont.Object.map ~kind:"asciicast theme" (fun fg bg colours cursor accent ->
      { fg; bg; colours; cursor; accent })
  |> Jsont.Object.mem "fg" Jsont.string ~enc:(fun t -> t.fg)
  |> Jsont.Object.mem "bg" Jsont.string ~enc:(fun t -> t.bg)
  |> Jsont.Object.mem "palette" Jsont.string ~enc:(fun t -> t.colours)
  |> Jsont.Object.mem "cursor" extension ~dec_absent:None ~enc:(fun t ->
      t.cursor)
  |> Jsont.Object.mem "cursor_accent" extension ~dec_absent:None ~enc:(fun t ->
      t.accent)
  |> Jsont.Object.finish

type header = {
  width : value;
  height : value;
  theme : raw_theme option;
  idle : float option;
  command : string option;
}

let common map =
  map
  |> Jsont.Object.opt_mem "idle_time_limit" Jsont.number ~enc:(fun h -> h.idle)
  |> Jsont.Object.opt_mem "command" Jsont.string ~enc:(fun h -> h.command)

let header_v2 =
  Jsont.Object.map ~kind:"asciicast header"
    (fun width height theme idle command ->
      { width; height; theme; idle; command })
  |> Jsont.Object.mem "width" value ~enc:(fun h -> h.width)
  |> Jsont.Object.mem "height" value ~enc:(fun h -> h.height)
  |> Jsont.Object.opt_mem "theme" theme ~enc:(fun h -> h.theme)
  |> common |> Jsont.Object.finish

let term =
  Jsont.Object.map ~kind:"asciicast term" (fun w h t -> (w, h, t))
  |> Jsont.Object.mem "cols" value ~enc:(fun (w, _, _) -> w)
  |> Jsont.Object.mem "rows" value ~enc:(fun (_, h, _) -> h)
  |> Jsont.Object.opt_mem "theme" theme ~enc:(fun (_, _, t) -> t)
  |> Jsont.Object.finish

let header_v3 =
  Jsont.Object.map ~kind:"asciicast header"
    (fun (width, height, theme) idle command ->
      { width; height; theme; idle; command })
  |> Jsont.Object.mem "term" term ~enc:(fun h -> (h.width, h.height, h.theme))
  |> common |> Jsont.Object.finish

let event = Jsont.t3 ~kind:"asciicast event" ~dec:(fun t c d -> (t, c, d)) value

let decode codec line =
  match Jsont_bytesrw.decode_string' ~locs:true codec line with
  | Ok v -> Ok v
  | Error (_, meta, kind) ->
      let at = Jsont.Textloc.first_byte (Jsont.Meta.textloc meta) in
      Fmt.error "%s at byte %d"
        (Palette.plain (Jsont.Error.kind_to_string kind))
        at

let max_ms = 9_223_372_036_854

let pp_number ppf x =
  if Float.is_integer x && Float.abs x < 1e15 then Fmt.pf ppf "%.0f" x
  else Fmt.pf ppf "%.17g" x

let size name = function
  | Number x when Float.is_integer x && x >= 1. && x <= float max_int ->
      Ok (Float.to_int x)
  | Number x -> Fmt.error "%s %a is not a positive integer" name pp_number x
  | String s -> Fmt.error "%s %S is not a number" name s

let read_theme { fg; bg; colours; cursor; accent } =
  let palette = String.split_on_char ':' colours in
  match List.find_opt (Fun.negate is_colour) (fg :: bg :: palette) with
  | Some c -> Fmt.error "theme: %S is not a colour #rrggbb" c
  | None ->
      let n = List.length palette in
      if n <> 8 && n <> 16 then
        Fmt.error "theme: a palette of %d colours, not 8 or 16" n
      else
        Ok
          {
            foreground = fg;
            background = bg;
            palette;
            cursor;
            cursor_accent = accent;
          }

type format = V2 | V3

let read_header line =
  let* v = decode version line in
  let* format, (w, h), codec =
    match v with
    | Number 2. -> Ok (V2, ("width", "height"), header_v2)
    | Number 3. -> Ok (V3, ("cols", "rows"), header_v3)
    | Number x -> Fmt.error "asciicast version %a, not 2 or 3" pp_number x
    | String s -> Fmt.error "asciicast version %S, not 2 or 3" s
  in
  let* hd = decode codec line in
  let* columns = size w hd.width in
  let* rows = size h hd.height in
  let* theme =
    match hd.theme with
    | None -> Ok None
    | Some t -> Result.map Option.some (read_theme t)
  in
  Ok
    ( format,
      hd.idle,
      { columns; rows; command = hd.command; theme; events = [] } )

(* Microseconds. *)
let us seconds = Float.round (seconds *. 1e6)
let ms us = (Float.to_int us + 500) / 1000
let max_us = float max_ms *. 1e3
let digits s = s <> "" && String.for_all Char.Ascii.is_digit s

let positive s =
  match int_of_string_opt s with
  | Some n when digits s && n > 0 -> Some n
  | _ -> None

let read_data code data =
  match code with
  | "o" -> Ok (Some (Output data))
  | "r" -> (
      match String.split_on_char 'x' data with
      | [ c; r ] -> (
          match (positive c, positive r) with
          | Some c, Some r -> Ok (Some (Resize (c, r)))
          | _ -> Fmt.error "resize %S is not COLSxROWS" data)
      | _ -> Fmt.error "resize %S is not COLSxROWS" data)
  | "x" -> (
      match int_of_string_opt data with
      | Some n when string_of_int n = data -> Ok (Some (Exit n))
      | _ -> Fmt.error "exit status %S is not an integer" data)
  | _ -> Ok None

(* [last] is the time of the event ahead as written, in seconds; [prev] is
   that time in microseconds, and [at] its time once idle intervals are cut. *)
let read_event format ~idle ~last ~prev ~at line =
  let* time, code, data = decode event line in
  match (time, code, data) with
  | Number t, String code, String data ->
      let* written, interval =
        match format with
        | V2 ->
            if t < 0. then Fmt.error "time %as is negative" pp_number t
            else if t < last then
              Fmt.error "time %as is before %as" pp_number t pp_number last
            else Ok (us t, us t -. prev)
        | V3 ->
            if t < 0. then Fmt.error "interval %as is negative" pp_number t
            else Ok (prev +. us t, us t)
      in
      let interval =
        Option.fold ~none:interval ~some:(Float.min interval) idle
      in
      let at = at +. interval in
      if at > max_us || written > max_us then
        Fmt.error "time %as is beyond %dms" pp_number t max_ms
      else
        let* e = read_data code data in
        Ok (t, written, at, Option.map (fun e -> (ms at, e)) e)
  | String _, _, _ -> Error "the time is not a number"
  | _, Number _, _ -> Error "the code is not a string"
  | _, _, Number _ -> Error "the data is not a string"

let is_blank s =
  String.for_all (function ' ' | '\t' | '\r' -> true | _ -> false) s

let of_string s =
  let at n r = Result.map_error (Fmt.str "line %d: %s" n) r in
  let first, rest =
    match String.index_opt s '\n' with
    | Some i ->
        (String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1))
    | None -> (s, "")
  in
  if is_blank first then Error "line 1: no asciicast header"
  else
    let* format, idle, t = at 1 (read_header first) in
    let idle = Option.map (fun s -> us (Float.max 0. s)) idle in
    let rec events n ~last ~prev ~at:time acc = function
      | [] -> Ok (List.rev acc)
      | line :: rest when is_blank line ->
          events (n + 1) ~last ~prev ~at:time acc rest
      | line :: rest when format = V3 && line.[0] = '#' ->
          events (n + 1) ~last ~prev ~at:time acc rest
      | line :: rest ->
          let* last, prev, time, e =
            at n (read_event format ~idle ~last ~prev ~at:time line)
          in
          let acc = match e with Some e -> e :: acc | None -> acc in
          events (n + 1) ~last ~prev ~at:time acc rest
    in
    let* events =
      events 2 ~last:0. ~prev:0. ~at:0. [] (String.split_on_char '\n' rest)
    in
    Ok { t with events }

(* An incomplete UTF-8 sequence at the end of [s], which the next output may
   complete: [s] splits before it. *)
let incomplete s =
  let n = String.length s in
  let rec go i =
    if i >= n then n
    else
      let d = String.get_utf_8_uchar s i in
      let len = Uchar.utf_decode_length d in
      if (not (Uchar.utf_decode_is_valid d)) && i + len = n then i
      else go (i + len)
  in
  go 0

let valid_utf_8 s =
  if String.is_valid_utf_8 s then s
  else
    let b = Buffer.create (String.length s) in
    let rec go i =
      if i < String.length s then (
        let d = String.get_utf_8_uchar s i in
        Buffer.add_utf_8_uchar b (Uchar.utf_decode_uchar d);
        go (i + Uchar.utf_decode_length d))
    in
    go 0;
    Buffer.contents b

let utf_8 events =
  let is_output = function _, Output _ -> true | _ -> false in
  let rec go ~pending ~outputs acc = function
    | [] -> List.rev acc
    | (ms, Output s) :: rest ->
        let s = pending ^ s in
        let i = if outputs > 1 then incomplete s else String.length s in
        let acc =
          if i = 0 then acc
          else (ms, Output (valid_utf_8 (String.sub s 0 i))) :: acc
        in
        go
          ~pending:(String.sub s i (String.length s - i))
          ~outputs:(outputs - 1) acc rest
    | e :: rest -> go ~pending ~outputs (e :: acc) rest
  in
  go ~pending:"" ~outputs:(List.length (List.filter is_output events)) [] events

let json codec v =
  match Jsont_bytesrw.encode_string codec v with
  | Ok s -> s
  | Error e -> invalid_arg e

let string = json Jsont.string

let header t =
  let b = Buffer.create 256 in
  Fmt.kstr (Buffer.add_string b)
    {|{"version":3,"term":{"cols":%d,"rows":%d,"type":"xterm-256color"|}
    t.columns t.rows;
  Option.iter
    (fun th ->
      Fmt.kstr (Buffer.add_string b) {|,"theme":{"fg":%s,"bg":%s,"palette":%s|}
        (string th.foreground) (string th.background)
        (string (String.concat ":" th.palette));
      let colour name =
        Option.iter (fun c ->
            Fmt.kstr (Buffer.add_string b) {|,%s:%s|} (string name) (string c))
      in
      colour "cursor" th.cursor;
      colour "cursor_accent" th.cursor_accent;
      Buffer.add_char b '}')
    t.theme;
  Buffer.add_char b '}';
  Option.iter
    (fun c -> Fmt.kstr (Buffer.add_string b) {|,"command":%s|} (string c))
    t.command;
  Buffer.add_string b "}\n";
  b

let to_string t =
  let b = header t in
  let line ~prev ms code data =
    let d = ms - prev in
    Fmt.kstr (Buffer.add_string b) "[%d.%03d,%s,%s]\n" (d / 1000) (d mod 1000)
      (string code) (string data)
  in
  ignore
    (List.fold_left
       (fun prev (ms, e) ->
         (match e with
         | Output s -> line ~prev ms "o" s
         | Resize (c, r) -> line ~prev ms "r" (Fmt.str "%dx%d" c r)
         | Exit n -> line ~prev ms "x" (string_of_int n));
         ms)
       0 (utf_8 t.events));
  Buffer.contents b
