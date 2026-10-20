(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The terminal stores resolved colours only, so its defaults are colours no
   theme uses. Its SGR 39 sets the foreground to the default background, so
   either default reads as default. *)
let default_fg = 0x010203
let default_bg = 0x040506

let color rgb =
  Ansi.Color.of_rgb (rgb lsr 16) ((rgb lsr 8) land 0xff) (rgb land 0xff)

let terminal ~rows ~cols =
  Vte.create ~default_fg:(color default_fg) ~default_bg:(color default_bg) ~rows
    ~cols ()

type state = Ground | Escape | Csi | String | String_escape
type input = { mutable state : state }

let input () = { state = Ground }

(* ECMA-48 sequences: CSI ends at a final byte, the strings of OSC, DCS,
   SOS, PM and APC at BEL or ST. *)
let escape = function
  | '[' -> Csi
  | ']' | 'P' | 'X' | '^' | '_' -> String
  | ' ' .. '/' -> Escape
  | _ -> Ground

let feed i vte s =
  let b = Buffer.create (String.length s + 16) in
  String.iter
    (fun c ->
      (match (i.state, c) with
      | Ground, '\b' -> Buffer.add_string b "\027[D"
      | _ -> Buffer.add_char b c);
      i.state <-
        (match (i.state, c) with
        | Ground, '\027' -> Escape
        | Ground, _ -> Ground
        | Escape, c -> escape c
        | Csi, '@' .. '~' -> Ground
        | Csi, _ -> Csi
        | String, '\007' -> Ground
        | String, '\027' -> String_escape
        | String, _ -> String
        | String_escape, '\\' -> Ground
        | String_escape, c -> escape c))
    s;
  Vte.feed_string vte (Buffer.contents b)

type cell = {
  text : string;
  width : int;
  fg : int;
  bg : int;
  attrs : Ansi.Attr.t;
}

type row = { cells : cell array; text : string }

let cells r = r.cells
let row_text r = r.text
let row_equal r0 r1 = r0 == r1 || r0.cells = r1.cells

type t = { rows : row array; cursor : (int * int) option }

let rows t = t.rows
let cursor t = t.cursor

let rgb ~alpha r g b =
  let byte f = int_of_float (Float.round (f *. 255.)) in
  if alpha < 0.5 then -1
  else
    let c = (byte r lsl 16) lor (byte g lsl 8) lor byte b in
    if c = default_fg || c = default_bg then -1 else c

(* The attributes that show on a blank cell. *)
let visible_on_blank =
  Ansi.Attr.(
    of_list [ Inverse; Underline; Double_underline; Strikethrough; Overline ])

(* Valid UTF-8 without control characters, which the terminal never stores
   but which a cell must not hold. *)
let clean s =
  if
    String.is_valid_utf_8 s
    && not (String.exists (fun c -> Char.code c < 0x20 || c = '\x7f') s)
  then s
  else "\xef\xbf\xbd"

let cell grid idx =
  let attrs = Ansi.Attr.unpack (Grid.get_attrs grid idx) in
  let bg =
    rgb ~alpha:(Grid.get_bg_a grid idx) (Grid.get_bg_r grid idx)
      (Grid.get_bg_g grid idx) (Grid.get_bg_b grid idx)
  in
  if Grid.is_continuation grid idx then
    { text = ""; width = 0; fg = -1; bg; attrs }
  else
    let text =
      if Grid.is_empty grid idx then " " else clean (Grid.get_text grid idx)
    in
    let text = if text = "" then " " else text in
    let width = max 1 (Grid.cell_width grid idx) in
    let fg =
      rgb ~alpha:(Grid.get_fg_a grid idx) (Grid.get_fg_r grid idx)
        (Grid.get_fg_g grid idx) (Grid.get_fg_b grid idx)
    in
    if text = " " then
      let attrs = Ansi.Attr.intersect attrs visible_on_blank in
      {
        text;
        width;
        fg = (if Ansi.Attr.is_empty attrs then -1 else fg);
        bg;
        attrs;
      }
    else { text; width; fg; bg; attrs }

let trim_end s =
  let rec go n = if n > 0 && s.[n - 1] = ' ' then go (n - 1) else n in
  String.sub s 0 (go (String.length s))

let row grid y =
  let width = Grid.width grid in
  let cells = Array.init width (fun x -> cell grid (Grid.idx grid ~x ~y)) in
  let b = Buffer.create width in
  Array.iter (fun (c : cell) -> Buffer.add_string b c.text) cells;
  { cells; text = trim_end (Buffer.contents b) }

let of_vte ?previous ?(offset = 0) vte =
  let rows = Vte.rows vte and cols = Vte.cols vte in
  let offset = max 0 (min offset (Vte.scrollback_size vte)) in
  let grid =
    if offset = 0 then Vte.grid vte
    else
      let g = Grid.create ~width:cols ~height:rows () in
      Vte.render_with_scrollback vte ~offset g;
      g
  in
  let reuse =
    match previous with
    | Some p
      when offset = 0
           && Array.length p.rows = rows
           && rows > 0
           && Array.length p.rows.(0).cells = cols ->
        let dirty = Vte.dirty_rows vte in
        fun y -> if List.mem y dirty then None else Some p.rows.(y)
    | _ -> fun _ -> None
  in
  (* The terminal renders the scrollback lines newest first. *)
  let source y = if y < offset then offset - 1 - y else y in
  let rows' =
    Array.init rows (fun y ->
        match reuse y with Some r -> r | None -> row grid (source y))
  in
  let cursor =
    let r, c = Vte.cursor_pos vte in
    if Vte.cursor_visible vte && r + offset < rows then
      Some (r + offset, min c (cols - 1))
    else None
  in
  { rows = rows'; cursor }

let line t r = if r >= 0 && r < Array.length t.rows then t.rows.(r).text else ""

let text t =
  String.concat "\n" (Array.to_list (Array.map (fun r -> r.text) t.rows))

let equal t0 t1 =
  t0.cursor = t1.cursor
  && Array.length t0.rows = Array.length t1.rows
  && Array.for_all2 row_equal t0.rows t1.rows
