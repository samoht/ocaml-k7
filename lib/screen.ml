(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type terminal = Vte.t

(* As xterm.js: a thousand lines of scrollback, a width per code point, and
   DA2 answered as xterm.js 5. *)
let terminal ~rows ~cols =
  Vte.create ~scrollback:1000 ~width_method:`Wcwidth
    ~default_fg:Ansi.Color.default ~default_bg:Ansi.Color.default
    ~da2:"\027[>0;276;0c" ~rows ~cols ()

let feed = Vte.feed_string
let take_replies = Vte.take_replies
let is_dirty = Vte.is_dirty
let clear_dirty = Vte.clear_dirty
let history = Vte.scrollback_size
let cursor_keys = Vte.cursor_key_mode
let cursor_row t = fst (Vte.cursor_pos t)

type colour = Default | Indexed of int | Rgb of int

type cell = {
  text : string;
  width : int;
  fg : colour;
  bg : colour;
  attrs : Ansi.Attr.t;
}

type row = { cells : cell array; text : string }

let cells r = r.cells
let row_text r = r.text
let row_equal r0 r1 = r0 == r1 || r0.cells = r1.cells

type t = { rows : row array; cursor : (int * int) option }

let rows t = t.rows
let cursor t = t.cursor

let colour c =
  match Ansi.Color.intent c with
  | Default -> Default
  | Indexed i -> Indexed i
  | Rgb ->
      let r, g, b = Ansi.Color.to_rgb c in
      Rgb ((r lsl 16) lor (g lsl 8) lor b)

let visible_on_blank =
  Ansi.Attr.(
    of_list [ Inverse; Underline; Double_underline; Strikethrough; Overline ])

let clean s =
  if
    String.is_valid_utf_8 s
    && not (String.exists (fun c -> Char.code c < 0x20 || c = '\x7f') s)
  then s
  else "\xef\xbf\xbd"

(* xterm.js paints a wide glyph's cells with its first cell's style. *)
let rec cell grid idx =
  if Matrix_grid.is_continuation grid idx && idx > 0 then
    { (cell grid (idx - 1)) with text = ""; width = 0 }
  else
    let attrs = Ansi.Attr.unpack (Matrix_grid.get_attrs grid idx) in
    let bg = colour (Matrix_grid.get_bg grid idx) in
    let text =
      if Matrix_grid.is_empty grid idx then " "
      else clean (Matrix_grid.get_text grid idx)
    in
    let text = if text = "" then " " else text in
    let width = max 1 (Matrix_grid.cell_width grid idx) in
    let fg = colour (Matrix_grid.get_fg grid idx) in
    if text = " " then
      let attrs = Ansi.Attr.intersect attrs visible_on_blank in
      {
        text;
        width;
        fg = (if Ansi.Attr.is_empty attrs then Default else fg);
        bg;
        attrs;
      }
    else { text; width; fg; bg; attrs }

(* JavaScript's [trimEnd] set. *)
let is_js_space = function
  | 0x09 | 0x0a | 0x0b | 0x0c | 0x0d | 0x20 | 0xa0 | 0x1680 | 0x2028 | 0x2029
  | 0x202f | 0x205f | 0x3000 | 0xfeff ->
      true
  | u -> u >= 0x2000 && u <= 0x200a

let trim_end s =
  let rec go i keep =
    if i >= String.length s then keep
    else
      let d = String.get_utf_8_uchar s i in
      let next = i + Uchar.utf_decode_length d in
      if is_js_space (Uchar.to_int (Uchar.utf_decode_uchar d)) then go next keep
      else go next next
  in
  String.sub s 0 (go 0 0)

let row grid y =
  let width = Matrix_grid.width grid in
  let cells =
    Array.init width (fun x -> cell grid (Matrix_grid.idx grid ~x ~y))
  in
  let b = Buffer.create width in
  Array.iter (fun (c : cell) -> Buffer.add_string b c.text) cells;
  { cells; text = trim_end (Buffer.contents b) }

let of_terminal ?previous ?(offset = 0) vte =
  let rows = Vte.rows vte and cols = Vte.cols vte in
  let offset = max 0 (min offset (history vte)) in
  let grid =
    if offset = 0 then Vte.grid vte
    else
      let g = Matrix_grid.create ~width:cols ~height:rows () in
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
  let rows' =
    Array.init rows (fun y ->
        match reuse y with Some r -> r | None -> row grid y)
  in
  let cursor =
    let r, c = Vte.cursor_pos vte in
    if Vte.cursor_visible vte && r + offset < rows then
      Some (r + offset, min c (cols - 1))
    else None
  in
  { rows = rows'; cursor }

let buffer term t =
  let older = List.map trim_end (Vte.scrollback_lines term) in
  Array.append (Array.of_list older) (Array.map (fun r -> r.text) t.rows)

let line t r = if r >= 0 && r < Array.length t.rows then t.rows.(r).text else ""

let text t =
  String.concat "\n" (Array.to_list (Array.map (fun r -> r.text) t.rows))

let equal t0 t1 =
  t0.cursor = t1.cursor
  && Array.length t0.rows = Array.length t1.rows
  && Array.for_all2 row_equal t0.rows t1.rows
