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

type state = Ground | Escape | Csi | String | String_escape

type terminal = {
  vte : Vte.t;
  mutable state : state;
  pending : Buffer.t;  (** The first bytes of a character in [Ground]. *)
  out : Buffer.t;  (** The bytes [vte] reads next. *)
  mutable dirty : int list;  (** The rows {!attach} changed. *)
  seq : Buffer.t;  (** The escape sequence [vte] reads once it ends. *)
  mutable count : int;  (** The scrollback lines [vte] holds. *)
  mutable floor : int;  (** The first of them xterm.js still holds. *)
  mutable lines : string array;  (** The scrollback lines, see {!snapshot}. *)
  mutable top : int;  (** The first row of the scroll region. *)
  mutable bottom : int;  (** The last row of the scroll region. *)
}

(* xterm.js keeps a thousand lines of scrollback, VHS's [Buffer] reads its
   first lines and [CSI 3 J] erases them. matrix 0.1.0 erases the screen on
   [CSI 3 J] instead, so it never reads one; it keeps more lines, and
   [floor] skips those erased. *)
let capacity = 10_000
let xterm_scrollback = 1_000

let terminal ~rows ~cols =
  {
    vte =
      Vte.create ~default_fg:(color default_fg) ~default_bg:(color default_bg)
        ~rows ~cols ~scrollback:capacity ();
    state = Ground;
    pending = Buffer.create 4;
    out = Buffer.create 4096;
    dirty = [];
    seq = Buffer.create 64;
    count = 0;
    floor = 0;
    lines = [||];
    top = 0;
    bottom = rows - 1;
  }

let vte t = t.vte
let is_dirty t = Vte.is_dirty t.vte || t.dirty <> []

let clear_dirty t =
  Vte.clear_dirty t.vte;
  t.dirty <- []

(* The number of lines [lines] drops from its start to become [lines']. *)
let dropped lines lines' =
  let n = Array.length lines in
  let rec shift k =
    if k >= n then n
    else
      let rec same i =
        i + k >= n || (lines.(i + k) = lines'.(i) && same (i + 1))
      in
      if same 0 then k else shift (k + 1)
  in
  shift 0

let scrollback t = Array.of_list (Vte.scrollback_lines t.vte)

(* [lines] holds the scrollback while a read can fill it before it pushes
   [floor] out of the last thousand lines: a full scrollback drops a line
   for each line it takes, which only its lines tell. *)
let snapshot t =
  t.lines <-
    (if t.floor > 0 && t.count >= capacity - xterm_scrollback then scrollback t
     else [||])

let track t =
  if not (Vte.is_alternate_screen t.vte) then (
    let count = Vte.scrollback_size t.vte in
    if count < t.count then t.floor <- 0
    else if t.floor > 0 && t.lines <> [||] then
      t.floor <- max 0 (t.floor - dropped t.lines (scrollback t));
    t.count <- count;
    if t.floor <= count - xterm_scrollback then t.floor <- 0;
    snapshot t)

let flush t =
  if Buffer.length t.out > 0 then (
    Vte.feed_string t.vte (Buffer.contents t.out);
    Buffer.clear t.out;
    track t)

let erase_scrollback t =
  flush t;
  if not (Vte.is_alternate_screen t.vte) then (
    t.floor <- t.count;
    snapshot t)

(* After a character in the last column, xterm keeps the cursor there and
   wraps the next character; matrix 0.1.0 puts the cursor one column beyond
   the screen, where its controls misplace it (invariant-hq/mosaic#24). So
   [control] brings it back before a control, and after a move that ends
   beyond the last column. *)
let move_to_last_column t =
  let row, col = Vte.cursor_pos t.vte in
  let last = Vte.cols t.vte - 1 in
  if col > last then Vte.set_cursor_pos t.vte ~row ~col:last

(* [control t s] makes the terminal read the control [s] from the column
   xterm reads it at. *)
let control t s =
  flush t;
  move_to_last_column t;
  Buffer.add_string t.out s;
  flush t;
  move_to_last_column t

(* matrix 0.1.0 has no scroll region, returns the cursor on a line feed and
   reads neither an index nor CSI S and T, so the screen moves the cursor
   and scrolls the region itself, as xterm does. A full-screen scroll keeps
   the line it scrolls out in the scrollback. *)
let scroll t n =
  flush t;
  move_to_last_column t;
  let rows = Vte.rows t.vte in
  let n = max (-(t.bottom - t.top + 1)) (min n (t.bottom - t.top + 1)) in
  if n > 0 && t.top = 0 && t.bottom = rows - 1 then Vte.scroll_up t.vte n
  else Grid.scroll (Vte.grid t.vte) ~top:t.top ~bottom:t.bottom n;
  (* The scrolled-in rows are blank, not of the colour the rows had. *)
  let first = if n > 0 then t.bottom - n + 1 else t.top in
  Grid.fill_rect (Vte.grid t.vte) ~x:0 ~y:first ~width:(Vte.cols t.vte)
    ~height:(abs n) ~color:(color default_bg);
  for r = t.top to t.bottom do
    t.dirty <- r :: t.dirty
  done;
  track t

let move t ~row ~col =
  flush t;
  Vte.set_cursor_pos t.vte ~row ~col

(* A line feed scrolls the region at its last row, and moves down a row
   elsewhere, in the same column. *)
let index t =
  flush t;
  move_to_last_column t;
  let row, col = Vte.cursor_pos t.vte in
  if row = t.bottom then scroll t 1
  else if row < Vte.rows t.vte - 1 then move t ~row:(row + 1) ~col

let reverse_index t =
  flush t;
  move_to_last_column t;
  let row, col = Vte.cursor_pos t.vte in
  if row = t.top then scroll t (-1)
  else if row > 0 then move t ~row:(row - 1) ~col

(* [CSI top ; bottom r] sets the scroll region and homes the cursor; a
   region of less than two rows is ignored. *)
let set_region t params =
  let rows = Vte.rows t.vte in
  let n i d =
    match List.nth_opt (String.split_on_char ';' params) i with
    | Some s -> (
        match int_of_string_opt s with Some v when v > 0 -> v | _ -> d)
    | None -> d
  in
  let top = n 0 1 - 1 and bottom = min rows (n 1 rows) - 1 in
  if top < bottom then (
    t.top <- top;
    t.bottom <- bottom;
    move t ~row:0 ~col:0)

let params s = String.sub s 2 (String.length s - 3)

let count s =
  match int_of_string_opt (params s) with Some n when n > 0 -> n | _ -> 1

(* The final byte of the CSI sequence [s], if it is one. *)
let csi_final s =
  let n = String.length s in
  if n >= 3 && s.[1] = '[' then Some s.[n - 1] else None

let history t =
  if Vte.is_alternate_screen t.vte then 0
  else min xterm_scrollback (t.count - t.floor)

(* matrix 0.1.0 drops a character of no width, so [attach] appends it to the
   cell before the cursor, the grapheme a wide cell continues, as xterm.js
   does. In the first column there is no such cell. *)
let attach t s =
  flush t;
  let g = Vte.grid t.vte in
  let row, col = Vte.cursor_pos t.vte in
  if col > 0 && row < Grid.height g then (
    let rec start x =
      if x > 0 && Grid.is_continuation g (Grid.idx g ~x ~y:row) then
        start (x - 1)
      else x
    in
    let x = start (min col (Grid.width g) - 1) in
    let i = Grid.idx g ~x ~y:row in
    let text = (if Grid.is_empty g i then " " else Grid.get_text g i) ^ s in
    let width = max 1 (Grid.cell_width g i) in
    let glyph =
      Glyph.Pool.intern_sub (Grid.glyph_pool g)
        ~width_method:(Grid.width_method g) ~tab_width:2 text ~pos:0
        ~len:(String.length text) ~width
    in
    let style = Grid.get_style g i in
    let fg = Option.value style.fg ~default:(color default_fg)
    and bg = Option.value style.bg ~default:(color default_bg)
    and attrs = style.attrs
    and link = style.link in
    Grid.set_cell g ~x ~y:row ~glyph ~fg ~bg ~attrs ?link ~blend:false ();
    for k = 1 to width - 1 do
      Grid.set_cell g ~x:(x + k) ~y:row
        ~glyph:
          (Glyph.make_continuation ~code:glyph ~left:k ~right:(width - 1 - k))
        ~fg ~bg ~attrs ?link ~blend:false ()
    done;
    t.dirty <- row :: t.dirty)

(* The length of the UTF-8 encoding a byte starts, 0 if it starts none. *)
let utf_8_length = function
  | '\xc2' .. '\xdf' -> 2
  | '\xe0' .. '\xef' -> 3
  | '\xf0' .. '\xf4' -> 4
  | _ -> 0

let is_zero_width t s =
  let d = String.get_utf_8_uchar s 0 in
  Uchar.utf_decode_is_valid d
  && Uchar.to_int (Uchar.utf_decode_uchar d) >= 0xa0
  && Glyph.String.measure
       ~width_method:(Grid.width_method (Vte.grid t.vte))
       ~tab_width:2 s
     = 0

(* A character outside escape sequences. *)
let character t s =
  if is_zero_width t s then attach t s else Buffer.add_string t.out s

let replacement = "\xef\xbf\xbd"

(* ECMA-48 sequences: CSI ends at a final byte, the strings of OSC, DCS,
   SOS, PM and APC at BEL or ST. *)
let escape = function
  | '[' -> Csi
  | ']' | 'P' | 'X' | '^' | '_' -> String
  | ' ' .. '/' -> Escape
  | _ -> Ground

(* The cursor moves and the edits that read the cursor's column. *)
let sequence t s =
  if s = "\027[3J" then erase_scrollback t
  else
    match (csi_final s, s) with
    | Some ('A' .. 'H' | 'J' | 'K' | 'L' | 'M' | 'P' | '@' | 'f'), _ ->
        control t s
    | Some 'S', _ -> scroll t (count s)
    | Some 'T', _ -> scroll t (-count s)
    | Some 'r', _ -> set_region t (params s)
    | None, "\027D" -> index t
    | None, "\027E" ->
        index t;
        move t ~row:(fst (Vte.cursor_pos t.vte)) ~col:0
    | None, "\027M" -> reverse_index t
    | None, "\027c" ->
        t.top <- 0;
        t.bottom <- Vte.rows t.vte - 1;
        Buffer.add_string t.out s
    | _ -> Buffer.add_string t.out s

(* The terminal reads valid UTF-8 outside escape sequences: each maximal
   invalid subsequence is U+FFFD, as for [String.get_utf_8_uchar], wherever
   the reads split it. *)
let rec byte t c =
  if Buffer.length t.pending > 0 then
    let s = Buffer.contents t.pending ^ String.make 1 c in
    let d = String.get_utf_8_uchar s 0 in
    if Uchar.utf_decode_is_valid d then (
      Buffer.clear t.pending;
      character t s)
    else if Uchar.utf_decode_length d = String.length s then
      Buffer.add_char t.pending c
    else (
      Buffer.clear t.pending;
      Buffer.add_string t.out replacement;
      byte t c)
  else if t.state = Ground && c >= '\x80' then
    if utf_8_length c > 0 then Buffer.add_char t.pending c
    else Buffer.add_string t.out replacement
  else
    let state = t.state in
    (match (state, c) with
    | Ground, '\b' -> control t "\027[D"
    | Ground, ('\n' | '\011' | '\012') -> index t
    | Ground, '\027' -> Buffer.add_char t.seq c
    | Ground, _ -> Buffer.add_char t.out c
    | _ -> Buffer.add_char t.seq c);
    t.state <-
      (match (state, c) with
      | Ground, '\027' -> Escape
      | Ground, _ -> Ground
      | Escape, c -> escape c
      | Csi, '@' .. '~' -> Ground
      | Csi, _ -> Csi
      | String, '\007' -> Ground
      | String, '\027' -> String_escape
      | String, _ -> String
      | String_escape, '\\' -> Ground
      | String_escape, c -> escape c);
    if state <> Ground && t.state = Ground then (
      let s = Buffer.contents t.seq in
      Buffer.clear t.seq;
      sequence t s)

let feed t s =
  String.iter (byte t) s;
  flush t

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

(* The white space and line terminators of JavaScript, which VHS's
   [trimEnd] removes. *)
let is_js_space = function
  | 0x09 | 0x0a | 0x0b | 0x0c | 0x0d | 0x20 | 0xa0 | 0x1680 | 0x2028 | 0x2029
  | 0x202f | 0x205f | 0x3000 | 0xfeff ->
      true
  | u -> u >= 0x2000 && u <= 0x200a

let trim_end s =
  let rec start i =
    if i > 0 && Char.code s.[i] land 0xc0 = 0x80 then start (i - 1) else i
  in
  let rec go n =
    if n = 0 then 0
    else
      let i = start (n - 1) in
      let d = String.get_utf_8_uchar s i in
      if is_js_space (Uchar.to_int (Uchar.utf_decode_uchar d)) then go i else n
  in
  String.sub s 0 (go (String.length s))

let row grid y =
  let width = Grid.width grid in
  let cells = Array.init width (fun x -> cell grid (Grid.idx grid ~x ~y)) in
  let b = Buffer.create width in
  Array.iter (fun (c : cell) -> Buffer.add_string b c.text) cells;
  { cells; text = trim_end (Buffer.contents b) }

(* The text of each cell of [r] from [line], the text matrix 0.1.0 keeps
   of a scrollback line, whose zero-width characters it drops when it draws
   the line again: each cell's text starts [line] at [pos], and the
   zero-width characters that follow it are its own. *)
let restore t r line =
  let n = String.length line in
  let rec marks pos =
    if pos >= n then pos
    else
      let len = Uchar.utf_decode_length (String.get_utf_8_uchar line pos) in
      if is_zero_width t (String.sub line pos len) then marks (pos + len)
      else pos
  in
  let rec go x pos acc =
    if x = Array.length r.cells then Some (List.rev acc)
    else
      let (c : cell) = r.cells.(x) in
      let len = String.length c.text in
      if c.width = 0 then go (x + 1) pos (c :: acc)
      else if pos + len <= n && String.sub line pos len = c.text then
        let stop = marks (pos + len) in
        let text = String.sub line pos (stop - pos) in
        go (x + 1) stop ({ c with text } :: acc)
      else if pos >= n && c.text = " " then go (x + 1) pos (c :: acc)
      else None
  in
  match go 0 0 [] with
  | None -> r
  | Some cells ->
      let cells = Array.of_list cells in
      let b = Buffer.create (Array.length cells) in
      Array.iter (fun (c : cell) -> Buffer.add_string b c.text) cells;
      { cells; text = trim_end (Buffer.contents b) }

let of_terminal ?previous ?(offset = 0) t =
  let vte = t.vte in
  let rows = Vte.rows vte and cols = Vte.cols vte in
  let offset = max 0 (min offset (history t)) in
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
        let dirty = t.dirty @ Vte.dirty_rows vte in
        fun y -> if List.mem y dirty then None else Some p.rows.(y)
    | _ -> fun _ -> None
  in
  (* matrix 0.1.0 renders the scrollback lines it shows newest first. *)
  let shown = min offset rows in
  let source y = if y < shown then shown - 1 - y else y in
  let history =
    if offset = 0 then [||]
    else
      let lines = Array.of_list (Vte.scrollback_lines vte) in
      Array.sub lines (Array.length lines - offset) offset
  in
  let rows' =
    Array.init rows (fun y ->
        match reuse y with
        | Some r -> r
        | None when y < shown -> restore t (row grid (source y)) history.(y)
        | None -> row grid (source y))
  in
  let cursor =
    let r, c = Vte.cursor_pos vte in
    if Vte.cursor_visible vte && r + offset < rows then
      Some (r + offset, min c (cols - 1))
    else None
  in
  { rows = rows'; cursor }

let buffer term t =
  let h = history term in
  let older =
    if h = 0 then [||]
    else
      let lines = Array.of_list (Vte.scrollback_lines term.vte) in
      Array.sub lines (Array.length lines - h) h
  in
  Array.append (Array.map trim_end older) (Array.map (fun r -> r.text) t.rows)

let line t r = if r >= 0 && r < Array.length t.rows then t.rows.(r).text else ""

let text t =
  String.concat "\n" (Array.to_list (Array.map (fun r -> r.text) t.rows))

let equal t0 t1 =
  t0.cursor = t1.cursor
  && Array.length t0.rows = Array.length t1.rows
  && Array.for_all2 row_equal t0.rows t1.rows
