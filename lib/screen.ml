(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* matrix 0.1.0 stores resolved colours only, so its defaults are colours no
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
  mutable top : int;  (** The first row of the scroll region. *)
  mutable bottom : int;  (** The last row of the scroll region. *)
  mutable wrap : bool;  (** Auto-wrap mode. *)
  mutable origin : bool;  (** Origin mode. *)
  mutable g0 : bool;  (** Whether G0 is DEC line drawing. *)
  mutable g1 : bool;  (** Whether G1 is DEC line drawing. *)
  mutable shifted : bool;  (** Whether SO selected G1. *)
  mutable last : string;  (** The last character printed, for REP. *)
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
    top = 0;
    bottom = rows - 1;
    wrap = true;
    origin = false;
    g0 = false;
    g1 = false;
    shifted = false;
    last = "";
  }

let vte t = t.vte
let is_dirty t = Vte.is_dirty t.vte || t.dirty <> []

let clear_dirty t =
  Vte.clear_dirty t.vte;
  t.dirty <- []

(* Lines a full scrollback drops for those it takes push [floor] down, so
   the screen counts them: the lines its own scrolls push, and the growth of
   the scrollback, which counts matrix's own pushes until the scrollback is
   full. From then on, while [floor] still matters, the screen writes the
   characters that could make matrix scroll itself, see {!careful}. *)
let track ?(pushed = 0) t =
  if not (Vte.is_alternate_screen t.vte) then (
    let count = Vte.scrollback_size t.vte in
    if count < t.count then t.floor <- 0
    else t.floor <- max 0 (t.floor - max 0 (t.count + pushed - count));
    t.count <- count;
    if t.floor <= count - xterm_scrollback then t.floor <- 0)

let careful t = t.floor > 0 && t.count >= capacity - xterm_scrollback

let flush t =
  if Buffer.length t.out > 0 then (
    Vte.feed_string t.vte (Buffer.contents t.out);
    Buffer.clear t.out;
    track t)

let erase_scrollback t =
  flush t;
  if not (Vte.is_alternate_screen t.vte) then t.floor <- t.count

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
  let full = n > 0 && t.top = 0 && t.bottom = rows - 1 in
  if full then Vte.scroll_up t.vte n
  else Grid.scroll (Vte.grid t.vte) ~top:t.top ~bottom:t.bottom n;
  (* The scrolled-in rows are blank, not of the colour the rows had. *)
  let first = if n > 0 then t.bottom - n + 1 else t.top in
  Grid.fill_rect (Vte.grid t.vte) ~x:0 ~y:first ~width:(Vte.cols t.vte)
    ~height:(abs n) ~color:(color default_bg);
  for r = t.top to t.bottom do
    t.dirty <- r :: t.dirty
  done;
  track ~pushed:(if full then n else 0) t

let move t ~row ~col =
  flush t;
  Vte.set_cursor_pos t.vte ~row ~col

(* [index t dir] moves the cursor a row down ([dir = 1]) or up ([-1]) in
   its column, as a line feed or a reverse index does: at the region's edge
   it scrolls the region instead, and at the screen's it stays. *)
let index t dir =
  flush t;
  move_to_last_column t;
  let row, col = Vte.cursor_pos t.vte in
  let edge = if dir > 0 then t.bottom else t.top in
  if row = edge then scroll t dir
  else if row + dir >= 0 && row + dir < Vte.rows t.vte then
    move t ~row:(row + dir) ~col

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
    t.bottom <- bottom)

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

(* VT100's DEC Special Graphics, as xterm.js maps them, from ['`'] to
   ['~']. *)
let dec_graphics =
  [| "\u{25c6}"; "\u{2592}"; "\u{2409}"; "\u{240c}"; "\u{240d}"; "\u{240a}";
     "\u{b0}"; "\u{b1}"; "\u{2424}"; "\u{240b}"; "\u{2518}"; "\u{2510}";
     "\u{250c}"; "\u{2514}"; "\u{253c}"; "\u{23ba}"; "\u{23bb}"; "\u{2500}";
     "\u{23bc}"; "\u{23bd}"; "\u{251c}"; "\u{2524}"; "\u{2534}"; "\u{252c}";
     "\u{2502}"; "\u{2264}"; "\u{2265}"; "\u{3c0}"; "\u{2260}"; "\u{a3}";
     "\u{b7}" |]
[@@ocamlformat "disable"]

let full_region t = t.top = 0 && t.bottom = Vte.rows t.vte - 1

let width t s =
  Glyph.String.measure
    ~width_method:(Grid.width_method (Vte.grid t.vte))
    ~tab_width:2 s

(* A character that does not fit the line wraps, or overwrites the last
   column without auto-wrap. matrix 0.1.0 wraps it with a full-screen
   scroll, and writes nothing without auto-wrap, so the screen places it
   itself in a scroll region and without auto-wrap. *)
let place t s =
  flush t;
  let _, col = Vte.cursor_pos t.vte in
  if col + max 1 (width t s) > Vte.cols t.vte then
    if t.wrap then (
      index t 1;
      move t ~row:(fst (Vte.cursor_pos t.vte)) ~col:0)
    else
      let row, _ = Vte.cursor_pos t.vte in
      move t ~row ~col:(Vte.cols t.vte - max 1 (width t s))

(* A character outside escape sequences. *)
let character t s =
  if is_zero_width t s then attach t s
  else (
    t.last <- s;
    if t.wrap && full_region t && not (careful t) then Buffer.add_string t.out s
    else (
      place t s;
      Buffer.add_string t.out s;
      flush t))

let ascii = Array.init 128 (fun i -> String.make 1 (Char.chr i))

let print_ascii t c =
  let dec = if t.shifted then t.g1 else t.g0 in
  if dec && c >= '`' && c <= '~' then
    character t dec_graphics.(Char.code c - Char.code '`')
  else character t ascii.(Char.code c)

let replacement = "\xef\xbf\xbd"

(* ECMA-48 sequences: CSI ends at a final byte, the strings of OSC, DCS,
   SOS, PM and APC at BEL or ST. *)
let escape = function
  | '[' -> Csi
  | ']' | 'P' | 'X' | '^' | '_' -> String
  | ' ' .. '/' -> Escape
  | _ -> Ground

let int_param s ~default =
  match int_of_string_opt s with Some n when n > 0 -> n | _ -> default

let csi_params s =
  List.map (int_param ~default:1) (String.split_on_char ';' (params s))

(* A move up or down stops at the scroll region's edge when the cursor is
   in the region. *)
let vertical t n ~col =
  flush t;
  move_to_last_column t;
  let row, c = Vte.cursor_pos t.vte in
  let lo = if row >= t.top then t.top else 0
  and hi = if row <= t.bottom then t.bottom else Vte.rows t.vte - 1 in
  move t ~row:(max lo (min hi (row + n))) ~col:(Option.value col ~default:c)

(* In origin mode, rows count from the top of the scroll region and stop at
   its bottom. *)
let position t ~row ~col =
  flush t;
  let row =
    if t.origin then min t.bottom (t.top + row - 1)
    else min (Vte.rows t.vte) row - 1
  in
  move t ~row ~col:(min (Vte.cols t.vte) col - 1)

(* An inserted or deleted line scrolls the region from the cursor's row,
   which it leaves in the first column; outside the region it does
   nothing. *)
let edit_lines t n =
  flush t;
  move_to_last_column t;
  let row, _ = Vte.cursor_pos t.vte in
  if row >= t.top && row <= t.bottom then (
    let top = t.top in
    t.top <- row;
    scroll t n;
    t.top <- top;
    move t ~row ~col:0)

let home t = move t ~row:(if t.origin then t.top else 0) ~col:0

let reset t =
  t.top <- 0;
  t.bottom <- Vte.rows t.vte - 1;
  t.wrap <- true;
  t.origin <- false;
  t.g0 <- false;
  t.g1 <- false;
  t.shifted <- false

(* The sequences matrix 0.1.0 misreads or lacks, and the cursor moves and
   edits that read the cursor's column. *)
let sequence t s =
  let n () = count s in
  let last = t.last in
  (* REP repeats the character just before it, as in xterm.js. *)
  t.last <- "";
  match (csi_final s, s) with
  | _, "\027[3J" -> erase_scrollback t
  | Some ('A' | 'F'), _ ->
      vertical t (-n ()) ~col:(if csi_final s = Some 'F' then Some 0 else None)
  | Some ('B' | 'e' | 'E'), _ ->
      vertical t (n ()) ~col:(if csi_final s = Some 'E' then Some 0 else None)
  | Some ('H' | 'f'), _ -> (
      match csi_params s with
      | [] | [ _ ] -> position t ~row:(int_param (params s) ~default:1) ~col:1
      | row :: col :: _ -> position t ~row ~col)
  | Some 'd', _ ->
      let _, col = Vte.cursor_pos t.vte in
      position t ~row:(n ()) ~col:(col + 1)
  | Some '`', _ -> control t ("\027[" ^ params s ^ "G")
  | Some 'a', _ -> control t ("\027[" ^ params s ^ "C")
  | Some 'X', _ ->
      let n = string_of_int (n ()) in
      control t ("\027[" ^ n ^ "P\027[" ^ n ^ "@")
  | Some 'b', _ ->
      if last <> "" then
        for _ = 1 to n () do
          character t last
        done
  | Some 'L', _ -> edit_lines t (-n ())
  | Some 'M', _ -> edit_lines t (n ())
  | Some ('C' | 'D' | 'G' | 'J' | 'K' | 'P' | '@'), _ -> control t s
  | Some 'S', _ -> scroll t (n ())
  | Some 'T', _ -> scroll t (-n ())
  | Some 'r', _ ->
      set_region t (params s);
      home t
  | Some ('h' | 'l'), _ ->
      let set = csi_final s = Some 'h' in
      (match params s with
      | "?6" ->
          t.origin <- set;
          home t
      | "?7" -> t.wrap <- set
      | _ -> ());
      Buffer.add_string t.out s
  | None, "\027D" -> index t 1
  | None, "\027E" ->
      index t 1;
      move t ~row:(fst (Vte.cursor_pos t.vte)) ~col:0
  | None, "\027M" -> index t (-1)
  | None, ("\027(0" | "\027(B" | "\027)0" | "\027)B") ->
      let dec = s.[2] = '0' in
      if s.[1] = '(' then t.g0 <- dec else t.g1 <- dec
  | None, "\027c" ->
      reset t;
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
      character t replacement;
      byte t c)
  else if t.state = Ground && c >= '\x80' then
    if utf_8_length c > 0 then Buffer.add_char t.pending c
    else character t replacement
  else
    let state = t.state in
    (match (state, c) with
    | Ground, '\b' ->
        (* matrix 0.1.0 erases the character a backspace moves over, and
           not the one a cursor move does. *)
        t.last <- "";
        control t "\027[D"
    | Ground, ('\n' | '\011' | '\012') ->
        t.last <- "";
        index t 1
    | Ground, '\014' -> t.shifted <- true
    | Ground, '\015' -> t.shifted <- false
    | Ground, '\027' -> Buffer.add_char t.seq c
    | Ground, (' ' .. '~' as c) -> print_ascii t c
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
