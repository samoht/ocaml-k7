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

type state = Ground | Escape | Csi | Osc | String | String_escape

(* What a save keeps, as in xterm.js: the column, the line in the buffer,
   which counts the scrollback, the colours and attributes, and the active
   character set, named by the final byte of its designation. *)
type saved = { col : int; line : int; style : Ansi.Style.t; charset : char }

let unsaved = { col = 0; line = 0; style = Ansi.Style.default; charset = 'B' }

(* xterm.js sets a tab stop every eight columns. *)
let reset_stops stops = Array.iteri (fun i _ -> stops.(i) <- i mod 8 = 0) stops

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
  mutable region : int * int;
      (** The normal screen's scroll region, while the alternate one shows. *)
  sets : char array;  (** The character sets G0 to G3 designate. *)
  mutable level : int;  (** The G set SI, SO or a locking shift invoked. *)
  mutable charset : char;  (** The character set characters print in. *)
  mutable style : Ansi.Style.t;  (** The colours and attributes of [vte]. *)
  mutable saved : saved;  (** The save of the normal screen. *)
  mutable saved_alternate : saved;  (** The save of the alternate screen. *)
  stops : bool array;  (** The tab stops of the normal screen. *)
  stops_alternate : bool array;  (** The tab stops of the alternate screen. *)
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
    region = (0, rows - 1);
    sets = Array.make 4 'B';
    level = 0;
    charset = 'B';
    style = Ansi.Style.default;
    saved = unsaved;
    saved_alternate = unsaved;
    stops = Array.init cols (fun i -> i mod 8 = 0);
    stops_alternate = Array.init cols (fun i -> i mod 8 = 0);
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
   and scrolls the region itself, as xterm does. As in xterm.js, only an
   index of the full screen, [~history:true], keeps the lines it scrolls
   out in the scrollback. *)
let scroll ?(history = false) t n =
  flush t;
  move_to_last_column t;
  let rows = Vte.rows t.vte in
  let n = max (-(t.bottom - t.top + 1)) (min n (t.bottom - t.top + 1)) in
  let full = history && n > 0 && t.top = 0 && t.bottom = rows - 1 in
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
  if row = edge then scroll ~history:true t dir
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

(* The count of a CSI sequence [s] with an intermediate byte. *)
let edit_count s =
  let p = params s in
  match int_of_string_opt (String.sub p 0 (String.length p - 1)) with
  | Some n when n > 0 -> n
  | _ -> 1

(* The final byte of the CSI sequence [s], if it is one. *)
let csi_final s =
  let n = String.length s in
  if n >= 3 && s.[1] = '[' then Some s.[n - 1] else None

let history t =
  if Vte.is_alternate_screen t.vte then 0
  else min xterm_scrollback (t.count - t.floor)

(* The first column and the width of the glyph at column [x] of [row]. *)
let glyph t ~row x =
  let g = Vte.grid t.vte in
  let rec start x =
    if x > 0 && Grid.is_continuation g (Grid.idx g ~x ~y:row) then start (x - 1)
    else x
  in
  let x = start x in
  (x, max 1 (Grid.cell_width g (Grid.idx g ~x ~y:row)))

(* matrix 0.1.0 drops a character of no width, so [attach] appends it to the
   cell before the cursor, the grapheme a wide cell continues, as xterm.js
   does. In the first column there is no such cell. *)
let attach t s =
  flush t;
  let g = Vte.grid t.vte in
  let row, col = Vte.cursor_pos t.vte in
  if col > 0 && row < Grid.height g then (
    let x, width = glyph t ~row (min col (Grid.width g) - 1) in
    let i = Grid.idx g ~x ~y:row in
    let text = (if Grid.is_empty g i then " " else Grid.get_text g i) ^ s in
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

(* The national character sets of xterm.js: the characters each replaces,
   and what replaces them. xterm.js prints the first character of the
   Dutch "ij". *)
let national = function
  | 'A' -> ("#", [| "£" |])
  | '4' -> ({x|#@[\]{|}~|x}, [| "£"; "¾"; "i"; "½"; "|"; "¨"; "f"; "¼"; "´" |])
  | 'C' | '5' -> ({x|[\]^`{|}~|x}, [| "Ä"; "Ö"; "Å"; "Ü"; "é"; "ä"; "ö"; "å"; "ü" |])
  | 'R' -> ({x|#@[\]{|}~|x}, [| "£"; "à"; "°"; "ç"; "§"; "é"; "ù"; "è"; "¨" |])
  | 'Q' -> ({x|@[\]^`{|}~|x}, [| "à"; "â"; "ç"; "ê"; "î"; "ô"; "é"; "ù"; "è"; "û" |])
  | 'K' -> ({x|@[\]{|}~|x}, [| "§"; "Ä"; "Ö"; "Ü"; "ä"; "ö"; "ü"; "ß" |])
  | 'Y' -> ({x|#@[\]`{|}~|x}, [| "£"; "§"; "°"; "ç"; "é"; "ù"; "à"; "ò"; "è"; "ì" |])
  | 'E' | '6' -> ({x|@[\]^`{|}~|x}, [| "Ä"; "Æ"; "Ø"; "Å"; "Ü"; "ä"; "æ"; "ø"; "å"; "ü" |])
  | 'Z' -> ({x|#@[\]{|}|x}, [| "£"; "§"; "¡"; "Ñ"; "¿"; "°"; "ñ"; "ç" |])
  | 'H' | '7' -> ({x|@[\]^`{|}~|x}, [| "É"; "Ä"; "Ö"; "Å"; "Ü"; "é"; "ä"; "ö"; "å"; "ü" |])
  | '=' -> ({x|#@[\]^_`{|}~|x}, [| "ù"; "à"; "é"; "ç"; "ê"; "î"; "è"; "ô"; "ä"; "ö"; "ü"; "û" |])
  | _ -> ("", [||])
[@@ocamlformat "disable"]

(* The final bytes of the designations xterm.js reads. *)
let is_charset c = String.contains "0AB4C5RQKYE6ZH7=" c
let ascii = Array.init 128 (fun i -> String.make 1 (Char.chr i))

let print_ascii t c =
  match t.charset with
  | 'B' -> character t ascii.(Char.code c)
  | '0' when c >= '`' && c <= '~' ->
      character t dec_graphics.(Char.code c - Char.code '`')
  | set -> (
      let keys, values = national set in
      match String.index_opt keys c with
      | Some i -> character t values.(i)
      | None -> character t ascii.(Char.code c))

(* As in xterm.js, invoking a G set or designating the invoked one sets the
   active character set, which a restore sets alone. *)
let invoke t level =
  t.level <- level;
  t.charset <- t.sets.(level)

let designate t level set =
  t.sets.(level) <- set;
  if t.level = level then t.charset <- set

(* The G set each intermediate byte of a designation designates. *)
let level_of = function
  | '(' -> Some 0
  | ')' | '-' -> Some 1
  | '*' | '.' -> Some 2
  | '+' -> Some 3
  | _ -> None

(* matrix 0.1.0 does not keep the style it draws in, so the screen reads
   SGR as matrix does to know it. *)
let sgr style : Ansi.Parser.sgr_attr -> Ansi.Style.t = function
  | `Reset -> Ansi.Style.default
  | `Bold -> Ansi.Style.with_bold true style
  | `Dim -> Ansi.Style.with_dim true style
  | `Italic -> Ansi.Style.with_italic true style
  | `Underline -> Ansi.Style.with_underline true style
  | `Double_underline -> Ansi.Style.with_double_underline true style
  | `Blink -> Ansi.Style.with_blink true style
  | `Inverse -> Ansi.Style.with_inverse true style
  | `Hidden -> Ansi.Style.with_hidden true style
  | `Strikethrough -> Ansi.Style.with_strikethrough true style
  | `Overline -> Ansi.Style.with_overline true style
  | `Framed -> Ansi.Style.with_framed true style
  | `Encircled -> Ansi.Style.with_encircled true style
  | `No_bold -> Ansi.Style.with_bold false style
  | `No_dim -> Ansi.Style.with_dim false style
  | `No_italic -> Ansi.Style.with_italic false style
  | `No_underline -> Ansi.Style.with_underline false style
  | `No_blink -> Ansi.Style.with_blink false style
  | `No_inverse -> Ansi.Style.with_inverse false style
  | `No_hidden -> Ansi.Style.with_hidden false style
  | `No_strikethrough -> Ansi.Style.with_strikethrough false style
  | `No_overline -> Ansi.Style.with_overline false style
  | `No_framed -> Ansi.Style.with_framed false style
  | `No_encircled -> Ansi.Style.with_encircled false style
  | `Fg c -> Ansi.Style.fg c style
  | `Bg c -> Ansi.Style.bg c style

let read_sgr t s =
  List.iter
    (function
      | Ansi.Parser.SGR attrs -> t.style <- List.fold_left sgr t.style attrs
      | _ -> ())
    (Ansi.Parser.parse s)

(* matrix 0.1.0 restores neither the style nor the character set, and
   restores the cursor's visibility, so the screen saves and restores the
   cursor itself, on each screen apart. *)
let save t =
  flush t;
  let row, col = Vte.cursor_pos t.vte in
  let s =
    { col; line = history t + row; style = t.style; charset = t.charset }
  in
  if Vte.is_alternate_screen t.vte then t.saved_alternate <- s else t.saved <- s

let restore t =
  flush t;
  let s =
    if Vte.is_alternate_screen t.vte then t.saved_alternate else t.saved
  in
  let row = max 0 (s.line - history t) in
  let row =
    if t.origin then max t.top (min t.bottom row)
    else min (Vte.rows t.vte - 1) row
  in
  move t ~row ~col:(min s.col (Vte.cols t.vte - 1));
  t.charset <- s.charset;
  t.style <- s.style;
  Buffer.add_string t.out ("\027[0m" ^ Ansi.Style.sgr_sequence s.style);
  flush t

let replacement = "\xef\xbf\xbd"

(* ECMA-48 sequences: CSI ends at a final byte, OSC at BEL or ST, and the
   strings of DCS, SOS, PM and APC, which xterm.js ignores here, at ST. *)
let escape = function
  | '[' -> Csi
  | ']' -> Osc
  | 'P' | 'X' | '^' | '_' -> String
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

(* [edit t ~row ~col s] makes matrix read the edit [s] at [row] and [col]. *)
let edit t ~row ~col s =
  flush t;
  Vte.set_cursor_pos t.vte ~row ~col;
  Buffer.add_string t.out s;
  flush t

(* matrix 0.1.0 deletes and inserts whole glyphs, where xterm.js edits
   cells and blanks the cells left of a wide glyph it cuts. [split t ~row x]
   blanks the wide glyph that column [x] of [row] cuts, if any, so that an
   edit from [x] cuts none. *)
let split t ~row x =
  if x > 0 && x < Vte.cols t.vte then
    let start, width = glyph t ~row x in
    if start < x then
      edit t ~row ~col:start (Fmt.str "\027[%dP\027[%d@" width width)

(* [cells t ~row ~col op n] deletes ([`Delete]), inserts or erases [n]
   cells of [row] from [col], as xterm.js does. *)
let cells t ~row ~col op n =
  split t ~row col;
  if op <> `Insert then split t ~row (col + n);
  edit t ~row ~col
    (match op with
    | `Delete -> Fmt.str "\027[%dP" n
    | `Insert -> Fmt.str "\027[%d@" n
    | `Erase -> Fmt.str "\027[%dP\027[%d@" n n)

(* A character edit starts from the cursor, which moves from beyond the
   last column. *)
let edit_chars t op n =
  flush t;
  move_to_last_column t;
  let row, col = Vte.cursor_pos t.vte in
  cells t ~row ~col op n

(* A column edit, of SL, SR, DECIC or DECDC, edits each row of the scroll
   region from the first column, or from the cursor's ([at_cursor]), when
   the cursor is in the region. As in xterm.js, the cursor beyond the last
   column edits from the first. *)
let edit_columns ?(at_cursor = false) t op n =
  flush t;
  let row, x = Vte.cursor_pos t.vte in
  let col = if at_cursor then x mod Vte.cols t.vte else 0 in
  if row >= t.top && row <= t.bottom then (
    for r = t.top to t.bottom do
      cells t ~row:r ~col op n
    done;
    Vte.set_cursor_pos t.vte ~row ~col:x)

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

let stops t =
  if Vte.is_alternate_screen t.vte then t.stops_alternate else t.stops

(* [tab t n] moves the cursor to the [n]th tab stop after it, or before it
   if [n] is negative, as xterm.js's [nextStop] and [prevStop] do, and not
   from beyond the last column. *)
let tab t n =
  flush t;
  let row, col = Vte.cursor_pos t.vte in
  let cols = Vte.cols t.vte and stops = stops t in
  let rec stop step x =
    let x = x + step in
    if x > 0 && x < cols && not stops.(x) then stop step x else x
  in
  if col < cols then
    let rec go k x = if k = 0 then x else go (k - 1) (stop (compare n 0) x) in
    move t ~row ~col:(max 0 (min (cols - 1) (go (abs n) col)))

(* [set_stop t b] sets the tab stop at the cursor ([b]) or clears it. *)
let set_stop t b =
  flush t;
  let _, col = Vte.cursor_pos t.vte in
  let stops = stops t in
  if col < Array.length stops then stops.(col) <- b

let reset t =
  t.top <- 0;
  t.bottom <- Vte.rows t.vte - 1;
  t.region <- (t.top, t.bottom);
  t.wrap <- true;
  t.origin <- false;
  Array.fill t.sets 0 4 'B';
  invoke t 0;
  t.style <- Ansi.Style.default;
  t.saved <- unsaved;
  t.saved_alternate <- unsaved;
  reset_stops t.stops;
  reset_stops t.stops_alternate

(* Whether the CSI sequence [s] has neither a private prefix nor an
   intermediate byte. *)
let plain s =
  String.for_all (fun c -> String.contains "0123456789;:" c) (params s)

(* A switch of screens gives the alternate one a full scroll region, and
   the normal one back its own; leaving the alternate one resets its tab
   stops. matrix 0.1.0 also restores the cursor's
   visibility when it leaves the alternate screen, which xterm.js keeps. *)
let switched t ~visible =
  if Vte.cursor_visible t.vte <> visible then
    Vte.set_cursor_visible t.vte visible;
  if Vte.is_alternate_screen t.vte then (
    t.region <- (t.top, t.bottom);
    t.top <- 0;
    t.bottom <- Vte.rows t.vte - 1)
  else
    let top, bottom = t.region in
    t.top <- top;
    t.bottom <- bottom;
    reset_stops t.stops_alternate

(* [mode t ~dec ~set p] sets ([set]) or resets the ANSI or DEC ([dec]) mode
   [p] as xterm.js does; mode 1049 saves the cursor before it enters the
   alternate screen and restores it after it leaves. *)
let mode t ~dec ~set p =
  let alternate = Vte.is_alternate_screen t.vte in
  let visible = Vte.cursor_visible t.vte in
  let cursor = dec && p = "1049" in
  if cursor && set then save t;
  (match (dec, p) with
  | true, "6" ->
      t.origin <- set;
      home t
  | true, "7" -> t.wrap <- set
  | _ -> ());
  Buffer.add_string t.out
    ("\027[" ^ (if dec then "?" else "") ^ p ^ if set then "h" else "l");
  flush t;
  if Vte.is_alternate_screen t.vte <> alternate then switched t ~visible;
  if cursor && not set then restore t

let modes t s =
  let set = csi_final s = Some 'h' and p = params s in
  let dec = String.starts_with ~prefix:"?" p in
  let p = if dec then String.sub p 1 (String.length p - 1) else p in
  List.iter (mode t ~dec ~set) (String.split_on_char ';' p)

(* DECSTR resets the modes, the scroll region, the style, the character
   sets and the save of the screen, whose line it moves to the first row,
   and shows the cursor, as in xterm.js. *)
let soft_reset t =
  flush t;
  t.top <- 0;
  t.bottom <- Vte.rows t.vte - 1;
  t.wrap <- true;
  t.origin <- false;
  Array.fill t.sets 0 4 'B';
  invoke t 0;
  t.style <- Ansi.Style.default;
  let s = { unsaved with line = history t } in
  if Vte.is_alternate_screen t.vte then t.saved_alternate <- s else t.saved <- s;
  Buffer.add_string t.out "\027[0m\027[4l\027[?1l\027[?7h\027[?25h\027[?2004l";
  flush t

(* ESC # 8 homes the cursor and fills the rows from it with E. *)
let align t =
  home t;
  let row, _ = Vte.cursor_pos t.vte and cols = Vte.cols t.vte in
  for r = row to Vte.rows t.vte - 1 do
    edit t ~row:r ~col:0 (String.make cols 'E')
  done;
  home t

(* The sequences matrix 0.1.0 misreads or lacks, the cursor moves and edits
   that read the cursor's column, and those xterm.js ignores: an escape
   sequence it has no handler for, and a CSI sequence with a private prefix
   or an intermediate byte, which matrix reads as one without. *)
let sequence t s =
  let n () = count s in
  let last = t.last in
  (* REP repeats the character just before it, as in xterm.js. *)
  t.last <- "";
  match (csi_final s, s) with
  | _, ("\027[3J" | "\027[?3J") -> erase_scrollback t
  | Some '@', _ when String.ends_with ~suffix:" @" s ->
      edit_columns t `Delete (edit_count s)
  | Some 'A', _ when String.ends_with ~suffix:" A" s ->
      edit_columns t `Insert (edit_count s)
  | Some '}', _ when String.ends_with ~suffix:"'}" s ->
      edit_columns t ~at_cursor:true `Insert (edit_count s)
  | Some '~', _ when String.ends_with ~suffix:"'~" s ->
      edit_columns t ~at_cursor:true `Delete (edit_count s)
  | Some 'p', _ when String.ends_with ~suffix:"!p" s -> soft_reset t
  | Some ('h' | 'l'), _ -> modes t s
  | Some ('J' | 'K'), _ -> control t s
  | Some _, _ when not (plain s) -> ()
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
  | Some 'P', _ -> edit_chars t `Delete (n ())
  | Some '@', _ -> edit_chars t `Insert (n ())
  | Some 'X', _ -> edit_chars t `Erase (n ())
  | Some 'b', _ ->
      if last <> "" then
        for _ = 1 to n () do
          character t last
        done
  | Some 'L', _ -> edit_lines t (-n ())
  | Some 'M', _ -> edit_lines t (n ())
  | Some ('C' | 'D' | 'G'), _ -> control t s
  | Some 'S', _ -> scroll t (n ())
  | Some 'T', _ -> scroll t (-n ())
  | Some 'r', _ ->
      set_region t (params s);
      home t
  | Some 's', _ -> save t
  | Some 'u', _ -> restore t
  | Some 'I', _ -> tab t (n ())
  | Some 'Z', _ -> tab t (-n ())
  | Some 'g', _ -> (
      match params s with
      | "" | "0" -> set_stop t false
      | "3" -> Array.fill (stops t) 0 (Array.length (stops t)) false
      | _ -> ())
  | None, "\027H" -> set_stop t true
  | Some 'm', _ ->
      read_sgr t s;
      Buffer.add_string t.out s
  | None, "\0277" -> save t
  | None, "\0278" -> restore t
  | None, "\027D" -> index t 1
  | None, "\027E" ->
      index t 1;
      move t ~row:(fst (Vte.cursor_pos t.vte)) ~col:0
  | None, "\027M" -> index t (-1)
  | None, ("\027n" | "\027}") -> invoke t 2
  | None, ("\027o" | "\027|") -> invoke t 3
  | None, "\027~" -> invoke t 1
  | None, ("\027%@" | "\027%G") ->
      invoke t 0;
      designate t 0 'B'
  | None, _ when String.length s = 3 && is_charset s.[2] -> (
      match level_of s.[1] with
      | Some level -> designate t level s.[2]
      | None -> ())
  | None, "\027#8" -> align t
  | None, "\027c" ->
      reset t;
      Buffer.add_string t.out s
  | None, _ when String.starts_with ~prefix:"\027]" s ->
      Buffer.add_string t.out s
  | None, _ -> ()
  | Some _, _ -> Buffer.add_string t.out s

(* The C0 controls, which xterm.js also runs inside an escape or CSI
   sequence. *)
let execute t = function
  | '\b' ->
      (* matrix 0.1.0 erases the character a backspace moves over, and not
         the one a cursor move does. *)
      t.last <- "";
      control t "\027[D"
  | '\n' | '\011' | '\012' ->
      t.last <- "";
      index t 1
  | '\t' ->
      t.last <- "";
      tab t 1
  | '\014' -> invoke t 1
  | '\015' -> invoke t 0
  | c -> Buffer.add_char t.out c

(* An escape starts a sequence, even inside one; CAN and SUB end it. *)
let start t =
  Buffer.clear t.seq;
  Buffer.add_char t.seq '\027';
  t.state <- Escape

let abort t =
  Buffer.clear t.seq;
  t.state <- Ground

let finish t =
  let s = Buffer.contents t.seq in
  abort t;
  sequence t s

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
    match (t.state, c) with
    | Ground, ' ' .. '~' -> print_ascii t c
    | (Ground | Escape | Csi), '\027' -> start t
    | (Ground | Escape | Csi), ('\x00' .. '\x17' | '\x19' | '\x1c' .. '\x1f') ->
        execute t c
    | _, ('\x18' | '\x1a') -> abort t
    | Ground, _ -> Buffer.add_char t.out c
    | Escape, c -> (
        Buffer.add_char t.seq c;
        match escape c with Ground -> finish t | state -> t.state <- state)
    | Csi, c ->
        Buffer.add_char t.seq c;
        if c >= '@' && c <= '~' then finish t
    | Osc, '\007' ->
        Buffer.add_char t.seq c;
        finish t
    | (Osc | String), '\027' -> t.state <- String_escape
    | Osc, c -> if c >= ' ' then Buffer.add_char t.seq c
    | String, _ -> ()
    | String_escape, c ->
        (* ST ends the string, and so does any other escape, which then
           starts. *)
        if Buffer.length t.seq > 1 && Buffer.nth t.seq 1 = ']' then
          Buffer.add_string t.seq "\027\\";
        finish t;
        if c <> '\\' then (
          start t;
          byte t c)

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
