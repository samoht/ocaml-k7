(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* xterm.js keeps a buffer per screen, with its own scroll region and tab
   stops, and saves the cursor's line in the buffer, where matrix shares the
   region and the stops and saves a row of the screen. A terminal keeps what
   the normal screen gets back when it shows again. *)
type terminal = {
  vte : Vte.t;
  scan : Ansi.Parser.t;  (** Reads the bytes [vte] reads, for {!quirk}. *)
  mutable hit : Ansi.Parser.control option;
  replies : Buffer.t;
  kept : Matrix_grid.t;  (** The cells an erasure at the margin keeps. *)
  mutable region : int * int;  (** The normal screen's scroll region. *)
  mutable stops : int list;  (** The normal screen's tab stops. *)
  mutable line : int option;  (** The buffer line of its saved cursor. *)
}

let default_stops cols =
  List.filter (fun c -> c < cols) (List.init (cols / 8) (fun i -> 8 * (i + 1)))

(* xterm.js keeps a thousand lines of scrollback, gives each code point its
   own width and answers DA2 as xterm.js 5. *)
let terminal ~rows ~cols =
  {
    vte =
      Vte.create ~scrollback:1000 ~width_method:`Wcwidth
        ~default_fg:Ansi.Color.default ~default_bg:Ansi.Color.default
        ~da2:"\027[>0;276;0c" ~rows ~cols ();
    scan = Ansi.Parser.create ();
    hit = None;
    replies = Buffer.create 16;
    kept = Matrix_grid.create ~width:2 ~height:1 ();
    region = (0, rows - 1);
    stops = default_stops cols;
    line = None;
  }

let vte t = t.vte
let is_dirty t = Vte.is_dirty t.vte
let clear_dirty t = Vte.clear_dirty t.vte
let history t = Vte.scrollback_size t.vte
let send t s = Vte.feed_string t.vte s

(* [keeping t f] is [f ()], after which the cursor is back where it was. *)
let keeping t f =
  let row, col = Vte.cursor_pos t.vte in
  let v = f () in
  Vte.set_cursor_pos t.vte ~row ~col;
  v

(* Origin mode homes the cursor to the top of the region, and a move down
   stops at its bottom. *)
let region t =
  let origin = Vte.origin_mode t.vte in
  send t "\027[?6h";
  let top = fst (Vte.cursor_pos t.vte) in
  send t "\027[9999B";
  let bottom = fst (Vte.cursor_pos t.vte) in
  if not origin then send t "\027[?6l";
  (top, bottom)

let tab_stops t =
  let last = Vte.cols t.vte - 1 in
  Vte.set_cursor_pos t.vte ~row:0 ~col:0;
  let rec go acc =
    send t "\t";
    let col = snd (Vte.cursor_pos t.vte) in
    if col >= last then List.rev acc else go (col :: acc)
  in
  go []

let set_stops t stops =
  send t "\027[3g";
  List.iter
    (fun col ->
      Vte.set_cursor_pos t.vte ~row:0 ~col;
      send t "\027H")
    stops

let restore_line t =
  match t.line with
  | Some line when not (Vte.origin_mode t.vte) ->
      let _, col = Vte.cursor_pos t.vte in
      Vte.set_cursor_pos t.vte ~row:(line - history t) ~col
  | _ -> ()

(* The cells from the glyph in the last column of [row] to the margin. *)
let last_glyph t ~row =
  let g = Vte.grid t.vte and cols = Vte.cols t.vte in
  let x = cols - 1 in
  let x =
    if x > 0 && Matrix_grid.is_continuation g (Matrix_grid.idx g ~x ~y:row) then
      x - 1
    else x
  in
  (x, cols - x)

let erase_kept t ~row c final =
  let x, width = last_glyph t ~row and g = Vte.grid t.vte in
  Matrix_grid.blit_region ~src:g ~dst:t.kept ~src_x:x ~src_y:row ~width
    ~height:1 ~dst_x:0 ~dst_y:0;
  final ();
  if c = Ansi.Parser.ED 0 || c = Ansi.Parser.EL 0 then
    Matrix_grid.blit_region ~src:t.kept ~dst:g ~src_x:0 ~src_y:0 ~width
      ~height:1 ~dst_x:x ~dst_y:row

let switches = List.exists (fun m -> m = 47 || m = 1047 || m = 1049)

(* [quirk t c final] reads the control [c], whose last byte [final] feeds,
   as xterm.js does where it departs from xterm, and so from matrix. An
   erasure in the pending-wrap column starts beyond the last column and keeps
   the wrap pending; the cursor reports count that column and DECXCPR's has
   no page; and CSI S keeps no scrollback. *)
let quirk t c final =
  let v = t.vte in
  let row, col = Vte.cursor_pos v and cols = Vte.cols v in
  let normal () = not (Vte.is_alternate_screen v) in
  match c with
  | Ansi.Parser.(ED _ | EL _) when col = cols ->
      erase_kept t ~row c final;
      Vte.set_cursor_pos v ~row ~col
  | DSR 6 | DECXCPR ->
      Buffer.add_string t.replies (Vte.take_replies v);
      final ();
      ignore (Vte.take_replies v);
      Fmt.kstr
        (Buffer.add_string t.replies)
        "\027[%s%d;%dR"
        (if c = Ansi.Parser.DECXCPR then "?" else "")
        (row + 1) (col + 1)
  | SU _ when normal () ->
      (* DL from the top of the region, the final byte M for S. *)
      Vte.set_cursor_pos v ~row:(fst t.region) ~col:0;
      send t "M";
      Vte.set_cursor_pos v ~row ~col
  | DECSC when normal () ->
      final ();
      t.line <- Some (history t + row)
  | DECRC when normal () ->
      final ();
      restore_line t
  | DECSET m when normal () && switches m ->
      if List.mem 1049 m then t.line <- Some (history t + row);
      final ();
      keeping t (fun () -> set_stops t (default_stops cols))
  | DECRST m when (not (normal ())) && switches m ->
      final ();
      keeping t (fun () ->
          let top, bottom = t.region in
          Fmt.kstr (send t) "\027[%d;%dr" (top + 1) (bottom + 1);
          set_stops t t.stops);
      if List.mem 1049 m then restore_line t
  | DECSTBM _ | DECALN | DECSTR | Reset ->
      final ();
      if c = Reset || c = DECSTR then t.line <- None;
      if normal () then t.region <- keeping t (fun () -> region t);
      if c = Reset then t.stops <- default_stops cols
  | HTS | TBC _ ->
      final ();
      if normal () then t.stops <- keeping t (fun () -> tab_stops t)
  | _ -> final ()

let scan t = function
  | Ansi.Parser.Control
      (( ED _ | EL _
       | DSR 6
       | DECXCPR | SU _ | DECSC | DECRC | DECSET _ | DECRST _ | DECSTBM _
       | DECALN | DECSTR | Reset | HTS | TBC _ ) as c) ->
      t.hit <- Some c
  | _ -> ()

let feed t s =
  let b = Bytes.unsafe_of_string s in
  let start = ref 0 in
  for i = 0 to Bytes.length b - 1 do
    Ansi.Parser.feed t.scan b ~off:i ~len:1 (scan t);
    match t.hit with
    | None -> ()
    | Some c ->
        t.hit <- None;
        Vte.feed t.vte b !start (i - !start);
        quirk t c (fun () -> Vte.feed t.vte b i 1);
        start := i + 1
  done;
  Vte.feed t.vte b !start (Bytes.length b - !start)

let take_replies t =
  Buffer.add_string t.replies (Vte.take_replies t.vte);
  let s = Buffer.contents t.replies in
  Buffer.clear t.replies;
  s

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
  let attrs = Ansi.Attr.unpack (Matrix_grid.get_attrs grid idx) in
  let bg = colour (Matrix_grid.get_bg grid idx) in
  if Matrix_grid.is_continuation grid idx then
    { text = ""; width = 0; fg = Default; bg; attrs }
  else
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
  let width = Matrix_grid.width grid in
  let cells =
    Array.init width (fun x -> cell grid (Matrix_grid.idx grid ~x ~y))
  in
  let b = Buffer.create width in
  Array.iter (fun (c : cell) -> Buffer.add_string b c.text) cells;
  { cells; text = trim_end (Buffer.contents b) }

let of_terminal ?previous ?(offset = 0) t =
  let vte = t.vte in
  let rows = Vte.rows vte and cols = Vte.cols vte in
  let offset = max 0 (min offset (history t)) in
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
  let older = List.map trim_end (Vte.scrollback_lines term.vte) in
  Array.append (Array.of_list older) (Array.map (fun r -> r.text) t.rows)

let line t r = if r >= 0 && r < Array.length t.rows then t.rows.(r).text else ""

let text t =
  String.concat "\n" (Array.to_list (Array.map (fun r -> r.text) t.rows))

let equal t0 t1 =
  t0.cursor = t1.cursor
  && Array.length t0.rows = Array.length t1.rows
  && Array.for_all2 row_equal t0.rows t1.rows
