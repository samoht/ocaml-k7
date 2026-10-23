(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Ints = Map.Make (Int)

(* Where an OSC is read: its code so far, [-1] before a digit, or the code of
   an OSC 4 or 104 whose payload is read, or one that is ignored. *)
type osc = Code of int | Payload of int | Ignored

(* Where a [CSI ? Pm h] or [CSI ? Pm l] is read: [n] is the parameter read so
   far, and [sync] whether an earlier one is 2026. *)
type scan =
  | Ground
  | Esc
  | Csi
  | Private of { n : int; sync : bool }
  | Osc of osc
  | String

type terminal = {
  vte : Vte.t;
  mutable scan : scan;
  mutable partial : string;
  payload : Buffer.t;
  mutable colours : int Ints.t;
  mutable recoloured : bool;
}

(* As xterm.js: a thousand lines of scrollback, a width per code point, and
   DA2 answered as xterm.js 5. *)
let terminal ~rows ~cols =
  {
    vte =
      Vte.create ~scrollback:1000 ~width_method:`Wcwidth
        ~default_fg:Ansi.Color.default ~default_bg:Ansi.Color.default
        ~da2:"\027[>0;276;0c" ~rows ~cols ();
    scan = Ground;
    partial = "";
    payload = Buffer.create 64;
    colours = Ints.empty;
    recoloured = false;
  }

let next scan c =
  match (scan, c) with
  | _, '\027' -> Esc
  | Esc, '[' -> Csi
  | _, '\x9d' | Esc, ']' -> Osc (Code (-1))
  | _, ('\x90' | '\x98' | '\x9e' | '\x9f') | Esc, ('P' | 'X' | '^' | '_') ->
      String
  | Csi, '?' -> Private { n = 0; sync = false }
  | Private p, '0' .. '9' ->
      Private { p with n = min 10_000 ((p.n * 10) + Char.code c - 48) }
  | Private p, ';' -> Private { n = 0; sync = p.sync || p.n = 2026 }
  | _ -> Ground

let number s =
  if
    s = ""
    || not (String.for_all (function '0' .. '9' -> true | _ -> false) s)
  then None
  else
    Some
      (String.fold_left (fun n c -> min 1000 ((n * 10) + Char.code c - 48)) 0 s)

let hex s =
  let digit = function
    | '0' .. '9' as c -> Some (Char.code c - 48)
    | 'a' .. 'f' as c -> Some (Char.code c - 87)
    | _ -> None
  in
  String.fold_left
    (fun n c ->
      match (n, digit c) with
      | Some n, Some d -> Some ((n * 16) + d)
      | _ -> None)
    (Some 0) s

let rgb r g b = (r lsl 16) lor (g lsl 8) lor b

(* xterm.js's [parseColor]: [rgb:R/G/B] with one to four hex digits a channel,
   scaled to eight bits and rounded, or [#RGB] to [#RRRRGGGGBBBB], whose
   channels keep their high eight bits; [#RGB] reads as [#R0G0B0]. *)
let colour spec =
  let spec = String.lowercase_ascii spec in
  let channels s n =
    if String.length s mod 3 <> 0 || n = 0 then None
    else
      match List.init 3 (fun i -> hex (String.sub s (i * n) n)) with
      | [ Some r; Some g; Some b ] -> Some (r, g, b)
      | _ -> None
  in
  match String.split_on_char ':' spec with
  | [ "rgb"; s ] -> (
      match String.split_on_char '/' s with
      | [ r; g; b ]
        when String.length r = String.length g
             && String.length g = String.length b
             && String.length r >= 1
             && String.length r <= 4 -> (
          let n = String.length r in
          let base = (1 lsl (4 * n)) - 1 in
          let scale v = ((v * 510) + base) / (2 * base) in
          match channels (r ^ g ^ b) n with
          | Some (r, g, b) -> Some (rgb (scale r) (scale g) (scale b))
          | None -> None)
      | _ -> None)
  | _ when String.starts_with ~prefix:"#" spec -> (
      let s = String.sub spec 1 (String.length spec - 1) in
      let n = String.length s / 3 in
      match channels s n with
      | Some (r, g, b) when n <= 4 ->
          let scale v = if n = 1 then v lsl 4 else v lsr (4 * (n - 2)) in
          Some (rgb (scale r) (scale g) (scale b))
      | _ -> None)
  | _ -> None

let recolour t colours =
  if colours != t.colours then (
    t.colours <- colours;
    t.recoloured <- true)

(* xterm.js's [setOrReportIndexedColor] and [restoreIndexedColor]; k7 answers
   no colour query. *)
let set_colours t data =
  let rec go colours = function
    | index :: spec :: rest -> (
        match (number index, colour spec) with
        | Some i, Some c when i < 256 -> go (Ints.add i c colours) rest
        | _ -> go colours rest)
    | _ -> colours
  in
  recolour t (go t.colours (String.split_on_char ';' data))

let restore_colours t data =
  if data = "" then recolour t Ints.empty
  else
    recolour t
      (List.fold_left
         (fun colours index ->
           match number index with
           | Some i -> Ints.remove i colours
           | None -> colours)
         t.colours
         (String.split_on_char ';' data))

let end_osc t = function
  | Code 104 -> restore_colours t ""
  | Payload 4 -> set_colours t (Buffer.contents t.payload)
  | Payload 104 -> restore_colours t (Buffer.contents t.payload)
  | Code _ | Payload _ | Ignored -> ()

(* xterm.js's limit, past which it drops an OSC. *)
let payload_limit = 10_000_000

let put t osc c u =
  match (osc, c) with
  | Code n, '0' .. '9' -> Code (min 1000 ((max n 0 * 10) + Char.code c - 48))
  | Code ((4 | 104) as n), ';' ->
      Buffer.clear t.payload;
      Payload n
  | Code _, _ -> Ignored
  | Payload _, _ when Buffer.length t.payload >= payload_limit -> Ignored
  | Payload _, _ ->
      Buffer.add_utf_8_uchar t.payload u;
      osc
  | Ignored, _ -> Ignored

(* As xterm.js: BEL, ESC and ST end an OSC, CAN and SUB cancel it, another C1
   control leaves it, and it ignores the other C0 controls. *)
let scan_osc t osc c u =
  match c with
  | '\007' | '\x9c' ->
      end_osc t osc;
      Ground
  | '\027' ->
      end_osc t osc;
      Esc
  | '\x18' | '\x1a' -> Ground
  | '\000' .. '\031' -> Osc osc
  | '\x80' .. '\x9f' -> next Ground c
  | c -> Osc (put t osc c u)

(* xterm.js reads characters, not bytes. Those above U+00FF are all printable,
   as U+00A0 is. *)
let latin_1 u = if Uchar.is_char u then Uchar.to_char u else '\xa0'

(* A DCS, SOS, PM or APC string: as xterm.js, ESC, CAN, SUB and the C1 controls
   end it. *)
let scan_string c =
  match c with
  | '\027' | '\x18' | '\x1a' | '\x80' .. '\x9f' -> next Ground c
  | _ -> String

let step t c u =
  t.scan <-
    (match t.scan with
    | Osc osc -> scan_osc t osc c u
    | String -> scan_string c
    | scan -> next scan c)

(* [s] ends at [i] with the first [n] bytes of a character. *)
let truncated s i n =
  i + n = String.length s
  && (n > 1 || match s.[i] with '\xc2' .. '\xf4' -> true | _ -> false)

(* matrix ignores DEC mode 2026 and the OSC 4 and 104 palette sequences, so the
   characters are scanned here too. A character that [s] cuts is scanned and
   fed with the next [s]. matrix also reads an OSC, DCS, SOS, PM or APC string
   on through CAN and SUB, which cancel it in xterm.js, so ST ends it in their
   place: matrix then applies at most a title or a hyperlink, which k7 does not
   draw. And matrix draws a U+FFFD for each byte of an ill-formed sequence, so
   it is fed one U+FFFD for each maximal subpart, as Stdlib decodes it. *)
let feed ?(sync = ignore) t s =
  let s = t.partial ^ s in
  t.partial <- "";
  let feed_to start i =
    Vte.feed t.vte (Bytes.unsafe_of_string s) start (i - start)
  in
  let rec go start i =
    if i = String.length s then feed_to start i
    else
      let d = String.get_utf_8_uchar s i in
      let n = Uchar.utf_decode_length d in
      let u = Uchar.utf_decode_uchar d in
      let valid = Uchar.utf_decode_is_valid d in
      if (not valid) && truncated s i n then (
        t.partial <- String.sub s i n;
        feed_to start i)
      else if not valid then (
        feed_to start i;
        Vte.feed_string t.vte "\xef\xbf\xbd";
        step t (latin_1 u) u;
        go (i + n) (i + n))
      else
        match (t.scan, latin_1 u) with
        | Private p, (('h' | 'l') as c) when p.sync || p.n = 2026 ->
            t.scan <- Ground;
            feed_to start (i + 1);
            sync (c = 'h');
            go (i + 1) (i + 1)
        | (Osc _ | String), ('\x18' | '\x1a') ->
            t.scan <- Ground;
            feed_to start i;
            Vte.feed_string t.vte "\027\\";
            go (i + 1) (i + 1)
        | _, c ->
            step t c u;
            go start (i + n)
  in
  go 0 0

let take_replies t = Vte.take_replies t.vte
let is_dirty t = t.recoloured || Vte.is_dirty t.vte

let clear_dirty t =
  t.recoloured <- false;
  Vte.clear_dirty t.vte

let history t = Vte.scrollback_size t.vte
let cursor_keys t = Vte.cursor_key_mode t.vte
let cursor_row t = fst (Vte.cursor_pos t.vte)

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

type t = { rows : row array; cursor : (int * int) option; colours : int Ints.t }

let rows t = t.rows
let cursor t = t.cursor
let indexed t i = Ints.find_opt i t.colours

let same_palette t0 t1 =
  t0.colours == t1.colours || Ints.equal Int.equal t0.colours t1.colours

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

let of_terminal ?previous ?(offset = 0) term =
  let vte = term.vte in
  let rows = Vte.rows vte and cols = Vte.cols vte in
  let offset = max 0 (min offset (history term)) in
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
  { rows = rows'; cursor; colours = term.colours }

let shown_text r =
  let hidden (c : cell) = Ansi.Attr.mem Hidden c.attrs in
  if not (Array.exists hidden r.cells) then r.text
  else
    let b = Buffer.create (Array.length r.cells) in
    Array.iter
      (fun c ->
        Buffer.add_string b
          (if hidden c then String.make c.width ' ' else c.text))
      r.cells;
    trim_end (Buffer.contents b)

let buffer term t =
  let older = List.map trim_end (Vte.scrollback_lines term.vte) in
  Array.append (Array.of_list older) (Array.map (fun r -> r.text) t.rows)

let line t r = if r >= 0 && r < Array.length t.rows then t.rows.(r).text else ""

let text t =
  String.concat "\n" (Array.to_list (Array.map (fun r -> r.text) t.rows))

let equal t0 t1 =
  t0.cursor = t1.cursor && same_palette t0 t1
  && Array.length t0.rows = Array.length t1.rows
  && Array.for_all2 row_equal t0.rows t1.rows

let sgr b (c : cell) =
  let codes =
    (0 :: Ansi.Attr.to_sgr_codes c.attrs)
    @ (match c.fg with
      | Default -> []
      | Indexed i -> [ 38; 5; i ]
      | Rgb x -> [ 38; 2; x lsr 16; (x lsr 8) land 0xff; x land 0xff ])
    @
    match c.bg with
    | Default -> []
    | Indexed i -> [ 48; 5; i ]
    | Rgb x -> [ 48; 2; x lsr 16; (x lsr 8) land 0xff; x land 0xff ]
  in
  Buffer.add_string b "\027[";
  Buffer.add_string b (String.concat ";" (List.map string_of_int codes));
  Buffer.add_char b 'm'

(* [Vte.resize] clears the screen; a terminal keeps what fits. DECSC and DECRC
   keep the cursor, the pen and the character sets around the repaint. *)
let resize term ~rows ~cols =
  let vte = term.vte in
  if Vte.rows vte <> rows || Vte.cols vte <> cols then (
    let before = of_terminal term in
    Vte.resize vte ~rows ~cols;
    let b = Buffer.create 4096 in
    Buffer.add_string b "\0277\027(B";
    Array.iteri
      (fun r row ->
        if r < rows then (
          Fmt.kstr (Buffer.add_string b) "\027[%d;1H" (r + 1);
          let x = ref 0 in
          Array.iter
            (fun (c : cell) ->
              if c.width > 0 && !x + c.width <= cols then (
                sgr b c;
                Buffer.add_string b c.text);
              x := !x + c.width)
            row.cells))
      before.rows;
    Buffer.add_string b "\0278";
    Vte.feed_string vte (Buffer.contents b))
