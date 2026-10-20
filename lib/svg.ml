(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  width : int;
  height : int;
  x : int;
  y : int;
  cols : int;
  rows : int;
  cell_width : int;
  cell_height : int;
  baseline : int;
  font_size : int;
  font_family : string;
  palette : Palette.t;
  margin : int;
  margin_fill : int;
  radius : int;
  blink : bool;
  loop_offset : int;
  speed : float;
}

let pp ppf t =
  Fmt.pf ppf "@[<h>%dx%d image, %dx%d cells of %dx%d at (%d, %d)@]" t.width
    t.height t.cols t.rows t.cell_width t.cell_height t.x t.y

(* xterm's sixteen colours, which the terminal resolves the ANSI colours to
   and the theme replaces. *)
let xterm =
  [| 0x000000; 0xcd0000; 0x00cd00; 0xcdcd00; 0x0000ee; 0xcd00cd; 0x00cdcd;
     0xe5e5e5; 0x7f7f7f; 0xff0000; 0x00ff00; 0xffff00; 0x5c5cff; 0xff00ff;
     0x00ffff; 0xffffff |]
[@@ocamlformat "disable"]

type paint = Foreground | Background | Ansi of int | Rgb of int

(* xterm.js draws bold text in the bright variant of the first eight. *)
let paint ~bold ~default rgb =
  if rgb < 0 then default
  else
    let rec find i =
      if i = 16 then Rgb rgb
      else if xterm.(i) = rgb then Ansi (if bold && i < 8 then i + 8 else i)
      else find (i + 1)
    in
    find 0

let has flag (c : Screen.cell) = Ansi.Attr.mem flag c.attrs

(* The text and background paints of [c]. *)
let paints (c : Screen.cell) =
  let bold = has Bold c in
  let fg = paint ~bold ~default:Foreground c.fg in
  let bg = paint ~bold:false ~default:Background c.bg in
  if has Inverse c then (paint ~bold:false ~default:Background c.bg, fg)
  else (fg, bg)

(* Escaping *)

let add_escaped b s =
  let rec go i =
    if i < String.length s then (
      let d = String.get_utf_8_uchar s i in
      let u = Uchar.to_int (Uchar.utf_decode_uchar d) in
      (match u with
      | 0x26 -> Buffer.add_string b "&amp;"
      | 0x3c -> Buffer.add_string b "&lt;"
      | 0x3e -> Buffer.add_string b "&gt;"
      | 0x22 -> Buffer.add_string b "&quot;"
      | _
        when u < 0x20 || u = 0xfffe || u = 0xffff
             || not (Uchar.utf_decode_is_valid d) ->
          Buffer.add_string b "\xef\xbf\xbd"
      | _ -> Buffer.add_substring b s i (Uchar.utf_decode_length d));
      go (i + Uchar.utf_decode_length d))
  in
  go 0

let hex rgb = Fmt.str "#%06x" rgb

(* Rows *)

let class_of = function
  | Foreground -> Some "d"
  | Background -> Some "e"
  | Ansi i -> Fmt.kstr Option.some "p%x" i
  | Rgb _ -> None

let add_paint b ~text p =
  match (p, class_of p) with
  | Foreground, _ when text -> ()
  | Rgb c, _ -> Fmt.kstr (Buffer.add_string b) " fill=\"%s\"" (hex c)
  | _, Some cls -> Fmt.kstr (Buffer.add_string b) " class=\"%s\"" cls
  | _, None -> ()

let add_backgrounds t b ~from cells =
  let n = Array.length cells in
  let rec go x =
    if x < n then
      match snd (paints cells.(x)) with
      | Background -> go (x + 1)
      | p ->
          let rec stop y =
            if y < n && snd (paints cells.(y)) = p then stop (y + 1) else y
          in
          let y = stop (x + 1) in
          Fmt.kstr (Buffer.add_string b)
            "<rect x=\"%d\" width=\"%d\" height=\"%d\"" (x * t.cell_width)
            ((y - x) * t.cell_width)
            t.cell_height;
          add_paint b ~text:false p;
          Buffer.add_string b "/>";
          go y
  in
  go from

(* The classes of a text run other than its colour. *)
let text_classes (c : Screen.cell) =
  let deco =
    (if has Underline c || has Double_underline c then "u" else "")
    ^ (if has Strikethrough c then "s" else "")
    ^ if has Overline c then "o" else ""
  in
  List.filter (( <> ) "")
    [
      (if has Bold c then "b" else "");
      (if has Dim c then "f" else "");
      (if has Italic c then "i" else "");
      (if deco = "" then "" else "x" ^ deco);
    ]

let decorated c = List.exists (String.starts_with ~prefix:"x") (text_classes c)

let shows (c : Screen.cell) =
  c.width > 0 && (not (has Hidden c)) && (c.text <> " " || decorated c)

let is_simple s =
  String.length s = Uchar.utf_decode_length (String.get_utf_8_uchar s 0)

let add_tspan t b (c : Screen.cell) xs text =
  Buffer.add_string b "<tspan x=\"";
  Buffer.add_string b
    (String.concat " "
       (List.map (fun x -> Int.to_string (x * t.cell_width)) xs));
  Buffer.add_char b '"';
  let fg = fst (paints c) in
  let classes =
    Option.to_list (match fg with Foreground -> None | p -> class_of p)
    @ text_classes c
  in
  (match fg with
  | Rgb rgb -> Fmt.kstr (Buffer.add_string b) " fill=\"%s\"" (hex rgb)
  | _ -> ());
  if classes <> [] then
    Fmt.kstr (Buffer.add_string b) " class=\"%s\"" (String.concat " " classes);
  Buffer.add_char b '>';
  add_escaped b text;
  Buffer.add_string b "</tspan>"

let same_style (a : Screen.cell) (b : Screen.cell) =
  fst (paints a) = fst (paints b) && text_classes a = text_classes b

(* A tspan per run of cells of one style, a multi-code-point grapheme in a
   tspan of its own, whose position its first code point takes. *)
let add_texts t b ~from cells =
  let n = Array.length cells in
  let flush first xs text =
    if xs <> [] then add_tspan t b first (List.rev xs) (Buffer.contents text);
    Buffer.clear text
  in
  let text = Buffer.create 64 in
  let rec go x first xs =
    if x >= n then Option.iter (fun f -> flush f xs text) first
    else
      let (c : Screen.cell) = cells.(x) in
      if c.width = 0 then go (x + 1) first xs
      else if not (shows c) then (
        Option.iter (fun f -> flush f xs text) first;
        go (x + 1) None [])
      else
        match first with
        | Some f when same_style f c && is_simple c.text ->
            Buffer.add_string text c.text;
            go (x + 1) first (x :: xs)
        | _ ->
            Option.iter (fun f -> flush f xs text) first;
            Buffer.add_string text c.text;
            if is_simple c.text then go (x + 1) (Some c) [ x ]
            else (
              flush c [ x ] text;
              go (x + 1) None [])
  in
  go from None []

let row_markup t ?(from = 0) row =
  let cells = Screen.cells row in
  let b = Buffer.create 256 in
  add_backgrounds t b ~from cells;
  let texts = Buffer.create 256 in
  add_texts t texts ~from cells;
  if Buffer.length texts > 0 then (
    Fmt.kstr (Buffer.add_string b) "<text y=\"%d\">" t.baseline;
    Buffer.add_buffer b texts;
    Buffer.add_string b "</text>");
  Buffer.contents b

module Rows = Hashtbl.Make (struct
  type t = Screen.row

  let equal = Screen.row_equal
  let hash r = Hashtbl.hash (Screen.row_text r)
end)

(* The id of each distinct non-blank row, the markup of its definition, and
   how many rows its definition nests. *)
type defs = {
  ids : string option Rows.t;
  by_markup : (string, string) Hashtbl.t;
  depths : (string, int) Hashtbl.t;
  defs : Buffer.t;
  mutable count : int;
}

let is_blank (c : Screen.cell) =
  c.text = " " && c.bg < 0 && Ansi.Attr.is_empty c.attrs

(* The cells up to the last one that is not blank. *)
let used cells =
  let rec go n = if n > 0 && is_blank cells.(n - 1) then go (n - 1) else n in
  go (Array.length cells)

let rec same_prefix c0 c1 k =
  k = 0 || (c0.(k - 1) = c1.(k - 1) && same_prefix c0 c1 (k - 1))

(* A chain of rows that each add to the one before, as typing makes, is
   drawn by reference to the previous row, up to this depth. *)
let max_depth = 16

let markup t d ~base row =
  let full = (row_markup t row, 0) in
  match base with
  | None -> full
  | Some (prev, id) ->
      let depth = Option.value (Hashtbl.find_opt d.depths id) ~default:0 in
      let p = Screen.cells prev and c = Screen.cells row in
      let k = used p in
      if
        depth >= max_depth || k = 0
        || Array.length p <> Array.length c
        || not (same_prefix p c k)
      then full
      else
        ( Fmt.str "<use href=\"#%s\"/>%s" id (row_markup t ~from:k row),
          depth + 1 )

let base36 n =
  let digits = "0123456789abcdefghijklmnopqrstuvwxyz" in
  let rec go n acc =
    if n < 36 then String.make 1 digits.[n] ^ acc
    else go (n / 36) (String.make 1 digits.[n mod 36] ^ acc)
  in
  go n ""

let row_id t d ~base row =
  match Rows.find_opt d.ids row with
  | Some id -> id
  | None ->
      let markup, depth =
        if used (Screen.cells row) = 0 then ("", 0) else markup t d ~base row
      in
      let id =
        if markup = "" then None
        else
          match Hashtbl.find_opt d.by_markup markup with
          | Some id -> Some id
          | None ->
              let id = "r" ^ base36 d.count in
              d.count <- d.count + 1;
              Hashtbl.replace d.by_markup markup id;
              Hashtbl.replace d.depths id depth;
              Fmt.kstr (Buffer.add_string d.defs) "<g id=\"%s\">%s</g>" id
                markup;
              Some id
      in
      Rows.replace d.ids row id;
      id

(* The cursor is a block, [#c], or [#w] on a wide glyph, which it draws in
   the cursor accent colour. *)
let add_cursor_defs t b =
  Fmt.kstr (Buffer.add_string b)
    "<rect id=\"c\" class=\"kc\" width=\"%d\" height=\"%d\"/><rect id=\"w\" \
     class=\"kc\" width=\"%d\" height=\"%d\"/>"
    t.cell_width t.cell_height (2 * t.cell_width) t.cell_height

let add_cursor t b screen (r, c) =
  let cell = (Screen.cells (Screen.rows screen).(r)).(c) in
  Fmt.kstr (Buffer.add_string b)
    "<use href=\"#%s\" class=\"k\" x=\"%d\" y=\"%d\"/>"
    (if cell.width = 2 then "w" else "c")
    (c * t.cell_width) (r * t.cell_height);
  if cell.width > 0 && cell.text <> " " then (
    Fmt.kstr (Buffer.add_string b) "<text class=\"ka k\" x=\"%d\" y=\"%d\">"
      (c * t.cell_width)
      ((r * t.cell_height) + t.baseline);
    add_escaped b cell.text;
    Buffer.add_string b "</text>")

(* [shown] holds, for each row, the row the previous screen showed there and
   its id. *)
let add_frame t d b shown i screen =
  if i = 0 then Buffer.add_string b "<g>"
  else
    Fmt.kstr (Buffer.add_string b) "<g transform=\"translate(%d)\">"
      (i * t.cols * t.cell_width);
  Array.iteri
    (fun r row ->
      match row_id t d ~base:shown.(r) row with
      | None -> shown.(r) <- None
      | Some id ->
          shown.(r) <- Some (row, id);
          Fmt.kstr (Buffer.add_string b) "<use href=\"#%s\" y=\"%d\"/>" id
            (r * t.cell_height))
    (Screen.rows screen);
  Option.iter (add_cursor t b screen) (Screen.cursor screen);
  Buffer.add_string b "</g>"

(* Style *)

(* A percentage of [duration] with four decimals, in integers so that the
   output does not depend on float printing. *)
let percent ~duration ms =
  let v = ms * 1_000_000 / duration in
  Fmt.str "%d.%04d%%" (v / 10_000) (v mod 10_000)

let add_style t b ~duration frames =
  let p = t.palette in
  Fmt.kstr (Buffer.add_string b)
    "<style>text{fill:%s}.d{fill:%s}.e{fill:%s}.kc{fill:%s}.ka{fill:%s}"
    (hex p.foreground) (hex p.foreground) (hex p.background) (hex p.cursor)
    (hex p.cursor_accent);
  Array.iteri
    (fun i c -> Fmt.kstr (Buffer.add_string b) ".p%x{fill:%s}" i (hex c))
    p.ansi;
  Buffer.add_string b
    ".b{font-weight:bold}.f{fill-opacity:.5}.i{font-style:italic}.xu{text-decoration:underline}.xs{text-decoration:line-through}.xo{text-decoration:overline}.xus{text-decoration:underline \
     line-through}.xuo{text-decoration:underline \
     overline}.xso{text-decoration:line-through \
     overline}.xuso{text-decoration:underline line-through overline}";
  if t.blink then
    Buffer.add_string b
      ".k{animation:b 1.2s step-end infinite}@keyframes b{50%{opacity:0}}";
  (match frames with
  | [] | [ _ ] -> ()
  | _ ->
      let ms = max 1 (int_of_float (Float.round (float duration /. t.speed))) in
      let delay =
        if t.loop_offset = 0 then ""
        else
          Fmt.str ";animation-delay:-%dms"
            (int_of_float (Float.round (float t.loop_offset /. t.speed)))
      in
      Fmt.kstr (Buffer.add_string b)
        ".s{animation:s %dms steps(1,end) infinite%s}@keyframes s{" ms delay;
      List.iteri
        (fun i (time, _) ->
          Fmt.kstr (Buffer.add_string b) "%s{transform:translateX(%dpx)}"
            (percent ~duration time)
            (-i * t.cols * t.cell_width))
        frames;
      Fmt.kstr (Buffer.add_string b) "100%%{transform:translateX(%dpx)}}"
        (-(List.length frames - 1) * t.cols * t.cell_width));
  Buffer.add_string b "</style>"

let to_string t ~duration frames =
  let b = Buffer.create 65536 in
  let d =
    {
      ids = Rows.create 64;
      by_markup = Hashtbl.create 64;
      depths = Hashtbl.create 64;
      defs = Buffer.create 65536;
      count = 0;
    }
  in
  let strip = Buffer.create 65536 in
  let shown = Array.make t.rows None in
  List.iteri (fun i (_, screen) -> add_frame t d strip shown i screen) frames;
  Fmt.kstr (Buffer.add_string b)
    "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%d\" height=\"%d\" \
     viewBox=\"0 0 %d %d\" font-family=\""
    t.width t.height t.width t.height;
  add_escaped b t.font_family;
  Fmt.kstr (Buffer.add_string b) "\" font-size=\"%d\" xml:space=\"preserve\">"
    t.font_size;
  add_style t b ~duration frames;
  if t.margin > 0 then
    Fmt.kstr (Buffer.add_string b)
      "<rect width=\"%d\" height=\"%d\" fill=\"%s\"/>" t.width t.height
      (hex t.margin_fill);
  Fmt.kstr (Buffer.add_string b)
    "<rect x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\" rx=\"%d\" class=\"e\"/>"
    t.margin t.margin
    (t.width - (2 * t.margin))
    (t.height - (2 * t.margin))
    t.radius;
  Buffer.add_string b "<defs>";
  add_cursor_defs t b;
  Buffer.add_buffer b d.defs;
  Buffer.add_string b "</defs>";
  Fmt.kstr (Buffer.add_string b)
    "<svg x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\"><g class=\"s\">" t.x t.y
    (t.cols * t.cell_width) (t.rows * t.cell_height);
  Buffer.add_buffer b strip;
  Buffer.add_string b "</g></svg></svg>\n";
  Buffer.contents b
