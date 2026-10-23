(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type bar = { rings : bool; right : bool; size : int }
type fill = Colour of int | Image of string

type look = {
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
  margin_fill : fill;
  radius : int;
  bar : bar option;
  blink : bool;
  loop_offset : int;
  speed : float;
}

type paint = Foreground | Background | Ansi of int | Rgb of int

(* xterm.js draws bold in the bright variant of the first eight colours. *)
let paint ~bold ~default : Screen.colour -> paint = function
  | Default -> default
  | Indexed i when i < 16 -> Ansi (if bold && i < 8 then i + 8 else i)
  | Indexed i ->
      let r, g, b = Ansi.Color.to_rgb (Ansi.Color.indexed i) in
      Rgb ((r lsl 16) lor (g lsl 8) lor b)
  | Rgb c -> Rgb c

let has flag (c : Screen.cell) = Ansi.Attr.mem flag c.attrs

let paints (c : Screen.cell) =
  let bold = has Bold c in
  let fg = paint ~bold ~default:Foreground c.fg in
  let bg = paint ~bold:false ~default:Background c.bg in
  if has Inverse c then (paint ~bold:false ~default:Background c.bg, fg)
  else (fg, bg)

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

(* Stands for the image's name prefix until the image is complete: escaped text
   never holds a C0 control. *)
let ns = "\001"
let add_fill b rgb = Fmt.kstr (Buffer.add_string b) " fill=\"%s\"" (hex rgb)

(* The foreground is the root's fill, which every element inherits. *)
let add_rect_paint t b = function
  | Foreground -> ()
  | Background -> add_fill b t.palette.background
  | Ansi i -> add_fill b t.palette.ansi.(i)
  | Rgb c -> add_fill b c

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
          add_rect_paint t b p;
          Buffer.add_string b "/>";
          go y
  in
  go from

let text_classes (c : Screen.cell) =
  let deco =
    (if has Underline c && not (has Double_underline c) then "u" else "")
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

let decorated c =
  has Double_underline c
  || List.exists (String.starts_with ~prefix:"x") (text_classes c)

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
    (match fg with Ansi i -> [ Fmt.str "p%x" i ] | _ -> []) @ text_classes c
  in
  (match fg with
  | Background -> add_fill b t.palette.background
  | Rgb rgb -> add_fill b rgb
  | Foreground | Ansi _ -> ());
  if classes <> [] then
    Fmt.kstr (Buffer.add_string b) " class=\"%s\""
      (String.concat " " (List.map (( ^ ) ns) classes));
  Buffer.add_char b '>';
  add_escaped b text;
  Buffer.add_string b "</tspan>"

let same_style (a : Screen.cell) (b : Screen.cell) =
  fst (paints a) = fst (paints b) && text_classes a = text_classes b

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
      let shown = match Unicode.strip_format c.text with "" -> " " | s -> s in
      if c.width = 0 then go (x + 1) first xs
      else if not (shows c) then (
        Option.iter (fun f -> flush f xs text) first;
        go (x + 1) None [])
      else
        match first with
        | Some f when same_style f c && is_simple shown ->
            Buffer.add_string text shown;
            go (x + 1) first (x :: xs)
        | _ ->
            Option.iter (fun f -> flush f xs text) first;
            Buffer.add_string text shown;
            if is_simple shown then go (x + 1) (Some c) [ x ]
            else (
              flush c [ x ] text;
              go (x + 1) None [])
  in
  go from None []

(* Rectangles: WebKit ignores text-decoration-style on SVG text. *)
let add_double_underlines t b ~from cells =
  let n = Array.length cells in
  let double x = has Double_underline cells.(x) && shows cells.(x) in
  let thickness = max 1 (t.font_size / 16) in
  let rec go x =
    if x < n then (
      if not (double x) then go (x + 1)
      else
        let p = fst (paints cells.(x)) in
        let rec stop y =
          if
            y < n
            && (cells.(y).width = 0 || (double y && fst (paints cells.(y)) = p))
          then stop (y + 1)
          else y
        in
        let y = stop (x + 1) in
        List.iter
          (fun k ->
            Fmt.kstr (Buffer.add_string b)
              "<rect x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\""
              (x * t.cell_width)
              (t.baseline + (k * thickness))
              ((y - x) * t.cell_width)
              thickness;
            add_rect_paint t b p;
            Buffer.add_string b "/>")
          [ 2; 4 ];
        go y)
  in
  go from

let row_markup t ?(from = 0) row =
  let cells = Screen.cells row in
  let b = Buffer.create 256 in
  add_backgrounds t b ~from cells;
  add_double_underlines t b ~from cells;
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

type defs = {
  ids : string option Rows.t;
  by_markup : (string, string) Hashtbl.t;
  depths : (string, int) Hashtbl.t;
  defs : Buffer.t;
  mutable count : int;
}

let is_blank (c : Screen.cell) =
  c.text = " " && c.bg = Default && Ansi.Attr.is_empty c.attrs

let used cells =
  let rec go n = if n > 0 && is_blank cells.(n - 1) then go (n - 1) else n in
  go (Array.length cells)

let rec same_prefix c0 c1 k =
  k = 0 || (c0.(k - 1) = c1.(k - 1) && same_prefix c0 c1 (k - 1))

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
              let id = ns ^ "r" ^ base36 d.count in
              d.count <- d.count + 1;
              Hashtbl.replace d.by_markup markup id;
              Hashtbl.replace d.depths id depth;
              Fmt.kstr (Buffer.add_string d.defs) "<g id=\"%s\">%s</g>" id
                markup;
              Some id
      in
      Rows.replace d.ids row id;
      id

let add_cursor_defs t b =
  Fmt.kstr (Buffer.add_string b)
    "<rect id=\"%sc\" width=\"%d\" height=\"%d\" fill=\"%s\"/><rect id=\"%sw\" \
     width=\"%d\" height=\"%d\" fill=\"%s\"/>"
    ns t.cell_width t.cell_height (hex t.palette.cursor) ns (2 * t.cell_width)
    t.cell_height (hex t.palette.cursor)

let add_cursor t b screen (r, c) =
  let cells = Screen.cells (Screen.rows screen).(r) in
  let c = if c > 0 && cells.(c).width = 0 then c - 1 else c in
  let cell = cells.(c) in
  Fmt.kstr (Buffer.add_string b)
    "<use href=\"#%s%s\" class=\"%sk\" x=\"%d\" y=\"%d\"/>" ns
    (if cell.width = 2 then "w" else "c")
    ns (c * t.cell_width) (r * t.cell_height);
  let text = Unicode.strip_format cell.text in
  if cell.width > 0 && text <> "" && text <> " " && not (has Hidden cell) then (
    Fmt.kstr (Buffer.add_string b)
      "<text class=\"%sk\" x=\"%d\" y=\"%d\" fill=\"%s\">" ns (c * t.cell_width)
      ((r * t.cell_height) + t.baseline)
      (hex t.palette.cursor_accent);
    add_escaped b text;
    Buffer.add_string b "</text>")

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

(* In integers, so that the output does not depend on float printing. *)
let percent ~duration ms =
  (* In two steps, so that neither product overflows. *)
  let q = ms * 1000 / duration and r = ms * 1000 mod duration in
  let v = (q * 1000) + (r * 1000 / duration) in
  Fmt.str "%d.%04d%%" (v / 10_000) (v mod 10_000)

(* One class each, with no combinator: a rule must also match inside the shadow
   tree of a [use], which holds no ancestor of the referenced row. *)
let rule b cls decl = Fmt.kstr (Buffer.add_string b) ".%s%s{%s}" ns cls decl

let reduced b f =
  Buffer.add_string b "@media (prefers-reduced-motion:reduce){";
  f ();
  Buffer.add_string b "}"

let add_colours b (p : Palette.t) =
  Array.iteri (fun i c -> rule b (Fmt.str "p%x" i) ("fill:" ^ hex c)) p.ansi

let attributes =
  [
    ("b", "font-weight:bold");
    ("f", "fill-opacity:.6");
    ("i", "font-style:italic");
    ("xu", "text-decoration:underline");
    ("xs", "text-decoration:line-through");
    ("xo", "text-decoration:overline");
    ("xus", "text-decoration:underline line-through");
    ("xuo", "text-decoration:underline overline");
    ("xso", "text-decoration:line-through overline");
    ("xuso", "text-decoration:underline line-through overline");
  ]

let add_frames t b ~duration frames =
  let ms = max 1 (int_of_float (Float.round (float duration /. t.speed))) in
  let delay =
    if t.loop_offset = 0 then ""
    else
      Fmt.str ";animation-delay:-%dms"
        (int_of_float (Float.round (float t.loop_offset /. t.speed)))
  in
  rule b "s" (Fmt.str "animation:%ss %dms steps(1,end) infinite%s" ns ms delay);
  Fmt.kstr (Buffer.add_string b) "@keyframes %ss{" ns;
  List.iteri
    (fun i (time, _) ->
      Fmt.kstr (Buffer.add_string b) "%s{transform:translateX(%dpx)}"
        (percent ~duration time)
        (-i * t.cols * t.cell_width))
    frames;
  let last = -(List.length frames - 1) * t.cols * t.cell_width in
  Fmt.kstr (Buffer.add_string b) "100%%{transform:translateX(%dpx)}}" last;
  reduced b (fun () ->
      rule b "s" (Fmt.str "animation:none;transform:translateX(%dpx)" last))

let add_style t b ~duration frames =
  Buffer.add_string b "<style>";
  add_colours b t.palette;
  List.iter (fun (selector, decl) -> rule b selector decl) attributes;
  if t.blink then (
    rule b "k" (Fmt.str "animation:%sb 1.2s step-end infinite" ns);
    Fmt.kstr (Buffer.add_string b) "@keyframes %sb{50%%{opacity:0}}" ns;
    reduced b (fun () -> rule b "k" "animation:none"));
  (match frames with [] | [ _ ] -> () | _ -> add_frames t b ~duration frames);
  Buffer.add_string b "</style>"

(* VHS's draw.go; Go antialiases a circle of radius [r] from [r - 1]. *)
let add_bar t b bar =
  let circle ~cx ~cy ~r fill =
    if r > 0 then
      Fmt.kstr (Buffer.add_string b)
        "<circle cx=\"%d\" cy=\"%d\" r=\"%d.5\" %s/>" cx cy (r - 1) fill
  in
  let width = t.width - (2 * t.margin) in
  let centre ~gap ~r ~space i =
    let x = gap + r + (i * space) in
    t.margin + if bar.right then width - x else x
  in
  let cy r gap = t.margin + r + gap in
  if bar.rings then
    let outer = bar.size / 5 in
    let inner = 2 * 2 * outer / 5 in
    let gap = (bar.size - (2 * outer)) / 2 in
    let space = (2 * outer) + (bar.size / 6) in
    for i = 0 to 2 do
      let cx = centre ~gap ~r:outer ~space i in
      circle ~cx ~cy:(cy outer gap) ~r:outer "fill=\"#333333\"";
      circle ~cx ~cy:(cy outer gap) ~r:inner
        (Fmt.str "fill=\"%s\"" (hex t.palette.background))
    done
  else
    let r = bar.size / 6 in
    let gap = (bar.size - (2 * r)) / 2 in
    let space = (2 * r) + (bar.size / 6) in
    List.iteri
      (fun i fill ->
        circle ~cx:(centre ~gap ~r ~space i) ~cy:(cy r gap) ~r fill)
      [ {|fill="#ff4f4d"|}; {|fill="#febb00"|}; {|fill="#00cc1d"|} ]

let shown_text row =
  let b = Buffer.create 256 in
  Array.iter
    (fun (c : Screen.cell) ->
      if has Hidden c then Buffer.add_string b (String.make c.width ' ')
      else Buffer.add_string b (Unicode.strip_format c.text))
    (Screen.cells row);
  let s = Buffer.contents b in
  let rec stop n = if n > 0 && s.[n - 1] = ' ' then stop (n - 1) else n in
  String.sub s 0 (stop (String.length s))

let add_desc b screen =
  let lines = Array.to_list (Array.map shown_text (Screen.rows screen)) in
  let rec drop_blank = function "" :: rest -> drop_blank rest | l -> l in
  match List.rev (drop_blank (List.rev lines)) with
  | [] -> ()
  | lines ->
      Fmt.kstr (Buffer.add_string b) "<desc id=\"%sd\">" ns;
      List.iteri
        (fun i line ->
          if i > 0 then Buffer.add_char b '\n';
          add_escaped b line)
        lines;
      Buffer.add_string b "</desc>"

let add_margin t b =
  if t.margin > 0 then
    match t.margin_fill with
    | Colour c ->
        Fmt.kstr (Buffer.add_string b)
          "<rect width=\"%d\" height=\"%d\" fill=\"%s\"/>" t.width t.height
          (hex c)
    | Image uri ->
        Fmt.kstr (Buffer.add_string b)
          "<image width=\"%d\" height=\"%d\" preserveAspectRatio=\"none\" \
           href=\"%s\"/>"
          t.width t.height uri

let image t ~duration frames =
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
     viewBox=\"0 0 %d %d\" fill=\"%s\" font-family=\""
    t.width t.height t.width t.height (hex t.palette.foreground);
  add_escaped b t.font_family;
  let desc = Buffer.create 256 in
  (match List.rev frames with
  | (_, screen) :: _ -> add_desc desc screen
  | [] -> ());
  Fmt.kstr (Buffer.add_string b)
    "\" font-size=\"%d\" xml:space=\"preserve\" role=\"img\" \
     aria-labelledby=\"%st\"%s><title id=\"%st\">Terminal recording</title>"
    t.font_size ns
    (if Buffer.length desc > 0 then Fmt.str " aria-describedby=\"%sd\"" ns
     else "")
    ns;
  Buffer.add_buffer b desc;
  add_style t b ~duration frames;
  add_margin t b;
  Fmt.kstr (Buffer.add_string b)
    "<rect x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\" rx=\"%d\" fill=\"%s\"/>"
    t.margin t.margin
    (t.width - (2 * t.margin))
    (t.height - (2 * t.margin))
    t.radius (hex t.palette.background);
  Option.iter (add_bar t b) t.bar;
  Buffer.add_string b "<defs>";
  add_cursor_defs t b;
  Buffer.add_buffer b d.defs;
  Buffer.add_string b "</defs>";
  Fmt.kstr (Buffer.add_string b)
    "<svg x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\"><g class=\"%ss\">" t.x
    t.y (t.cols * t.cell_width) (t.rows * t.cell_height) ns;
  Buffer.add_buffer b strip;
  Buffer.add_string b "</g></svg></svg>\n";
  let s = Buffer.contents b in
  let prefix = "k" ^ String.sub (Digest.to_hex (Digest.string s)) 0 6 ^ "-" in
  String.concat prefix (String.split_on_char ns.[0] s)
