(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Alcobar

(* A 50 x 13 terminal of 7 x 13 pixel cells. *)
let settings = Vhs.[ Width 400; Height 200; Font_size 10; Padding 10 ]
let cols = 50
let rows = 13

(* The characters XML cannot hold show as U+FFFD. *)
let xml_text s =
  let b = Buffer.create (String.length s) in
  let rec go i =
    if i < String.length s then (
      let d = String.get_utf_8_uchar s i in
      (match Uchar.to_int (Uchar.utf_decode_uchar d) with
      | 0xfffe | 0xffff -> Buffer.add_string b "\xef\xbf\xbd"
      | _ -> Buffer.add_substring b s i (Uchar.utf_decode_length d));
      go (i + Uchar.utf_decode_length d))
  in
  go 0;
  Buffer.contents b

(* The text of each row of a screen as the image draws it: a hidden cell
   is blank, format characters and trailing spaces do not show. *)
let drawn screen =
  Array.to_list (Screen.rows screen)
  |> List.map (fun r ->
      Array.to_list (Screen.cells r)
      |> List.map (fun (c : Screen.cell) ->
          if c.width = 0 then ""
          else if Ansi.Attr.mem Hidden c.attrs then " "
          else match Unicode.strip_format c.text with "" -> " " | s -> s)
      |> String.concat "")
  |> String.concat "\n" |> Svg_check.trim_rows |> xml_text

let rec collapse = function
  | a :: (b :: _ as rest) when a = b -> collapse rest
  | a :: rest -> a :: collapse rest
  | [] -> []

(* Whatever a program prints, the image is well-formed XML and each of its
   screens has the text the screen shows, in order. *)
let test_frames chunks =
  let output = List.mapi (fun i s -> (Vhs.Ms (i * 20), s)) chunks in
  match Vhs.svg ~settings output with
  | Error e -> failf "%a" Vhs.pp_error e
  | Ok svg -> (
      match Svg_check.frames ~cell_width:7 ~cell_height:13 ~rows svg with
      | Error e -> failf "not well-formed: %s" e
      | Ok frames ->
          let term = Screen.terminal ~rows ~cols in
          let expected =
            List.map
              (fun s ->
                Screen.feed term s;
                drawn (Screen.of_terminal term))
              chunks
          in
          let expected =
            if chunks = [] then [ String.make (rows - 1) '\n' ] else expected
          in
          check_eq
            ~pp:Fmt.(Dump.list string)
            (collapse expected) (collapse frames))

let chunk =
  choose
    [
      bytes;
      const "\027[31mred\027[0m";
      const "\xe4\xb8\xad\xe6\x96\x87";
      const "\r\n";
      const "]]><&\"";
      const "\027[2J\027[H";
      const "\027[?1049h";
      const "e\xcc\x81";
      const "abc\b\bX";
      const "\027]0;t\bitle\007";
      const "\027[2;5r";
      const "\027M\027D\027E";
      const "\027(0lqk\027(B";
      const "\027[?7l";
      const "\027[?6h\027[H";
      const "\027[3b\027[2X\027[L\027[M";
      const "\027[S\027[T";
    ]

let bases = [| "a"; " "; "\xc3\xa9"; "\xe4\xb8\xad"; "\xf0\x9f\x91\xa8" |]

let marks =
  [|
    "\xcc\x81"; "\xe2\x80\x8d"; "\xe2\x80\x8b"; "\xe2\x81\xa0"; "\xef\xb8\x8f";
  |]

(* Each character followed by a zero-width one shows both, whether they
   arrive together or apart: the first row holds them all, in order, but the
   format characters U+200B and U+2060, which the image leaves out. *)
let test_zero_width items =
  let items = List.take 20 items in
  let pieces =
    List.map
      (fun (b, m, apart) ->
        let b = bases.(b mod Array.length bases)
        and m = marks.(m mod Array.length marks) in
        if apart then [ b; m ] else [ b ^ m ])
      items
  in
  let output =
    List.mapi (fun i s -> (Vhs.Ms (i * 20), s)) (List.concat pieces)
  in
  match Vhs.svg ~settings output with
  | Error e -> failf "%a" Vhs.pp_error e
  | Ok svg -> (
      match Svg_check.frames ~cell_width:7 ~cell_height:13 ~rows svg with
      | Error e -> failf "not well-formed: %s" e
      | Ok frames ->
          let last = List.nth frames (List.length frames - 1) in
          let first = List.hd (String.split_on_char '\n' last) in
          let shown m = m <> "\xe2\x80\x8b" && m <> "\xe2\x81\xa0" in
          let expected =
            List.map
              (fun (b, m, _) ->
                let m = marks.(m mod Array.length marks) in
                bases.(b mod Array.length bases) ^ if shown m then m else "")
              (List.take 20 items)
            |> String.concat "" |> Svg_check.trim_rows
          in
          check_eq ~pp:pp_string expected first)

(* Saving and restoring a cursor also restores its active G0 character set.
   These DEC mappings were checked against native and VHS text recordings. *)
let test_saved_charset n dec =
  let glyphs =
    [| ('q', 0x2500); ('x', 0x2502); ('l', 0x250c); ('k', 0x2510) |]
  in
  let c, u = glyphs.(n mod Array.length glyphs) in
  let select dec = if dec then "\027(0" else "\027(B" in
  let output =
    select dec ^ "\0277" ^ select (not dec) ^ "\0278" ^ String.make 1 c
  in
  let b = Buffer.create 4 in
  Buffer.add_utf_8_uchar b (Uchar.of_int (if dec then u else Char.code c));
  let term = Screen.terminal ~rows ~cols in
  Screen.feed term output;
  check_eq ~pp:pp_string (Buffer.contents b)
    (Screen.line (Screen.of_terminal term) 0)

(* Without any tab stops, HT moves to the last column at every width. *)
let test_clear_tabs n =
  let cols = 10 + (n mod 70) in
  let term = Screen.terminal ~rows:2 ~cols in
  Screen.feed term "a\tX\027[3g\r\tY";
  check_eq ~pp:pp_string
    ("a" ^ String.make 7 ' ' ^ "X" ^ String.make (cols - 10) ' ' ^ "Y")
    (Screen.line (Screen.of_terminal term) 0)

(* Deleting one cell cannot shift the suffix by a whole wide character. *)
let test_delete_wide n glyph =
  let prefix = String.make (n mod 20) 'a' in
  let b = Buffer.create 4 in
  Buffer.add_utf_8_uchar b (Uchar.of_int (if glyph then 0x4e2d else 0x65e5));
  let output =
    Fmt.str "%s%sbc\r\027[%dG\027[P" prefix (Buffer.contents b)
      (String.length prefix + 1)
  in
  let term = Screen.terminal ~rows ~cols in
  Screen.feed term output;
  check_eq ~pp:pp_string (prefix ^ " bc")
    (Screen.line (Screen.of_terminal term) 0)

(* PTY reads may split anywhere, including inside UTF-8 and escape sequences.
   Delivery boundaries must not change the screen, cursor or scrollback. *)
let test_chunk_boundaries chunks =
  let output = String.concat "" chunks in
  let whole = Screen.terminal ~rows ~cols in
  let split = Screen.terminal ~rows ~cols in
  Screen.feed whole output;
  String.iter (fun c -> Screen.feed split (String.make 1 c)) output;
  let a = Screen.of_terminal whole and b = Screen.of_terminal split in
  check (Screen.equal a b);
  check_eq ~pp:Fmt.int (Screen.history whole) (Screen.history split);
  check_eq
    ~pp:Fmt.(Dump.array string)
    (Screen.buffer whole a) (Screen.buffer split b)

(* Check both the matrix.vte scenario and the public SVG output against text
   specified independently of the emulator. *)
let check_output_rows output lines =
  let expected =
    String.concat "\n"
      (lines @ List.init (rows - List.length lines) (fun _ -> ""))
  in
  let term = Screen.terminal ~rows ~cols in
  Screen.feed term output;
  check_eq ~pp:pp_string expected (Screen.text (Screen.of_terminal term));
  match Vhs.svg ~settings [ (Ms 0, output) ] with
  | Error e -> failf "%a" Vhs.pp_error e
  | Ok svg -> (
      match Svg_check.frames ~cell_width:7 ~cell_height:13 ~rows svg with
      | Error e -> failf "%s" e
      | Ok frames -> check_eq ~pp:pp_string expected (List.hd frames))

(* REP copies a cell's complete grapheme, rather than its base code point. *)
let test_repeat_combining n mark =
  let marks = [| 0x0301; 0x0308; 0x0327 |] in
  let b = Buffer.create 8 in
  Buffer.add_char b 'a';
  Buffer.add_utf_8_uchar b (Uchar.of_int marks.(mark mod Array.length marks));
  let glyph = Buffer.contents b in
  let count = 1 + (n mod 10) in
  let output = glyph ^ Fmt.str "\027[%db" count in
  let expected = String.concat "" (List.init (count + 1) (fun _ -> glyph)) in
  check_output_rows output [ expected ]

(* C1 controls, encoded as UTF-8, are equivalent to their ESC forms. *)
let test_c1_controls n osc =
  let count = 1 + (n mod 20) in
  let control, expected =
    if osc then ("\xc2\x9d" ^ String.make count 't' ^ "\xc2\x9c", "ab")
    else (Fmt.str "\xc2\x9b%dC" count, "a" ^ String.make count ' ' ^ "b")
  in
  check_output_rows ("a" ^ control ^ "b") [ expected ]

(* After a character in the last column, the cursor waits there to wrap.
   As in xterm, ED 0 and EL 1 erase that column and end the wait, so the
   next character takes it. *)
let test_margin_erasure n display =
  let c = Char.chr (Char.code 'a' + (n mod 26)) in
  let line = String.make cols c in
  let erase, kept = if display then ("\027[J", c) else ("\027[1K", ' ') in
  check_output_rows (line ^ erase ^ "X") [ String.make (cols - 1) kept ^ "X" ]

(* Modes 47 and 1047 share the cursor; they do not restore a saved one. *)
let test_alternate_cursor n mode =
  let count = 1 + (n mod 10) in
  let mode = if mode then 47 else 1047 in
  let output =
    Fmt.str "abc\027[?%dh%s\027[?%dlX" mode (String.make count 'z') mode
  in
  check_output_rows output [ "abc" ^ String.make count ' ' ^ "X" ]

(* Mode 1049, used by full-screen programs, restores the original cursor
   and main-screen text regardless of what they wrote to the other buffer. *)
let test_alternate_restore prefix_len alternate_len =
  let prefix = String.make (1 + (prefix_len mod 20)) 'a' in
  let alternate = String.make (1 + (alternate_len mod 20)) 'z' in
  check_output_rows
    (prefix ^ "\027[?1049h" ^ alternate ^ "\027[?1049lX")
    [ prefix ^ "X" ]

(* Protected cells survive selective erasure; an ordinary erase still clears
   them. Check matrix directly as well as the public SVG path. *)
let test_selective_erase n selective =
  let prefix = String.make (1 + (n mod 20)) 'a' in
  let erase = if selective then "\027[?2K" else "\027[2K" in
  let output = "\027[1\"q" ^ prefix ^ "\027[0\"qtail\r" ^ erase in
  let lines = [ (if selective then prefix else "") ] in
  let expected =
    String.concat "\n" (lines @ List.init (rows - 1) (fun _ -> ""))
  in
  let term = Vte.create ~rows ~cols () in
  Vte.feed_string term output;
  check_eq ~pp:pp_string expected (Matrix_grid.to_text (Vte.grid term));
  check_output_rows output lines

(* With reverse wrap enabled, enough backspaces cross an automatic wrap.
   Disabled, backspace stops in the first column of the current row. *)
let test_reverse_wrap n enabled =
  let count = 1 + (n mod 10) in
  let mode = if enabled then "\027[?45h" else "\027[?45l" in
  let first = String.make cols 'a' and second = String.make count 'z' in
  let output = mode ^ first ^ second ^ String.make (count + 1) '\b' ^ "X" in
  let lines =
    if enabled then [ String.make (cols - 1) 'a' ^ "X"; second ]
    else [ first; "X" ^ String.make (count - 1) 'z' ]
  in
  let expected =
    String.concat "\n" (lines @ List.init (rows - 2) (fun _ -> ""))
  in
  let term = Vte.create ~rows ~cols () in
  Vte.feed_string term output;
  check_eq ~pp:pp_string expected (Matrix_grid.to_text (Vte.grid term));
  check_output_rows output lines

(* Input grouping must not change a Unicode Prepend character's placement.
   Compare the underlying emulator and the public image independently. *)
let test_prepend_boundaries n =
  let b = Buffer.create 8 in
  Buffer.add_string b "\027[?7l";
  Buffer.add_utf_8_uchar b (Uchar.of_int 0x06dd);
  Buffer.add_char b (Char.chr (Char.code 'a' + (n mod 26)));
  let output = Buffer.contents b in
  let whole = Vte.create ~rows ~cols () and split = Vte.create ~rows ~cols () in
  Vte.feed_string whole output;
  String.iter (fun c -> Vte.feed_string split (String.make 1 c)) output;
  check_eq ~pp:pp_string
    (Matrix_grid.to_text (Vte.grid whole))
    (Matrix_grid.to_text (Vte.grid split));
  let final chunks =
    let output = List.mapi (fun i s -> (Vhs.Ms (i * 20), s)) chunks in
    match Vhs.svg ~settings output with
    | Error e -> failf "%a" Vhs.pp_error e
    | Ok image -> (
        match Svg_check.frames ~cell_width:7 ~cell_height:13 ~rows image with
        | Error e -> fail e
        | Ok frames -> List.hd (List.rev frames))
  in
  check_eq ~pp:pp_string (final [ output ])
    (final
       (List.init (String.length output) (fun i -> String.make 1 output.[i])))

(* Full-screen programs scroll their body while keeping a header and footer.
   Neither boundary row may move, even if the whole body scrolls away. *)
let test_scroll_region_boundaries n =
  let count = 1 + (n mod (rows - 2)) in
  let body = List.init (rows - 2) (fun i -> Fmt.str "row-%d" i) in
  let initial = String.concat "\r\n" ([ "header" ] @ body @ [ "footer" ]) in
  let output =
    initial
    ^ Fmt.str "\027[2;%dr\027[%d;1H" (rows - 1) (rows - 1)
    ^ String.make count '\n'
  in
  let lines =
    [ "header" ] @ List.drop count body
    @ List.init count (fun _ -> "")
    @ [ "footer" ]
  in
  let expected = String.concat "\n" lines in
  let term = Vte.create ~rows ~cols () in
  Vte.feed_string term output;
  check_eq ~pp:pp_string expected (Matrix_grid.to_text (Vte.grid term));
  check_output_rows output lines

(* [svg] refuses as [Invalid] the settings [tape] refuses, and only
   those. *)
let test_setting_errors setting =
  let tape =
    Vhs.tape ~settings:[ setting ] ~outputs:[ "x.svg" ] [ Vhs.type' "hi" ]
  in
  match (tape, Vhs.svg ~settings:[ setting ] [ (Ms 0, "hello") ]) with
  | Error (Invalid _), Error (Invalid _) -> ()
  | Ok _, (Ok _ | Error (Unsupported _)) -> ()
  | Error e, _ -> failf "tape: %a, but svg did not refuse it" Vhs.pp_error e
  | Ok _, Error e -> failf "svg: %a" Vhs.pp_error e

let setting =
  let text =
    choose [ bytes; const "Mono\nSpace"; const "\"'`"; const "{\n}" ]
  in
  choose
    [
      map [ text ] (fun s -> Vhs.Font_family s);
      map [ text ] (fun s -> Vhs.Theme s);
      map [ int ] (fun n -> Vhs.Width n);
      map [ int ] (fun n -> Vhs.Height n);
      map [ int ] (fun n -> Vhs.Padding n);
      map [ int ] (fun n -> Vhs.Font_size n);
      map [ int ] (fun n -> Vhs.Framerate n);
      map [ int ] (fun n -> Vhs.Typing_speed (Ms n));
      const (Vhs.Shell Cmd);
    ]

(* Several settings can interact, and repeated settings must be checked in
   the same order by both public entry points. Native size and colour limits
   are documented separately from tape validation. *)
let test_setting_list_errors settings =
  let tape = Vhs.tape ~settings ~outputs:[ "x.svg" ] [ Vhs.type' "hi" ] in
  match (tape, Vhs.svg ~settings [ (Ms 0, "hello") ]) with
  | Error (Invalid expected), Error (Invalid actual) ->
      check_eq ~pp:pp_string expected actual
  | Ok _, (Ok _ | Error (Unsupported _)) -> ()
  | Error e, _ -> failf "tape: %a, but svg did not agree" Vhs.pp_error e
  | Ok _, Error e -> failf "svg: %a" Vhs.pp_error e

(* A frame rate is not a pixel dimension and has no documented native
   upper bound. Keep ordinary rates and the size-check boundary in scope. *)
let test_positive_framerate n =
  let settings = Vhs.Framerate (max 1 n) :: settings in
  (match Vhs.tape ~settings ~outputs:[ "x.svg" ] [ Vhs.sleep (Ms 100) ] with
  | Ok _ -> ()
  | Error e -> failf "tape: %a" Vhs.pp_error e);
  match Vhs.svg ~settings [ (Ms 0, "hello") ] with
  | Ok _ -> ()
  | Error e -> failf "svg: %a" Vhs.pp_error e

let contains ~sub s =
  let rec go i =
    i + String.length sub <= String.length s
    && (String.sub s i (String.length sub) = sub || go (i + 1))
  in
  go 0

(* SGR conceal is retained by matrix and respected by every SVG text path. *)
let concealed n =
  let secret = String.make (1 + (n mod 20)) 's' in
  let column = n mod String.length secret in
  let output = Fmt.str "\027[8m%s\027[%dG" secret (column + 1) in
  let term = Vte.create ~rows ~cols () in
  Vte.feed_string term output;
  check (Ansi.Attr.mem Hidden (Matrix_grid.get_style (Vte.grid term) 0).attrs);
  check_eq ~pp:pp_string "s" (Matrix_grid.get_text (Vte.grid term) 0);
  (secret, output)

let test_concealed_cursor n =
  let _, output = concealed n in
  match Vhs.svg ~settings [ (Ms 0, output) ] with
  | Error e -> failf "%a" Vhs.pp_error e
  | Ok image -> check (not (contains ~sub:{|<text class="ka k"|} image))

let test_concealed_description n =
  let _, output = concealed n in
  match Vhs.svg ~settings [ (Ms 0, output ^ "\027[?25l") ] with
  | Error e -> failf "%a" Vhs.pp_error e
  | Ok image -> check (not (contains ~sub:"<desc" image))

(* The XML guarantee also holds when themes change the generated style. *)
let test_theme_xml output theme =
  let settings = Vhs.Theme theme :: settings in
  match Vhs.tape ~settings ~outputs:[ "a.svg" ] [ Vhs.sleep (Ms 100) ] with
  | Error _ -> ()
  | Ok _ -> (
      match Vhs.svg ~settings [ (Ms 0, output) ] with
      | Error (Unsupported _) -> ()
      | Error e -> failf "%a" Vhs.pp_error e
      | Ok image -> (
          match Svg_check.frames ~cell_width:7 ~cell_height:13 ~rows image with
          | Ok _ -> ()
          | Error e -> failf "not well-formed XML: %s" e))

(* Timestamps are ordered durations, including equal adjacent times. *)
let test_time_bounds first second =
  let max_ms = 9_223_372_036_854 in
  let valid = first >= 0 && first <= second && second <= max_ms in
  match Vhs.svg ~settings [ (Ms first, "a"); (Ms second, "b") ] with
  | Ok _ -> check valid
  | Error (Invalid _) -> check (not valid)
  | Error e -> failf "%a" Vhs.pp_error e

let theme =
  choose
    [
      bytes;
      const "Dracula";
      const "TokyoNight";
      const "{}";
      const {|{"foreground":"#abcd","background":"#1230","cursor":"#ff000080"}|};
      const {|{"red":"rgb(1,2,3)","cursorAccent":"rgba(0,0,0,0.5)"}|};
      const {|{"name":"<>&\u0000","red":null,"extra":[1,{}]}|};
    ]

(* xterm's Unicode 11 tables give these text-presentation symbols one cell,
   and either variation selector zero. Fragmenting PTY output changes neither
   their width nor the row on which a following character lands. *)
let test_narrow_symbols n selector split_at =
  let symbols = [| 0x2600; 0x263a; 0x2665; 0x2708; 0x2764 |] in
  let b = Buffer.create 8 in
  Buffer.add_utf_8_uchar b (Uchar.of_int symbols.(n mod Array.length symbols));
  (match selector mod 3 with
  | 1 -> Buffer.add_utf_8_uchar b (Uchar.of_int 0xfe0e)
  | 2 -> Buffer.add_utf_8_uchar b (Uchar.of_int 0xfe0f)
  | _ -> ());
  let glyph = Buffer.contents b in
  let prefix_len = cols - 1 - (n mod 3) in
  let prefix = String.make prefix_len 'a' in
  let output = prefix ^ glyph ^ "X" in
  let cut = split_at mod (String.length output + 1) in
  let chunks =
    [
      String.sub output 0 cut; String.sub output cut (String.length output - cut);
    ]
  in
  let lines, cursor =
    if prefix_len = cols - 1 then ([ prefix ^ glyph; "X" ], (1, 1))
    else ([ output ], (0, prefix_len + 2))
  in
  let expected =
    String.concat "\n"
      (lines @ List.init (rows - List.length lines) (fun _ -> ""))
  in
  let term = Vte.create ~width_method:`Wcwidth ~rows ~cols () in
  List.iter (Vte.feed_string term) chunks;
  let image =
    match
      Vhs.svg ~settings (List.mapi (fun i s -> (Vhs.Ms (20 * i), s)) chunks)
    with
    | Error e -> failf "%a" Vhs.pp_error e
    | Ok svg -> (
        match Svg_check.frames ~cell_width:7 ~cell_height:13 ~rows svg with
        | Error e -> failf "%s" e
        | Ok frames -> List.nth frames (List.length frames - 1))
  in
  check_eq
    ~pp:Fmt.(Dump.pair string string)
    (expected, expected)
    (Matrix_grid.to_text (Vte.grid term), image);
  check_eq ~pp:Fmt.(Dump.pair int int) cursor (Vte.cursor_pos term)

(* A cursor redraw cannot reintroduce a format character removed from
   the rows and description. Check that matrix retains the exact input cell,
   so a dropped terminal character cannot make the property vacuous. *)
let test_cursor_format n =
  let forbidden =
    [| 0x061c; 0x180e; 0x200b; 0x200e; 0x200f; 0x202a; 0x202e;
       0x2060; 0x2066; 0xfeff; 0xfff9; 0x13430; 0x1bca0; 0xe0001 |]
  in
  let b = Buffer.create 8 in
  Buffer.add_utf_8_uchar b (Uchar.of_int forbidden.(n mod Array.length forbidden));
  let mark = Buffer.contents b in
  let output = "a" ^ mark ^ "b\r" in
  let term = Vte.create ~width_method:`Wcwidth ~rows ~cols () in
  Vte.feed_string term output;
  check_eq ~pp:pp_string ("a" ^ mark) (Matrix_grid.get_text (Vte.grid term) 0);
  check_eq ~pp:Fmt.(Dump.pair int int) (0, 0) (Vte.cursor_pos term);
  match Vhs.svg ~settings [ (Ms 0, output) ] with
  | Error e -> failf "%a" Vhs.pp_error e
  | Ok image ->
      check (contains ~sub:"<desc>ab</desc>" image);
      check (not (contains ~sub:mark image))

(* The documented Cf exceptions shape visible text, so every SVG text
   path and every human-readable error must retain them. *)
let test_visible_format n =
  let allowed =
    [| 0x200c; 0x200d; 0x0600; 0x0601; 0x0602; 0x0603; 0x0604; 0x0605;
       0x06dd; 0x070f; 0x0890; 0x0891; 0x08e2; 0x110bd; 0x110cd |]
  in
  let b = Buffer.create 8 in
  Buffer.add_utf_8_uchar b (Uchar.of_int allowed.(n mod Array.length allowed));
  let mark = Buffer.contents b in
  check_eq ~pp:pp_string ("a" ^ mark ^ "b")
    (Fmt.str "%a" Vhs.pp_error (Io ("a" ^ mark ^ "b")));
  match Vhs.svg ~settings [ (Ms 0, "a" ^ mark ^ "b\r") ] with
  | Error e -> failf "%a" Vhs.pp_error e
  | Ok image ->
      check (contains ~sub:("<desc>a" ^ mark ^ "b</desc>") image);
      check (contains ~sub:(">a" ^ mark ^ "</tspan>") image);
      check
        (contains
           ~sub:("<text class=\"ka k\" x=\"0\" y=\"10\">a" ^ mark ^ "</text>")
           image)

(* xterm's wcwidth gives regional indicators one cell each, including
   when a pair straddles the right margin. Every byte split must preserve
   their cells and the final public image. *)
let test_regional_splits n =
  let glyph u =
    let b = Buffer.create 4 in
    Buffer.add_utf_8_uchar b (Uchar.of_int u);
    Buffer.contents b
  in
  let prefix = List.init (cols - 1 - (n mod 3)) (fun _ -> "a") in
  let flags = List.init (2 + (n mod 3)) (fun i -> glyph (0x1f1e6 + ((n + i) mod 26))) in
  let cells = prefix @ flags @ [ "X" ] in
  let output = String.concat "" cells in
  let rec lines = function
    | [] -> []
    | cells -> String.concat "" (List.take cols cells) :: lines (List.drop cols cells)
  in
  let lines = lines cells in
  let expected =
    String.concat "\n" (lines @ List.init (rows - List.length lines) (fun _ -> ""))
  in
  for cut = 0 to String.length output do
    let a = String.sub output 0 cut in
    let b = String.sub output cut (String.length output - cut) in
    let term = Vte.create ~width_method:`Wcwidth ~rows ~cols () in
    Vte.feed_string term a;
    Vte.feed_string term b;
    check_eq ~pp:pp_string expected (Matrix_grid.to_text (Vte.grid term));
    match Vhs.svg ~settings [ (Ms 0, a); (Ms 20, b) ] with
    | Error e -> failf "%a" Vhs.pp_error e
    | Ok image -> (
        match Svg_check.frames ~cell_width:7 ~cell_height:13 ~rows image with
        | Error e -> failf "%s" e
        | Ok frames -> check_eq ~pp:pp_string expected (List.hd (List.rev frames)))
  done

(* The emulator's terminal state across reads, through the SVG image. *)
let terminal =
  [
    test_case "scroll regions preserve header and footer" [ uint8 ]
      test_scroll_region_boundaries;
    test_case "Unicode Prepend survives delivery boundaries" [ uint8 ]
      test_prepend_boundaries;
    test_case "reverse wrap crosses automatic line breaks" [ uint8; bool ]
      test_reverse_wrap;
    test_case "selective erasure preserves protected cells" [ uint8; bool ]
      test_selective_erase;
    test_case "1049 restores the main screen and cursor" [ uint8; uint8 ]
      test_alternate_restore;
    test_case "47 and 1047 share the alternate-screen cursor" [ uint8; bool ]
      test_alternate_cursor;
    test_case "erasure ends a pending right-margin wrap" [ uint8; bool ]
      test_margin_erasure;
    test_case "UTF-8 C1 controls act as terminal commands" [ uint8; bool ]
      test_c1_controls;
    test_case "REP preserves combining marks" [ uint8; uint8 ]
      test_repeat_combining;
    test_case "PTY chunk boundaries preserve terminal state"
      [ list chunk ]
      test_chunk_boundaries;
    test_case "wide glyph deletion preserves columns" [ uint8; bool ]
      test_delete_wide;
    test_case "cleared tabs reach the right margin" [ uint8 ] test_clear_tabs;
    test_case "saved character sets survive restore" [ uint8; bool ]
      test_saved_charset;
    test_case "zero width"
      [ list (map [ uint8; uint8; bool ] (fun b m a -> (b, m, a))) ]
      test_zero_width;
  ]

let suite =
  ( "svg",
    [
      test_case "frames" [ list chunk ] test_frames;
      test_case "regional indicators retain cells at every PTY split"
        [ uint8 ] test_regional_splits;
      test_case "format exceptions survive errors and all SVG text paths"
        [ uint8 ] test_visible_format;
      test_case "cursor redraw leaves invisible format characters out"
        [ uint8 ] test_cursor_format;
      test_case "text symbols keep xterm cell widths across PTY splits"
        [ uint8; uint8; uint8 ] test_narrow_symbols;
      test_case "positive SVG frame rates are not pixel sizes"
        [
          choose
            [ uint16; map [ uint16 ] (fun n -> 100_001 + n); const max_int ];
        ]
        test_positive_framerate;
      test_case "SVG and tape return identical setting-list errors"
        [ list setting ]
        test_setting_list_errors;
      test_case "SVG times obey duration bounds and ordering"
        [
          choose [ int; const (-1); const 0; const 9_223_372_036_854 ];
          choose
            [ int; const 0; const 9_223_372_036_854; const 9_223_372_036_855 ];
        ]
        test_time_bounds;
      test_case "arbitrary output and themes produce XML" [ bytes; theme ]
        test_theme_xml;
      test_case "text alternatives respect SGR conceal" [ uint8 ]
        test_concealed_description;
      test_case "cursor respects SGR conceal" [ uint8 ] test_concealed_cursor;
      test_case "svg refuses the settings tape refuses" [ setting ]
        test_setting_errors;
    ]
    @ terminal )
