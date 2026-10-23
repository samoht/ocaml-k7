(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Alcobar

let settings = K7.[ Width 400; Height 200; Font_size 10; Padding 10 ]
let cols = 50
let rows = 13

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

let test_frames chunks =
  let output = List.mapi (fun i s -> (K7.Ms (i * 20), s)) chunks in
  match K7.svg ~settings output with
  | Error e -> failf "%a" K7.pp_error e
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
    List.mapi (fun i s -> (K7.Ms (i * 20), s)) (List.concat pieces)
  in
  match K7.svg ~settings output with
  | Error e -> failf "%a" K7.pp_error e
  | Ok svg -> (
      match Svg_check.frames ~cell_width:7 ~cell_height:13 ~rows svg with
      | Error e -> failf "not well-formed: %s" e
      | Ok frames ->
          let last = List.nth frames (List.length frames - 1) in
          let first = List.hd (String.split_on_char '\n' last) in
          let shown b m =
            m <> "\xe2\x80\x8b" && m <> "\xe2\x81\xa0"
            && not (b = " " && m = "\xef\xb8\x8f")
          in
          let expected =
            List.map
              (fun (b, m, _) ->
                let b = bases.(b mod Array.length bases)
                and m = marks.(m mod Array.length marks) in
                b ^ if shown b m then m else "")
              (List.take 20 items)
            |> String.concat "" |> Svg_check.trim_rows
          in
          check_eq ~pp:pp_string expected first)

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

let test_clear_tabs n =
  let cols = 10 + (n mod 70) in
  let term = Screen.terminal ~rows:2 ~cols in
  Screen.feed term "a\tX\027[3g\r\tY";
  check_eq ~pp:pp_string
    ("a" ^ String.make 7 ' ' ^ "X" ^ String.make (cols - 10) ' ' ^ "Y")
    (Screen.line (Screen.of_terminal term) 0)

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

let check_output_rows output lines =
  let expected =
    String.concat "\n"
      (lines @ List.init (rows - List.length lines) (fun _ -> ""))
  in
  let term = Screen.terminal ~rows ~cols in
  Screen.feed term output;
  check_eq ~pp:pp_string expected (Screen.text (Screen.of_terminal term));
  match K7.svg ~settings [ (Ms 0, output) ] with
  | Error e -> failf "%a" K7.pp_error e
  | Ok svg -> (
      match Svg_check.frames ~cell_width:7 ~cell_height:13 ~rows svg with
      | Error e -> failf "%s" e
      | Ok frames -> check_eq ~pp:pp_string expected (List.hd frames))

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

let test_c1_controls n osc =
  let count = 1 + (n mod 20) in
  let control, expected =
    if osc then ("\xc2\x9d" ^ String.make count 't' ^ "\xc2\x9c", "ab")
    else (Fmt.str "\xc2\x9b%dC" count, "a" ^ String.make count ' ' ^ "b")
  in
  check_output_rows ("a" ^ control ^ "b") [ expected ]

(* As in xterm, ED 0 and EL 1 end the pending wrap of the last column. *)
let test_margin_erasure n display =
  let c = Char.chr (Char.code 'a' + (n mod 26)) in
  let line = String.make cols c in
  let erase, kept = if display then ("\027[J", c) else ("\027[1K", ' ') in
  check_output_rows (line ^ erase ^ "X") [ String.make (cols - 1) kept ^ "X" ]

let test_alternate_cursor n mode =
  let count = 1 + (n mod 10) in
  let mode = if mode then 47 else 1047 in
  let output =
    Fmt.str "abc\027[?%dh%s\027[?%dlX" mode (String.make count 'z') mode
  in
  check_output_rows output [ "abc" ^ String.make count ' ' ^ "X" ]

let test_alternate_restore prefix_len alternate_len =
  let prefix = String.make (1 + (prefix_len mod 20)) 'a' in
  let alternate = String.make (1 + (alternate_len mod 20)) 'z' in
  check_output_rows
    (prefix ^ "\027[?1049h" ^ alternate ^ "\027[?1049lX")
    [ prefix ^ "X" ]

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
    let output = List.mapi (fun i s -> (K7.Ms (i * 20), s)) chunks in
    match K7.svg ~settings output with
    | Error e -> failf "%a" K7.pp_error e
    | Ok image -> (
        match Svg_check.frames ~cell_width:7 ~cell_height:13 ~rows image with
        | Error e -> fail e
        | Ok frames -> List.hd (List.rev frames))
  in
  check_eq ~pp:pp_string (final [ output ])
    (final
       (List.init (String.length output) (fun i -> String.make 1 output.[i])))

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

let test_setting_errors setting =
  let tape =
    K7.tape ~settings:[ setting ] ~outputs:[ "x.svg" ] [ K7.type' "hi" ]
  in
  match (tape, K7.svg ~settings:[ setting ] [ (Ms 0, "hello") ]) with
  | Error (Invalid _), Error (Invalid _) -> ()
  | Ok _, (Ok _ | Error (Unsupported _)) -> ()
  | Error e, _ -> failf "tape: %a, but svg did not refuse it" K7.pp_error e
  | Ok _, Error e -> failf "svg: %a" K7.pp_error e

let setting =
  let text =
    choose [ bytes; const "Mono\nSpace"; const "\"'`"; const "{\n}" ]
  in
  choose
    [
      map [ text ] (fun s -> K7.Font_family s);
      map [ text ] (fun s -> K7.Theme s);
      map [ int ] (fun n -> K7.Width n);
      map [ int ] (fun n -> K7.Height n);
      map [ int ] (fun n -> K7.Padding n);
      map [ int ] (fun n -> K7.Font_size n);
      map [ int ] (fun n -> K7.Framerate n);
      map [ int ] (fun n -> K7.Typing_speed (Ms n));
      const (K7.Shell Cmd);
    ]

let test_setting_list_errors settings =
  let tape = K7.tape ~settings ~outputs:[ "x.svg" ] [ K7.type' "hi" ] in
  match (tape, K7.svg ~settings [ (Ms 0, "hello") ]) with
  | Error (Invalid expected), Error (Invalid actual) ->
      check_eq ~pp:pp_string expected actual
  | Ok _, (Ok _ | Error (Unsupported _)) -> ()
  | Error e, _ -> failf "tape: %a, but svg did not agree" K7.pp_error e
  | Ok _, Error e -> failf "svg: %a" K7.pp_error e

let test_positive_framerate n =
  let settings = K7.Framerate (max 1 n) :: settings in
  (match K7.tape ~settings ~outputs:[ "x.svg" ] [ K7.sleep (Ms 100) ] with
  | Ok _ -> ()
  | Error e -> failf "tape: %a" K7.pp_error e);
  match K7.svg ~settings [ (Ms 0, "hello") ] with
  | Ok _ -> ()
  | Error e -> failf "svg: %a" K7.pp_error e

let contains ~sub s =
  let rec go i =
    i + String.length sub <= String.length s
    && (String.sub s i (String.length sub) = sub || go (i + 1))
  in
  go 0

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
  match K7.svg ~settings [ (Ms 0, output) ] with
  | Error e -> failf "%a" K7.pp_error e
  | Ok image -> check (not (contains ~sub:{|<text class=|} image))

let test_concealed_description n =
  let _, output = concealed n in
  match K7.svg ~settings [ (Ms 0, output ^ "\027[?25l") ] with
  | Error e -> failf "%a" K7.pp_error e
  | Ok image -> check (not (contains ~sub:"<desc" image))

let test_theme_xml output theme =
  let settings = K7.Theme theme :: settings in
  match K7.tape ~settings ~outputs:[ "a.svg" ] [ K7.sleep (Ms 100) ] with
  | Error _ -> ()
  | Ok _ -> (
      match K7.svg ~settings [ (Ms 0, output) ] with
      | Error (Unsupported _) -> ()
      | Error e -> failf "%a" K7.pp_error e
      | Ok image -> (
          match Svg_check.frames ~cell_width:7 ~cell_height:13 ~rows image with
          | Ok _ -> ()
          | Error e -> failf "not well-formed XML: %s" e))

let test_time_bounds first second =
  let max_ms = 9_223_372_036_854 in
  let valid = first >= 0 && first <= second && second <= max_ms in
  match K7.svg ~settings [ (Ms first, "a"); (Ms second, "b") ] with
  | Ok _ -> check valid
  | Error (Invalid _) -> check (not valid)
  | Error e -> failf "%a" K7.pp_error e

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

(* xterm's Unicode 11 tables: one cell, and none for a variation selector. *)
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
      K7.svg ~settings (List.mapi (fun i s -> (K7.Ms (20 * i), s)) chunks)
    with
    | Error e -> failf "%a" K7.pp_error e
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

(* Checks matrix keeps the input cell, so that the property is not vacuous. *)
let test_cursor_format n =
  let forbidden =
    [|
      0x061c;
      0x180e;
      0x200b;
      0x200e;
      0x200f;
      0x202a;
      0x202e;
      0x2060;
      0x2066;
      0xfeff;
      0xfff9;
      0x13430;
      0x1bca0;
      0xe0001;
    |]
  in
  let b = Buffer.create 8 in
  Buffer.add_utf_8_uchar b
    (Uchar.of_int forbidden.(n mod Array.length forbidden));
  let mark = Buffer.contents b in
  let output = "a" ^ mark ^ "b\r" in
  let term = Vte.create ~width_method:`Wcwidth ~rows ~cols () in
  Vte.feed_string term output;
  check_eq ~pp:pp_string ("a" ^ mark) (Matrix_grid.get_text (Vte.grid term) 0);
  check_eq ~pp:Fmt.(Dump.pair int int) (0, 0) (Vte.cursor_pos term);
  match K7.svg ~settings [ (Ms 0, output) ] with
  | Error e -> failf "%a" K7.pp_error e
  | Ok image ->
      check (contains ~sub:"d\">ab</desc>" image);
      check (not (contains ~sub:mark image))

let test_visible_format n =
  let allowed =
    [|
      0x200c;
      0x200d;
      0x0600;
      0x0601;
      0x0602;
      0x0603;
      0x0604;
      0x0605;
      0x06dd;
      0x070f;
      0x0890;
      0x0891;
      0x08e2;
      0x110bd;
      0x110cd;
    |]
  in
  let b = Buffer.create 8 in
  Buffer.add_utf_8_uchar b (Uchar.of_int allowed.(n mod Array.length allowed));
  let mark = Buffer.contents b in
  check_eq ~pp:pp_string
    ("a" ^ mark ^ "b")
    (Fmt.str "%a" K7.pp_error (Io ("a" ^ mark ^ "b")));
  match K7.svg ~settings [ (Ms 0, "a" ^ mark ^ "b\r") ] with
  | Error e -> failf "%a" K7.pp_error e
  | Ok image ->
      check (contains ~sub:("d\">a" ^ mark ^ "b</desc>") image);
      check (contains ~sub:(">a" ^ mark ^ "</tspan>") image);
      check
        (contains
           ~sub:
             ("<text class=\"k\" x=\"0\" y=\"10\" fill=\"#171717\">a" ^ mark
            ^ "</text>")
           (Native_test.unprefixed image))

(* xterm's wcwidth gives each regional indicator one cell. *)
let test_regional_splits n =
  let glyph u =
    let b = Buffer.create 4 in
    Buffer.add_utf_8_uchar b (Uchar.of_int u);
    Buffer.contents b
  in
  let prefix = List.init (cols - 1 - (n mod 3)) (fun _ -> "a") in
  let flags =
    List.init (2 + (n mod 3)) (fun i -> glyph (0x1f1e6 + ((n + i) mod 26)))
  in
  let cells = prefix @ flags @ [ "X" ] in
  let output = String.concat "" cells in
  let rec lines = function
    | [] -> []
    | cells ->
        String.concat "" (List.take cols cells) :: lines (List.drop cols cells)
  in
  let lines = lines cells in
  let expected =
    String.concat "\n"
      (lines @ List.init (rows - List.length lines) (fun _ -> ""))
  in
  for cut = 0 to String.length output do
    let a = String.sub output 0 cut in
    let b = String.sub output cut (String.length output - cut) in
    let term = Vte.create ~width_method:`Wcwidth ~rows ~cols () in
    Vte.feed_string term a;
    Vte.feed_string term b;
    check_eq ~pp:pp_string expected (Matrix_grid.to_text (Vte.grid term));
    match K7.svg ~settings [ (Ms 0, a); (Ms 20, b) ] with
    | Error e -> failf "%a" K7.pp_error e
    | Ok image -> (
        match Svg_check.frames ~cell_width:7 ~cell_height:13 ~rows image with
        | Error e -> failf "%s" e
        | Ok frames ->
            check_eq ~pp:pp_string expected (List.hd (List.rev frames)))
  done

let json_string s =
  match Jsont_bytesrw.encode_string Jsont.string s with
  | Ok j -> j
  | Error e -> failf "%s" e

(* As console writes a time, rounded to the microsecond, or with all six
   digits. *)
let seconds ~short us =
  if short then Fmt.str "%.15g" (Float.of_int us /. 1e6)
  else Fmt.str "%d.%06d" (us / 1_000_000) (us mod 1_000_000)

let codes = [| "o"; "o"; "i"; "m"; "r" |]

let test_cast columns rows events =
  let columns = columns + 1 and rows = rows + 1 in
  let _, lines, output =
    List.fold_left
      (fun (us, lines, output) (gap, code, short, data) ->
        let us = us + gap in
        let code = codes.(code) in
        let data =
          if code = "r" then Fmt.str "%dx%d" columns rows
          else if String.is_valid_utf_8 data then data
          else String.escaped data
        in
        let line =
          Fmt.str "[%s, %S, %s]" (seconds ~short us) code (json_string data)
        in
        let output =
          if code = "o" then (K7.Ms ((us + 500) / 1000), data) :: output
          else output
        in
        (us, line :: lines, output))
      (0, [], []) events
  in
  let cast =
    String.concat "\n"
      (Fmt.str {|{"version": 2, "width": %d, "height": %d, "title": "t"}|}
         columns rows
      :: List.rev lines)
  in
  let last = match output with (Ms ms, _) :: _ -> ms | [] -> 0 in
  let output = List.rev ((K7.Ms (last + 1000), "") :: output) in
  let settings = K7.[ Font_size 10 ] in
  let draw src =
    Result.bind (K7.cast_of_string src) (fun c ->
        K7.svg_of_casts ~settings [ c ])
  in
  match
    ( draw cast,
      draw cast,
      K7.svg ~settings:(K7.Columns columns :: Rows rows :: settings) output )
  with
  | Ok a, Ok b, Ok expected ->
      check_eq ~pp:pp_string a b;
      check_eq ~pp:pp_string expected a
  | Error e, _, _ | _, Error e, _ | _, _, Error e -> failf "%a" K7.pp_error e

let test_cast_total src =
  match
    Result.bind (K7.cast_of_string src) (fun c -> K7.svg_of_casts [ c ])
  with
  | Ok _ | Error (Invalid _ | Unsupported _) -> ()
  | Error e -> failf "%a" K7.pp_error e

let test_cast_text_conceal n glyph =
  let glyph, width =
    List.nth [ ("x", 1); ("é", 1); ("中", 2); ("👍", 2) ] (glyph mod 4)
  in
  let count = 1 + (n mod 16) in
  let hidden = String.concat "" (List.init count (fun _ -> glyph)) in
  let src =
    {|{"version":3,"term":{"cols":80,"rows":2}}|} ^ "\n[0,\"o\","
    ^ json_string ("a\027[8m" ^ hidden ^ "\027[0mz")
    ^ "]\n"
  in
  let actual =
    Result.bind (K7.cast_of_string src) (fun c -> K7.text_of_casts [ c ])
  in
  match actual with
  | Error e -> failf "%a" K7.pp_error e
  | Ok text ->
      check_eq ~pp:pp_string
        ("a" ^ String.make (count * width) ' ' ^ "z\n")
        text

(* Absolute v2 times and relative v3 intervals describe the same stream.
   The exit anchors the last event even when all preceding events are ignored. *)
let test_cast_versions events =
  let events = List.take 32 events in
  let total, v2, v3, output =
    List.fold_left
      (fun (us, v2, v3, output) (gap, ignored, data) ->
        let gap = gap * 37 in
        let us = us + gap in
        let data =
          if String.is_valid_utf_8 data then data else String.escaped data
        in
        let code = if ignored then "i" else "o" in
        let event time =
          Fmt.str "[%s,\"%s\",%s]"
            (seconds ~short:false time)
            code (json_string data)
        in
        let output =
          if ignored then output
          else (K7.Ms ((us + 500) / 1000), data) :: output
        in
        (us, event us :: v2, event gap :: v3, output))
      (0, [], [], []) events
  in
  let source header lines exit eol =
    String.concat eol ((header :: List.rev lines) @ [ exit; "" ])
  in
  let a =
    source {|{"version":2,"width":20,"height":3}|} v2
      (Fmt.str "[%s,\"x\",\"7\"]" (seconds ~short:false (total + 13000)))
      "\n"
  and b =
    source {|{"version":3,"term":{"cols":20,"rows":3}}|} v3 {|[0.013,"x","7"]|}
      "\r\n"
  in
  let settings = K7.[ Columns 20; Rows 3; Font_size 10 ] in
  let image c =
    match K7.svg_of_casts ~settings [ c ] with
    | Ok s -> s
    | Error e -> failf "%a" K7.pp_error e
  in
  let parse src =
    match K7.cast_of_string src with
    | Ok c -> c
    | Error e -> failf "%a" K7.pp_error e
  in
  let a = parse a and b = parse b in
  let output =
    List.rev output @ [ (K7.Ms (((total + 13000 + 500) / 1000) + 1000), "") ]
  in
  let expected =
    match K7.svg ~settings output with
    | Ok s -> s
    | Error e -> failf "%a" K7.pp_error e
  in
  List.iter
    (fun c ->
      check_eq ~pp:pp_string expected (image c);
      check_eq ~pp:pp_string expected (image (parse (K7.string_of_cast c))))
    [ a; b ]

let test_cast_palette_resize_roundtrip n =
  let colours = if n mod 2 = 0 then 8 else 16 in
  let palette =
    List.init colours (fun i -> Fmt.str "#%06x" (((n + 1) * 257) + (i * 4096)))
    |> String.concat ":"
  in
  let source =
    Fmt.str
      {|{"version":3,"term":{"cols":8,"rows":3,"theme":{"fg":"#abcdef","bg":"#123456","palette":"%s"}},"command":"echo café"}
[0.0005,"o","\u001b[31mé中\u001b[0m"]
[0.0015,"r","12x4"]
[0.001,"o","\r\né👍"]
[0.002,"x","7"]
|}
      palette
  in
  let parse source =
    match K7.cast_of_string source with
    | Ok c -> c
    | Error e -> failf "%a" K7.pp_error e
  in
  let original = parse source in
  let serialized = K7.string_of_cast original in
  let restored = parse serialized in
  check_eq ~pp:Fmt.Dump.string serialized (K7.string_of_cast restored);
  let get = function Ok s -> s | Error e -> failf "%a" K7.pp_error e in
  check_eq ~pp:Fmt.Dump.string
    (get (K7.svg_of_casts [ original ]))
    (get (K7.svg_of_casts [ restored ]));
  check_eq ~pp:Fmt.Dump.string
    (get (K7.text_of_casts [ original ]))
    (get (K7.text_of_casts [ restored ]))

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

let casts =
  [
    test_case "cast serialization preserves palettes, resizes and Unicode"
      [ uint8 ] test_cast_palette_resize_roundtrip;
    test_case "v2 and v3 timings survive serialization and CRLF"
      [
        list
          (map [ uint8; bool; chunk ] (fun gap ignored data ->
               (gap, ignored, data)));
      ]
      test_cast_versions;
    test_case "asciicast text exports conceal hidden cells" [ uint8; uint8 ]
      test_cast_text_conceal;
    test_case "a cast draws its output events at their milliseconds"
      [
        range 80;
        range 24;
        list
          (map
             [ range 2_000_000; range 5; bool; chunk ]
             (fun gap code short data -> (gap, code, short, data)));
      ]
      test_cast;
    test_case "svg_of_casts is total"
      [
        choose
          [
            bytes;
            map [ bytes ] (fun s ->
                {|{"version": 2, "width": 8, "height": 2}|} ^ "\n" ^ s);
            map [ bytes ] (fun s ->
                {|{"version": 2, "width": 8, "height": 2}|} ^ "\n[1, \"o\", "
                ^ s);
          ];
      ]
      test_cast_total;
  ]

let suite =
  ( "svg",
    [
      test_case "frames" [ list chunk ] test_frames;
      test_case "regional indicators retain cells at every PTY split" [ uint8 ]
        test_regional_splits;
      test_case "format exceptions survive errors and all SVG text paths"
        [ uint8 ] test_visible_format;
      test_case "cursor redraw leaves invisible format characters out" [ uint8 ]
        test_cursor_format;
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
    @ terminal @ casts )
