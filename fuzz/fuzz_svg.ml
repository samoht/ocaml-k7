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
   is blank, and trailing spaces do not show. *)
let drawn screen =
  Array.to_list (Screen.rows screen)
  |> List.map (fun r ->
      Array.to_list (Screen.cells r)
      |> List.map (fun (c : Screen.cell) ->
          if c.width = 0 then ""
          else if Ansi.Attr.mem Hidden c.attrs then " "
          else c.text)
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
   arrive together or apart: the first row holds them all, in order. *)
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
          let expected =
            Svg_check.trim_rows (String.concat "" (List.concat pieces))
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

(* The right-margin cursor is logically past the last cell until it wraps.
   ED 0 keeps that cell, and neither ED 0 nor EL 1 cancels the pending wrap. *)
let test_margin_erasure n display =
  let c = Char.chr (Char.code 'a' + (n mod 26)) in
  let line = String.make cols c in
  let erase, first = if display then ("\027[J", line) else ("\027[1K", "") in
  check_output_rows (line ^ erase ^ "X") [ first; "X" ]

(* Modes 47 and 1047 share the cursor; they do not restore a saved one. *)
let test_alternate_cursor n mode =
  let count = 1 + (n mod 10) in
  let mode = if mode then 47 else 1047 in
  let output =
    Fmt.str "abc\027[?%dh%s\027[?%dlX" mode (String.make count 'z') mode
  in
  check_output_rows output [ "abc" ^ String.make count ' ' ^ "X" ]

let suite =
  ( "svg",
    [
      test_case "frames" [ list chunk ] test_frames;
      test_case "47 and 1047 share the alternate-screen cursor" [ uint8; bool ]
        test_alternate_cursor;
      test_case "erasure preserves pending right-margin wrap" [ uint8; bool ]
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
    ] )
