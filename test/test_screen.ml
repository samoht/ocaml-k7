(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Native_test

let test_wide () =
  let s = svg [ (Ms 0, "\xe4\xb8\xadab") ] in
  check_contains "positions" ~sub:{|<tspan x="0 28 42">中ab</tspan>|} s;
  Alcotest.(check (list string)) "text" [ screen [ "中ab" ] ] (frames s)

let test_default_fg () =
  let s = svg [ (Ms 0, "\027[31ma\027[39mb") ] in
  check_contains "default" ~sub:">b</tspan>" s;
  Alcotest.(check bool)
    "not drawn in the background" false
    (contains ~sub:{|fill="#171717">b|} s)

let test_cursor () =
  let s = svg [ (Ms 0, "ab") ] in
  check_contains "cursor" ~sub:{|<use href="#c" class="k" x="28" y="0"/>|} s;
  check_contains "blink" ~sub:".k{animation:b" s;
  check_contains "reduced motion"
    ~sub:"@media (prefers-reduced-motion:reduce){.k{animation:none}}" s;
  let hidden = svg [ (Ms 0, "ab\027[?25l") ] in
  Alcotest.(check bool)
    "hidden cursor" false
    (contains ~sub:{|href="#c"|} hidden)

let test_concealed_cursor () =
  let s = svg [ (Ms 0, "\027[8msecret\r") ] in
  Alcotest.(check bool)
    "concealed glyph is not redrawn over the cursor" false
    (contains ~sub:{|<text class=|} s)

let test_backspace () =
  Alcotest.(check (list string))
    "moved"
    [ screen [ "aXc" ] ]
    (frames (svg [ (Ms 0, "abc\b\bX") ]));
  Alcotest.(check (list string))
    "title"
    [ screen [ "ab" ] ]
    (frames (svg [ (Ms 0, "a\027]0;t\bitle\007b") ]))

let test_zero_width () =
  let check name expected output =
    Alcotest.(check string) name expected (last (frames (svg output)))
  in
  check "combining mark" (screen [ "e\xcc\x81x" ]) [ (Ms 0, "e\xcc\x81x") ];
  check "alone" (screen [ "e\xcc\x81" ]) [ (Ms 0, "e"); (Ms 20, "\xcc\x81") ];
  check "split code point" (screen [ "e\xcc\x81" ])
    [ (Ms 0, "e\xcc"); (Ms 20, "\x81") ];
  check "wide"
    (screen [ "\xe4\xb8\xad\xcc\x81a" ])
    [ (Ms 0, "\xe4\xb8\xad\xcc\x81a") ];
  check "zero-width space" (screen [ "> x" ]) [ (Ms 0, "> \xe2\x80\x8bx") ];
  check "last column"
    (screen [ String.make 45 'a' ^ "b\xcc\x81" ])
    [ (Ms 0, String.make 45 'a' ^ "b\xcc\x81") ];
  (* xterm.js gives each emoji of a ZWJ sequence its two cells. *)
  let zwj = "\xf0\x9f\x91\xa8\xe2\x80\x8d\xf0\x9f\x91\xa9" in
  let s = svg [ (Ms 0, zwj ^ "x") ] in
  Alcotest.(check string) "joiner" (screen [ zwj ^ "x" ]) (last (frames s));
  check_contains "cursor after the sequence"
    ~sub:{|<use href="#c" class="k" x="70" y="0"/>|} s

let test_erase_scrollback () =
  Alcotest.(check string)
    "kept" (screen [ "ab" ])
    (last (frames (svg [ (Ms 0, "ab\027[3J") ])))

let check_screens cases =
  List.iter
    (fun (name, output, expected) ->
      Alcotest.(check string)
        name (screen expected)
        (last (frames (svg [ (Ms 0, output) ]))))
    cases

let test_pending_wrap () =
  let full = String.make 46 'a' in
  check_screens
    [
      ("backspace", full ^ "\bX", [ String.make 44 'a' ^ "Xa" ]);
      ("cursor back", full ^ "\027[DX", [ String.make 44 'a' ^ "Xa" ]);
      ("erase line", full ^ "\027[KX", [ String.make 45 'a' ^ "X" ]);
      ("erase to the start", full ^ "\027[1KX", [ String.make 45 ' ' ^ "X" ]);
      ("erase below", full ^ "\027[JX", [ String.make 45 'a' ^ "X" ]);
      ("cursor position", "\027[1;99HX", [ String.make 45 ' ' ^ "X" ]);
      ("column", "\027[99GX", [ String.make 45 ' ' ^ "X" ]);
      ("cursor forward", "ab\027[99CX", [ "ab" ^ String.make 43 ' ' ^ "X" ]);
      ("character", full ^ "\027[31mX", [ full; "X" ]);
    ]

let test_line_feeds () =
  let full = String.make 46 'a' in
  check_screens
    [
      ("line feed", "ab\ncd", [ "ab"; "  cd" ]);
      ("vertical tab", "ab\011cd", [ "ab"; "  cd" ]);
      ("form feed", "ab\012cd", [ "ab"; "  cd" ]);
      ("last column", full ^ "\nX", [ full; String.make 45 ' ' ^ "X" ]);
      ("index", "ab\027Dc", [ "ab"; "  c" ]);
      ("next line", "ab\027Ec", [ "ab"; "c" ]);
      ("reverse index", "\r\n\r\nab\027Mc", [ ""; "  c"; "ab" ]);
      ("reverse index at the top", "ab\027Mc", [ "  c"; "ab" ]);
      ( "reverse index in a region",
        "a\r\nb\r\nc\027[2;3r\027[2;1H\027Mx",
        [ "a"; "x"; "b" ] );
      ("scroll up", "a\r\nb\r\nc\027[S", [ "b"; "c" ]);
      ("scroll down", "a\r\nb\r\nc\027[T", [ ""; "a"; "b"; "c" ]);
    ]

let test_controls () =
  let full = String.make 46 'a' in
  check_screens
    [
      ("column absolute", "\027[3`x", [ "  x" ]);
      ("column relative", "ab\027[2ax", [ "ab  x" ]);
      ("row relative", "\027[2ex", [ ""; ""; "x" ]);
      ("row absolute", "ab\027[3dx", [ "ab"; ""; "  x" ]);
      ("erase characters", "abcde\r\027[2X", [ "  cde" ]);
      ("repeat", "a\027[3b", [ "aaaa" ]);
      ("repeat after a control", "a\027[D\027[3b", [ "a" ]);
      ("no auto-wrap", "\027[?7l" ^ full ^ "bc", [ String.make 45 'a' ^ "c" ]);
      ("origin", "\027[2;3r\027[?6h\027[1;1Hx", [ ""; "x" ]);
      ("insert line", "a\r\nb\027[A\027[Lc", [ "c"; "a"; "b" ]);
      ("delete line", "ab\027[Mc", [ "c" ]);
      ( "insert line in a region",
        "a\r\nb\r\nc\r\nd\027[1;3r\027[1;1H\027[L",
        [ ""; "a"; "b"; "d" ] );
      ( "delete line in a region",
        "a\r\nb\r\nc\r\nd\027[1;3r\027[1;1H\027[M",
        [ "b"; "c"; ""; "d" ] );
      ( "wrap at the region's end",
        "a\r\nb\r\nc\027[1;2r\027[2;1H" ^ full ^ "X",
        [ full; "X"; "c" ] );
      ("down in a region", "\027[1;2r\027[5Bx", [ ""; "x" ]);
      ("line drawing", "\027(0qx\027(Bq", [ "\xe2\x94\x80\xe2\x94\x82q" ]);
      ("shift out", "\027)0\014q\015q", [ "\xe2\x94\x80q" ]);
    ]

let test_history () =
  let history s =
    let t = Screen.terminal ~rows:2 ~cols:5 in
    Screen.feed t s;
    Screen.history t
  in
  Alcotest.(check int) "line feed" 1 (history "a\r\nb\r\nc");
  Alcotest.(check int) "delete line" 0 (history "a\r\nb\027[H\027[M");
  Alcotest.(check int) "scroll up" 1 (history "a\r\nb\027[S");
  Alcotest.(check int)
    "after a clear, a wrap" 2
    (history "a\r\nb\r\nc\027[3J\r\n0123456789")

(* Real VHS writes U+2500 for this stream. *)
let test_saved_charset () =
  check_screens
    [ ("restored line drawing", "\027(0\0277\027(B\0278q", [ "\xe2\x94\x80" ]) ]

let test_charsets () =
  check_screens
    [
      ( "designations and shifts",
        "\027(A#\027(B#\027*0\027nq\015q\027(0q\027(Aq#\027(4[",
        [ "\xc2\xa3#\xe2\x94\x80q\xe2\x94\x80q\xc2\xa3\xc4\xb3" ] );
      ("UTF-8", "\027(0\027%Gq", [ "\xe2\x94\x80" ]);
    ]

let first_fg s =
  let t = Screen.terminal ~rows:2 ~cols:5 in
  Screen.feed t s;
  (Screen.cells (Screen.rows (Screen.of_terminal t)).(0)).(0).fg

let colour =
  Alcotest.testable
    (fun ppf -> function
      | Screen.Default -> Fmt.string ppf "default"
      | Indexed i -> Fmt.pf ppf "index %d" i
      | Rgb c -> Fmt.pf ppf "#%06x" c)
    ( = )

let test_restore () =
  check_screens
    [
      ("without a save", "\027[5;5Habc\0278X", [ "X"; ""; ""; ""; "    abc" ]);
      ( "after scrolling",
        "\027[9;3H\0277\r\n\r\nY\0278X",
        [ ""; ""; ""; ""; ""; ""; ""; ""; "Y X" ] );
      ( "shift in",
        "\027(0\0277\027(B\0278q\015q",
        [ "\xe2\x94\x80\xe2\x94\x80" ] );
      ("ANSI restore", "\027(0\027[s\027(B\027[uq", [ "\xe2\x94\x80" ]);
      ( "alternate screen",
        "\027(0\027[?1049h\027(B\027[?1049lq",
        [ "\xe2\x94\x80" ] );
    ];
  Alcotest.check colour "saved colour" (first_fg "\027[31mx")
    (first_fg "\027[31m\0277\027[0m\0278x");
  Alcotest.check colour "colour after the save" (first_fg "x")
    (first_fg "\0277\027[31m\0278x");
  Alcotest.(check bool)
    "hidden cursor" false
    (contains ~sub:{|href="#c"|} (svg [ (Ms 0, "\0277\027[?25l\0278") ]))

let test_screens () =
  check_screens
    [
      ( "scroll regions",
        "\027[?1049h\027[1;2r\027[?1049la\r\nb\r\nc",
        [ "b"; "c" ] );
      ( "private prefix",
        "\0277\027[5;5Habc\027[?uX",
        [ ""; ""; ""; ""; "    abcX" ] );
    ];
  Alcotest.(check bool)
    "hidden cursor" false
    (contains ~sub:{|href="#c"|}
       (svg [ (Ms 0, "\027[?1049h\027[?25l\027[?1049l") ]))

(* VHS keeps X at column 9 and puts Y in column 46. *)
let test_clear_tabs () =
  check_screens
    [
      ( "cleared tab stops",
        "a\tX\027[3g\r\tY",
        [ "a" ^ String.make 7 ' ' ^ "X" ^ String.make 36 ' ' ^ "Y" ] );
    ]

let test_tab_stops () =
  let full = String.make 46 'a' in
  check_screens
    [
      ("set", "\027[3g\027[11G\027H\r\tX", [ String.make 10 ' ' ^ "X" ]);
      ("clear one", "\027[9G\027[0g\r\tX", [ String.make 16 ' ' ^ "X" ]);
      ("forward and back", "a\027[2IX\027[2ZY", [ "a       Y       X" ]);
      ("pending wrap", full ^ "\tX", [ full; "X" ]);
      ( "normal screen",
        "\027[3g\027[?1049h\027[?1049l\tX",
        [ String.make 45 ' ' ^ "X" ] );
      ( "alternate screen",
        "\027[?1049h\027[3g\027[?1049l\027[?1049h\tX",
        [ String.make 45 ' ' ^ "X" ] );
    ]

(* VHS leaves the following text at column 2. *)
let test_delete_wide () =
  check_screens
    [ ("delete half of a wide glyph", "\xe4\xb8\xadab\r\027[P", [ " ab" ]) ]

(* As in xterm.js; real VHS writes these screens. *)
let test_cell_edits () =
  let wide = "\xe4\xb8\xad" in
  check_screens
    [
      ("delete from the second cell", wide ^ "ab\027[2G\027[P", [ " ab" ]);
      ("erase from the second cell", wide ^ "ab\027[2G\027[X", [ "  ab" ]);
      ("erase up to the first cell", "a" ^ wide ^ "b\r\027[2X", [ "   b" ]);
      ("insert at the second cell", wide ^ "ab\027[2G\027[@", [ "   ab" ]);
      ("delete up to the first cell", "a" ^ wide ^ "b\r\027[2P", [ " b" ]);
      ( "insert at the right margin",
        String.make 44 'a' ^ wide ^ "\r\027[@",
        [ " " ^ String.make 44 'a' ] );
      ("scroll left", "ab\r\ncd\027[ @", [ "b"; "d" ]);
      ("scroll right", "ab\r\ncd\027[ A", [ " ab"; " cd" ]);
      ("insert column", "abc\r\ndef\027[2G\027['}", [ "a bc"; "d ef" ]);
      ("delete column", "abc\r\ndef\027[2G\027['~", [ "ac"; "df" ]);
    ]

let first_attrs s =
  let t = Screen.terminal ~rows:2 ~cols:5 in
  Screen.feed t s;
  (Screen.cells (Screen.rows (Screen.of_terminal t)).(0)).(0).attrs

(* As in xterm.js; real VHS writes these screens. *)
let test_sequences () =
  let e = String.make 46 'E' in
  check_screens
    [
      ("keypad modes", "a\027=b\027>c", [ "abc" ]);
      ("S7C1T", "a\027 Fb", [ "ab" ]);
      ("DCS", "a\027Pq#0\027\\b", [ "ab" ]);
      ("APC", "a\027_Gx\007y\027\\b", [ "ab" ]);
      ("DCS ended by an escape", "a\027Pq\027[Cb", [ "a b" ]);
      ("control in CSI", "ab\027[1\rCx", [ "ax" ]);
      ("CAN", "a\027[31\024mb", [ "amb" ]);
      ("DECSTR", "\027[1;2r\027(0\027[!p\027[Ha\r\nb\r\nc", [ "a"; "b"; "c" ]);
      ("DECALN", "x\027#8", List.init 9 (fun _ -> e));
      ("erase the scrollback", "ab\027[?3J", [ "ab" ]);
    ];
  Alcotest.(check bool)
    "modifyOtherKeys" true
    (Ansi.Attr.is_empty (first_attrs "\027[>4;2mx"))

let test_replies () =
  let replies s =
    let t = Screen.terminal ~rows:9 ~cols:46 in
    Screen.feed t s;
    let r = Screen.take_replies t in
    Alcotest.(check string) (Fmt.str "%S taken" s) "" (Screen.take_replies t);
    r
  in
  List.iter
    (fun (s, expected) ->
      Alcotest.(check string) (Fmt.str "%S" s) expected (replies s))
    [
      ("\027[c", "\027[?1;2c");
      ("\027[0c", "\027[?1;2c");
      ("\027[1c", "");
      ("\027[>c", "\027[>0;276;0c");
      ("\027[5n", "\027[0n");
      ("ab\r\nc\027[6n", "\027[2;2R");
      ("ab\r\nc\027[?6n", "\027[?2;2;1R");
      (String.make 46 'a' ^ "\027[6n", "\027[1;46R");
      ("\027[c\027[5n", "\027[?1;2c\027[0n");
      ("\027[=c\027[?5n", "");
    ]

let test_prepend_chunk_boundaries () =
  let whole = last (frames (svg [ (Ms 0, "\xdb\x9dq") ])) in
  let split = last (frames (svg [ (Ms 0, "\xdb"); (Ms 20, "\x9dq") ])) in
  Alcotest.(check string) "same terminal text across a UTF-8 split" whole split

let test_prepend_no_wrap_boundaries () =
  let whole = last (frames (svg [ (Ms 0, "\027[?7l\xdb\x9dq") ])) in
  let split =
    last
      (frames
         (svg
            [
              (Ms 0, "\027[?7l"); (Ms 20, "\xdb"); (Ms 40, "\x9d"); (Ms 60, "q");
            ]))
  in
  Alcotest.(check string)
    "same text with automatic wrapping disabled" whole split

(* U+2764 is narrow in xterm's Unicode 11 width table. *)
let test_text_emoji_width () =
  let b = Buffer.create 4 in
  Buffer.add_utf_8_uchar b (Uchar.of_int 0x2764);
  let heart = Buffer.contents b in
  let prefix = String.make 45 'a' in
  Alcotest.(check string)
    "a text heart occupies the final cell"
    (screen [ prefix ^ heart; "X" ])
    (last (frames (svg [ (Ms 0, prefix ^ heart ^ "X") ])))

(* A frame in synchronized output (DEC mode 2026) shows when the mode is
   reset, or 150ms after it is set. *)
let test_synchronized () =
  let same name expected output =
    Alcotest.(check string) name (svg expected) (svg output)
  in
  same "reset"
    [ (Ms 0, ""); (Ms 100, "HALF") ]
    [ (Ms 0, "\027[?2026hHA"); (Ms 100, "LF\027[?2026l") ];
  same "timeout"
    [ (Ms 0, ""); (Ms 150, "HA"); (Ms 400, "LF") ]
    [ (Ms 0, "\027[?2026hHA"); (Ms 400, "LF") ];
  same "within a read"
    [ (Ms 0, "A"); (Ms 100, "BCD"); (Ms 200, "EF") ]
    [
      (Ms 0, "A\027[?2026hB");
      (Ms 100, "C\027[?2026lD\027[?25;2026hE");
      (Ms 200, "F\027[?20");
      (Ms 200, "26l");
    ];
  same "other modes" [ (Ms 0, "AB") ] [ (Ms 0, "A\027[?2025h\027[?2026$pB") ]

let suite =
  ( "screen",
    List.map
      (fun (name, fn) -> Alcotest.test_case name `Quick fn)
      [
        ("wide glyphs", test_wide);
        ("text heart at the right margin", test_text_emoji_width);
        ( "Arabic prepend with wrapping disabled",
          test_prepend_no_wrap_boundaries );
        ( "Arabic prepend survives a PTY read boundary",
          test_prepend_chunk_boundaries );
        ("default foreground", test_default_fg);
        ("cursor", test_cursor);
        ("concealed cursor text", test_concealed_cursor);
        ("backspace", test_backspace);
        ("zero width", test_zero_width);
        ("erase scrollback", test_erase_scrollback);
        ("pending wrap", test_pending_wrap);
        ("line feeds", test_line_feeds);
        ("controls", test_controls);
        ("history", test_history);
        ("saved character set", test_saved_charset);
        ("restore", test_restore);
        ("screens", test_screens);
        ("character sets", test_charsets);
        ("cleared tab stops", test_clear_tabs);
        ("tab stops", test_tab_stops);
        ("delete wide glyph cell", test_delete_wide);
        ("cell edits", test_cell_edits);
        ("sequences", test_sequences);
        ("replies", test_replies);
        ("synchronized output", test_synchronized);
      ] )
