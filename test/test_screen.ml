(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Native_test

(* A wide glyph takes two cells, and the next glyph is placed after both. *)
let test_wide () =
  let s = svg [ (Ms 0, "\xe4\xb8\xadab") ] in
  check_contains "positions" ~sub:{|<tspan x="0 28 42">中ab</tspan>|} s;
  Alcotest.(check (list string)) "text" [ screen [ "中ab" ] ] (frames s)

(* SGR 39 resets the foreground to the theme's, not to its background. *)
let test_default_fg () =
  let s = svg [ (Ms 0, "\027[31ma\027[39mb") ] in
  check_contains "default" ~sub:">b</tspan>" s;
  Alcotest.(check bool)
    "not drawn in the background" false
    (contains ~sub:{|class="e">b|} s)

let test_cursor () =
  let s = svg [ (Ms 0, "ab") ] in
  check_contains "cursor" ~sub:{|<use href="#c" class="k" x="28" y="0"/>|} s;
  check_contains "blink" ~sub:".k{animation:b" s;
  let hidden = svg [ (Ms 0, "ab\027[?25l") ] in
  Alcotest.(check bool)
    "hidden cursor" false
    (contains ~sub:{|href="#c"|} hidden)

(* A backspace moves the cursor without erasing, also when a terminal title
   holds one. *)
let test_backspace () =
  Alcotest.(check (list string))
    "moved"
    [ screen [ "aXc" ] ]
    (frames (svg [ (Ms 0, "abc\b\bX") ]));
  Alcotest.(check (list string))
    "title"
    [ screen [ "ab" ] ]
    (frames (svg [ (Ms 0, "a\027]0;t\bitle\007b") ]))

(* A zero-width character joins the cell before it, as in xterm.js, also
   when it arrives on its own, after a wide glyph or in the last column. *)
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
  check "zero-width space"
    (screen [ "> \xe2\x80\x8b" ])
    [ (Ms 0, "> \xe2\x80\x8b") ];
  check "last column"
    (screen [ String.make 45 'a' ^ "b\xcc\x81" ])
    [ (Ms 0, String.make 45 'a' ^ "b\xcc\x81") ];
  (* xterm.js gives each emoji of a ZWJ sequence its two cells. *)
  let zwj = "\xf0\x9f\x91\xa8\xe2\x80\x8d\xf0\x9f\x91\xa9" in
  let s = svg [ (Ms 0, zwj ^ "x") ] in
  Alcotest.(check string) "joiner" (screen [ zwj ^ "x" ]) (last (frames s));
  check_contains "cursor after the sequence"
    ~sub:{|<use href="#c" class="k" x="70" y="0"/>|} s

(* CSI 3 J erases the scrollback, not the screen. *)
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

(* After a character in the last column, the cursor waits there for the
   next one to wrap it, as in xterm: a control moves it from the last
   column, and a move beyond it stops there. *)
let test_pending_wrap () =
  let full = String.make 46 'a' in
  check_screens
    [
      ("backspace", full ^ "\bX", [ String.make 44 'a' ^ "Xa" ]);
      ("cursor back", full ^ "\027[DX", [ String.make 44 'a' ^ "Xa" ]);
      ("erase line", full ^ "\027[K", [ String.make 45 'a' ]);
      ("cursor position", "\027[1;99HX", [ String.make 45 ' ' ^ "X" ]);
      ("column", "\027[99GX", [ String.make 45 ' ' ^ "X" ]);
      ("cursor forward", "ab\027[99CX", [ "ab" ^ String.make 43 ' ' ^ "X" ]);
      ("character", full ^ "\027[31mX", [ full; "X" ]);
    ]

(* A line feed, a vertical tab and a form feed move down a row and keep the
   column, as an index does; a next line also returns; a reverse index moves
   up a row, and scrolls the scroll region down at its top; and CSI S and T
   scroll it, as in xterm. *)
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

(* The controls of xterm that matrix 0.1.0 lacks or reads otherwise: the
   relative and absolute moves, erasing and repeating characters, no
   auto-wrap, origin mode, line edits in the scroll region and from its
   first column, wrapping at the region's last row, and DEC line drawing. *)
let test_controls () =
  let full = String.make 46 'a' in
  check_screens
    [
      ("column absolute", "\027[3`x", [ "  x" ]);
      ("column relative", "ab\027[2ax", [ "ab  x" ]);
      ("row relative", "\027[2ex", [ ""; ""; "x" ]);
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

(* Only a line feed at the bottom of the screen keeps the line it scrolls
   out in the scrollback, as in xterm.js: deleted lines and CSI S drop
   theirs. *)
let test_history () =
  let history s =
    let t = Vhs.Screen.terminal ~rows:2 ~cols:5 in
    Vhs.Screen.feed t s;
    Vhs.Screen.history t
  in
  Alcotest.(check int) "line feed" 1 (history "a\r\nb\r\nc");
  Alcotest.(check int) "delete line" 0 (history "a\r\nb\027[H\027[M");
  Alcotest.(check int) "scroll up" 0 (history "a\r\nb\027[S");
  Alcotest.(check int)
    "after a clear, a wrap" 2
    (history "a\r\nb\r\nc\027[3J\r\n0123456789")

(* DECSC saves the active character set as well as the cursor position.
   Real VHS writes U+2500 for this stream. *)
let test_saved_charset () =
  check_screens
    [ ("restored line drawing", "\027(0\0277\027(B\0278q", [ "\xe2\x94\x80" ]) ]

(* TBC removes all tab stops; the next tab reaches the right margin.
   VHS keeps X at column 9 and puts Y in column 46. *)
let test_clear_tabs () =
  check_screens
    [
      ( "cleared tab stops",
        "a\tX\027[3g\r\tY",
        [ "a" ^ String.make 7 ' ' ^ "X" ^ String.make 36 ' ' ^ "Y" ] );
    ]

(* DCH deletes one cell, even when it cuts a wide glyph in half. The
   remaining half becomes a blank; VHS leaves the following text at column 2. *)
let test_delete_wide () =
  check_screens
    [ ("delete half of a wide glyph", "\xe4\xb8\xadab\r\027[P", [ " ab" ]) ]

let suite =
  ( "screen",
    List.map
      (fun (name, fn) -> Alcotest.test_case name `Quick fn)
      [
        ("wide glyphs", test_wide);
        ("default foreground", test_default_fg);
        ("cursor", test_cursor);
        ("backspace", test_backspace);
        ("zero width", test_zero_width);
        ("erase scrollback", test_erase_scrollback);
        ("pending wrap", test_pending_wrap);
        ("line feeds", test_line_feeds);
        ("controls", test_controls);
        ("history", test_history);
        ("saved character set", test_saved_charset);
        ("cleared tab stops", test_clear_tabs);
        ("delete wide glyph cell", test_delete_wide);
      ] )
