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

let suite =
  ( "screen",
    List.map
      (fun (name, fn) -> Alcotest.test_case name `Quick fn)
      [
        ("wide glyphs", test_wide);
        ("default foreground", test_default_fg);
        ("cursor", test_cursor);
        ("backspace", test_backspace);
      ] )
