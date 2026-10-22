(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Native_test

let test_hostile () =
  let hostile = "<a&b>]]>\"'\x00\x01\x07\xff\xfe\xc2\x9b\xef\xbf\xbe</svg>" in
  let s = svg [ (Ms 0, hostile) ] in
  match frames s with
  | [ f ] ->
      Alcotest.(check bool)
        "markup survives as text" true
        (contains ~sub:"<a&b>]]>\"'" f);
      check_contains "closing tag kept as text" ~sub:"</svg>" f
  | fs -> Alcotest.failf "%d frames" (List.length fs)

let test_dedup () =
  let s = svg [ (Ms 0, "same\r\nsame\r\nsame") ] in
  Alcotest.(check int) "one definition" 1 (count ~sub:">same</tspan>" s);
  Alcotest.(check int) "three uses" 3 (count ~sub:"<use href=\"#r0\"" s);
  Alcotest.(check (list string))
    "text"
    [ screen [ "same"; "same"; "same" ] ]
    (frames s)

(* Frames switch at their times, in a loop that ends a frame after the last
   output, and screens that do not change make no frame. *)
let test_keyframes () =
  let s =
    svg [ (Ms 0, "a"); (Ms 500, "b"); (Ms 700, ""); (Ms 1000, "\r\nc") ]
  in
  check_contains "duration" ~sub:".s{animation:s 1020ms steps(1,end) infinite}"
    s;
  check_contains "first" ~sub:"0.0000%{transform:translateX(0px)}" s;
  check_contains "second" ~sub:"49.0196%{transform:translateX(-644px)}" s;
  check_contains "third" ~sub:"98.0392%{transform:translateX(-1288px)}" s;
  Alcotest.(check (list string))
    "frames"
    [ screen [ "a" ]; screen [ "ab" ]; screen [ "ab"; "c" ] ]
    (frames s)

(* A reader who prefers reduced motion sees the last screen as a still
   (WCAG 2.2.2), and the image names itself and holds that screen's text as
   its text alternative. *)
let test_accessible () =
  let s = svg [ (Ms 0, "a"); (Ms 500, "\r\n<b>") ] in
  check_contains "still"
    ~sub:
      "@media \
       (prefers-reduced-motion:reduce){.s{animation:none;transform:translateX(-644px)}}"
    s;
  check_contains "role" ~sub:{|role="img"|} s;
  check_contains "named and described"
    ~sub:{|aria-labelledby="t" aria-describedby="d">|} s;
  check_contains "title" ~sub:"<title id=\"t\">Terminal recording</title>" s;
  check_contains "description" ~sub:"<desc id=\"d\">a\n&lt;b&gt;</desc>" s;
  Alcotest.(check (list string))
    "frames"
    [ screen [ "a" ]; screen [ "a"; "<b>" ] ]
    (frames s)

let test_one_frame () =
  let s = svg [ (Ms 0, "still") ] in
  Alcotest.(check bool)
    "no strip animation" false
    (contains ~sub:"@keyframes s" s)

(* 10px makes 7 x 13 pixel cells. Pixels give 50 columns and 13 rows;
   columns and rows give the frame they need with the padding and xterm.js's
   25 x 10 pixels, rounded up to even. *)
let test_settings () =
  List.iter
    (fun (settings, size, grid) ->
      let s =
        svg
          ~settings:(K7.[ Font_size 10; Padding 10 ] @ settings)
          [ (Ms 0, "x") ]
      in
      check_contains "size" ~sub:size s;
      check_contains "grid" ~sub:grid s)
    [
      ( K7.[ Width 400; Height 200 ],
        {|width="400" height="200" viewBox="0 0 400 200"|},
        {|<svg x="10" y="10" width="350" height="169">|} );
      ( K7.[ Columns 100; Rows 5 ],
        {|width="746" height="96" viewBox="0 0 746 96"|},
        {|<svg x="10" y="10" width="700" height="65">|} );
    ]

(* Sizes far beyond a screen are refused, not computed with overflow. *)
let test_huge () =
  (match K7.svg ~settings:[ Width 1_000_000 ] [] with
  | Error (Unsupported why) ->
      check_contains "names the setting" ~sub:"Width" why
  | Ok _ -> Alcotest.fail "a huge width played"
  | Error e -> Alcotest.failf "%a" K7.pp_error e);
  match K7.svg ~settings:[ Width 100_000; Font_size 1 ] [] with
  | Error (Unsupported _) -> ()
  | Ok _ -> Alcotest.fail "a huge terminal played"
  | Error e -> Alcotest.failf "%a" K7.pp_error e

(* The same output gives the same image. *)
let test_deterministic () =
  let output =
    [ (K7.Ms 0, "a\027[31mb"); (Ms 33, "\r\nc"); (Ms 65, "\027[2J") ]
  in
  Alcotest.(check string) "same bytes" (svg output) (svg output)

(* An .svg output never reaches VHS. *)
let test_svg_output () =
  match K7.tape ~outputs:[ "demo.svg"; "demo.gif" ] [ K7.type' "x" ] with
  | Error e -> Alcotest.failf "%a" K7.pp_error e
  | Ok t ->
      Alcotest.(check bool)
        "not printed" false
        (contains ~sub:".svg" (K7.to_string t));
      Alcotest.(check bool)
        "raw Output refused" true
        (Result.is_error
           (K7.tape ~outputs:[ "a.gif" ]
              [ K7.raw "Output b.svg"; K7.type' "x" ]))

(* Times are a tape's durations, in increasing order. *)
let test_times () =
  List.iter
    (fun output ->
      match K7.svg output with
      | Error (Invalid _) -> ()
      | Ok _ -> Alcotest.fail "drawn"
      | Error e -> Alcotest.failf "%a" K7.pp_error e)
    [
      [ (Ms (-1), "a") ];
      [ (Ms 10, "a"); (Ms 5, "b") ];
      [ (Ms 0, "a"); (Ms max_int, "b") ];
    ]

(* [svg] refuses the settings [tape] refuses, and starts no shell. *)
let test_setting_errors () =
  List.iteri
    (fun i setting ->
      match K7.svg ~settings:[ setting ] [ (Ms 0, "hello") ] with
      | Error (Invalid _) -> ()
      | Error e -> Alcotest.failf "setting %d: %a" i K7.pp_error e
      | Ok _ -> Alcotest.failf "setting %d: played" i)
    K7.
      [
        Font_family "Mono\nSpace";
        Font_family "\"'`";
        Theme "{\n}";
        Theme "Dracula\nSet Width 50";
        Framerate 0;
        Font_size 0;
        Width 0;
        Height 1;
        Width (-10);
        Padding (-1);
        Font_size (-5);
        Framerate (-1);
        Typing_speed (Ms (-1));
        Width max_int;
      ];
  match K7.svg ~settings:[ Shell Cmd ] [ (Ms 0, "hello") ] with
  | Ok _ -> ()
  | Error e -> Alcotest.failf "Shell Cmd: %a" K7.pp_error e

(* A screen reader must not receive text concealed on the last screen, and
   a blank last screen has no description, only the title. *)
let test_concealed_description () =
  let s = svg [ (Ms 0, "\027[8msecret\027[?25l") ] in
  Alcotest.(check bool) "no description" false (contains ~sub:"<desc" s);
  Alcotest.(check bool) "no secret" false (contains ~sub:"secret" s);
  check_contains "title" ~sub:"<title id=\"t\">Terminal recording</title>" s;
  Alcotest.(check bool)
    "described by nothing" false
    (contains ~sub:"aria-describedby" s);
  let s = svg [ (Ms 0, "hello"); (Ms 100, "\027[2J\027[H") ] in
  Alcotest.(check bool) "cleared" false (contains ~sub:"<desc" s)

(* The zero-width characters of k7 cram's prompt mark do not reach the text
   alternative; a joiner and a combining mark, which shape what shows, do. *)
let test_zero_width_description () =
  let s =
    svg
      [
        ( Ms 0,
          "> \xe2\x80\x8b\xe2\x81\xa0\xe2\x80\x8b\r\n\
           a\xe2\x80\x8bb\xe2\x81\xa0c e\xcc\x81 \
           \xf0\x9f\x91\xa9\xe2\x80\x8d\xf0\x9f\x92\xbb" );
      ]
  in
  check_contains "marks left out"
    ~sub:
      "<desc id=\"d\">&gt;\n\
       abc e\xcc\x81 \xf0\x9f\x91\xa9\xe2\x80\x8d\xf0\x9f\x92\xbb</desc>"
    s;
  Alcotest.(check (pair int int))
    "nor the image's text" (0, 0)
    (count ~sub:"\xe2\x80\x8b" s, count ~sub:"\xe2\x81\xa0" s);
  (* A prepended concatenation mark, U+0605, shows over the digits; it
     joins the cell before it, as a zero-width character does. *)
  let s = svg [ (Ms 0, "a\xd8\x8512") ] in
  check_contains "number mark" ~sub:"<desc id=\"d\">a\xd8\x8512</desc>" s;
  check_contains "number mark shown" ~sub:">a\xd8\x85</tspan>" s

(* A spacing mark, such as the vowel sign after Devanagari KA, joins the
   cluster before it but adds a cell to it, the cell xterm.js gives it. *)
let test_spacing_mark () =
  let s = svg [ (Ms 0, "\u{0915}\u{093E}x \u{0F7F}\xff") ] in
  Alcotest.(check (list string))
    "text"
    [ screen [ "\u{0915}\u{093E}x \u{0F7F}\u{FFFD}" ] ]
    (frames s)

(* Framerate is documented as positive; the native limits enumerate pixel
   sizes and terminal dimensions, without a frame-rate ceiling. *)
let test_framerate_limit () =
  let settings = K7.Framerate 100_001 :: settings in
  (match K7.tape ~settings ~outputs:[ "x.svg" ] [ K7.sleep (Ms 100) ] with
  | Ok _ -> ()
  | Error e -> Alcotest.failf "tape: %a" K7.pp_error e);
  match K7.svg ~settings [ (Ms 0, "hello") ] with
  | Ok _ -> ()
  | Error e -> Alcotest.failf "svg: %a" K7.pp_error e

(* Cursor redraws obey the same format-character policy as row text and
   the accessible description. *)
let test_cursor_format () =
  let s = svg [ (Ms 0, "a\u{200B}\u{202E}b\r") ] in
  check_contains "row text" ~sub:">ab</tspan>" s;
  check_contains "description" ~sub:"<desc id=\"d\">ab</desc>" s;
  check_contains "cursor glyph"
    ~sub:{|<text class="ka k" x="0" y="22">a</text>|} s;
  Alcotest.(check bool)
    "zero-width space removed everywhere" false
    (contains ~sub:"\u{200B}" s);
  Alcotest.(check bool)
    "bidi override removed everywhere" false
    (contains ~sub:"\u{202E}" s)

(* A cursor moved onto the second cell of a wide glyph covers the whole
   glyph, as the cursor at its first cell does. *)
let test_cursor_wide_right () =
  let s = svg [ (Ms 0, "\u{4e2d}\027[D") ] in
  check_contains "wide cursor on the glyph"
    ~sub:{|<use href="#w" class="k" x="0" y="0"/>|} s;
  check_contains "glyph under the cursor"
    ~sub:"<text class=\"ka k\" x=\"0\" y=\"22\">\u{4e2d}</text>" s

(* SGR 21 draws a double underline, as xterm.js does, and SGR 4 a single
   one. *)
let test_double_underline () =
  let s = svg [ (Ms 0, "\027[21md\027[0m \027[4ms\027[0m") ] in
  (* Two lines, as rectangles in the text's colour, under a cell of 14
     pixels whose baseline is at 22. *)
  check_contains "double"
    ~sub:
      {|<rect x="0" y="24" width="14" height="1" class="d"/><rect x="0" y="26" width="14" height="1" class="d"/>|}
    s;
  check_contains "no single line" ~sub:{|<tspan x="0">d</tspan>|} s;
  check_contains "single" ~sub:{|<tspan x="28" class="xu">s</tspan>|} s

(* Reverse video paints a wide glyph's two cells with its foreground, as
   xterm.js does, not its right half with the default foreground. *)
let test_inverse_wide () =
  let s = svg [ (Ms 0, "\027[7;31m\u{4e2d}\027[0m") ] in
  check_contains "both halves"
    ~sub:{|<rect x="0" width="28" height="29" class="p1"/>|} s;
  Alcotest.(check int)
    "no default-foreground half" 0
    (count ~sub:{|height="29" class="d"/>|} s)

(* Tag characters are format characters that pick a subdivision flag: the
   image text and its description keep them, or Scotland's flag shows as a
   plain black flag. *)
let test_tag_flag () =
  let flag =
    "\u{1F3F4}\u{E0067}\u{E0062}\u{E0073}\u{E0063}\u{E0074}\u{E007F}"
  in
  let s = svg [ (Ms 0, flag) ] in
  check_contains "description" ~sub:("<desc id=\"d\">" ^ flag ^ "</desc>") s;
  check_contains "image text" ~sub:(">" ^ flag ^ "</tspan>") s

(* A variation selector shows only after a base: the image text and its
   description keep the one after the heart and drop the stray ones. *)
let test_stray_selector () =
  let s = svg [ (Ms 0, "\u{2764}\u{FE0F}\u{FE0F}a \u{FE0F}b") ] in
  check_contains "description" ~sub:"<desc id=\"d\">\u{2764}\u{FE0F}a b</desc>"
    s;
  Alcotest.(check int)
    "one selector in the row text and one in the description" 2
    (count ~sub:"\u{FE0F}" s)

let suite =
  ( "svg",
    List.map
      (fun (name, fn) -> Alcotest.test_case name `Quick fn)
      [
        ("hostile text", test_hostile);
        ("inverse wide glyph", test_inverse_wide);
        ("subdivision flag", test_tag_flag);
        ("stray variation selector", test_stray_selector);
        ("cursor format characters", test_cursor_format);
        ("double underline", test_double_underline);
        ("cursor on a wide glyph's second cell", test_cursor_wide_right);
        ("positive frame rate above the pixel limit", test_framerate_limit);
        ("row dedup", test_dedup);
        ("keyframes", test_keyframes);
        ("accessible", test_accessible);
        ("concealed text alternative", test_concealed_description);
        ("zero-width text alternative", test_zero_width_description);
        ("spacing mark", test_spacing_mark);
        ("one frame", test_one_frame);
        ("settings", test_settings);
        ("huge sizes", test_huge);
        ("deterministic", test_deterministic);
        ("svg output", test_svg_output);
        ("times", test_times);
        ("direct SVG setting errors", test_setting_errors);
      ] )
