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
  check_contains "title" ~sub:"<title>Terminal recording</title>" s;
  check_contains "description" ~sub:"<desc>a\n&lt;b&gt;</desc>" s;
  Alcotest.(check (list string))
    "frames"
    [ screen [ "a" ]; screen [ "a"; "<b>" ] ]
    (frames s)

let test_one_frame () =
  let s = svg [ (Ms 0, "still") ] in
  Alcotest.(check bool)
    "no strip animation" false
    (contains ~sub:"@keyframes s" s)

let test_settings () =
  let s =
    svg
      ~settings:Vhs.[ Width 400; Height 200; Font_size 10; Padding 10 ]
      [ (Ms 0, "x") ]
  in
  check_contains "size" ~sub:{|width="400" height="200" viewBox="0 0 400 200"|}
    s;
  (* 10px: 7 x 13 pixel cells, 50 columns and 13 rows. *)
  check_contains "grid" ~sub:{|<svg x="10" y="10" width="350" height="169">|} s

(* Sizes far beyond a screen are refused, not computed with overflow. *)
let test_huge () =
  (match Vhs.svg ~settings:[ Width 1_000_000 ] [] with
  | Error (Unsupported why) ->
      check_contains "names the setting" ~sub:"Width" why
  | Ok _ -> Alcotest.fail "a huge width played"
  | Error e -> Alcotest.failf "%a" Vhs.pp_error e);
  match Vhs.svg ~settings:[ Width 100_000; Font_size 1 ] [] with
  | Error (Unsupported _) -> ()
  | Ok _ -> Alcotest.fail "a huge terminal played"
  | Error e -> Alcotest.failf "%a" Vhs.pp_error e

(* The same output gives the same image. *)
let test_deterministic () =
  let output =
    [ (Vhs.Ms 0, "a\027[31mb"); (Ms 33, "\r\nc"); (Ms 65, "\027[2J") ]
  in
  Alcotest.(check string) "same bytes" (svg output) (svg output)

(* An .svg output never reaches VHS. *)
let test_svg_output () =
  match Vhs.tape ~outputs:[ "demo.svg"; "demo.gif" ] [ Vhs.type' "x" ] with
  | Error e -> Alcotest.failf "%a" Vhs.pp_error e
  | Ok t ->
      Alcotest.(check bool)
        "not printed" false
        (contains ~sub:".svg" (Vhs.to_string t));
      Alcotest.(check bool)
        "raw Output refused" true
        (Result.is_error
           (Vhs.tape ~outputs:[ "a.gif" ]
              [ Vhs.raw "Output b.svg"; Vhs.type' "x" ]))

(* Times are a tape's durations, in increasing order. *)
let test_times () =
  List.iter
    (fun output ->
      match Vhs.svg output with
      | Error (Invalid _) -> ()
      | Ok _ -> Alcotest.fail "drawn"
      | Error e -> Alcotest.failf "%a" Vhs.pp_error e)
    [
      [ (Ms (-1), "a") ];
      [ (Ms 10, "a"); (Ms 5, "b") ];
      [ (Ms 0, "a"); (Ms max_int, "b") ];
    ]

(* [svg] refuses the settings [tape] refuses, and starts no shell. *)
let test_setting_errors () =
  List.iteri
    (fun i setting ->
      match Vhs.svg ~settings:[ setting ] [ (Ms 0, "hello") ] with
      | Error (Invalid _) -> ()
      | Error e -> Alcotest.failf "setting %d: %a" i Vhs.pp_error e
      | Ok _ -> Alcotest.failf "setting %d: played" i)
    Vhs.
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
  match Vhs.svg ~settings:[ Shell Cmd ] [ (Ms 0, "hello") ] with
  | Ok _ -> ()
  | Error e -> Alcotest.failf "Shell Cmd: %a" Vhs.pp_error e

(* A screen reader must not receive text concealed on the last screen, and
   a blank last screen has no description, only the title. *)
let test_concealed_description () =
  let s = svg [ (Ms 0, "\027[8msecret\027[?25l") ] in
  Alcotest.(check bool) "no description" false (contains ~sub:"<desc" s);
  Alcotest.(check bool) "no secret" false (contains ~sub:"secret" s);
  check_contains "title" ~sub:"<title>Terminal recording</title>" s;
  let s = svg [ (Ms 0, "hello"); (Ms 100, "\027[2J\027[H") ] in
  Alcotest.(check bool) "cleared" false (contains ~sub:"<desc" s)

(* The zero-width characters of vhs-cram's prompt mark do not reach the text
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
      "<desc>&gt;\n\
       abc e\xcc\x81 \xf0\x9f\x91\xa9\xe2\x80\x8d\xf0\x9f\x92\xbb</desc>"
    s;
  Alcotest.(check (pair int int))
    "nor the image's text" (0, 0)
    (count ~sub:"\xe2\x80\x8b" s, count ~sub:"\xe2\x81\xa0" s);
  (* A prepended concatenation mark, U+0605, shows over the digits; it
     joins the cell before it, as a zero-width character does. *)
  let s = svg [ (Ms 0, "a\xd8\x8512") ] in
  check_contains "number mark" ~sub:"<desc>a\xd8\x8512</desc>" s;
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
  let settings = Vhs.Framerate 100_001 :: settings in
  (match Vhs.tape ~settings ~outputs:[ "x.svg" ] [ Vhs.sleep (Ms 100) ] with
  | Ok _ -> ()
  | Error e -> Alcotest.failf "tape: %a" Vhs.pp_error e);
  match Vhs.svg ~settings [ (Ms 0, "hello") ] with
  | Ok _ -> ()
  | Error e -> Alcotest.failf "svg: %a" Vhs.pp_error e

let suite =
  ( "svg",
    List.map
      (fun (name, fn) -> Alcotest.test_case name `Quick fn)
      [
        ("hostile text", test_hostile);
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
