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
  (match Vhs.svg ~settings:[ Width max_int ] [] with
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

(* Direct callers of [svg] have not passed through [tape]'s validation. *)
let test_setting_errors () =
  List.iter
    (fun setting ->
      match Vhs.svg ~settings:[ setting ] [ (Ms 0, "hello") ] with
      | Error (Invalid _ | Unsupported _) -> ()
      | Error e -> Alcotest.failf "%a" Vhs.pp_error e
      | Ok _ -> ())
    [ Font_family "Mono\nSpace"; Font_family "\"'`"; Theme "{\n}" ]

(* A screen reader must not receive text concealed on the last screen. *)
let test_concealed_description () =
  let s = svg [ (Ms 0, "\027[8msecret\027[?25l") ] in
  check_contains "blank text alternative" ~sub:"<desc></desc>" s

let suite =
  ( "svg",
    List.map
      (fun (name, fn) -> Alcotest.test_case name `Quick fn)
      [
        ("hostile text", test_hostile);
        ("row dedup", test_dedup);
        ("keyframes", test_keyframes);
        ("accessible", test_accessible);
        ("concealed text alternative", test_concealed_description);
        ("one frame", test_one_frame);
        ("settings", test_settings);
        ("huge sizes", test_huge);
        ("deterministic", test_deterministic);
        ("svg output", test_svg_output);
        ("times", test_times);
        ("direct SVG setting errors", test_setting_errors);
      ] )
