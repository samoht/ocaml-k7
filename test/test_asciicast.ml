(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Native_test

let cast ?settings ?show ?delay src =
  match K7.svg_of_cast ?settings ?show ?delay src with
  | Ok s -> unprefixed s
  | Error e -> Alcotest.failf "%a" K7.pp_error e

let screens ~rows s =
  match Svg_check.frames ~cell_width ~cell_height ~rows s with
  | Ok f -> f
  | Error e -> Alcotest.failf "not well-formed XML: %s" e

let timed =
  {|{"version": 2, "width": 20, "height": 3}
[0.0015, "o", "a"]
[0.0025, "i", "x"]
[0.0025, "o", "b"]
[0.1234994, "o", "c"]
[0.1235, "o", "d"]
[1.0005, "m", ""]

[2, "o", "\r\ne"]
[2.0000004, "o", "f"]
|}

let at_ms =
  K7.
    [
      (Ms 2, "a");
      (Ms 3, "b");
      (Ms 123, "c");
      (Ms 124, "d");
      (Ms 2000, "\r\ne");
      (Ms 2000, "f");
      (Ms 3000, "");
    ]

let test_times () =
  let settings = K7.[ Framerate 1000 ] in
  let image = cast ~settings timed in
  Alcotest.(check string)
    "the events at their milliseconds"
    (svg ~settings:K7.[ Columns 20; Rows 3; Framerate 1000 ] at_ms)
    image;
  Alcotest.(check bool)
    "a millisecond later is another image" true
    (image
    <> svg
         ~settings:K7.[ Columns 20; Rows 3; Framerate 1000 ]
         (List.map
            (fun (K7.Ms ms, s) -> (K7.Ms (if ms = 124 then 125 else ms), s))
            at_ms));
  check_contains "the last screen holds a second" ~sub:"animation:s 3000ms"
    image;
  Alcotest.(check (list string))
    "screens"
    [ "\n\n"; "a\n\n"; "ab\n\n"; "abc\n\n"; "abcd\n\n"; "abcd\nef\n" ]
    (screens ~rows:3 image)

let test_show () =
  let line = "\027[38;2;90;86;224m$ \027[0mmake demo\r\n" in
  let shifted d =
    (K7.Ms 0, line) :: List.map (fun (K7.Ms ms, s) -> (K7.Ms (ms + d), s)) at_ms
  in
  let settings = K7.[ Columns 20; Rows 3 ] in
  Alcotest.(check string)
    "500ms by default"
    (svg ~settings (shifted 500))
    (cast ~show:"make demo" timed);
  Alcotest.(check string)
    "delay"
    (svg ~settings (shifted 250))
    (cast ~show:"make demo" ~delay:(Ms 250) timed);
  Alcotest.(check string)
    "no delay without a line" (svg ~settings at_ms)
    (cast ~delay:(Ms 250) timed);
  match K7.svg_of_cast ~show:"a\nb" timed with
  | Error (Invalid msg) ->
      Alcotest.(check string)
        "control" {|show "a\nb": holds a control character|} msg
  | _ -> Alcotest.fail "a control character shown"

let test_size () =
  Alcotest.(check string)
    "--columns replaces the width"
    (svg ~settings:K7.[ Rows 3; Columns 30 ] at_ms)
    (cast ~settings:K7.[ Columns 30 ] timed);
  Alcotest.(check string)
    "a pixel width replaces the width"
    (svg ~settings:K7.[ Rows 3; Width 400 ] at_ms)
    (cast ~settings:K7.[ Width 400 ] timed);
  Alcotest.(check string)
    "a pixel height replaces the height"
    (svg ~settings:K7.[ Columns 20; Height 300; Font_size 10 ] at_ms)
    (cast ~settings:K7.[ Height 300; Font_size 10 ] timed)

let header = {|{"version": 2, "width": 8, "height": 2}|}

let test_errors () =
  List.iter
    (fun (src, expected) ->
      match K7.svg_of_cast src with
      | Error (Invalid msg) -> Alcotest.(check string) src expected msg
      | Error e -> Alcotest.failf "%S: %a" src K7.pp_error e
      | Ok _ -> Alcotest.failf "%S: drawn" src)
    [
      ("", "line 1: no asciicast header");
      ("\n[0, \"o\", \"a\"]\n", "line 1: no asciicast header");
      ( {|{"version": 3, "term": {"cols": 80, "rows": 24}}|},
        "line 1: asciicast version 3, not 2" );
      ( {|{"version": 1, "width": 80, "height": 24, "stdout": []}|},
        "line 1: asciicast version 1, not 2" );
      ( {|{"version": "2", "width": 80, "height": 24}|},
        {|line 1: asciicast version "2", not 2|} );
      ( {|{"width": 80, "height": 24}|},
        "line 1: Missing member version in asciicast header object at byte 0" );
      ( {|[2, "o", "a"]|},
        "line 1: Expected asciicast header object but found array at byte 0" );
      ( {|{"version": 2, "width": 0, "height": 24}|},
        "line 1: width 0 is not a positive integer" );
      ( {|{"version": 2, "width": 80, "height": 2.5}|},
        "line 1: height 2.5 is not a positive integer" );
      ( {|{"version": 2, "width": 80, "height": 24|},
        "line 1: Expected , or } after object member but found: end of text at \
         byte 40" );
      ( header ^ "\n[0.5, \"o\"]\n",
        "line 2: Expected 3 elements in asciicast event tuple but found 2 at \
         byte 0" );
      (header ^ "\n[0.5, \"o\", 1]\n", "line 2: the data is not a string");
      (header ^ "\n[0.5, 111, \"a\"]\n", "line 2: the code is not a string");
      ( header ^ "\n[\"0.5\", \"o\", \"a\"]\n",
        "line 2: the time is not a number" );
      ( header ^ "\n[0.5, \"o\", null]\n",
        "line 2: Expected number or string but found null at byte 11" );
      (header ^ "\n[-1, \"o\", \"a\"]\n", "line 2: time -1s is negative");
      ( header ^ "\n[1, \"o\", \"a\"]\n\n[0.5, \"i\", \"b\"]\n",
        "line 4: time 0.5s is before 1s" );
      ( header ^ "\n[1, \"o\", \"\xff\"]\n",
        "line 2: UTF-8 decoding error: invalid byte xff at byte 9" );
      ( header ^ "\n[1, \"o\", \"a\"] x\n",
        "line 2: Expected end of text but found x at byte 14" );
    ]

let read file = In_channel.with_open_bin file In_channel.input_all

let test_asciinema () =
  let image = cast (read "asciinema.cast") in
  Alcotest.(check string)
    "the output events"
    (svg
       ~settings:K7.[ Columns 40; Rows 6 ]
       [
         (Ms 12, "\027[?2004h$ ");
         (Ms 513, "l");
         (Ms 634, "s");
         (Ms 902, "\r\n\027[?2004l\r");
         (Ms 910, "a.txt  b.txt  café.md  漢字.txt\r\n");
         (Ms 911, "\027[?2004h$ ");
         (Ms 2310, "\027[?2004l\r\r\nexit\r\n");
         (Ms 3310, "");
       ])
    image;
  match List.rev (screens ~rows:6 image) with
  | last :: _ ->
      Alcotest.(check string)
        "last screen" "$ ls\na.txt  b.txt  café.md  漢字.txt\n$\nexit\n\n" last
  | [] -> Alcotest.fail "no screen"

let suite =
  ( "asciicast",
    List.map
      (fun (name, fn) -> Alcotest.test_case name `Quick fn)
      [
        ("times", test_times);
        ("show", test_show);
        ("size", test_size);
        ("errors", test_errors);
        ("asciinema", test_asciinema);
      ] )
