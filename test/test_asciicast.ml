(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Native_test

let parse src =
  match K7.cast_of_string src with
  | Ok c -> c
  | Error e -> Alcotest.failf "%a" K7.pp_error e

let casts ?settings ?show ?delay srcs =
  match K7.svg_of_casts ?settings ?show ?delay (List.map parse srcs) with
  | Ok s -> unprefixed s
  | Error e -> Alcotest.failf "%a" K7.pp_error e

let cast ?settings ?show ?delay src = casts ?settings ?show ?delay [ src ]

let text ?settings ?show srcs =
  match K7.text_of_casts ?settings ?show (List.map parse srcs) with
  | Ok s -> s
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

(* The same events, as intervals in version 3. *)
let timed_v3 =
  {|{"version": 3, "term": {"cols": 20, "rows": 3}}
# a comment
[0.0015, "o", "a"]
[0.001, "i", "x"]
[0, "o", "b"]
[0.120999, "o", "c"]
[0.000001, "o", "d"]
[0.877, "m", ""]

[0.9995, "o", "\r\ne"]
[0.0000004, "o", "f"]
|}

let test_v3 () =
  let settings = K7.[ Framerate 1000 ] in
  Alcotest.(check string)
    "intervals"
    (svg ~settings:K7.[ Columns 20; Rows 3; Framerate 1000 ] at_ms)
    (cast ~settings timed_v3)

let line cmd = "\027[38;2;90;86;224m$ \027[0m" ^ cmd ^ "\r\n"

let test_show () =
  let shifted d =
    (K7.Ms 0, line "make demo")
    :: List.map (fun (K7.Ms ms, s) -> (K7.Ms (ms + d), s)) at_ms
  in
  let settings = K7.[ Columns 20; Rows 3 ] in
  Alcotest.(check string)
    "500ms by default"
    (svg ~settings (shifted 500))
    (cast ~show:[ "make demo" ] timed);
  Alcotest.(check string)
    "delay"
    (svg ~settings (shifted 250))
    (cast ~show:[ "make demo" ] ~delay:(Ms 250) timed);
  Alcotest.(check string)
    "no delay without a line" (svg ~settings at_ms)
    (cast ~delay:(Ms 250) timed);
  let commanded =
    {|{"version": 3, "term": {"cols": 20, "rows": 3}, "command": "make demo"}|}
    ^ "\n" ^ {|[0.002, "o", "a"]|}
  in
  Alcotest.(check string)
    "the header's command"
    (svg ~settings
       K7.[ (Ms 0, line "make demo"); (Ms 502, "a"); (Ms 1502, "") ])
    (cast commanded);
  Alcotest.(check string)
    "an empty show hides it"
    (svg ~settings K7.[ (Ms 2, "a"); (Ms 1002, "") ])
    (cast ~show:[ "" ] commanded);
  Alcotest.(check string)
    "a command on several lines continues after > "
    (svg ~settings
       K7.[ (Ms 0, line "a\r\n>  b"); (Ms 502, "a"); (Ms 1502, "") ])
    (cast
       ({|{"version": 3, "term": {"cols": 20, "rows": 3}, "command": "a\n\tb"}|}
      ^ "\n" ^ {|[0.002, "o", "a"]|}));
  Alcotest.(check string)
    "a shown command on several lines continues after > "
    (svg ~settings K7.[ (Ms 0, line "a\r\n> b"); (Ms 502, "a"); (Ms 1502, "") ])
    (cast ~show:[ "a\nb" ]
       ({|{"version": 3, "term": {"cols": 20, "rows": 3}}|} ^ "\n"
      ^ {|[0.002, "o", "a"]|}));
  match K7.svg_of_casts ~show:[ "a\tb" ] [ parse timed ] with
  | Error (Invalid msg) ->
      Alcotest.(check string)
        "control"
        {|show "a\tb": holds a control character other than a newline|} msg
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

let resized =
  {|{"version": 3, "term": {"cols": 6, "rows": 2}}
[0.1, "o", "abcdef"]
[0.1, "r", "10x3"]
[0.1, "o", "\r\nghij"]
[0.1, "r", "4x2"]
[0.1, "x", "0"]
|}

let test_resize () =
  let image = cast resized in
  let frames = screens ~rows:3 image in
  Alcotest.(check (list string))
    "the terminal follows the resizes, on the largest canvas"
    [ "\n\n"; "abcdef\n\n"; "abcdef\n\n"; "abcdef\nghij\n"; "abcd\nghij\n" ]
    frames;
  check_contains "the image is as wide as ten columns"
    ~sub:
      (Fmt.str "width=\"%d\" height=\"%d\"><g" (10 * cell_width)
         (3 * cell_height))
    image;
  Alcotest.(check (list string))
    "--columns keeps the width, the rows still follow"
    [ "\n\n"; "abcdef\n\n"; "abcdef\n\n"; "abcdef\nghij\n"; "abcdef\nghij\n" ]
    (screens ~rows:3 (cast ~settings:K7.[ Columns 8 ] resized))

let test_exit () =
  let src =
    {|{"version": 3, "term": {"cols": 8, "rows": 2}}
[0.1, "o", "a"]
[2.5, "x", "3"]
|}
  in
  Alcotest.(check string)
    "the last screen holds from the exit"
    (svg ~settings:K7.[ Columns 8; Rows 2 ] K7.[ (Ms 100, "a"); (Ms 3600, "") ])
    (cast src)

let test_resize_last () =
  let src =
    {|{"version": 2, "width": 53, "height": 7, "title": "t"}
[0.313261, "o", "+;"]
[0.349305, "r", "53x7"]
|}
  in
  let settings = K7.[ Font_size 10 ] in
  Alcotest.(check string)
    "the last screen holds from the resize"
    (svg
       ~settings:K7.[ Columns 53; Rows 7; Font_size 10 ]
       K7.[ (Ms 313, "+;"); (Ms 1349, "") ])
    (cast ~settings src)

let test_idle () =
  let v2 =
    {|{"version": 2, "width": 8, "height": 2, "idle_time_limit": 0.5}
[0.1, "o", "a"]
[3, "o", "b"]
[3.25, "o", "c"]
|}
  and v3 =
    {|{"version": 3, "term": {"cols": 8, "rows": 2}, "idle_time_limit": 0.5}
[0.1, "o", "a"]
[2.9, "o", "b"]
[0.25, "o", "c"]
|}
  in
  let expected =
    svg
      ~settings:K7.[ Columns 8; Rows 2 ]
      K7.[ (Ms 100, "a"); (Ms 600, "b"); (Ms 850, "c"); (Ms 1850, "") ]
  in
  Alcotest.(check string) "v2" expected (cast v2);
  Alcotest.(check string) "v3" expected (cast v3)

let test_theme () =
  let palette =
    "#000001:#000002:#000003:#000004:#000005:#000006:#000007:#000008"
  in
  let src =
    Fmt.str
      {|{"version": 3, "term": {"cols": 8, "rows": 2, "theme": {"fg": "#abcdef", "bg": "#123456", "palette": "%s"}}}
[0.1, "o", "\u001b[31ma\u001b[91mb"]
|}
      palette
  in
  let image = cast src in
  check_contains "background" ~sub:"fill=\"#123456\"" image;
  check_contains "foreground" ~sub:"fill=\"#abcdef\"" image;
  check_contains "red" ~sub:"fill:#000002" image;
  check_contains "bright red is red with eight colours" ~sub:".p9{fill:#000002}"
    image;
  let themed = cast ~settings:K7.[ Theme "Dracula" ] src in
  Alcotest.(check bool)
    "--theme replaces it" false
    (contains ~sub:"#123456" themed);
  List.iter
    (fun (theme, expected) ->
      let src =
        {|{"version": 2, "width": 8, "height": 2, "theme": |} ^ theme ^ "}"
      in
      match K7.cast_of_string src with
      | Error (Invalid msg) -> Alcotest.(check string) theme expected msg
      | _ -> Alcotest.failf "%s: read" theme)
    [
      ( {|{"fg": "#abc", "bg": "#123456", "palette": ""}|},
        {|line 1: theme: "#abc" is not a colour #rrggbb|} );
      ( {|{"fg": "#abcdef", "bg": "#123456", "palette": "#000000:#ffffff"}|},
        "line 1: theme: a palette of 2 colours, not 8 or 16" );
    ]

let header = {|{"version": 2, "width": 8, "height": 2}|}
let header3 = {|{"version": 3, "term": {"cols": 8, "rows": 2}}|}

let test_errors () =
  List.iter
    (fun (src, expected) ->
      match K7.cast_of_string src with
      | Error (Invalid msg) -> Alcotest.(check string) src expected msg
      | Error e -> Alcotest.failf "%S: %a" src K7.pp_error e
      | Ok _ -> Alcotest.failf "%S: read" src)
    [
      ("", "line 1: no asciicast header");
      ("\n[0, \"o\", \"a\"]\n", "line 1: no asciicast header");
      ( {|{"version": 1, "width": 80, "height": 24, "stdout": []}|},
        "line 1: asciicast version 1, not 2 or 3" );
      ( {|{"version": "2", "width": 80, "height": 24}|},
        {|line 1: asciicast version "2", not 2 or 3|} );
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
      ( {|{"version": 3, "width": 80, "height": 24}|},
        "line 1: Missing member term in asciicast header object at byte 0" );
      ( {|{"version": 3, "term": {"cols": 80}}|},
        "line 1: Missing member rows in asciicast term object at byte 23" );
      ( {|{"version": 3, "term": {"cols": -1, "rows": 2}}|},
        "line 1: cols -1 is not a positive integer" );
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
      ( header ^ "\n# no comment\n",
        "line 2: Expected JSON value but found # at byte 0" );
      ( header3 ^ "\n[-0.5, \"o\", \"a\"]\n",
        "line 2: interval -0.5s is negative" );
      ( header3 ^ "\n[1, \"r\", \"80\"]\n",
        {|line 2: resize "80" is not COLSxROWS|} );
      ( header3 ^ "\n[1, \"r\", \"0x2\"]\n",
        {|line 2: resize "0x2" is not COLSxROWS|} );
      ( header3 ^ "\n[1, \"r\", \"+1x2\"]\n",
        {|line 2: resize "+1x2" is not COLSxROWS|} );
      ( header3 ^ "\n[1, \"x\", \"ok\"]\n",
        {|line 2: exit status "ok" is not an integer|} );
    ]

let read file = In_channel.with_open_bin file In_channel.input_all

(* asciinema 3.2.1: rec --headless -f asciicast-v2 --window-size 40x6 -c 'ls
   "$DIR"', DIR a directory of four empty files. *)
let test_asciinema_v2 () =
  let src = read "asciinema.cast" in
  let listing = "漢字.txt\tb.txt\r\na.txt\t\tcafé.md\r\n" in
  Alcotest.(check string)
    "the output events, after the command"
    (svg
       ~settings:K7.[ Columns 40; Rows 6 ]
       [ (Ms 0, line {|ls "$DIR"|}); (Ms 847, listing); (Ms 1851, "") ])
    (cast src);
  Alcotest.(check string)
    "the last screen"
    "$ ls \"$DIR\"\n漢字.txt        b.txt\na.txt           café.md\n"
    (text [ src ])

(* asciinema 3.2.1: rec --headless -f asciicast-v3 --window-size 40x6 -i 0.5
   -c "printf ...; sleep 1; seq 3; exit 2". *)
let test_asciinema_v3 () =
  let src = read "asciinema-v3.cast" in
  let command =
    {|printf '\033[1;32mgreen\033[0m and \033[34mblue\033[0m\n'; sleep 1; seq 3; exit 2|}
  in
  Alcotest.(check string)
    "intervals, the idle time limit and the exit"
    (svg
       ~settings:K7.[ Columns 40; Rows 6 ]
       [
         (Ms 0, line command);
         (Ms 755, "\027[1;32mgreen\027[0m and \027[34mblue\027[0m\r\n");
         (Ms 1255, "1\r\n2\r\n3\r\n");
         (Ms 2258, "");
       ])
    (cast src);
  Alcotest.(check string)
    "the last screen without the command" "green and blue\n1\n2\n3\n"
    (text ~show:[ "" ] [ src ])

let test_several () =
  let a =
    {|{"version": 3, "term": {"cols": 10, "rows": 3}, "command": "a"}
[0.1, "o", "1\r\n"]
[0.1, "x", "0"]
|}
  and b =
    {|{"version": 3, "term": {"cols": 12, "rows": 4}, "command": "b"}
[0.3, "o", "2\r\n"]
|}
  in
  Alcotest.(check (list string))
    "screens"
    [ "$ a\n\n\n"; "$ a\n1\n\n"; "$ a\n1\n$ b\n"; "1\n$ b\n2\n" ]
    (screens ~rows:4 (casts [ a; b ]));
  check_contains "timed" ~sub:"animation:s 3500ms" (casts [ a; b ]);
  Alcotest.(check string)
    "the last screen, scrolled" "1\n$ b\n2\n"
    (text [ a; b ]);
  Alcotest.(check string)
    "show replaces each command" "1\n$ c\n2\n"
    (text ~show:[ "c" ] [ a; b ]);
  Alcotest.(check string)
    "one show per recording" "1\n$ d\n2\n"
    (text ~show:[ "c"; "d" ] [ a; b ]);
  match K7.text_of_casts ~show:[ "c"; "d"; "e" ] [ parse a; parse b ] with
  | Error (Invalid msg) ->
      Alcotest.(check string)
        "a show per recording or one" "3 commands to show for 2 recordings" msg
  | _ -> Alcotest.fail "three shows for two recordings"

let test_round_trip () =
  let c = parse (read "asciinema-v3.cast") in
  let again = parse (K7.string_of_cast c) in
  Alcotest.(check string)
    "the same image"
    (Result.get_ok (K7.svg_of_casts [ c ]))
    (Result.get_ok (K7.svg_of_casts [ again ]));
  Alcotest.(check string)
    "v3, without the idle time limit"
    ({|{"version":3,"term":{"cols":40,"rows":6,"type":"xterm-256color"},"command":"printf '\\033[1;32mgreen\\033[0m and \\033[34mblue\\033[0m\\n'; sleep 1; seq 3; exit 2"}|}
   ^ "\n"
   ^ {|[0.255,"o","\u001B[1;32mgreen\u001B[0m and \u001B[34mblue\u001B[0m\r\n"]|}
   ^ "\n" ^ {|[0.500,"o","1\r\n2\r\n3\r\n"]|} ^ "\n" ^ {|[0.003,"x","2"]|}
   ^ "\n")
    (K7.string_of_cast c)

let test_cancelled_osc () =
  let src =
    {|{"version":2,"width":20,"height":3}
[0,"o","A\u001b]4;1;#123456\u0018B"]
|}
  in
  Alcotest.(check string)
    "CAN ends OSC before the next printable character" "AB\n" (text [ src ]);
  let src =
    {|{"version":2,"width":20,"height":3}
[0,"o","A\u001b]0;title\u001aB"]
|}
  in
  Alcotest.(check string)
    "SUB ends OSC before the next printable character" "AB\n" (text [ src ])

let test_cancelled_dcs () =
  let src =
    {|{"version":3,"term":{"cols":20,"rows":3}}
[0,"o","A\u001bPqpayload\u0018B"]
|}
  in
  Alcotest.(check string)
    "CAN cancels DCS before the following printable output" "AB\n"
    (text [ src ])

let test_text_conceal () =
  let src =
    header3 ^ "\n" ^ {|[0, "o", "a\u001b[8msecret\u001b[0mz"]|} ^ "\n"
  in
  Alcotest.(check (list string))
    "the SVG conceals the text" [ "a      z\n" ]
    (screens ~rows:2 (cast src));
  Alcotest.(check string)
    "text is the visible last screen" "a      z\n" (text [ src ])

let test_submicrosecond_time_order () =
  let src =
    {|{"version":2,"width":8,"height":2}
[0.0000004,"o","a"]
[0.0000001,"o","b"]
|}
  in
  match K7.cast_of_string src with
  | Error (K7.Invalid _) -> ()
  | Error e -> Alcotest.failf "expected Invalid: %a" K7.pp_error e
  | Ok _ -> Alcotest.fail "decreasing v2 timestamps must fail before rounding"

let suite =
  ( "asciicast",
    List.map
      (fun (name, fn) -> Alcotest.test_case name `Quick fn)
      [
        ("times", test_times);
        ( "submicrosecond timestamps remain ordered",
          test_submicrosecond_time_order );
        ("cancelled OSC preserves following text", test_cancelled_osc);
        ("cancelled DCS preserves following text", test_cancelled_dcs);
        ("v3", test_v3);
        ("show", test_show);
        ("size", test_size);
        ("resize", test_resize);
        ("exit", test_exit);
        ("a resize that ends a recording", test_resize_last);
        ("idle time limit", test_idle);
        ("theme", test_theme);
        ("errors", test_errors);
        ("asciinema v2", test_asciinema_v2);
        ("asciinema v3", test_asciinema_v3);
        ("several", test_several);
        ("round trip", test_round_trip);
        ("text conceals hidden cells", test_text_conceal);
      ] )
