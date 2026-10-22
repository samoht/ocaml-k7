(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let expect = Alcotest.(check string)
let outputs = [ "demo.gif" ]

(* VHS records no frame without a visible action. *)
let last = "Sleep 100ms\n"

let tape ?settings ?(outputs = outputs) actions =
  K7.tape ?settings ~outputs (actions @ [ K7.sleep (Ms 100) ])
  |> Result.map (fun t ->
      let s = K7.to_string t in
      String.sub s 0 (String.length s - String.length last))
  |> Result.map_error Native_test.invalid

let str ?settings ?outputs actions =
  match tape ?settings ?outputs actions with
  | Ok s -> s
  | Error e -> Alcotest.failf "rejected: %s" e

let body ?settings actions =
  let s = str ?settings actions in
  let header = "Output \"demo.gif\"\n" in
  if String.starts_with ~prefix:header s then
    String.sub s (String.length header) (String.length s - String.length header)
  else Alcotest.failf "no output line in %S" s

let rejects ?settings ?outputs name actions =
  match tape ?settings ?outputs actions with
  | Ok s -> Alcotest.failf "%s: accepted %S" name s
  | Error _ -> ()

let error ?settings ?outputs name expected actions =
  Alcotest.(check (result string string))
    name (Error expected)
    (tape ?settings ?outputs actions)

let test_basic_tape () =
  let open K7 in
  expect "tape"
    {|Output "demo.gif"
Set Shell "bash"
Set FontSize 18
Set Width 1000
Set Height 560
Set Theme "Builtin Dark"
# render from repo root
Require "dune"

Type "dune test"
Enter
Sleep 1500ms
|}
    (str
       ~settings:
         [
           Shell Bash;
           Font_size 18;
           Width 1000;
           Height 560;
           Theme "Builtin Dark";
         ]
       [
         raw "# render from repo root";
         raw {|Require "dune"|};
         raw "";
         type' "dune test";
         key Enter;
         sleep (Ms 1500);
       ])

let test_quotes () =
  let open K7 in
  expect "double" "Type 'printf \"hello\\n\" && echo \\done'\n"
    (body [ type' {|printf "hello\n" && echo \done|} ]);
  expect "backticks" "Type `echo \"it's\"`\n" (body [ type' {|echo "it's"|} ]);
  expect "three quotes" "Type `a\"b'c`\nType \"`d\"\n"
    (body [ type' {|a"b'c`d|} ]);
  expect "theme" "Set Theme '{\"name\": \"x\"}'\n"
    (body ~settings:[ Theme {|{"name": "x"}|} ] [])

(* VHS types it verbatim: [\t] reaches the shell as one backslash. *)
let test_backslash_verbatim () =
  expect "backslash"
    "Type \"docker ps --format 'table {{.Names}}\\t{{.Image}}'\"\n"
    (body [ K7.type' {|docker ps --format 'table {{.Names}}\t{{.Image}}'|} ])

let test_type_keys () =
  let open K7 in
  expect "tab and newline" "Type \"ls\"\nTab\nType \"x\"\nEnter\nEnter\n"
    (body [ type' "ls\tx\n\n" ]);
  expect "empty" "" (body [ type' "" ]);
  error "control"
    {|Type "bad\x01byte": holds a control character other than tab and newline|}
    [ type' "bad\x01byte" ];
  rejects "carriage return" [ type' "a\r\n" ];
  (* A terminal reads U+009B as CSI. *)
  error "C1 control"
    {|Type "X\u{9B}2J": holds a control character other than tab and newline|}
    [ type' "X\xc2\x9b2J" ];
  expect "not C1" "Type \"\xc2\xa0\xc3\x9b\"\n"
    (body [ type' "\xc2\xa0\xc3\x9b" ]);
  error "invalid UTF-8" "Type \"\u{FFFD}\": invalid UTF-8" [ type' "\xff" ];
  error "UTF-8 kept" ~outputs:[ "caf\u{e9}\".GIF" ]
    "Output \"caf\u{e9}\\\".GIF\": not .gif, .mp4, .webm, .txt, .ascii, .test, \
     .png or .svg; extensions are lower case"
    []

let test_line_injection () =
  let open K7 in
  let evil = "x\nOutput evil.gif" in
  rejects "output" ~outputs:[ "a.gif\nOutput evil.gif" ] [];
  rejects "theme" ~settings:[ Theme "a\x00" ] [];
  rejects "font" ~settings:[ Font_family evil ] [];
  rejects "raw" [ raw evil ];
  rejects "screen" [ wait (Screen evil) ];
  rejects "three quotes" ~outputs:[ {|a"b'c`d.gif|} ] [];
  rejects "C1 in output" ~outputs:[ "a\xc2\x85.gif" ] [];
  rejects "C1 in raw" [ raw "# a\xc2\x9bb" ];
  rejects "C1 in theme" ~settings:[ Theme "\xc2\x90x" ] [];
  rejects "C1 in screen" [ wait (Screen "a\xc2\x9db") ]

let test_raw () =
  let open K7 in
  expect "kept" "Set Margin 20\nEnv A \"b\"\n"
    (body [ raw "Set Margin 20"; raw {|Env A "b"|} ]);
  expect "typing speed after actions" "Enter\nSet TypingSpeed 10ms\nEnter\n"
    (body [ key Enter; raw "Set TypingSpeed 10ms"; key Enter ]);
  expect "comment after actions" "Enter\n# done\n"
    (body [ key Enter; raw "# done" ]);
  error "late setting"
    {|raw line "Set Width 320": VHS ignores it after the first action|}
    [ type' "x"; raw "Set Width 320" ];
  rejects "late require" [ sleep (Ms 1); raw "Require dune" ];
  (* VHS applies a Set Shell anywhere, and warns after the first action. *)
  error "late shell"
    {|raw line "Set Shell fish": use the setting Shell instead|}
    [ key Enter; raw "Set Shell fish" ];
  rejects "setting after env" [ raw "Env A b"; raw "Set Margin 2" ];
  rejects "setting in hidden" [ hidden [ raw "Set Margin 2" ] ];
  expect "setting after empty actions" "Set Margin 2\n"
    (body [ type' ""; hidden [ type' "" ]; raw "Set Margin 2" ]);
  error "hide" {|raw line "Hide": use hidden instead|} [ raw "Hide" ];
  rejects "show" [ raw "  Show" ]

let test_hidden () =
  let open K7 in
  expect "hidden" "Hide\nType \"a\"\nEnter\nShow\nType \"b\"\n"
    (body [ hidden [ type' "a"; hidden [ key Enter ]; hidden [] ]; type' "b" ]);
  expect "empty" "" (body [ hidden [] ])

let test_durations () =
  let open K7 in
  expect "sleep" "Sleep 25ms\nSleep 0ms\n"
    (body [ sleep (Ms 25); sleep (Ms 0) ]);
  error "negative" "Sleep -1ms: negative duration" [ sleep (Ms (-1)) ];
  error "zero timeout" "Wait@0ms: the timeout must be positive"
    [ wait ~timeout:(Ms 0) Prompt ];
  rejects "negative timeout" [ wait ~timeout:(Ms (-1)) Prompt ];
  (* Go durations are signed 64-bit nanoseconds. *)
  expect "largest" "Wait@9223372036854ms\n"
    (body [ wait ~timeout:(Ms 9_223_372_036_854) Prompt ]);
  rejects "overflowing timeout" [ wait ~timeout:(Ms 9_223_372_036_855) Prompt ];
  rejects "overflowing sleep" [ sleep (Ms 9_223_372_036_855) ];
  rejects "typing speed" ~settings:[ Typing_speed (Ms (-1)) ] []

let test_settings () =
  let open K7 in
  expect "all"
    "Set Shell \"fish\"\n\
     Set FontFamily \"JetBrains Mono\"\n\
     Set Padding 0\n\
     Set Width 10\n\
     Set Height 10\n\
     Set TypingSpeed 20ms\n\
     Set Framerate 24\n\
     Set WaitTimeout 3000ms\n"
    (body
       ~settings:
         [
           Shell Fish;
           Font_family "JetBrains Mono";
           Padding 0;
           Width 10;
           Height 10;
           Typing_speed (Ms 20);
           Framerate 24;
           Wait_timeout (Ms 3000);
         ]
       []);
  error "small width" "Set Width 100: below twice the default padding (60)"
    ~settings:[ Width 100 ] [];
  error "small height" "Set Height 100: below twice the padding (60)"
    ~settings:[ Padding 60; Height 100 ] [];
  error "padding and default height"
    "Set Padding 400: above half the default height (600)"
    ~settings:[ Padding 400; Width 1000 ]
    [];
  error "padding and default width"
    "Set Padding 700: above half the default width (1200)"
    ~settings:[ Padding 700; Height 2000 ]
    [];
  rejects "negative padding" ~settings:[ Padding (-1) ] [];
  rejects "font size" ~settings:[ Font_size 0 ] [];
  rejects "framerate" ~settings:[ Framerate 0 ] [];
  (* VHS puts the font list in single quotes in browser code. *)
  rejects "font quote" ~settings:[ Font_family "x'; alert(1); '" ] [];
  rejects "font backslash" ~settings:[ Font_family {|x\|} ] [];
  (* VHS fits twice the padding and margin, and the window bar's height. *)
  error "margin"
    "Set Margin 300: the frame needs at least 720 x 720 pixels, not 1200 x 600"
    [ raw "Set Margin 300" ];
  error "window bar"
    "Set WindowBar Colorful: the frame needs at least 120 x 150 pixels, not \
     1200 x 149"
    ~settings:[ Height 149 ]
    [ raw "Set WindowBar Colorful" ];
  expect "negative margin"
    "Set Padding 400\nSet Width 1000\nSet Margin \"-100\"\n"
    (body ~settings:[ Padding 400; Width 1000 ] [ raw {|Set Margin "-100"|} ]);
  expect "bar size without a bar" "Set Height 120\nSet WindowBarSize 100\n"
    (body ~settings:[ Height 120 ] [ raw "Set WindowBarSize 100" ]);
  (* ffmpeg encodes an MP4 only with an even width and height. *)
  error "odd MP4 width" "Set Width 601: odd, which an MP4 output cannot have"
    ~outputs:[ "a.mp4" ] ~settings:[ Width 601 ] [];
  error "odd MP4 height" "Set Height 301: odd, which an MP4 output cannot have"
    ~settings:[ Height 301 ]
    [ raw "Output a.mp4" ];
  expect "odd GIF and WebM"
    "Output \"a.gif\"\nOutput \"a.webm\"\nSet Width 601\n"
    (str ~outputs:[ "a.gif"; "a.webm" ] ~settings:[ Width 601 ] []);
  (* VHS sizes the frame by its pixels or by its cells, not both. *)
  expect "rows" "Set Width 800\nSet Rows 10\n"
    (body ~settings:[ Width 800; Rows 10 ] []);
  expect "columns" "Set Columns 100\nSet Height 400\n"
    (body ~settings:[ Columns 100; Height 400 ] []);
  error "rows and height" "Set Rows 10: VHS cannot set both Height and Rows"
    ~settings:[ Height 400; Rows 10 ] [];
  error "columns and width"
    "Set Columns 80: VHS cannot set both Width and Columns"
    ~settings:[ Columns 80; Width 800 ] [];
  error "zero columns" "Set Columns 0: not positive" ~settings:[ Columns 0 ] [];
  error "negative rows" "Set Rows -1: not positive" ~settings:[ Rows (-1) ] [];
  error "raw columns"
    {|raw line "Set Columns 80": use the setting Columns instead|}
    [ raw "Set Columns 80" ];
  error "raw wait timeout"
    {|raw line "Set WaitTimeout 3s": use the setting Wait_timeout instead|}
    [ raw "Set WaitTimeout 3s" ];
  error "negative wait timeout" "Set WaitTimeout -1ms: negative duration"
    ~settings:[ Wait_timeout (Ms (-1)) ] []

(* VHS fails with "no frames" on a recording without a visible action. *)
let test_frames () =
  let open K7 in
  let frames name expected actions =
    Alcotest.(check (result string string))
      name expected
      (K7.tape ~outputs:[ "demo.gif" ] actions
      |> Result.map K7.to_string
      |> Result.map_error Native_test.invalid)
  in
  let none = Error "no action outside hidden, so VHS records no frame" in
  frames "no action" none [];
  frames "hidden only" none [ hidden [ type' "ls\n" ]; type' "" ];
  frames "settings only" none [ raw "Set Margin 2"; raw "# done" ];
  frames "sleep" (Ok "Output \"demo.gif\"\nSleep 100ms\n") [ sleep (Ms 100) ];
  frames "raw" (Ok "Output \"demo.gif\"\nEnv A \"b\"\n") [ raw {|Env A "b"|} ]

let test_outputs () =
  expect "no output" last (str ~outputs:[] [ K7.sleep (Ms 100) ]);
  error "no extension"
    {|Output "demo": neither an extension nor a trailing slash|}
    ~outputs:[ "demo" ] [];
  rejects "dotted directory" ~outputs:[ "a.b/c" ] [];
  expect "several" "Output \"frames/\"\nOutput \"x.txt\"\n"
    (str ~outputs:[ "frames/"; "x.txt" ] []);
  List.iter
    (fun path -> ignore (str ~outputs:[ path ] []))
    [ "a.gif"; "a.mp4"; "a.webm"; "a.txt"; "a.ascii"; "a.test"; "a.png" ];
  (* VHS keeps one output of each kind, the last one. *)
  error "two GIFs"
    {|Output "a.gif": VHS writes only the last GIF output, "b.gif"|}
    ~outputs:[ "a.gif"; "b.gif" ] [];
  error "two texts"
    {|Output "a.txt": VHS writes only the last text output, "b.ascii"|}
    ~outputs:[ "a.txt"; "c.gif"; "b.ascii" ]
    [];
  error "two frame directories"
    {|Output "a/": VHS writes only the last frames output, "b.png"|}
    ~outputs:[ "a/"; "b.png" ] [];
  (* VHS never reads an .svg output. *)
  error "two SVGs"
    {|Output "a.svg": a tape has one SVG output, not also "b.svg"|}
    ~outputs:[ "a.svg"; "b.svg" ] [];
  rejects "raw output" [ K7.raw "Output b.mp4" ] ~outputs:[ "a.mp4" ];
  (* VHS hands other extensions to ffmpeg, which picks the format. *)
  error "unknown extension"
    {|Output "demo.gfi": not .gif, .mp4, .webm, .txt, .ascii, .test, .png or .svg|}
    ~outputs:[ "demo.gfi" ] [];
  (* ffmpeg reads an output starting with a dash as an option. *)
  error "dash"
    {|Output "-y.gif": starts with -, which ffmpeg reads as an option|}
    ~outputs:[ "-y.gif" ] []

let test_keys () =
  let open K7 in
  expect "keys" "Ctrl+c\nCtrl+\\\nAlt+.\nPageUp\nScrollDown\n"
    (body
       [
         key (Ctrl 'c');
         key (Ctrl '\\');
         key (Alt '.');
         key Page_up;
         key Scroll_down;
       ]);
  error "ctrl digit" "Ctrl+1: not a VHS key" [ key (Ctrl '1') ];
  error "alt backslash" {|Alt+\\: not a VHS key|} [ key (Alt '\\') ];
  rejects "ctrl newline" [ key (Ctrl '\n') ];
  (* VHS lexes a bare m or s as a time unit. *)
  expect "ctrl m and s" "Ctrl+\"m\"\nCtrl+\"s\"\nCtrl+S\n"
    (body [ key (Ctrl 'm'); key (Ctrl 's'); key (Ctrl 'S') ]);
  error "alt s" "Alt+s: VHS cannot press it" [ key (Alt 's') ];
  rejects "alt m" [ key (Alt 'm') ]

let test_waits () =
  let open K7 in
  expect "waits"
    "Wait+Screen /ready/\n\
     Wait\n\
     Wait@500ms\n\
     Wait+Screen@2000ms /ok$/\n\
     Wait+Line@10ms /a\\x2fb$/\n"
    (body
       [
         wait (Screen "ready");
         wait Prompt;
         wait ~timeout:(Ms 500) Prompt;
         wait ~timeout:(Ms 2000) (Screen "ok$");
         wait ~timeout:(Ms 10) (Line "a/b$");
       ]);
  error "invalid line" {|Wait+Line "(": missing closing ): `(`|}
    [ wait (Line "(") ];
  error "invalid" {|Wait+Screen "[": missing closing ]: `[`|}
    [ wait (Screen "[") ];
  rejects "line break" [ wait (Screen "a\nb") ]

(* Validation only: the oracle must never record or publish. *)
let validate_accepted actions =
  match K7.tape ~outputs:[ "demo.gif" ] actions with
  | Error _ -> ()
  | Ok t ->
      let path = Filename.temp_file "vhs-validate" ".tape" in
      Fun.protect
        ~finally:(fun () -> Sys.remove path)
        (fun () ->
          let oc = open_out_bin path in
          output_string oc (K7.to_string t);
          close_out oc;
          Alcotest.(check int)
            "accepted tape validates" 0
            (Sys.command
               ("vhs validate " ^ Filename.quote path ^ " > /dev/null 2>&1")))

let test_raw_validation () = validate_accepted [ K7.raw "Sleep -1ms" ]

let test_padding_overflow () =
  rejects "padding exceeds the default frame" ~settings:[ K7.Padding max_int ]
    []

let test_raw_margin_overflow () =
  rejects "raw margin exceeds the default frame"
    [ K7.raw "Set Margin 4611686018427387903" ]

(* VHS's browser takes a viewport of 0 to 10,000,000 pixels a side; vhs
   panics on another. *)
let test_viewport () =
  rejects "wider than the browser" ~settings:[ K7.Width 10_000_121 ] [];
  rejects "taller than the browser" ~settings:[ K7.Height 10_000_121 ] [];
  rejects "widened by a negative margin" [ K7.raw {|Set Margin "-5000000"|} ];
  rejects "negative without a margin fill" ~settings:[ K7.Width 100 ]
    [ K7.raw {|Set MarginFill ""|}; K7.raw {|Set Margin "-100"|} ];
  ignore (str ~outputs:[ "a.txt" ] ~settings:[ K7.Width 10_000_120 ] []);
  (* With [Set Rows], VHS first measures the cells in that viewport. *)
  rejects "rows measured in a viewport the browser refuses"
    [
      K7.raw "Set WindowBar Colorful"; K7.raw {|Set WindowBarSize "-20000000"|};
    ]
    ~settings:[ K7.Rows 10 ];
  (* VHS measures cells at up to 32 times the viewport, a pixel at least. *)
  ignore (body ~settings:[ K7.Columns 34560 ] []);
  ignore (body ~settings:[ K7.Rows 15360 ] []);
  rejects "more columns than VHS measures" ~settings:[ K7.Columns 34561 ] [];
  rejects "more rows than VHS measures" ~settings:[ K7.Rows 15361 ] [];
  rejects "rows in an empty viewport"
    ~settings:[ K7.Padding 0; K7.Rows 1 ]
    [ K7.raw "Set Margin 300" ]

(* ffmpeg's limits: GIF 65535 pixels a side, libx264 16384, libvpx 65536,
   and (8 width + 1024) (height + 128) below 2{^31}. *)
let test_encoders () =
  let accepts name ~outputs settings =
    match K7.tape ~settings ~outputs [ K7.sleep (Ms 100) ] with
    | Ok _ -> ()
    | Error e -> Alcotest.failf "%s: %a" name K7.pp_error e
  and refuses name ~outputs settings =
    match K7.tape ~settings ~outputs [ K7.sleep (Ms 100) ] with
    | Ok _ -> Alcotest.failf "%s: accepted" name
    | Error _ -> ()
  in
  let open K7 in
  accepts "GIF" ~outputs:[ "a.gif" ] [ Width 65535 ];
  refuses "wide GIF" ~outputs:[ "a.gif" ] [ Width 65536 ];
  refuses "tall GIF" ~outputs:[ "a.gif" ] [ Height 65536 ];
  accepts "MP4" ~outputs:[ "a.mp4" ] [ Width 16384; Height 16000 ];
  refuses "wide MP4" ~outputs:[ "a.mp4" ] [ Width 16386 ];
  refuses "tall MP4" ~outputs:[ "a.mp4" ] [ Height 16386 ];
  accepts "WebM" ~outputs:[ "a.webm" ] [ Width 65536 ];
  refuses "wide WebM" ~outputs:[ "a.webm" ] [ Width 65538 ];
  refuses "too many pixels" ~outputs:[ "a.gif" ] [ Width 65534; Height 4000 ];
  accepts "text" ~outputs:[ "a.txt" ] [ Width 70000 ]

let test_of_string () =
  let read src = Result.map_error Native_test.invalid (K7.of_string src) in
  (match
     read
       "Output demo.svg\r\n\
        Output demo.txt\n\
        Set FontSize 18\n\
        Set Shell zsh\n\
        Set TypingSpeed 0.5s\n\
        Set WaitTimeout 3s\n\
        # a comment\n\
        Hide\n\
        Type \"cd /tmp\"\n\
        Enter\n\
        Show\n\
        Type \"ls\"\n\
        Sleep 1s\n"
   with
  | Error e -> Alcotest.failf "rejected: %s" e
  | Ok t ->
      expect "printed"
        "Output \"demo.txt\"\n\
         Set FontSize 18\n\
         Set Shell \"zsh\"\n\
         Set TypingSpeed 500ms\n\
         Set WaitTimeout 3000ms\n\
         # a comment\n\
         Hide\n\
         Type \"cd /tmp\"\n\
         Enter\n\
         Show\n\
         Type \"ls\"\n\
         Sleep 1s\n"
        (K7.to_string t));
  (match
     Result.bind
       (K7.of_string
          "Output a.gif\n\
           Set Width 900\n\
           Set WaitTimeout 2m\n\
           Set FontSize 18\n\
           Type x\n") (fun t ->
         Result.bind
           (K7.with_settings
              K7.[ Columns 100; Font_size 12; Wait_timeout (Ms 5) ]
              t)
           (K7.with_outputs [ "b.txt" ]))
   with
  | Ok t ->
      Alcotest.(check (list string)) "outputs" [ "b.txt" ] (K7.outputs t);
      (match K7.settings t with
      | [ Columns 100; Font_size 12; Wait_timeout (Ms 5) ] -> ()
      | _ -> Alcotest.fail "settings");
      expect "replaced"
        "Output \"b.txt\"\n\
         Set Columns 100\n\
         Set FontSize 12\n\
         Set WaitTimeout 5ms\n\
         Type x\n"
        (K7.to_string t)
  | Error e -> Alcotest.failf "%a" K7.pp_error e);
  (* VHS ignores a setting after the first action. *)
  Alcotest.(check (result string string))
    "late setting"
    (Error {|raw line "Set FontSize 10": VHS ignores it after the first action|})
    (Result.map K7.to_string (read "Output a.gif\nType x\nSet FontSize 10\n"));
  Alcotest.(check (result string string))
    "sub-millisecond wait timeout"
    (Error
       {|line 1: "Set WaitTimeout 1.5ms": not a whole number of milliseconds|})
    (Result.map K7.to_string (read "Set WaitTimeout 1.5ms\nType x\n"));
  Alcotest.(check (result string string))
    "line number" (Error {|line 2: "Type \"x": unterminated string|})
    (Result.map K7.to_string (read "Output a.gif\nType \"x\n"));
  Alcotest.(check (result string string))
    "unknown shell"
    (Error {|line 1: "Set Shell \"tcsh\"": not a shell VHS starts|})
    (Result.map K7.to_string
       (read "Set Shell \"tcsh\"\nOutput a.gif\nType x\n"))

let test_of_string_outputs () =
  let pp_error = Fmt.to_to_string K7.pp_error in
  let read ?(outputs = [ "a.txt" ]) src =
    Result.bind (K7.of_string src) (K7.with_outputs outputs)
    |> Result.map K7.to_string |> Result.map_error pp_error
  in
  Alcotest.(check (result string string))
    "no Output line" (Ok "Output \"a.txt\"\nType \"hi\"\nSleep 1s\n")
    (read "Type \"hi\"\nSleep 1s\n");
  (match K7.tape ~outputs:[ "a.svg" ] K7.[ type' "hi"; sleep (Ms 1000) ] with
  | Error e -> Alcotest.failf "%a" K7.pp_error e
  | Ok t -> (
      match read ~outputs:[ "a.svg" ] (K7.to_string t) with
      | Ok _ -> ()
      | Error e -> Alcotest.failf "printed SVG tape: %s" e));
  match
    K7.of_string "Env FOO \"bar\"\nOutput a.txt\nType \"hi\"\nSleep 1s\n"
  with
  | Ok _ -> ()
  | Error e -> Alcotest.failf "Output after Env: %a" K7.pp_error e

let suite =
  ( "k7",
    List.map
      (fun (name, fn) -> Alcotest.test_case name `Quick fn)
      [
        ("padding overflow", test_padding_overflow);
        ("raw margin overflow", test_raw_margin_overflow);
        ("viewport", test_viewport);
        ("encoders", test_encoders);
        ("raw validation", test_raw_validation);
        ("basic tape", test_basic_tape);
        ("quotes", test_quotes);
        ("backslash verbatim", test_backslash_verbatim);
        ("type keys", test_type_keys);
        ("line injection", test_line_injection);
        ("raw", test_raw);
        ("hidden", test_hidden);
        ("durations", test_durations);
        ("settings", test_settings);
        ("frames", test_frames);
        ("outputs", test_outputs);
        ("keys", test_keys);
        ("waits", test_waits);
        ("of_string", test_of_string);
        ("of_string without outputs", test_of_string_outputs);
      ] )
