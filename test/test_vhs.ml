(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let expect = Alcotest.(check string)
let outputs = [ "demo.gif" ]

(* VHS records no frame without a visible action, so each tape ends with
   [last], which [tape] does not print. *)
let last = "Sleep 100ms\n"

let tape ?settings ?(outputs = outputs) actions =
  Vhs.tape ?settings ~outputs (actions @ [ Vhs.sleep (Ms 100) ])
  |> Result.map (fun t ->
      let s = Vhs.to_string t in
      String.sub s 0 (String.length s - String.length last))

let str ?settings ?outputs actions =
  match tape ?settings ?outputs actions with
  | Ok s -> s
  | Error e -> Alcotest.failf "rejected: %s" e

(* The printed actions, without the [Output] line. *)
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
  let open Vhs in
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
  let open Vhs in
  expect "double" "Type 'printf \"hello\\n\" && echo \\done'\n"
    (body [ type' {|printf "hello\n" && echo \done|} ]);
  expect "backticks" "Type `echo \"it's\"`\n" (body [ type' {|echo "it's"|} ]);
  expect "three quotes" "Type `a\"b'c`\nType \"`d\"\n"
    (body [ type' {|a"b'c`d|} ]);
  expect "theme" "Set Theme '{\"name\": \"x\"}'\n"
    (body ~settings:[ Theme {|{"name": "x"}|} ] [])

(* A docker format string forces double quotes and VHS types it verbatim, so
   the [\t] must reach the shell as one backslash. *)
let test_backslash_verbatim () =
  expect "backslash"
    "Type \"docker ps --format 'table {{.Names}}\\t{{.Image}}'\"\n"
    (body [ Vhs.type' {|docker ps --format 'table {{.Names}}\t{{.Image}}'|} ])

let test_type_keys () =
  let open Vhs in
  expect "tab and newline" "Type \"ls\"\nTab\nType \"x\"\nEnter\nEnter\n"
    (body [ type' "ls\tx\n\n" ]);
  expect "empty" "" (body [ type' "" ]);
  error "control"
    {|Type "bad\001byte": holds a control character other than tab and newline|}
    [ type' "bad\x01byte" ];
  rejects "carriage return" [ type' "a\r\n" ];
  (* A terminal reads U+009B as CSI: this would clear the screen. *)
  error "C1 control"
    {|Type "X\194\1552J": holds a control character other than tab and newline|}
    [ type' "X\xc2\x9b2J" ];
  expect "not C1" "Type \"\xc2\xa0\xc3\x9b\"\n"
    (body [ type' "\xc2\xa0\xc3\x9b" ]);
  error "invalid UTF-8" {|Type "\255": invalid UTF-8|} [ type' "\xff" ]

let test_line_injection () =
  let open Vhs in
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
  let open Vhs in
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
  (* An action that prints nothing does not end the settings. *)
  expect "setting after empty actions" "Set Margin 2\n"
    (body [ type' ""; hidden [ type' "" ]; raw "Set Margin 2" ]);
  error "hide" {|raw line "Hide": use hidden instead|} [ raw "Hide" ];
  rejects "show" [ raw "  Show" ]

let test_hidden () =
  let open Vhs in
  expect "hidden" "Hide\nType \"a\"\nEnter\nShow\nType \"b\"\n"
    (body [ hidden [ type' "a"; hidden [ key Enter ]; hidden [] ]; type' "b" ]);
  expect "empty" "" (body [ hidden [] ])

let test_durations () =
  let open Vhs in
  expect "sleep" "Sleep 25ms\nSleep 0ms\n"
    (body [ sleep (Ms 25); sleep (Ms 0) ]);
  error "negative" "Sleep -1ms: negative duration" [ sleep (Ms (-1)) ];
  error "zero timeout" "Wait@0ms: the timeout must be positive"
    [ wait ~timeout:(Ms 0) Prompt ];
  rejects "negative timeout" [ wait ~timeout:(Ms (-1)) Prompt ];
  (* Go durations are signed 64-bit nanoseconds; one more millisecond
     overflows and fails [vhs validate]. *)
  expect "largest" "Wait@9223372036854ms\n"
    (body [ wait ~timeout:(Ms 9_223_372_036_854) Prompt ]);
  rejects "overflowing timeout" [ wait ~timeout:(Ms 9_223_372_036_855) Prompt ];
  rejects "overflowing sleep" [ sleep (Ms 9_223_372_036_855) ];
  rejects "typing speed" ~settings:[ Typing_speed (Ms (-1)) ] []

let test_settings () =
  let open Vhs in
  expect "all"
    "Set Shell \"fish\"\n\
     Set FontFamily \"JetBrains Mono\"\n\
     Set Padding 0\n\
     Set Width 10\n\
     Set Height 10\n\
     Set TypingSpeed 20ms\n\
     Set Framerate 24\n"
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
         ]
       []);
  error "small width" "Set Width 100: below twice the default padding (60)"
    ~settings:[ Width 100 ] [];
  error "small height" "Set Height 100: below twice the padding (60)"
    ~settings:[ Padding 60; Height 100 ] [];
  (* The size the user did not set is not named as an offending line. *)
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
  (* VHS fits twice the padding and the margin in the frame, and the window
     bar too in its height. *)
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
    (body ~settings:[ Width 800 ] [ raw "Set Rows 10" ]);
  error "rows and height" "Set Rows 10: VHS cannot set both Height and Rows"
    ~settings:[ Height 400 ]
    [ raw "Set Rows 10" ];
  error "columns and width"
    "Set Columns 80: VHS cannot set both Width and Columns"
    ~settings:[ Width 800 ]
    [ raw "Set Columns 80" ]

(* VHS fails with "no frames" on a recording without a visible action. *)
let test_frames () =
  let open Vhs in
  let frames name expected actions =
    Alcotest.(check (result string string))
      name expected
      (Vhs.tape ~outputs actions |> Result.map Vhs.to_string)
  in
  let none = Error "no action outside hidden, so VHS records no frame" in
  frames "no action" none [];
  frames "hidden only" none [ hidden [ type' "ls\n" ]; type' "" ];
  frames "settings only" none [ raw "Set Margin 2"; raw "# done" ];
  frames "sleep" (Ok "Output \"demo.gif\"\nSleep 100ms\n") [ sleep (Ms 100) ];
  frames "raw" (Ok "Output \"demo.gif\"\nEnv A \"b\"\n") [ raw {|Env A "b"|} ]

let test_outputs () =
  error "no output" "no output" ~outputs:[] [];
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
  rejects "raw output" [ Vhs.raw "Output b.mp4" ] ~outputs:[ "a.mp4" ];
  (* VHS hands any other extension to ffmpeg as a GIF, and ffmpeg picks the
     format from the extension. *)
  error "unknown extension"
    {|Output "demo.gfi": not .gif, .mp4, .webm, .txt, .ascii, .test, .png or .svg|}
    ~outputs:[ "demo.gfi" ] [];
  (* ffmpeg reads its last argument, the output, as an option if it starts
     with a dash. *)
  error "dash"
    {|Output "-y.gif": starts with -, which ffmpeg reads as an option|}
    ~outputs:[ "-y.gif" ] []

let test_keys () =
  let open Vhs in
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
  let open Vhs in
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

(* Only validation runs here: the oracle must never record or publish. *)
let validate_accepted actions =
  match Vhs.tape ~outputs:[ "demo.gif" ] actions with
  | Error _ -> ()
  | Ok t ->
      let path = Filename.temp_file "vhs-validate" ".tape" in
      Fun.protect
        ~finally:(fun () -> Sys.remove path)
        (fun () ->
          let oc = open_out_bin path in
          output_string oc (Vhs.to_string t);
          close_out oc;
          Alcotest.(check int)
            "accepted tape validates" 0
            (Sys.command
               ("vhs validate " ^ Filename.quote path ^ " > /dev/null 2>&1")))

let test_raw_validation () = validate_accepted [ Vhs.raw "Sleep -1ms" ]

let test_padding_overflow () =
  rejects "padding exceeds the default frame" ~settings:[ Vhs.Padding max_int ]
    []

(* Adding twice a raw margin to the default padding must not wrap. *)
let test_raw_margin_overflow () =
  rejects "raw margin exceeds the default frame"
    [ Vhs.raw "Set Margin 4611686018427387903" ]

(* The browser VHS drives takes a viewport, the frame less twice the padding
   and, unless [MarginFill] is empty, twice the margin, from 0 to 10,000,000
   pixels a side; vhs panics on another. *)
let test_viewport () =
  rejects "wider than the browser" ~settings:[ Vhs.Width 10_000_121 ] [];
  rejects "taller than the browser" ~settings:[ Vhs.Height 10_000_121 ] [];
  rejects "widened by a negative margin" [ Vhs.raw {|Set Margin "-5000000"|} ];
  rejects "negative without a margin fill" ~settings:[ Vhs.Width 100 ]
    [ Vhs.raw {|Set MarginFill ""|}; Vhs.raw {|Set Margin "-100"|} ];
  ignore (str ~outputs:[ "a.txt" ] ~settings:[ Vhs.Width 10_000_120 ] []);
  (* With [Set Rows], VHS first measures the cells in that viewport. *)
  rejects "rows measured in a viewport the browser refuses"
    [
      Vhs.raw "Set WindowBar Colorful";
      Vhs.raw {|Set WindowBarSize "-20000000"|};
      Vhs.raw "Set Rows 10";
    ];
  (* VHS measures the cells at up to 32 times the viewport, where a cell
     takes at least a pixel: the 1080 x 480 pixels of the default frame
     less its padding hold at most 34560 columns and 15360 rows. *)
  ignore (body [ Vhs.raw "Set Columns 34560" ]);
  ignore (body [ Vhs.raw "Set Rows 15360" ]);
  rejects "more columns than VHS measures" [ Vhs.raw "Set Columns 34561" ];
  rejects "more rows than VHS measures" [ Vhs.raw "Set Rows 15361" ];
  rejects "rows in an empty viewport" ~settings:[ Vhs.Padding 0 ]
    [ Vhs.raw "Set Margin 300"; Vhs.raw "Set Rows 1" ]

(* ffmpeg encodes a GIF of at most 65535 pixels a side, libx264 an MP4 of
   16384 and libvpx a WebM of 65536, and takes a frame of at most about
   268 million pixels: (8 width + 1024) (height + 128) below 2{^31}. *)
let test_encoders () =
  let accepts name ~outputs settings =
    match Vhs.tape ~settings ~outputs [ Vhs.sleep (Ms 100) ] with
    | Ok _ -> ()
    | Error e -> Alcotest.failf "%s: %s" name e
  and refuses name ~outputs settings =
    match Vhs.tape ~settings ~outputs [ Vhs.sleep (Ms 100) ] with
    | Ok _ -> Alcotest.failf "%s: accepted" name
    | Error _ -> ()
  in
  let open Vhs in
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

let suite =
  ( "vhs",
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
      ] )
