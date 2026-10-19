(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let expect = Alcotest.(check string)
let outputs = [ "demo.gif" ]

let tape ?settings ?(outputs = outputs) actions =
  Vhs.tape ?settings ~outputs actions |> Result.map (Fmt.str "%a" Vhs.pp)

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
  rejects "setting after env" [ raw "Env A b"; raw "Set Margin 2" ];
  rejects "setting in hidden" [ hidden [ raw "Set Margin 2" ] ];
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
  rejects "font backslash" ~settings:[ Font_family {|x\|} ] []

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
    {|Output "demo.gfi": not .gif, .mp4, .webm, .txt, .ascii, .test or .png|}
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

let suite =
  ( "vhs",
    List.map
      (fun (name, fn) -> Alcotest.test_case name `Quick fn)
      [
        ("padding overflow", test_padding_overflow);
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
        ("outputs", test_outputs);
        ("keys", test_keys);
        ("waits", test_waits);
      ] )
