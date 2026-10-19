(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The checker is internal: test it through [Vhs.raw]. *)

let tape actions = Vhs.tape ~outputs:[ "demo.gif" ] actions |> Result.map ignore

(* A raw line is one command VHS's parser reads without error, and that
   takes nothing from the line after it. *)
let test_accepted () =
  List.iter
    (fun line ->
      match tape [ Vhs.raw line ] with
      | Ok _ -> ()
      | Error e -> Alcotest.failf "rejected %S: %s" line e)
    [
      "Ctrl+Alt+c";
      "Ctrl+[";
      "Alt+Enter";
      "Shift+Tab";
      "Enter@10ms 3 # three times";
      {|Wait+Line /x\\/|};
      {|Wait+Screen@2s /a\/b/|};
      "Screenshot shot.png";
      {|Copy "a" 'b'|};
      "Paste";
      "Sleep .5";
      "Sleep 1.5m";
      "Output frames/";
      "Set WindowBar Colorful";
      {|Set MarginFill "#ff00ff"|};
      "Set CursorBlink false";
      "Set LoopOffset 20%";
      "Set WaitPattern /ready/";
      "Set LetterSpacing 1.2";
    ]

let fails name line msg =
  Alcotest.(check (result unit string))
    name
    (Fmt.kstr Result.error "raw line %S: %s" line msg)
    (tape [ Vhs.raw line ])

let test_rejected () =
  fails "negative sleep" "Sleep -1ms" "Expected time after Sleep";
  fails "bad duration" "Sleep 1.2.3" "time 1.2.3s: not a Go duration";
  fails "zero wait" "Wait@0ms" "Wait expects positive duration";
  fails "two commands" "Enter Hide" "more than one command";
  fails "unterminated JSON" "Set MarginFill {" "unterminated JSON value";
  fails "unterminated string" {|Type "abc|} "unterminated string";
  fails "unterminated regex" {|Wait /abc\/|} "unterminated regular expression";
  fails "missing argument" "Set Margin" "Margin expects an argument";
  fails "env without value" "Env A" "A expects string";
  fails "unknown command" "Run x" "Invalid command: Run";
  fails "unknown setting" "Set Colour red" "Unknown setting: Colour";
  fails "bad bar" "Set WindowBar Square" "Square is not a valid bar style.";
  fails "bad color" {|Set MarginFill "#zz0000"|}
    {|"#zz0000" is not a valid color.|};
  fails "ctrl word" "Ctrl+ab" "Invalid control argument: ab";
  fails "late modifier" "Ctrl+c+Alt"
    "Modifiers must come before other characters";
  fails "screenshot" "Screenshot a.gif" "Expected file with .png extension";
  fails "source" "Source a.txt" "Expected file with .tape extension";
  fails "regex" "Wait /[/"
    "Invalid regular expression '[': missing closing ]: `[`";
  fails "font family" {|Set FontFamily "x'"|}
    "use the setting Font_family instead";
  fails "padding" "Set Padding 400" "use the setting Padding instead";
  fails "output" "Output x.foo"
    {|Output "x.foo": not .gif, .mp4, .webm, .txt, .ascii, .test or .png|}

(* Lines [vhs validate] judged one by one, written by tape_oracle.go: a line
   accepted here is one VHS reads without error. *)
let test_oracle () =
  let cases = In_channel.with_open_bin "tape_cases.txt" In_channel.input_all in
  String.split_on_char '\n' cases
  |> List.iter (fun l ->
      if l <> "" then
        let line = String.sub l 2 (String.length l - 2) in
        match tape [ Vhs.raw line ] with
        | Ok () when l.[0] = '0' -> Alcotest.failf "accepted %S" line
        | _ -> ())

(* Durations follow Go's time.ParseDuration, up to its signed 64-bit range
   of nanoseconds. *)
let test_durations () =
  List.iter
    (fun line ->
      match tape [ Vhs.raw line ] with
      | Ok () -> ()
      | Error e -> Alcotest.failf "rejected %S: %s" line e)
    [
      "Sleep 9223372036s";
      "Sleep 153722867m";
      "Sleep 0.000000000001ms";
      "Sleep 1.";
      "Type@.5ms \"x\"";
    ];
  fails "overflow" "Sleep 9223372037s" "time 9223372037s: not a Go duration";
  fails "minutes overflow" "Sleep 153722868m"
    "time 153722868m: not a Go duration";
  fails "two dots" "Set TypingSpeed 1..2" "TypingSpeed 1..2s: not a Go duration";
  fails "huge integer" "Sleep 99999999999999999999ms"
    "time 99999999999999999999ms: not a Go duration"

let suite =
  ( "tape_syntax",
    List.map
      (fun (name, fn) -> Alcotest.test_case name `Quick fn)
      [
        ("accepted", test_accepted);
        ("rejected", test_rejected);
        ("durations", test_durations);
        ("vhs validate oracle", test_oracle);
      ] )
