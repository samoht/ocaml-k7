(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Native_test

let foreground c = Fmt.str {|" fill="%s" font-family="|} c
let background c = Fmt.str {| fill="%s"/><defs>|} c
let cursor c = Fmt.str {| fill="%s"/><rect id=|} c
let accent c = Fmt.str {| fill="%s">x</text>|} c

let test_colours () =
  let s =
    svg
      ~settings:(K7.Theme "Dracula" :: settings)
      [
        ( Ms 0,
          "\027[31mr\027[1;32mg\027[0;38;5;208mo\027[38;2;10;20;30mt\027[0;7mi\027[0;4mu"
        );
      ]
  in
  check_contains "theme red" ~sub:".p1{fill:#ff5555}" s;
  check_contains "ANSI class" ~sub:{|class="p1">r|} s;
  check_contains "bold bright" ~sub:{|class="pa b">g|} s;
  check_contains "256 colours" ~sub:{|fill="#ff8700">o|} s;
  check_contains "true colour" ~sub:{|fill="#0a141e">t|} s;
  check_contains "inverse text" ~sub:{|fill="#1e1f29">i|} s;
  check_contains "inverse background" ~sub:{|height="29"/>|} s;
  check_contains "underline" ~sub:{|class="xu">u|} s

let test_json_theme () =
  let s =
    svg
      ~settings:
        [
          Theme
            {|{"background": "#102030", "Red": "#f00", "brightred": "rgb(1, 2, 3)", "cursor": null, "name": "x", "other": [1, {"a": true}]}|};
        ]
      [ (Ms 0, "x\r") ]
  in
  check_contains "background" ~sub:(background "#102030") s;
  check_contains "red" ~sub:".p1{fill:#ff0000}" s;
  check_contains "bright red" ~sub:".p9{fill:#010203}" s;
  check_contains "foreground" ~sub:(foreground "#ffffff") s;
  check_contains "cursor" ~sub:(cursor "#ffffff") s;
  check_contains "green" ~sub:".p2{fill:#4e9a06}" s;
  let themed theme = svg ~settings:[ Theme theme ] [ (Ms 0, "x\r") ] in
  check_contains "named theme" ~sub:(accent "#1e1f29") (themed "Dracula");
  check_contains "missing accent" ~sub:(accent "#102030") s;
  check_contains "light cursor" ~sub:(accent "#000000")
    (themed
       {|{"background": "#ffffff", "foreground": "#000000", "cursor": "#eeeeee"}|});
  check_contains "accent" ~sub:(accent "#00ff00")
    (themed {|{"cursor": "#ff0000", "cursorAccent": "#00ff00"}|});
  (match K7.svg ~settings:[ Theme {|{"red": "tomato"}|} ] [] with
  | Error (Unsupported why) ->
      check_contains "names it" ~sub:"tomato" why;
      check_contains "drawn" ~sub:"not drawn natively: " why
  | Ok _ -> Alcotest.fail "a colour name played"
  | Error e -> Alcotest.failf "%a" K7.pp_error e);
  List.iter
    (fun theme ->
      match
        K7.tape ~settings:[ Theme theme ] ~outputs:[ "a.gif" ] [ K7.type' "x" ]
      with
      | Error e ->
          check_contains "refused" ~sub:"VHS cannot read it" (invalid e)
      | Ok _ -> Alcotest.failf "%s accepted" theme)
    [ {|{"background": 1}|}; {|{"background": "#000"} x|}; {|{"a": tru}|}; "{" ];
  (* VHS reads these, with U+FFFD, and jsont does not. *)
  List.iter
    (fun theme ->
      match
        K7.tape ~settings:[ Theme theme ] ~outputs:[ "a.gif" ] [ K7.type' "x" ]
      with
      | Error e ->
          check_contains "refused here" ~sub:"this library cannot read it"
            (invalid e)
      | Ok _ -> Alcotest.failf "%s accepted" theme)
    [ {|{"background": "\ud800"}|}; "{\"background\": \"\xff\"}" ]

(* Written by theme_oracle.go. *)
let test_go_oracle () =
  let unhex h =
    String.init
      (String.length h / 2)
      (fun i -> Char.chr (int_of_string ("0x" ^ String.sub h (2 * i) 2)))
  in
  let colour default = function
    | "" -> Some default
    | "#123" -> Some "#112233"
    | "#abcdef" -> Some "#abcdef"
    | "rgb(1, 2, 3)" -> Some "#010203"
    | _ -> None
  in
  In_channel.with_open_bin "theme_cases.txt" In_channel.input_all
  |> String.split_on_char '\n'
  |> List.iter (fun line ->
      match String.split_on_char '\t' line with
      | [ ok; bg; red; json ] -> (
          let result =
            K7.tape ~settings:[ Theme json ] ~outputs:[ "a.gif" ]
              [ K7.type' "x" ]
          in
          let accepted = Result.is_ok result in
          (* Go reads these as U+FFFD; jsont refuses them. *)
          let refused_as_documented =
            match result with
            | Error (Invalid msg) ->
                contains ~sub:"UTF-8 decoding error" msg
                || contains ~sub:"surrogate" msg
            | _ -> false
          in
          if not (ok = "1" && refused_as_documented) then
            Alcotest.(check bool) json (ok = "1") accepted;
          if accepted then
            match
              ( colour "#000000" (unhex bg),
                colour "#cc0000" (unhex red),
                K7.svg ~settings:[ Theme json ] [] )
            with
            | Some bg, Some red, Ok s ->
                check_contains json ~sub:(background bg) s;
                check_contains json ~sub:("-p1{fill:" ^ red ^ "}") s
            | None, _, Error (Unsupported _) | _, None, Error (Unsupported _) ->
                ()
            | _, _, Ok _ -> Alcotest.failf "%s: played" json
            | _, _, Error e -> Alcotest.failf "%s: %a" json K7.pp_error e)
      | [ "" ] -> ()
      | _ -> Alcotest.failf "bad case %S" line)

let test_default () =
  List.iter
    (fun settings ->
      let s = svg ~settings [ (Ms 0, "x\r") ] in
      check_contains "background" ~sub:(background "#171717") s;
      check_contains "foreground" ~sub:(foreground "#dddddd") s;
      check_contains "red" ~sub:".p1{fill:#d74e6f}" s;
      check_contains "cursor accent" ~sub:(accent "#171717") s)
    [ settings; K7.Theme "  " :: settings ]

let test_no_cursor () =
  let s = svg ~settings:(K7.Theme "TokyoNight" :: settings) [ (Ms 0, "x\r") ] in
  check_contains "cursor" ~sub:(cursor "#ffffff") s;
  check_contains "accent" ~sub:(accent "#16161e") s

(* Go's encoding/json counts the root in its 10,000-level limit. *)
let test_json_depth () =
  let build arrays =
    let theme =
      "{\"extra\":" ^ String.make arrays '[' ^ "0" ^ String.make arrays ']'
      ^ "}"
    in
    K7.tape ~settings:[ Theme theme ] ~outputs:[ "demo.txt" ]
      [ K7.sleep (Ms 100) ]
  in
  (match build 9_999 with
  | Ok _ -> ()
  | Error e -> Alcotest.failf "Go accepts this depth: %a" K7.pp_error e);
  match build 10_000 with
  | Error (Invalid _) -> ()
  | Error e -> Alcotest.failf "%a" K7.pp_error e
  | Ok _ -> Alcotest.fail "accepted a theme Go refuses: exceeded max depth"

let suite =
  ( "palette",
    List.map
      (fun (name, fn) -> Alcotest.test_case name `Quick fn)
      [
        ("colours", test_colours);
        ("default theme", test_default);
        ("no cursor colour", test_no_cursor);
        ("JSON theme", test_json_theme);
        ("JSON nesting limit", test_json_depth);
        ("Go oracle", test_go_oracle);
      ] )
