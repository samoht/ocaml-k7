(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Native_test

let test_colours () =
  let s =
    svg
      ~settings:(Vhs.Theme "Dracula" :: settings)
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
  check_contains "inverse text" ~sub:{|class="e">i|} s;
  check_contains "inverse background" ~sub:{|class="d"/>|} s;
  check_contains "underline" ~sub:{|class="xu">u|} s

(* A JSON theme is read as Go reads it, keys ignoring case, and a colour it
   lacks is xterm.js's. *)
let test_json_theme () =
  let s =
    svg
      ~settings:
        [
          Theme
            {|{"background": "#102030", "Red": "#f00", "brightred": "rgb(1, 2, 3)", "cursor": null, "name": "x", "other": [1, {"a": true}]}|};
        ]
      [ (Ms 0, "x") ]
  in
  check_contains "background" ~sub:".e{fill:#102030}" s;
  check_contains "red" ~sub:".p1{fill:#ff0000}" s;
  check_contains "bright red" ~sub:".p9{fill:#010203}" s;
  check_contains "foreground" ~sub:"text{fill:#ffffff}" s;
  check_contains "cursor" ~sub:".kc{fill:#ffffff}" s;
  check_contains "green" ~sub:".p2{fill:#4e9a06}" s;
  (* The text under the cursor takes the theme's accent, or the one of its
     background and foreground that stands out more on the cursor. *)
  let accent theme = svg ~settings:[ Theme theme ] [ (Ms 0, "x") ] in
  check_contains "named theme" ~sub:".ka{fill:#1e1f29}" (accent "Dracula");
  check_contains "missing accent" ~sub:".ka{fill:#102030}" s;
  check_contains "light cursor" ~sub:".ka{fill:#000000}"
    (accent
       {|{"background": "#ffffff", "foreground": "#000000", "cursor": "#eeeeee"}|});
  check_contains "accent" ~sub:".ka{fill:#00ff00}"
    (accent {|{"cursor": "#ff0000", "cursorAccent": "#00ff00"}|});
  (match Vhs.svg ~settings:[ Theme {|{"red": "tomato"}|} ] [] with
  | Error (Unsupported why) -> check_contains "names it" ~sub:"tomato" why
  | Ok _ -> Alcotest.fail "a colour name played"
  | Error e -> Alcotest.failf "%a" Vhs.pp_error e);
  List.iter
    (fun theme ->
      match
        Vhs.tape ~settings:[ Theme theme ] ~outputs:[ "a.gif" ]
          [ Vhs.type' "x" ]
      with
      | Error e ->
          check_contains "refused" ~sub:"VHS cannot read it" (invalid e)
      | Ok _ -> Alcotest.failf "%s accepted" theme)
    [ {|{"background": 1}|}; {|{"background": "#000"} x|}; {|{"a": tru}|}; "{" ]

(* JSON themes as Go reads them, written by theme_oracle.go: whether VHS
   reads the theme and, if it does, the bytes of its background and red
   colours, in hex. *)
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
            Vhs.tape ~settings:[ Theme json ] ~outputs:[ "a.gif" ]
              [ Vhs.type' "x" ]
          in
          let accepted = Result.is_ok result in
          (* jsont refuses invalid UTF-8 and unpaired surrogate escapes in a
             string, which Go reads as U+FFFD: the README's known
             limitation. *)
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
                Vhs.svg ~settings:[ Theme json ] [] )
            with
            | Some bg, Some red, Ok s ->
                check_contains json ~sub:(".e{fill:" ^ bg ^ "}") s;
                check_contains json ~sub:(".p1{fill:" ^ red ^ "}") s
            | None, _, Error (Unsupported _) | _, None, Error (Unsupported _) ->
                ()
            | _, _, Ok _ -> Alcotest.failf "%s: played" json
            | _, _, Error e -> Alcotest.failf "%s: %a" json Vhs.pp_error e)
      | [ "" ] -> ()
      | _ -> Alcotest.failf "bad case %S" line)

(* Without a theme, or with a blank one, the colours are VHS's defaults. *)
let test_default () =
  List.iter
    (fun settings ->
      let s = svg ~settings [ (Ms 0, "x") ] in
      check_contains "background" ~sub:".e{fill:#171717}" s;
      check_contains "foreground" ~sub:"text{fill:#dddddd}" s;
      check_contains "red" ~sub:".p1{fill:#d74e6f}" s;
      check_contains "cursor accent" ~sub:".ka{fill:#171717}" s)
    [ settings; Vhs.Theme "  " :: settings ]

(* A theme without a cursor colour gets xterm.js's white, and text under the
   cursor is black. *)
let test_no_cursor () =
  let s = svg ~settings:(Vhs.Theme "TokyoNight" :: settings) [ (Ms 0, "x") ] in
  check_contains "cursor" ~sub:".kc{fill:#ffffff}" s;
  check_contains "accent" ~sub:".ka{fill:#16161e}" s

let suite =
  ( "palette",
    List.map
      (fun (name, fn) -> Alcotest.test_case name `Quick fn)
      [
        ("colours", test_colours);
        ("default theme", test_default);
        ("no cursor colour", test_no_cursor);
        ("JSON theme", test_json_theme);
        ("Go oracle", test_go_oracle);
      ] )
