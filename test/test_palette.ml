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

let test_json_theme () =
  match Vhs.svg ~settings:[ Theme {|{"background": "#000000"}|} ] [] with
  | Error (Unsupported _) -> ()
  | Ok _ -> Alcotest.fail "a JSON theme played natively"
  | Error e -> Alcotest.failf "%a" Vhs.pp_error e

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
  check_contains "accent" ~sub:".ka{fill:#000000}" s

let suite =
  ( "palette",
    List.map
      (fun (name, fn) -> Alcotest.test_case name `Quick fn)
      [
        ("colours", test_colours);
        ("default theme", test_default);
        ("no cursor colour", test_no_cursor);
        ("JSON theme", test_json_theme);
      ] )
