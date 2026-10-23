(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A test that hangs ends the run after two minutes, so that CI does not wait
   forever; no test comes near it. *)
let guard (name, cases) =
  ( name,
    List.map
      (fun (case, speed, f) ->
        ( case,
          speed,
          fun x ->
            ignore (Unix.alarm 120);
            Fun.protect
              ~finally:(fun () -> ignore (Unix.alarm 0))
              (fun () -> f x) ))
      cases )

let () =
  Sys.set_signal Sys.sigalrm Sys.Signal_default;
  Alcotest.run "k7"
  @@ List.map guard
       [
         Test_k7.suite;
         Test_regexp_syntax.suite;
         Test_tape_syntax.suite;
         Test_themes.suite;
         Test_cram.suite;
         Test_eio.suite;
         Test_cli.suite;
         Test_svg.suite;
         Test_asciicast.suite;
         Test_screen.suite;
         Test_palette.suite;
         Test_regexp_match.suite;
         Test_casefold.suite;
         Test_unicode.suite;
         Test_native.suite;
       ]
