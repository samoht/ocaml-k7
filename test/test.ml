(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let () =
  Alcotest.run "vhs"
    [
      Test_vhs.suite;
      Test_regexp_syntax.suite;
      Test_tape_syntax.suite;
      Test_themes.suite;
      Test_cram.suite;
      Test_eio.suite;
      Test_cli.suite;
      Test_svg.suite;
      Test_screen.suite;
      Test_palette.suite;
      Test_regexp_match.suite;
      Test_native.suite;
    ]
