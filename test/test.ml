(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let () =
  Alcotest.run "vhs"
    [ Test_vhs.suite; Test_cram.suite; Test_eio.suite; Test_cli.suite ]
