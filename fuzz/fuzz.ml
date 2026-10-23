(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let () =
  Native_test.serve_subprocess ();
  Alcobar.run "k7" [ Fuzz_k7.suite; Fuzz_cram.suite; Fuzz_svg.suite ]
