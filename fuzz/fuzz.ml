(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let () = Alcobar.run "vhs" [ Fuzz_vhs.suite; Fuzz_cram.suite; Fuzz_svg.suite ]
