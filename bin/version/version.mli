(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Version string of this binary. *)

val string : string
(** [string] is [PROJECT_VERSION] when set at build time, otherwise the package
    version when built via opam, otherwise ["dev"]. *)
