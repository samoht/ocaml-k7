(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The themes of VHS. *)

val mem : string -> bool
(** [mem name] is [true] iff [name] is the name of a theme of VHS v0.12.1, as
    [vhs themes] lists them. *)
