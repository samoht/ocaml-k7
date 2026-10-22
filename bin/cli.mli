(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The [k7] command line. *)

val cmd : int Cmdliner.Cmd.t
(** [cmd] is the [k7] command and its subcommands [play] and [rec]. *)
