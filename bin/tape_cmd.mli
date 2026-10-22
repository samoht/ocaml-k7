(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The [k7 tape] subcommand. *)

val run :
  Eio_unix.Stdenv.base ->
  print:bool ->
  vhs:string ->
  settings:Vhs.setting list ->
  env:(string * string) list ->
  outputs:string list ->
  string ->
  int
(** [run stdenv ~print ~vhs ~settings ~env ~outputs input] reads the tape in the
    file [input] with {!Vhs.of_string}, with [settings] in place of its own and
    [outputs], unless empty, in place of its outputs, and records it with
    {!Vhs.play}, which runs the executable [vhs] only for the outputs [vhs]
    writes, after setting the variables [env] in the environment of [k7], or
    prints it on standard output if [print] is [true]. It is the exit code as
    for {!Cram_cmd.run}. *)

val cmd : int Cmdliner.Cmd.t
(** [cmd] is the [tape] subcommand. *)
