(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** [k7 play] on a tape. *)

val run :
  Eio_unix.Stdenv.base ->
  print:bool ->
  vhs:string ->
  settings:K7.setting list ->
  env:(string * string) list ->
  outputs:string list ->
  string ->
  int
(** [run stdenv ~print ~vhs ~settings ~env ~outputs input] reads the tape in the
    file [input] with {!K7.of_string}, with [settings] in place of its own and
    [outputs], unless empty, in place of its outputs, and records it with
    {!K7.record}, which runs the executable [vhs] only for the outputs [vhs]
    writes, with the variables [env], or prints it on standard output if [print]
    is [true]. It is the exit code as for {!Play_cram.run}. *)
