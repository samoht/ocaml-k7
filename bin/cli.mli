(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The [vhs-cram] command line. *)

val tape :
  setup:string list ->
  output:string ->
  name:string ->
  string ->
  (Vhs.t, string) result
(** [tape ~setup ~output ~name src] is the tape that records the cram test of
    source [src] in a bash terminal, after the [setup] scripts, to [output]. It
    is [Error msg] if {!Cram.parse} or {!Vhs.tape} fail, or if [src] has no
    command; [msg] starts with [name] when it is about [src]. *)

val run :
  Eio_unix.Stdenv.base ->
  vhs:string ->
  setup:string list ->
  output:string ->
  string ->
  int
(** [run env ~vhs ~setup ~output input] records the cram test in the file
    [input] with the executable [vhs]. It is the exit code: [0] on success,
    [123] after printing an error on standard error. *)

val cmd : int Cmdliner.Cmd.t
(** [cmd] is the [vhs-cram] command. *)
