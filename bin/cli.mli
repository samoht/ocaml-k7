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
    is [Error msg] if {!Cram.parse} or {!Vhs.tape} fail, if [src] has no
    command, or if a [setup] path is not {!Cram.is_typeable}; [msg] starts with
    [name] when it is about [src]. *)

val run :
  Eio_unix.Stdenv.base ->
  print:bool ->
  vhs:string ->
  setup:string list ->
  output:string ->
  string ->
  int
(** [run env ~print ~vhs ~setup ~output input] records the cram test in the file
    [input], with {!Vhs.record} if [output] ends with [.svg] and with the
    executable [vhs] otherwise, or prints its tape on standard output if [print]
    is [true]. It is the exit code: [0] on success, also when the reader of the
    printed tape closes the pipe early, and [123] after printing an error on
    standard error, if standard error can be written. *)

val cmd : int Cmdliner.Cmd.t
(** [cmd] is the [vhs-cram] command. *)
