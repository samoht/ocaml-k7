(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The [k7 cram] subcommand. *)

val tape :
  setup:string list ->
  ?settings:K7.setting list ->
  output:string ->
  ?wait_timeout_s:int ->
  name:string ->
  string ->
  (K7.t, string) result
(** [tape ~setup ~settings ~output ~wait_timeout_s ~name src] is the tape that
    records the cram test of source [src] in a bash terminal with [settings]
    (defaults to [[]]), after the [setup] scripts, to [output], failing when a
    command has not given the prompt back after [wait_timeout_s] seconds
    (defaults to VHS's 15). It is [Error msg] if {!Cram.parse} or {!K7.tape}
    fail, if [src] has no command, or if a [setup] path is not
    {!Cram.is_typeable}; [msg] starts with [name] when it is about [src]. *)

val run :
  Eio_unix.Stdenv.base ->
  print:bool ->
  vhs:string ->
  setup:string list ->
  ?dir:string ->
  ?settings:K7.setting list ->
  ?env:(string * string) list ->
  ?wait_timeout_s:int ->
  output:string ->
  string ->
  int
(** [run stdenv ~print ~vhs ~setup ~dir ~settings ~env ~wait_timeout_s ~output
     input] records the cram test in the file [input] with {!K7.record}, which
    runs the executable [vhs] only for the outputs [vhs] writes, or prints its
    tape on standard output if [print] is [true], with the {!tape} of [setup],
    [settings], [wait_timeout_s] and [output], and the variables [env]. The
    commands run in [dir], or in a new temporary directory that [run] removes
    afterwards; relative paths of [setup], [output] and [vhs] are read from the
    current directory. It is the exit code: [0] on success, also when the reader
    of the printed tape closes the pipe early, and [123] after printing an error
    on standard error, if standard error can be written. *)

val cmd : int Cmdliner.Cmd.t
(** [cmd] is the [cram] subcommand. *)
