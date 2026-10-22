(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** [k7 play] on a cram test. *)

val tape :
  setup:string list ->
  ?settings:K7.setting list ->
  outputs:string list ->
  ?wait_timeout_s:int ->
  name:string ->
  string ->
  (K7.t, string) result
(** [tape ~setup ~settings ~outputs ~wait_timeout_s ~name src] is the tape that
    records the cram test of source [src] in a bash terminal with [settings]
    (defaults to [[]]), after the [setup] scripts, to [outputs], failing when a
    command has not given the prompt back after [wait_timeout_s] seconds
    (defaults to VHS's 15). It is [Error msg] if {!Cram.parse} or {!K7.tape}
    fail, if [src] has no command or [outputs] is empty, or if a [setup] path is
    not {!Cram.is_typeable}; [msg] starts with [name] when it is about [src]. *)

val run :
  Eio_unix.Stdenv.base ->
  dry_run:bool ->
  print:bool ->
  vhs:string ->
  setup:string list ->
  ?dir:string ->
  ?settings:K7.setting list ->
  ?env:(string * string) list ->
  ?wait_timeout_s:int ->
  outputs:string list ->
  string ->
  int
(** [run stdenv ~dry_run ~print ~vhs ~setup ~dir ~settings ~env ~wait_timeout_s
     ~outputs input] records the cram test in the file [input] with
    {!K7.record}, which runs the executable [vhs] only for the outputs [vhs]
    writes, or prints its tape on standard output if [print] is [true], or only
    {!K7.check}s it, in [dir] or the current directory, if [dry_run] is [true],
    with the {!tape} of [setup], [settings], [wait_timeout_s] and [outputs], and
    the variables [env]. The commands run in [dir], or in a new temporary
    directory that [run] removes afterwards; relative paths of [setup],
    [outputs] and [vhs] are read from the current directory. It is the exit
    code: [0] on success, also when the reader of the printed tape closes the
    pipe early, and [123] after printing an error on standard error, if standard
    error can be written. *)
