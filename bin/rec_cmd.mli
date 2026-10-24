(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The [k7 rec] subcommand. *)

val run :
  ?show:string ->
  ?delay:K7.duration ->
  ?idle_time_limit:K7.duration ->
  Eio_unix.Stdenv.base ->
  settings:K7.setting list ->
  env:(string * string) list ->
  outputs:string list ->
  string list ->
  int
(** [run ~show ~delay ~idle_time_limit stdenv ~settings ~env ~outputs argv]
    records the program [argv] and writes each of [outputs] by its extension, in
    any case: the image of {!K7.svg_of_casts} with [idle_time_limit] for [.svg],
    the recording itself for [.cast] and the text of {!K7.text_of_casts} for
    [.txt], from one {!K7.capture} with [settings], the variables [env], [show]
    and [delay], and the cram test of {!K7.cram} for [.t]. It is the exit code
    of the program, from the capture if there is one, or [128] plus the number
    of the signal that killed it. It is [123] after printing an error on
    standard error, if standard error can be written, before it runs anything if
    an output has another extension, and if the program cannot run or an output
    cannot be written. *)

val idle_time_limit : int option option -> K7.duration option
(** [idle_time_limit idle] is the limit of [--idle-time-limit] [idle], in
    milliseconds and [Some None] for [none], or two seconds if [idle] is [None].
*)

val sized : Unix.file_descr -> K7.setting list -> K7.setting list
(** [sized fd settings] is [settings] with the columns and the rows of the
    terminal [fd] where [settings] give none, or [settings] if [fd] is not a
    terminal. *)

val cmd : int Cmdliner.Cmd.t
(** [cmd] is the [k7 rec] subcommand. *)
