(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The [k7 play] subcommand. *)

val run :
  Eio_unix.Stdenv.base ->
  ?engine:K7.engine ->
  dry_run:bool ->
  print:bool ->
  ?vhs:string ->
  settings:K7.setting list ->
  env:(string * string) list ->
  outputs:string list ->
  setup:string list ->
  dir:string option ->
  wait_timeout:int option ->
  ?show:string list ->
  ?delay:K7.duration ->
  ?idle:int option ->
  ?typing:int option ->
  string list ->
  int
(** [run stdenv ~engine ~dry_run ~print ~vhs ~settings ~env ~outputs ~setup ~dir
     ~wait_timeout ~show ~delay ~idle ~typing inputs] is {!Play_tape.run} on one
    [.tape] file, {!Play_cram.run} on one [.t] file, with the [setup] scripts,
    the directory [dir] and the [wait_timeout] seconds of a cram test, and
    {!Play_cast.run} on one or more [.cast] files, with [show] (defaults to
    [[]]), [delay] and the [typing_speed] {!Flags.typing} [typing]; the
    extension is read in any case. Each gets the limit {!val-idle_time_limit}
    gives for [idle]. [vhs] defaults to ["vhs"]. It is [123] after printing an
    error on standard error for another extension, for several [inputs] that are
    not all [.cast] files, for [dry_run] and [print] together, and for an option
    given that does not apply to the kind of [inputs]: [engine], [print], [vhs]
    and a non-empty [env] apply to a tape or a cram test, [setup], [dir] and
    [wait_timeout] to a cram test, a non-empty [show], [delay] and [typing] to
    asciicast recordings. *)

val idle_time_limit : string -> int option option -> K7.duration option
(** [idle_time_limit input idle] is the limit of [--idle-time-limit] [idle] for
    the file [input], as for {!run}: two seconds if [idle] is [None], unless
    [input] is a tape, which then has no limit. *)

val cmd : int Cmdliner.Cmd.t
(** [cmd] is the [play] subcommand. *)
