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
  ?show:string ->
  ?delay:K7.duration ->
  string ->
  int
(** [run stdenv ~engine ~dry_run ~print ~vhs ~settings ~env ~outputs ~setup ~dir
     ~wait_timeout ~show ~delay input] is {!Play_tape.run} on a [.tape] file
    [input], {!Play_cram.run} on a [.t] one, with the [setup] scripts, the
    directory [dir] and the [wait_timeout] seconds of a cram test, and
    {!Play_cast.run} on a [.cast] one, with [show] and [delay]; the extension is
    read in any case. [vhs] defaults to ["vhs"]. It is [123] after printing an
    error on standard error for another extension, for [dry_run] and [print]
    together, and for an option given that does not apply to the kind of
    [input]: [engine], [print], [vhs] and a non-empty [env] apply to a tape or a
    cram test, [setup], [dir] and [wait_timeout] to a cram test, [show] and
    [delay] to an asciicast recording. *)

val cmd : int Cmdliner.Cmd.t
(** [cmd] is the [play] subcommand. *)
