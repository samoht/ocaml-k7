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
  vhs:string ->
  settings:K7.setting list ->
  env:(string * string) list ->
  outputs:string list ->
  setup:string list ->
  dir:string option ->
  wait_timeout:int option ->
  string ->
  int
(** [run stdenv ~engine ~dry_run ~print ~vhs ~settings ~env ~outputs ~setup ~dir
     ~wait_timeout input] is {!Play_tape.run} on a [.tape] file [input] and
    {!Play_cram.run} on a [.t] one, with the [setup] scripts, the directory
    [dir] and the [wait_timeout] seconds of a cram test. It is [123] after
    printing an error on standard error for another extension, and for a tape
    given [setup], [dir] or [wait_timeout]. *)

val cmd : int Cmdliner.Cmd.t
(** [cmd] is the [play] subcommand. *)
