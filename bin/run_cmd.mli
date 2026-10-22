(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The [k7 run] subcommand. *)

val run :
  Eio_unix.Stdenv.base ->
  settings:K7.setting list ->
  env:(string * string) list ->
  output:string ->
  string list ->
  int
(** [run stdenv ~settings ~env ~output argv] records the program [argv] with
    {!K7.run}, with [settings] and the variables [env], writes the image to
    [output] and is the program's exit code, or [128] plus the number of the
    signal that killed it. It is [123] after printing an error on standard
    error, if standard error can be written, if [output] does not end with
    [.svg] or its directory does not exist, if {!K7.run} fails, or if the image
    cannot be written. *)

val cmd : int Cmdliner.Cmd.t
(** [cmd] is the [k7 run] subcommand. *)
