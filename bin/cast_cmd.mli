(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The [k7 cast] subcommand. *)

val run :
  ?show:string ->
  ?delay:K7.duration ->
  Eio_unix.Stdenv.base ->
  settings:K7.setting list ->
  output:string ->
  string ->
  int
(** [run ~show ~delay stdenv ~settings ~output input] draws the asciicast
    recording in the file [input] with {!K7.svg_of_cast}, with [settings],
    [show] and [delay], writes the image to [output] and is [0]. It is [123]
    after printing an error on standard error, if standard error can be written,
    if [output] does not end with [.svg], if [input] cannot be read, if
    {!K7.svg_of_cast} fails, or if the image cannot be written. *)

val cmd : int Cmdliner.Cmd.t
(** [cmd] is the [k7 cast] subcommand. *)
