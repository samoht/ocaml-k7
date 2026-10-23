(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** [k7 play] on an asciicast recording. *)

val run :
  Eio_unix.Stdenv.base ->
  dry_run:bool ->
  settings:K7.setting list ->
  ?show:string ->
  ?delay:K7.duration ->
  outputs:string list ->
  string ->
  int
(** [run stdenv ~dry_run ~settings ~show ~delay ~outputs input] draws the
    asciicast recording in the file [input] with {!K7.svg_of_cast}, with
    [settings], [show] and [delay], writes the image to each of [outputs], or to
    none if [dry_run] is [true], and is [0]. It is [123] after printing an error
    on standard error, if standard error can be written, if [outputs] is empty
    or one of them does not end with [.svg], if [input] cannot be read, if
    {!K7.svg_of_cast} fails, or if the image cannot be written. *)
