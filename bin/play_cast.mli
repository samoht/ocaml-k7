(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** [k7 play] on an asciicast recording. *)

val run :
  Eio_unix.Stdenv.base ->
  dry_run:bool ->
  settings:K7.setting list ->
  show:string list ->
  ?delay:K7.duration ->
  ?idle_time_limit:K7.duration ->
  outputs:string list ->
  string list ->
  int
(** [run stdenv ~dry_run ~settings ~show ~delay ~idle_time_limit ~outputs
     inputs] reads the asciicast recordings in the files [inputs] with
    {!K7.cast_of_string}, draws them one after the other with {!K7.svg_of_casts}
    for each [.svg] output and gives their last screen with {!K7.text_of_casts}
    for each [.txt] output, extensions in any case, with [settings], [show],
    [delay] and [idle_time_limit], writes them to [outputs], or to none if
    [dry_run] is [true], and is [0]. It is [123] after printing an error on
    standard error, if standard error can be written, if [outputs] is empty or
    one of them is neither [.svg] nor [.txt], if [show] has several strings but
    not one per input, if an input cannot be read, if reading or drawing them
    fails, or if an output cannot be written, which it checks first, [dry_run]
    or not. *)
