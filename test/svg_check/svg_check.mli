(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Reading the screens back from an SVG recording. *)

val frames :
  cell_width:int ->
  cell_height:int ->
  rows:int ->
  string ->
  (string list, string) result
(** [frames ~cell_width ~cell_height ~rows svg] are the texts of the screens of
    [svg], in order: the rows joined by newlines, each without trailing spaces.
    It is [Error msg] if [svg] is not well-formed XML. *)

val trim_rows : string -> string
(** [trim_rows s] is [s] with the trailing spaces of each line removed. *)
