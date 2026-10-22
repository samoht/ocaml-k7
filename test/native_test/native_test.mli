(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Helpers for the tests of native recordings.

    The terminal of {!settings} has 9 rows and 46 columns of 14 x 29 pixel
    cells. *)

val settings : K7.setting list
(** [settings] are the settings of an 800 x 400 image in a 22 pixel font. *)

val cell_width : int
(** [cell_width] is the width of a cell of {!settings}, in pixels. *)

val cell_height : int
(** [cell_height] is the height of a cell of {!settings}, in pixels. *)

val rows : int
(** [rows] is the number of rows of the terminal of {!settings}. *)

val prefix : string -> string option
(** [prefix svg] is the id of the root element of [svg], which prefixes its
    other ids. *)

val unprefixed : string -> string
(** [unprefixed svg] is [svg] without its {!prefix}: in its ids, its references,
    its animation names and its scoped style rules. *)

val svg : ?settings:K7.setting list -> (K7.duration * string) list -> string
(** [svg ~settings output] is [K7.svg ~settings output] (defaults to
    {!settings}), {!unprefixed}, and fails the test on an error. *)

val frames : string -> string list
(** [frames svg] are the texts of the screens of [svg], and fail the test if
    [svg] is not well-formed. *)

val contains : sub:string -> string -> bool
(** [contains ~sub s] is [true] iff [sub] is a substring of [s]. *)

val count : sub:string -> string -> int
(** [count ~sub s] is the number of disjoint occurrences of [sub] in [s]. *)

val check_contains : string -> sub:string -> string -> unit
(** [check_contains what ~sub s] fails the test [what] unless [contains ~sub s].
*)

val screen : string list -> string
(** [screen lines] is the text of a screen whose first rows are [lines]. *)

val with_dir : (Eio_unix.Stdenv.base -> Eio.Fs.dir_ty Eio.Path.t -> 'a) -> 'a
(** [with_dir f] is [f env dir] in a fresh temporary directory [dir], which it
    removes afterwards. *)

val play :
  Eio_unix.Stdenv.base ->
  Eio.Fs.dir_ty Eio.Path.t ->
  K7.t ->
  (unit, K7.error) result
(** [play env cwd t] is {!K7.record} of [t] in [cwd], within a minute. *)

val record :
  ?image:string ->
  Eio_unix.Stdenv.base ->
  Eio.Fs.dir_ty Eio.Path.t ->
  K7.t ->
  (string, K7.error) result
(** [record ~image env cwd t] is {!play} of [t], then the image it wrote to
    [image] (defaults to ["demo.svg"]), relative to [cwd]. *)

val tape : K7.action list -> K7.t
(** [tape actions] is the tape of [actions] with {!settings}, typing every 5ms,
    to [demo.svg]. *)

val last : 'a list -> 'a
(** [last l] is the last element of [l]. *)

val invalid : K7.error -> string
(** [invalid e] is the message of [K7.Invalid msg], the error of {!K7.tape}; it
    fails the test on any other error. *)

val oracle : string option
(** [oracle] is the [vhs] the oracle alias names in [VHS_ORACLE]. *)

val separator : string
(** [separator] is the line that ends each screen of a VHS text output: 80
    U+2500 characters. *)

val screens : string -> string list
(** [screens txt] are the screens of the VHS text output [txt], in order: the
    lines VHS read after a command, joined by newlines. *)

val last_screen : string -> string
(** [last_screen txt] is the last screen of the VHS text output [txt]. *)
