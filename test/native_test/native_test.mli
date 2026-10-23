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

val replace : sub:string -> by:string -> string -> string
(** [replace ~sub ~by s] is [s] with each disjoint occurrence of [sub] replaced
    by [by]. *)

val prefix : string -> string option
(** [prefix svg] is the prefix of the names in [svg], read from the id of its
    title. *)

val unprefixed : string -> string
(** [unprefixed svg] is [svg] without its {!prefix}: in its ids, its references,
    its classes and its animation names. *)

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

val executable : string -> string -> unit
(** [executable path contents] writes [contents] to the executable file [path]
    with a blocking [close]. Under io_uring, Eio closes a file late, and running
    it at once fails with [ETXTBSY]. *)

val no_job_control : (string * string) list
(** [no_job_control] is the environment, for the [?env] of {!K7.record}, in
    which a recording's bash turns job control off before its first prompt: its
    [PROMPT_COMMAND] runs [set +m], since an interactive bash turns job control
    on whatever its command line says. *)

type clock = Eio.Time.Mono.ty Eio.Resource.t
(** The type for the clocks of recordings. *)

val with_clock : ?hold:float -> ?release:(unit -> bool) -> (clock -> 'a) -> 'a
(** [with_clock ~hold ~release f] is [f c], where [c] is a mock {!type-clock}
    that jumps to the next sleep that is due, so that a tape's sleeps take no
    time and timeouts expire in their order. A sleep of more than [hold] seconds
    (defaults to [1.]), such as the timeout of a wait, starts once [release ()]
    holds (defaults to never), so that until then a wait lasts as long as what
    it waits for takes to arrive. *)

val until : (unit -> bool) -> unit
(** [until p] yields until [p ()] holds. *)

val play :
  Eio_unix.Stdenv.base ->
  Eio.Fs.dir_ty Eio.Path.t ->
  K7.t ->
  (unit, K7.error) result
(** [play env cwd t] records [t] in [cwd] on a {!with_clock} clock, in the
    environment {!no_job_control}. *)

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

val shells : string list
(** [shells] are those of [/bin/sh], [/bin/dash], [/bin/bash] and [/bin/zsh]
    that exist. *)

val cram_command : string -> string
(** [cram_command t] is the first command of the cram test [t], as dune runs it:
    its [  $ ] line and the [  > ] lines after it, without those prefixes,
    joined by newlines. *)

val shown_command : string -> string
(** [shown_command screen] is the command line the screen [screen] shows: its
    first line without [$ ], and the lines after it that start with [>], without
    [> ], joined by newlines. *)

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

val subprocess :
  string -> (string list -> unit) -> string list -> Unix.process_status
(** [subprocess name f args] runs [f args] in a new process of this executable,
    started afresh rather than forked, and is how that process ended: exited
    with [0] once [f] returns, or with [1] after printing what it raised. That
    process finds [f] by [name], so [subprocess name f] is applied as the
    program starts, before {!serve_subprocess}. *)

val serve_subprocess : unit -> unit
(** [serve_subprocess ()] does nothing in a process that {!subprocess} did not
    start, and otherwise runs the function it names with the program's
    arguments, then exits. A test program calls it before its tests. *)
