(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Cram tests as VHS actions.

    A cram test is a [.t] file in the format of Mercurial's test runner,
    popularised by Brodie Rao's {{:https://bitheap.org/cram/}cram} and run by
    dune: a line [  $ cmd] starts a command, the [  > ] lines right after it
    continue it, and the other lines, expected output and exit codes included,
    are what the commands must print. *)

type t
(** The type for cram tests: a sequence of commands, each a non-empty list of
    lines without control characters. *)

val parse : string -> (t, string) result
(** [parse src] is the cram test of source [src], or [Error msg] if a command
    line holds a control character or invalid UTF-8, which a terminal cannot
    type as such. Lines may end with ["\r\n"]. *)

val is_empty : t -> bool
(** [is_empty t] is [true] iff [t] has no command. *)

val actions : setup:string list -> t -> Vhs.action list
(** [actions ~setup t] types each command of [t], waits for the
    {!Vhs.constructor-Prompt} and pauses one second. If [setup] is not empty,
    the actions first source the shell scripts [setup] in order and clear the
    screen, all {!Vhs.hidden}. *)
