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
    line is not {!is_typeable}. Lines may end with ["\r\n"]. [parse] reads [src]
    once. *)

val is_typeable : string -> bool
(** [is_typeable s] is [true] iff [s] is valid UTF-8 without a control
    character: a C0 control, DEL or a C1 control, which a terminal reads as an
    escape sequence. *)

val is_empty : t -> bool
(** [is_empty t] is [true] iff [t] has no command. *)

val actions : setup:string list -> t -> Vhs.action list
(** [actions ~setup t] sources the shell scripts [setup] in order, sets [PS1] to
    the VHS prompt followed by a zero-width space and clears the screen, all
    {!Vhs.hidden}; then it types each command of [t], waits until the line under
    the cursor ends with that prompt, and pauses one second. The space does not
    show in a GIF or a video, so the recording shows the VHS prompt, but it
    tells the prompt from a line the command prints that ends with [>]: each
    command's output is complete before the next command is typed. A text output
    of VHS holds the space after each prompt. *)
