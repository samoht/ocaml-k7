(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Cram tests as VHS actions.

    A cram test is a [.t] file in the format of Mercurial's test runner,
    popularised by Brodie Rao's {{:https://bitheap.org/cram/}cram} and run by
    dune: a line [  $ cmd] starts a command, the [  > ] lines right after it
    continue it, a bare [  >] with an empty line, and the other lines, expected
    output and exit codes included, are what the commands must print. *)

type t
(** The type for cram tests: a sequence of commands, each a non-empty list of
    lines without control characters. *)

val parse : string -> (t, string) result
(** [parse src] is the cram test of source [src], or [Error msg] if a command
    line is not {!is_typeable}. Lines may end with ["\r\n"]. *)

val is_typeable : string -> bool
(** [is_typeable s] is [true] iff [s] is valid UTF-8 without a control
    character: a C0 control, DEL or a C1 control, which a terminal reads as an
    escape sequence. *)

val is_empty : t -> bool
(** [is_empty t] is [true] iff [t] has no command. *)

val actions : setup:string list -> t -> Vhs.action list
(** [actions ~setup t] sources the shell scripts [setup] in order, appends to
    bash's [PROMPT_COMMAND], after the hooks [setup] installs, a command that
    sets [PS1] to the VHS prompt followed by a mark, and clears the screen, all
    {!Vhs.hidden}; then it types each command of [t], waits until the line under
    the cursor ends with the mark, and pauses one second. The mark is sixteen
    zero-width characters, U+200B and U+2060, spelling a digest of [setup] and
    [t]. It does not show in a GIF or a video, so the recording shows the VHS
    prompt, but it tells the prompt from a line the command prints, even one
    that ends with [>] and a zero-width space or with the prompt of another
    recording: each command's output is complete before the next command is
    typed. A text output of VHS holds the mark after each prompt. A hook that
    rewrites [PS1] runs before the appended command, so the prompt survives it;
    a command that replaces [PROMPT_COMMAND] makes the next wait time out. The
    appended command also sets [PS2] to bash's [> ] followed by the mark and a
    count of the prompts shown, which {!unfinished} reads, and makes readline
    take and show typed non-ASCII text, which bash 3.2 drops without a UTF-8
    locale. *)

val unfinished : setup:string list -> t -> string -> (int * string) option
(** [unfinished ~setup t output] is [Some (n, line)] if [output], what [vhs]
    printed when a recording of [actions ~setup t] timed out, shows bash waiting
    for more of command [n], counted from [1], whose first line is [line]: an
    unterminated quote or here-document, or a line that ends with a backslash,
    leaves bash at its continuation prompt [PS2], where no wait can end. It is
    [None] otherwise. *)
