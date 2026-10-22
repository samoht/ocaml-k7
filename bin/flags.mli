(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The options, input and output the subcommands of [k7] share. *)

(** {1:options Options} *)

val settings : K7.setting list Cmdliner.Term.t
(** [settings] are the terminal settings of [--cols], [--rows], [--font-size]
    and [--theme], in that order. *)

val env : (string * string) list Cmdliner.Term.t
(** [env] are the variables of the repeatable [--env VAR=VALUE], in order. *)

val wait_timeout : int option Cmdliner.Term.t
(** [wait_timeout] is the number of seconds of [--wait-timeout], if given. *)

val vhs : string Cmdliner.Term.t
(** [vhs] is the [vhs] executable of [--vhs] (defaults to ["vhs"]). *)

val print : bool Cmdliner.Term.t
(** [print] is [true] iff [--print] is given. *)

val exits : Cmdliner.Cmd.Exit.info list
(** [exits] are the exit codes of a subcommand that records a file. *)

(** {1:io Input and output} *)

val load : Eio_unix.Stdenv.base -> string -> (string, string) result
(** [load env path] is the contents of the file [path], or [Error msg]. *)

val print_tape : Eio_unix.Stdenv.base -> K7.t -> (unit, string) result
(** [print_tape env t] writes [K7.to_string t] on standard output. A reader that
    closes the pipe early is not an error. *)

val interruptible : (unit Eio.Promise.t -> 'a) -> 'a * Sys.signal option
(** [interruptible f] is [f stop] and the signal, [SIGINT] or [SIGTERM], that
    interrupted it, if any. [stop] resolves on the first of them that [k7] gets
    while [f] runs. *)

val fail : Eio_unix.Stdenv.base -> string -> int
(** [fail env msg] prints [k7: msg] on standard error, if it can be written, and
    is {!Cmdliner.Cmd.Exit.some_error}. *)

val exit :
  Eio_unix.Stdenv.base ->
  ok:('a -> int) ->
  ('a, string) result * Sys.signal option ->
  int
(** [exit env ~ok (r, signal)] is [ok x] for [Ok x] and [fail env msg] for
    [Error msg], when no [signal] interrupted the subcommand, and [128] plus the
    signal's number otherwise, after [fail env msg] for [Error msg]. *)
