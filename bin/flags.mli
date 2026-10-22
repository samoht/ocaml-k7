(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The options, input and output the subcommands of [k7] share. *)

(** {1:options Options} *)

val settings : Vhs.setting list Cmdliner.Term.t
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

val print_tape : Eio_unix.Stdenv.base -> Vhs.t -> (unit, string) result
(** [print_tape env t] writes [Vhs.to_string t] on standard output. A reader
    that closes the pipe early is not an error. *)

val fail : Eio_unix.Stdenv.base -> string -> int
(** [fail env msg] prints [k7: msg] on standard error, if it can be written, and
    is {!Cmdliner.Cmd.Exit.some_error}. *)
