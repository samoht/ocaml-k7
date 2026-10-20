(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The command syntax of VHS tapes.

    This module reads one tape line with the lexer and parser of VHS
    ([lexer/lexer.go] and [parser/parser.go] at v0.12.1), so that a line VHS
    rejects is found before VHS runs. *)

(** The type for the command a line holds. *)
type command =
  | Blank  (** No command: spaces or a comment. *)
  | Hide
  | Show
  | Require
  | Output of string  (** [Output path]. *)
  | Set of string * string
      (** [Set (name, arg)]: the VHS setting [name] and the token after it. *)
  | Other  (** Any other command. *)

val check : string -> (command, string) result
(** [check line] is the command of [line], or [Error msg] if VHS's parser
    reports an error on it, if it holds more than one command, or if it ends
    before a string, a regular expression, a JSON value or an argument it
    starts. It is also [Error msg] on what VHS only finds when it runs the
    command: a duration Go's [time.ParseDuration] rejects, the value of an
    integer or float setting Go's [strconv.Atoi] or [strconv.ParseFloat]
    rejects, and a [Rows] or [Columns] value that is not positive. [line] holds
    no newline. A [Source] line's tape is not read. *)

val no_extension : string
(** [no_extension] is the error message for an {!constructor-Output} path
    without an extension or a trailing slash. *)

val extension : string -> string
(** [extension path] is Go's [filepath.Ext path]: the suffix of [path] from the
    last dot of its last segment, or [""] if that segment has no dot. *)
