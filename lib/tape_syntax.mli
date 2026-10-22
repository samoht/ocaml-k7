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
  | Screenshot of string  (** [Screenshot path]. *)
  | Other  (** Any other command. *)

val check : string -> (command, string) result
(** [check line] is the command of [line], or [Error msg] if VHS's parser
    reports an error on it, but for a {!constructor-Screenshot} of an [.svg]
    image, which VHS refuses and the native recorder writes, if it holds more
    than one command, or if it ends before a string, a regular expression, a
    JSON value or an argument it starts. It is also [Error msg] on what VHS only
    finds when it runs the command: a duration Go's [time.ParseDuration]
    rejects, the value of an integer or float setting Go's [strconv.Atoi] or
    [strconv.ParseFloat] rejects, and a [Rows] or [Columns] value that is not
    positive. [line] holds no newline. A [Source] line's tape is not read. *)

val has_control : ?except:string -> string -> bool
(** [has_control ~except s] is [true] iff [s] holds a C0 control other than
    those of [except] (defaults to [""]), DEL, or a C1 control encoded in UTF-8,
    which a terminal runs as an escape sequence: U+009B is CSI. *)

val atoi : string -> int64 option
(** [atoi s] is Go's [strconv.Atoi s] on 64 bits, or [None] if it fails. *)

val no_extension : string
(** [no_extension] is the error message for an {!constructor-Output} path
    without an extension or a trailing slash. *)

val extension : string -> string
(** [extension path] is Go's [filepath.Ext path]: the suffix of [path] from the
    last dot of its last segment, or [""] if that segment has no dot. *)

(** {1:tokens Tokens} *)

(** The type for the tokens of VHS's lexer. *)
type lexeme =
  | Keyword of string  (** A keyword, such as [Type] or [ms]. *)
  | Str of string
      (** A string without its quotes, or a word that is not a keyword. *)
  | Pattern of string  (** A regular expression without its slashes. *)
  | Json_value of string  (** A JSON value, braces included. *)
  | Num of string  (** A number. *)
  | Sym of char  (** A punctuation character, such as [@] or [+]. *)

val tokens : string -> lexeme list
(** [tokens line] are the tokens of [line], its comment excluded. Only a line
    {!check} accepts gives the tokens VHS reads. *)

val duration : string -> int64 option
(** [duration s] is [s] as Go's [time.ParseDuration] reads it, in nanoseconds,
    or [None] if Go rejects it. *)
