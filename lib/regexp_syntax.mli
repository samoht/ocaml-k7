(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The regular expression syntax of VHS waits.

    VHS compiles a wait pattern with Go's [regexp.Compile], whose syntax is
    {{:https://github.com/google/re2/wiki/Syntax}RE2} with Perl extensions. This
    module checks that syntax as Go's [regexp/syntax] parser does and writes
    patterns so that the VHS lexer reads them back unchanged. *)

val check : string -> (unit, string) result
(** [check re] is [Ok ()] iff Go's [regexp.Compile] accepts [re], or [Error msg]
    with Go's reason. Go also bounds the size and nesting depth of a compiled
    expression; [check] does not, and only patterns thousands of bytes long
    reach those bounds. *)

val write : Buffer.t -> string -> unit
(** [write b re] adds [re] to [b] between slashes, as a VHS regexp token. Each
    slash of [re] is written [\x2f], or [\E\x2f\Q] inside a [\Q...\E] quote, and
    an unterminated quote is closed, so the token holds no slash and does not
    end with a backslash. If [check re] is [Ok ()], the pattern written matches
    the same strings as [re]. *)
