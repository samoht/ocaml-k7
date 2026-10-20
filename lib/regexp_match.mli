(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Matching VHS wait patterns.

    A pattern is in the syntax of Go's [regexp] package, which
    {!Regexp_syntax.check} accepts. Matching is Go's [MatchString] on the code
    points of UTF-8 text, in linear time: [(?i)] folds case as Go's
    [unicode.SimpleFold] does, [\b] and [\B] count ASCII letters, digits and the
    underscore only as word characters, and [\p] and [\P] read Go's Unicode
    tables. *)

type t
(** The type for compiled patterns. *)

val compile : string -> t
(** [compile re] is the pattern [re], which {!Regexp_syntax.check} accepts.

    @raise Invalid_argument if [re] names a Unicode class Go does not have. *)

val matches : t -> string -> bool
(** [matches t s] is [true] iff [t] matches a substring of [s]. *)
