(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Matching VHS wait patterns.

    A pattern is in the syntax of Go's [regexp] package, which
    {!Regexp_syntax.check} accepts. Matching is Go's [MatchString] on the code
    points of UTF-8 text, in linear time: [(?i)] folds case as Go's
    [unicode.SimpleFold] does, and [\b] and [\B] count ASCII letters, digits and
    the underscore only as word characters. *)

type t
(** The type for compiled patterns. *)

val compile : string -> (t, string) result
(** [compile re] is the pattern [re], or [Error msg] if [re] uses a Unicode
    class ([\p] or [\P]), which this module does not have. [re] is a pattern
    {!Regexp_syntax.check} accepts. *)

val matches : t -> string -> bool
(** [matches t s] is [true] iff [t] matches a substring of [s]. *)
