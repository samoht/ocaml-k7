(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The Unicode classes of Go's regular expressions, [\p{name}]. *)

val find : string -> ((int * int) list * bool) option
(** [find name] is the table the class [name] names, as Go's [unicodeTable]
    reads [name] (ignoring underscores, hyphens, spaces and the case of its
    letters): sorted, disjoint, inclusive ranges of code points, and whether
    [name] negates them, as [Assigned] does. It is [None] if Go has no such
    class. *)

val is_format : int -> bool
(** [is_format u] is [true] iff the code point [u] is a format character
    (category [Cf]), which does not show, other than the joiners U+200C and
    U+200D, which shape the characters around them. *)

val strip_format : string -> string
(** [strip_format s] is [s] without the characters {!is_format} holds of. The
    other bytes of [s], invalid UTF-8 included, are kept. *)
