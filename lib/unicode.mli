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
