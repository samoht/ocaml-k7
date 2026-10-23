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
    U+200D, which shape the characters around them, and the prepended
    concatenation marks, such as U+0605, which show over the digits after them.
*)

val is_selector : int -> bool
(** [is_selector u] is [true] iff the code point [u] is a variation selector,
    U+FE00 to U+FE0F or U+E0100 to U+E01EF. *)

val is_base : int -> bool
(** [is_base u] is [true] iff a selector or a joiner after the code point [u]
    joins it: [u] is not a space, a control, a selector or a joiner. *)

val strip_format : string -> string
(** [strip_format s] is [s] without the characters {!is_format} holds of, but
    for the tag characters of an emoji tag sequence, U+1F3F4 followed by tag
    characters and U+E007F, which picks a subdivision flag such as Scotland's.
    It also drops a variation selector ({!is_selector}) that does not follow a
    character that shows: at the start of [s], or after a space, a control, a
    joiner or another selector. The other bytes of [s], invalid UTF-8 included,
    are kept. *)
