(*---------------------------------------------------------------------------
  Copyright (c) 2024 The jsont programmers. All rights reserved.
  SPDX-License-Identifier: ISC

  Extracted from jsont (jsont.mli / jsont_base.mli) with JSON-specific pieces
  removed.
  ---------------------------------------------------------------------------*)

(** Text locations: byte ranges and line positions.

    A location identifies a span within a UTF-8 encoded file by an inclusive
    range of absolute byte positions and the line positions on which they occur.
    Designed for parser error reporting; the representation is compatible with
    the OCaml compiler's format and the GNU error convention. *)

(** {1:fpath File paths} *)

type fpath = string
(** The type for file paths. *)

val file_none : fpath
(** [file_none] is ["-"]. A file path to use when there is none. *)

(** {1:pos Positions} *)

(** {2:byte_pos Byte positions} *)

type byte_pos = int
(** The type for zero-based, absolute byte positions in text. If the text has
    [n] bytes, [0] is the first position and [n-1] is the last position. *)

val byte_pos_none : byte_pos
(** [byte_pos_none] is [-1]. A position to use when there is none. *)

val compare_byte_pos : byte_pos -> byte_pos -> int
(** [compare_byte_pos] orders byte positions by their distance from the start of
    the text, so {!byte_pos_none} sorts before every real position. *)

(** {2:lines Lines} *)

type line_num = int
(** The type for one-based line numbers. Lines increment after a newline which
    is either a line feed ['\n'] (U+000A), a carriage return ['\r'] (U+000D), or
    a carriage return followed by a line feed ["\r\n"] (<U+000D,U+000A>). *)

val line_num_none : line_num
(** [line_num_none] is [-1]. A line number to use when there is none. *)

val compare_line_num : line_num -> line_num -> int
(** [compare_line_num] orders line numbers by their distance from the start of
    the text, so {!line_num_none} sorts before every real line. *)

(** {2:line_pos Line positions}

    A line position is a line number paired with the absolute byte position
    following its newline (or the start of text for the first line). That byte
    position:
    - indexes the first byte of the line if it is non-empty;
    - indexes the first byte of the next newline sequence if the line is empty;
    - is out of bounds and equal to the text's length for a last empty line
      (also the case on empty text). *)

(** {1:tloc Text locations} *)

type t
(** The type for text locations. If the first byte equals the last byte the
    range contains exactly that byte. If the first byte is greater than the last
    byte the location represents an insertion point before the first byte; in
    that case last-position information should be ignored. *)

val none : t
(** [none] is a location to use when there is none. *)

val v :
  file:fpath ->
  first_byte:byte_pos ->
  last_byte:byte_pos ->
  first_line_num:line_num ->
  first_line_byte:byte_pos ->
  last_line_num:line_num ->
  last_line_byte:byte_pos ->
  t
(** [v ~file ~first_byte ~last_byte ~first_line_num ~first_line_byte
     ~last_line_num ~last_line_byte] is the text location spanning [first_byte]
    to [last_byte] of [file], on the lines those two byte positions start. Use
    {!file_none} when there is no file. *)

val file : t -> fpath
(** [file l] is [l]'s file path. *)

val set_file : t -> fpath -> t
(** [set_file l p] is [l] with file replaced by [p]. *)

val first_byte : t -> byte_pos
(** [first_byte l] is the byte offset of [l]'s first character. *)

val last_byte : t -> byte_pos
(** [last_byte l] is the byte offset of [l]'s last character. *)

val first_line_num : t -> line_num
(** [first_line_num l] is the 1-based line number of [l]'s first character. *)

val first_line_byte : t -> byte_pos
(** [first_line_byte l] is the byte offset of the start of [l]'s first line. *)

val last_line_num : t -> line_num
(** [last_line_num l] is the 1-based line number of [l]'s last character. *)

val last_line_byte : t -> byte_pos
(** [last_line_byte l] is the byte offset of the start of [l]'s last line. *)

(** {2:preds Predicates and comparisons} *)

val is_none : t -> bool
(** [is_none t] is [true] iff [first_byte t < 0]. *)

val is_empty : t -> bool
(** [is_empty t] is [true] iff [first_byte t > last_byte t]. *)

val equal : t -> t -> bool
(** [equal t0 t1] is [true] iff {!val-file}, {!val-first_byte}, and
    {!val-last_byte} are equal. Line information is ignored. *)

val compare : t -> t -> int
(** [compare] orders locations by [(file, first_byte, last_byte)], compatible
    with {!equal}. *)

(** {2:shrink_and_stretch Shrink and stretch} *)

val to_first : t -> t
(** [to_first l] has both positions set to [l]'s first position. *)

val to_last : t -> t
(** [to_last l] has both positions set to [l]'s last position. *)

val before : t -> t
(** [before l] is the empty location at {!val-first_byte}, on [l]'s first line.
*)

val after : t -> t
(** [after l] is the empty location at [last_byte + 1], on [l]'s last line. *)

val span : t -> t -> t
(** [span l0 l1] covers from the smallest first byte of either to the largest
    last byte of either. File is taken from the location with the greater last
    byte. *)

(** {2:fmt Formatting} *)

val pp_ocaml : Format.formatter -> t -> unit
(** [pp_ocaml ppf l] formats [l] like the OCaml compiler:
    [File "f.ml", line 3, characters 10-15]. *)

val pp_gnu : Format.formatter -> t -> unit
(** [pp_gnu ppf l] formats [l] per the
    {{:https://www.gnu.org/prep/standards/standards.html#Errors} GNU convention}:
    [f.ml:3.10-15]. *)

val pp : Format.formatter -> t -> unit
(** [pp] is {!pp_ocaml}. *)

(** {1:meta Metadata} *)

(** Abstract syntax tree node metadata.

    Keeps source text locations and surrounding whitespace. *)
module Meta : sig
  type loc := t

  type t
  (** The type for node metadata. *)

  val v : ?ws_before:string -> ?ws_after:string -> ?text:string -> loc -> t
  (** [v ~ws_before ~ws_after ~text loc] is the metadata with source location
      [loc], preceded by the whitespace [ws_before] and followed by [ws_after]
      (both default to [""]), reading into the source text [text] (defaults to
      none). *)

  val none : t
  (** [none] is metadata with no location and no whitespace. *)

  val is_none : t -> bool
  (** [is_none m] is [true] iff [m] is {!none} (physical equality). *)

  val loc : t -> loc
  (** [loc m] is the source location of [m]. *)

  val ws_before : t -> string
  (** [ws_before m] is the whitespace preceding the node. *)

  val ws_after : t -> string
  (** [ws_after m] is the whitespace following the node. *)

  val text : t -> string option
  (** [text m] is the source text {!val-loc} indexes into, when the parser that
      built [m] kept it, and [None] otherwise.

      A node that has it can be written back byte for byte by slicing [text] at
      [loc], layout included, without rendering the value the node denotes. A
      parser records it for that purpose, and it is also a claim: a node given a
      parsed node's metadata asserts that its bytes are those bytes, so a value
      built by hand from one takes the layout along with the location. Build
      such a value with {!val-v} on the location alone, or with
      {!val-clear_text}, and it is rendered rather than copied.

      Every node of one document shares one string, so keeping it costs a word
      per node rather than a copy per node. *)

  val with_loc : t -> loc -> t
  (** [with_loc m loc] is [m] with source location set to [loc]. *)

  val with_text : t -> string -> t
  (** [with_text m text] is [m] reading into [text]. *)

  val clear_text : t -> t
  (** [clear_text m] is [m] with no source text: it keeps where the node was and
      drops the claim that the bytes there are still its own. A replacement in
      an existing document uses it: the location says which bytes it takes the
      place of, and the missing text says they have to be written afresh. *)

  val clear_ws : t -> t
  (** [clear_ws m] is [m] with both whitespace fields cleared. *)

  val clear_loc : t -> t
  (** [clear_loc m] is [m] with its source location set to {!none}. *)

  val copy_ws : t -> dst:t -> t
  (** [copy_ws src ~dst] is [dst] with its whitespace fields copied from [src].
  *)
end

type 'a node = 'a * Meta.t
(** The type for abstract syntax tree nodes: a value paired with its {!Meta.t}.
*)

(** {1:paths Structural paths}

    Paths address sub-values in a structured document. The step alphabet is
    extensible: {!Path.extension-Mem} and {!Path.extension-Nth} are the baseline
    for name-addressed and index-addressed descent; formats add their own native
    steps (XML [Attribute], CBOR [Cbor_key], protobuf [Field_number], ...) via
    extension constructors and register printers for them. *)
module Path : sig
  type step = ..
  (** The type for path steps. Extensible: formats add native addressing via
      [type step += ...] and {!register_step_printer}. *)

  type step +=
    | Mem of string node
          (** Name-addressed step: object member, XML element local-name, TOML
              table name, etc. *)
    | Nth of int node  (** Index-addressed step: array index, tuple position. *)

  val register_step_printer :
    (step -> (Format.formatter -> unit) option) -> unit
  (** [register_step_printer p] adds [p] to the step printer registry {!pp_step}
      uses. Printers are tried in reverse order of registration and the first
      one returning [Some f] is used; the fallback handles {!extension-Mem} and
      {!extension-Nth}. Call it once per format at load time. *)

  val pp_step : Format.formatter -> step -> unit
  (** [pp_step] formats a step without source location, via the registry. *)

  type t
  (** The type for paths. Internally leaf-to-root (last step at the head) for
      O(1) cons; public accessors normalize. *)

  val root : t
  (** [root] is the empty path. *)

  val is_root : t -> bool
  (** [is_root p] is [true] iff [p] is {!root}. *)

  val push : step -> t -> t
  (** [push s p] extends [p] with step [s]. The new step becomes the innermost
      position. *)

  val nth : ?meta:Meta.t -> int -> t -> t
  (** [nth ~meta n p] is [push (Nth (n, meta)) p]. *)

  val mem : ?meta:Meta.t -> string -> t -> t
  (** [mem ~meta n p] is [push (Mem (n, meta)) p]. *)

  val steps : t -> step list
  (** [steps p] is [p]'s steps in {b root-to-leaf} order, which is what matching
      or printing a path from the outside in needs. It costs one list reversal.
  *)

  val rev_steps : t -> step list
  (** [rev_steps p] is [p]'s steps in {b leaf-to-root} order, the raw internal
      order. It is cheaper than {!steps}, for when iteration order does not
      matter. *)

  val of_string : string -> (t, string) result
  (** [of_string s] is the path the dot-separated string [s] denotes, as in
      ["a.b.[2].c"]. Only the {!extension-Mem} and {!extension-Nth} baseline is
      recognized. A step matching [-?[0-9]+] is an {!extension-Nth}, any other
      unbracketed step is a {!extension-Mem} named by it, and a bracketed step
      that is not an index is an error. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats paths as dot-separated steps, root-to-leaf. *)
end

(** {1:context Navigation contexts}

    A context describes {b where} in a structured document we are: the full
    {!Path.t} from the root, plus the {e sort} label of each parent container.
    Errors hold a context (pointing at the offending node); cursors produce a
    context (describing the current focus); stream callbacks receive a context
    (naming the child they were handed).

    One noun, top to bottom. *)
module Context : sig
  type t
  (** The type for contexts. *)

  val empty : t
  (** [empty] is the root context. *)

  val is_empty : t -> bool
  (** [is_empty ctx] is [true] iff [ctx] is {!empty}. *)

  val push : sort:string node -> Path.step -> t -> t
  (** [push ~sort s ctx] extends [ctx]: the current node's parent has sort label
      [sort], and we descend into it via step [s]. *)

  val push_nth : string node -> int node -> t -> t
  (** [push_nth sort n ctx] is [push ~sort (Path.Nth n) ctx]. *)

  val push_mem : string node -> string node -> t -> t
  (** [push_mem sort n ctx] is [push ~sort (Path.Mem n) ctx]. *)

  val snoc : sort:string node -> Path.step -> t -> t
  (** [snoc ~sort s ctx] is {!val-push}. It is the spelling for a decoder that
      builds the context only when an error unwinds: the frames it meets, from
      the innermost outwards, each enclose everything it has stored so far. *)

  (** {2:accessors Accessors} *)

  val path : t -> Path.t
  (** [path ctx] is the path from the root to the current position. *)

  val last_step : t -> Path.step option
  (** [last_step ctx] is the innermost step, or [None] at the root. *)

  val last_sort : t -> string node option
  (** [last_sort ctx] is the sort label of the innermost frame's parent, or
      [None] at the root. *)

  val frames : t -> (string node * Path.step) list
  (** [frames ctx] is the list of [(sort, step)] frames in root-to-leaf order.
  *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats a context as an indented trace. *)
end

(** {1:errors Errors} *)

(** Encoding, decoding, and query errors.

    Errors carry a {!Context.t} (the path and sort labels from the root value to
    the erroring sub-value), the source location of the error, and a kind. *)
module Error : sig
  type kind = ..
  (** The type of error kinds. Extensible: each codec library extends this with
      its own typed constructors (e.g. sort mismatches, missing members,
      duplicate keys) and registers a printer via {!register_kind_printer}. *)

  type kind +=
    | Msg of string
          (** [Msg s] is the generic message kind used by {!msg}, {!fail}, and
              {!failf}. *)

  val register_kind_printer :
    (kind -> (Format.formatter -> unit) option) -> unit
  (** [register_kind_printer p] adds [p] to the printer registry. When
      formatting a {!val-kind}, registered printers are tried in reverse order
      of registration; the first one returning [Some f] is used. Fallback
      handles {!Msg}. Call this once per codec library at load time. *)

  val pp_kind : Format.formatter -> kind -> unit
  (** [pp_kind] formats a kind via the registered printers. *)

  val string_of_kind : kind -> string
  (** [string_of_kind k] is the kind formatted as a string. *)

  type t = { ctx : Context.t; meta : Meta.t; kind : kind }
  (** The type for errors. Exposed as a record so consumers can destructure with
      [match e with Error { ctx; meta; kind }]. *)

  val v : ctx:Context.t -> meta:Meta.t -> kind -> t
  (** [v ~ctx ~meta kind] is [{ ctx; meta; kind }]. *)

  val ctx : t -> Context.t
  (** [ctx e] is [e.ctx]. *)

  val meta : t -> Meta.t
  (** [meta e] is [e.meta]. *)

  val kind : t -> kind
  (** [kind e] is [e.kind]. *)

  val msg : ctx:Context.t -> meta:Meta.t -> string -> t
  (** [msg ~ctx ~meta s] is [v ~ctx ~meta (Msg s)]. *)

  val raise : ctx:Context.t -> meta:Meta.t -> kind -> 'a
  (** [raise ~ctx ~meta kind] raises the {!Loc.exception-Error} exception
      carrying [v ~ctx ~meta kind]. *)

  val fail : Meta.t -> string -> 'a
  (** [fail meta s] raises an error with empty context and message [s]. *)

  val failf : Meta.t -> ('a, Format.formatter, unit, 'b) format4 -> 'a
  (** [failf meta fmt] raises an error with empty context, message formatted per
      [fmt]. *)

  val expected : Meta.t -> string -> fnd:string -> 'a
  (** [expected meta exp ~fnd] raises an ["Expected exp but found fnd"] error.
  *)

  val push_array : string node -> int node -> t -> 'a
  (** [push_array sort n e] re-raises [e] after pushing an array index onto its
      context. *)

  val push_object : string node -> string node -> t -> 'a
  (** [push_object sort n e] re-raises [e] after pushing an object member onto
      its context. *)

  val adjust_context :
    first_byte:byte_pos ->
    first_line_num:line_num ->
    first_line_byte:byte_pos ->
    t ->
    'a
  (** [adjust_context ~first_byte ~first_line_num ~first_line_byte e] re-raises
      [e] with the first-position of its innermost context's location updated.
  *)

  val to_string : t -> string
  (** [to_string e] is the error formatted as a string. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats errors with location, message, and context, one per line:

      {v
      File "-", line 1, characters 27-28:
      Expected OCaml int number but found bool
      File "-", line 1, characters 20-26: in member port of
      File "-", line 1, characters 0-28: config object
      v}

      The location leads and the colon that ends it introduces the message
      below, as every OCaml tool writes it, so editors, CI annotators and
      anything matching [file:line:col:] can parse the first line. A location
      naming a file and no position still leads, as [File "a.json":], which is
      the location an error about a whole document carries. An error whose meta
      names neither ({!Meta.none}) prints the message alone, with no leading
      colon and no blank line. Context frames print their own location prefix.
  *)

  val pp_label : Format.formatter -> unit -> unit
  (** [pp_label ppf ()] prints ["Error:"] in bold red.

      It belongs {e after} the location line and immediately before the message,
      which is where OCaml puts it. {!val-pp} prints location and message
      together, so a label prefixed to {!val-pp}'s output would land ahead of
      the location and break the [file:line:col:] prefix. {!pp_labelled} is that
      form assembled correctly; reach for this printer only to label something
      that is not an {!type-t}.

      Styling takes effect only on a formatter configured for ANSI output
      through [Fmt.set_style_renderer], as [Fmt.pr] and [Fmt.pp_stderr] are. A
      plain formatter, [Format.asprintf] and [Format.str_formatter] included,
      emits the text without escapes, so {!to_string} is always plain. *)

  val pp_labelled : Format.formatter -> t -> unit
  (** [pp_labelled] is {!val-pp} with {!pp_label} between the location and the
      message, which is the whole diagnostic as the OCaml compiler writes it:

      {v
      File "config.json", line 1, characters 27-28:
      Error: Expected OCaml int number but found bool
      File "config.json", line 1, characters 20-26: in member port of
      File "config.json", line 1, characters 0-28: config object
      v}

      A tool printing a decode failure to a terminal wants this one: the first
      line still carries the [file:line:col:] prefix editors match on, and the
      label says the diagnostic is an error rather than a warning. An error with
      no location opens on the label, which is also where OCaml puts it. *)
end

exception Error of Error.t
(** Raised by codec mappers and decoders to abort with a structured error. *)
