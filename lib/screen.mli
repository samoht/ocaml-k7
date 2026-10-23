(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Snapshots of a terminal screen.

    A snapshot holds the cells of each row of a {!Vte.t}, its cursor and the
    palette colours that OSC 4 set. *)

type terminal
(** The type for terminals: xterm's emulation, as {!Vte} gives it, with the
    scrollback of VHS's xterm.js. *)

val terminal : rows:int -> cols:int -> terminal
(** [terminal ~rows ~cols] is a terminal that has read nothing. *)

val feed : ?sync:(bool -> unit) -> terminal -> string -> unit
(** [feed ~sync term s] makes [term] read [s], the next bytes after those it
    read. Each time [s] sets synchronized output (DEC private mode 2026), with
    which a program asks that the frame it draws show once complete, [term]
    reads [s] up to there and calls [sync true]; likewise [sync false] when [s]
    resets it (defaults to [ignore]). OSC 4 and OSC 104 set and reset palette
    colours, as in xterm.js. *)

val resize : terminal -> rows:int -> cols:int -> unit
(** [resize term ~rows ~cols] gives [term] [rows] rows and [cols] columns,
    keeping the part of its screen that fits, as a terminal does. *)

val take_replies : terminal -> string
(** [take_replies term] is what [term] answers to the queries it read since the
    last [take_replies], such as its device attributes, its status and its
    cursor position. *)

val cursor_keys : terminal -> bool
(** [cursor_keys term] is [true] iff [term] is in application cursor-key mode,
    in which the arrow keys send [ESC O] rather than [CSI]. *)

val cursor_row : terminal -> int
(** [cursor_row term] is the row of the cursor of [term], from [0]. *)

val is_dirty : terminal -> bool
(** [is_dirty term] is [true] iff the screen, the cursor or the palette of
    [term] changed since the last {!clear_dirty}. *)

val clear_dirty : terminal -> unit
(** [clear_dirty term] marks [term] unchanged. *)

(** The type for colours. *)
type colour =
  | Default  (** The terminal's default colour. *)
  | Indexed of int  (** The colour of a palette index, in \[0;255\]. *)
  | Rgb of int  (** A colour [0xRRGGBB]. *)

type cell = {
  text : string;
      (** The grapheme, [""] on the second cell of a wide one. Valid UTF-8
          without control characters. *)
  width : int;  (** [2] for a wide grapheme, [0] after one, [1] otherwise. *)
  fg : colour;
  bg : colour;
  attrs : Ansi.Attr.t;
}
(** The type for cells. *)

type row
(** The type for rows. *)

val cells : row -> cell array
(** [cells r] are the cells of [r], one per column. *)

val trim_end : string -> string
(** [trim_end s] is [s] without the white space and line terminators of
    JavaScript at its end, which VHS's [trimEnd] removes. *)

val row_text : row -> string
(** [row_text r] is the text of [r] as xterm.js's [translateToString] and
    JavaScript's [trimEnd] give it: without its trailing white space, such as
    spaces and no-break spaces. *)

val shown_text : row -> string
(** [shown_text r] is {!row_text}[ r] with a space for each column of a cell
    that SGR 8 conceals. *)

val row_equal : row -> row -> bool
(** [row_equal r0 r1] is [true] iff [r0] and [r1] have equal cells. *)

type t
(** The type for screens. *)

val history : terminal -> int
(** [history term] is the number of scrollback lines of [term]: those that
    scrolled off the top of its normal screen since the last [CSI 3 J], at most
    a thousand, as in VHS's xterm.js. *)

val of_terminal : ?previous:t -> ?offset:int -> terminal -> t
(** [of_terminal ~previous ~offset term] is the screen of [term] scrolled
    [offset] lines into its {!history} (defaults to [0]). Rows that have not
    changed since the last {!clear_dirty} are those of [previous], if it has the
    same size. *)

val rows : t -> row array
(** [rows t] are the rows of [t], top first. *)

val cursor : t -> (int * int) option
(** [cursor t] is the row and column of the visible cursor, if any. *)

val indexed : t -> int -> int option
(** [indexed t i] is the colour [0xRRGGBB] that OSC 4 gave the palette index [i]
    of [t], if any, which replaces the theme's. *)

val same_palette : t -> t -> bool
(** [same_palette t0 t1] is [true] iff [t0] and [t1] have the same {!indexed}
    colours. *)

val text : t -> string
(** [text t] is the text of the rows of [t], joined by newlines. *)

val buffer : terminal -> t -> string array
(** [buffer term t] are the lines of the buffer of [term], as VHS reads those of
    xterm.js: the {!history} of [term], oldest first, then the {!row_text} of
    each row of [t], the screen of [term]. *)

val line : t -> int -> string
(** [line t r] is [row_text (rows t).(r)], or [""] if [t] has no row [r]. *)

val equal : t -> t -> bool
(** [equal t0 t1] is [true] iff [t0] and [t1] have equal rows, cursors and
    palettes. *)
