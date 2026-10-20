(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Snapshots of a terminal screen.

    A snapshot holds the cells of each row of a {!Vte.t} and its cursor. A
    colour is [-1] for the terminal's default, or [0xRRGGBB] as the terminal
    resolved it: an ANSI colour is xterm's RGB value for it. *)

type terminal
(** The type for terminals: a {!Vte.t} and the state of the bytes it reads. *)

val terminal : rows:int -> cols:int -> terminal
(** [terminal ~rows ~cols] is a terminal that has read nothing, whose default
    colours {!of_terminal} reads as [-1]. *)

val vte : terminal -> Vte.t
(** [vte t] is the emulator of [t], to query; {!feed} is how it reads. *)

val feed : terminal -> string -> unit
(** [feed t s] makes [t] read [s], the next bytes after those it read, as
    xterm.js reads them where {!Vte.feed} does not: a backspace moves the cursor
    without erasing; a character of no width, such as a combining mark, a
    zero-width space or joiner or a variation selector, joins the cell before
    the cursor, and the first column, which has none, drops it; after a
    character in the last column the cursor stays there until the next character
    wraps; line feeds keep the column; and the scroll region of each screen,
    indexes, line, character and column edits, REP, the modes of auto-wrap and
    origin, the saved cursor, the tab stops and the character sets work as in
    xterm.js, and the sequences xterm.js ignores change nothing. *)

val take_replies : terminal -> string
(** [take_replies t] is what [t] answers, as xterm.js does, to the queries it
    read since the last [take_replies]: of its primary and secondary device
    attributes, its status and its cursor position. *)

val is_dirty : terminal -> bool
(** [is_dirty t] is [true] iff the screen or the cursor of [t] changed since the
    last {!clear_dirty}. *)

val clear_dirty : terminal -> unit
(** [clear_dirty t] marks [t] unchanged. *)

type cell = {
  text : string;
      (** The grapheme, [""] on the second cell of a wide one. Valid UTF-8
          without control characters. *)
  width : int;  (** [2] for a wide grapheme, [0] after one, [1] otherwise. *)
  fg : int;
  bg : int;
  attrs : Ansi.Attr.t;
}
(** The type for cells. *)

type row
(** The type for rows. *)

val cells : row -> cell array
(** [cells r] are the cells of [r], one per column. *)

val row_text : row -> string
(** [row_text r] is the text of [r] as xterm.js's [translateToString] and
    JavaScript's [trimEnd] give it: without its trailing white space, such as
    spaces and no-break spaces. *)

val row_equal : row -> row -> bool
(** [row_equal r0 r1] is [true] iff [r0] and [r1] have equal cells. *)

type t
(** The type for screens. *)

val history : terminal -> int
(** [history t] is the number of scrollback lines of [t] that xterm.js keeps:
    those since the last [CSI 3 J], which erases them, at most a thousand, and
    none on the alternate screen. *)

val of_terminal : ?previous:t -> ?offset:int -> terminal -> t
(** [of_terminal ~previous ~offset t] is the screen of [t] scrolled [offset]
    lines into its {!history} (defaults to [0]). Rows that have not changed
    since the last {!clear_dirty} are those of [previous], if it has the same
    size. *)

val rows : t -> row array
(** [rows t] are the rows of [t], top first. *)

val cursor : t -> (int * int) option
(** [cursor t] is the row and column of the visible cursor, if any. *)

val text : t -> string
(** [text t] is the text of the rows of [t], joined by newlines. *)

val buffer : terminal -> t -> string array
(** [buffer term t] are the lines of the buffer of xterm.js, as VHS reads them:
    the {!history} of [term], oldest first, then the {!text} of each row of [t],
    the screen of [term]. *)

val line : t -> int -> string
(** [line t r] is the text of row [r] of [t]. *)

val equal : t -> t -> bool
(** [equal t0 t1] is [true] iff [t0] and [t1] have equal rows and cursors. *)
