(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Snapshots of a terminal screen.

    A snapshot holds the cells of each row of a {!Vte.t} and its cursor. A
    colour is [-1] for the terminal's default, or [0xRRGGBB] as the terminal
    resolved it: an ANSI colour is xterm's RGB value for it. *)

val terminal : rows:int -> cols:int -> Vte.t
(** [terminal ~rows ~cols] is a terminal whose default colours {!of_vte} reads
    as [-1]. *)

type input
(** The type for the state of the bytes a terminal reads. *)

val input : unit -> input
(** [input ()] is the state of a terminal that has read nothing. *)

val feed : input -> Vte.t -> string -> unit
(** [feed i vte s] makes [vte] read [s], the next bytes after those [i] saw. A
    backspace outside an escape sequence moves the cursor left, as in xterm,
    where {!Vte.feed} would also erase the character it moves over. *)

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
(** [row_text r] is the text of [r] without its trailing spaces, as xterm.js's
    [translateToString] and [trimEnd] give it. *)

val row_equal : row -> row -> bool
(** [row_equal r0 r1] is [true] iff [r0] and [r1] have equal cells. *)

type t
(** The type for screens. *)

val of_vte : ?previous:t -> ?offset:int -> Vte.t -> t
(** [of_vte ~previous ~offset vte] is the screen of [vte] scrolled [offset]
    lines into its scrollback (defaults to [0]). Rows [vte] reports clean are
    those of [previous], if it has the same size. *)

val rows : t -> row array
(** [rows t] are the rows of [t], top first. *)

val cursor : t -> (int * int) option
(** [cursor t] is the row and column of the visible cursor, if any. *)

val text : t -> string
(** [text t] is the text of the rows of [t], joined by newlines. *)

val line : t -> int -> string
(** [line t r] is the text of row [r] of [t]. *)

val equal : t -> t -> bool
(** [equal t0 t1] is [true] iff [t0] and [t1] have equal rows and cursors. *)
