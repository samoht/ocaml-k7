(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Animated SVG recordings.

    The image draws one screen at a time on a grid of cells. Each distinct row
    is drawn once and placed by each screen that shows it, the screens sit side
    by side on a strip, and a CSS animation slides the strip from one screen to
    the next with [steps] timing, in an infinite loop. Every glyph is placed on
    its cell, so the alignment does not depend on the font. The image holds no
    script and no external resource. *)

type t = {
  width : int;  (** The width of the image, in pixels. *)
  height : int;  (** The height of the image, in pixels. *)
  x : int;  (** The left of the first column. *)
  y : int;  (** The top of the first row. *)
  cols : int;
  rows : int;
  cell_width : int;
  cell_height : int;
  baseline : int;  (** The baseline of a row, from its top. *)
  font_size : int;
  font_family : string;
  palette : Palette.t;
  margin : int;  (** The width of the margin around the window. *)
  margin_fill : int;  (** The colour of the margin. *)
  radius : int;  (** The radius of the window's corners. *)
  blink : bool;  (** Whether the cursor blinks. *)
  loop_offset : int;  (** The time the loop starts at, in milliseconds. *)
  speed : float;  (** The playback speed, [1.] for real time. *)
}
(** The type for the look of a recording. *)

val pp : t Fmt.t
(** [pp] formats the geometry of a look for humans. *)

val to_string : t -> duration:int -> (int * Screen.t) list -> string
(** [to_string t ~duration frames] is the image that shows each screen of
    [frames] from its time, in milliseconds, to the next one's, and the last one
    until [duration]. The times are increasing, the first is [0] and all are
    below [duration]. *)
