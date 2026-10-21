(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The colours of VHS themes, as [0xRRGGBB]. *)

type t = {
  background : int;
  foreground : int;
  cursor : int;
  cursor_accent : int;
      (** The colour of the text under the cursor: the theme's accent, or the
          one of its background and foreground with the higher contrast on its
          cursor. *)
  ansi : int array;  (** The sixteen ANSI colours, black first. *)
}
(** The type for themes. *)

val default : t
(** [default] is the theme of a tape without a [Theme] setting, or with a blank
    one. *)

val find : string -> t option
(** [find name] is the theme VHS v0.12.1 names [name], if any. *)

val of_json : string -> (t, [ `Json of string | `Colour of string ]) result
(** [of_json s] is the theme of the JSON object [s], as VHS reads it with Go's
    [encoding/json] and gives it to xterm.js: a field, matched to a key ignoring
    case, is xterm.js's default colour if it is missing, [null] or empty. It is
    [Error (`Json msg)] if Go does not read [s], such as invalid JSON or a field
    that is not a string, and [Error (`Colour c)] if a colour [c] is not [#RGB],
    [#RGBA], [#RRGGBB], [#RRGGBBAA], [rgb()] or [rgba()], the forms xterm.js
    reads without a browser. *)

val pp : t Fmt.t
(** [pp] formats a theme's colours for humans. *)
