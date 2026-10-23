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

val accent : background:int -> foreground:int -> cursor:int -> int
(** [accent ~background ~foreground ~cursor] is the {!field-cursor_accent} of a
    theme that does not give one. *)

val find : string -> t option
(** [find name] is the theme VHS v0.12.1 names [name], if any. *)

val of_json :
  string ->
  (t, [ `Json of string | `Unread of string | `Colour of string ]) result
(** [of_json s] is the theme of the JSON object [s], read with jsont, with its
    fields looked up as VHS does with Go's [encoding/json] and given to
    xterm.js: a field matches a key ignoring case, takes the last string of the
    keys that match it, and is xterm.js's default colour if it is missing,
    [null] or empty. It is [Error (`Unread msg)] if [s] is invalid UTF-8 or
    holds an unpaired surrogate escape, which jsont does not read and Go reads
    as U+FFFD; [Error (`Json msg)] if jsont does not read [s] otherwise, if [s]
    nests arrays and objects deeper than the 10000 levels of Go's
    [encoding/json], or if a field is neither a string nor [null]; and
    [Error (`Colour c)] if a colour [c] is not [#RGB], [#RGBA], [#RRGGBB],
    [#RRGGBBAA], [rgb()] or [rgba()], the forms xterm.js reads without a
    browser. *)

val plain : string -> string
(** [plain msg] is [msg] without the SGR sequences that jsont styles its
    messages with. *)

val pp : t Fmt.t
(** [pp] formats a theme's colours for humans. *)
