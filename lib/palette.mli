(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The colours of VHS themes, as [0xRRGGBB]. *)

type t = {
  background : int;
  foreground : int;
  cursor : int;
  cursor_accent : int;  (** The colour of the text under the cursor. *)
  ansi : int array;  (** The sixteen ANSI colours, black first. *)
}
(** The type for themes. *)

val default : t
(** [default] is the theme of a tape without a [Theme] setting, or with a blank
    one. *)

val find : string -> t option
(** [find name] is the theme VHS v0.12.1 names [name], if any. *)

val pp : t Fmt.t
(** [pp] formats a theme's colours for humans. *)
