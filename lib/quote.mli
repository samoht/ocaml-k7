(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Strings quoted for messages. *)

val pp : string Fmt.t
(** [pp] formats a string between double quotes for a message: valid UTF-8 as it
    is, a double quote and a backslash escaped with a backslash, a tab, a
    newline and a carriage return as [\t], [\n] and [\r], another C0 control and
    DEL as [\xNN], a C1 control as [\u{NN}], and an invalid byte as U+FFFD. *)
