(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Terminal recordings in asciinema's
    {{:https://docs.asciinema.org/manual/asciicast/v2/}asciicast v2} and
    {{:https://docs.asciinema.org/manual/asciicast/v3/}asciicast v3} formats. *)

type theme = {
  foreground : string;
  background : string;
  palette : string list;  (** Eight or sixteen colours, black first. *)
  cursor : string option;  (** The [cursor] colour, not in asciicast v3. *)
  cursor_accent : string option;
      (** The [cursor_accent] colour, of the text under the cursor, not in
          asciicast v3. *)
}
(** The type for the theme of a header. Each colour is [#rrggbb]. *)

(** The type for events. *)
type event =
  | Output of string  (** An [o] event: what the program printed. *)
  | Input of string  (** An [i] event: what was typed. *)
  | Resize of int * int  (** An [r] event: the new columns and rows. *)
  | Exit of int  (** An [x] event: the exit status of the program. *)

type t = {
  columns : int;
  rows : int;
  command : string option;  (** The [command] of the header. *)
  theme : theme option;
  events : (int * event) list;
      (** The events, at their times from the start in milliseconds, after the
          [idle_time_limit] of the header. *)
}
(** The type for recordings. *)

val pp : t Fmt.t
(** [pp] formats a recording for debugging. *)

val of_string : string -> (t, string) result
(** [of_string s] is the recording [s]: a header object on its first line, then
    an event [[time, code, data]] on each line, [time] a number and [code] and
    [data] strings. Lines of JSON whitespace only are skipped, and so are the
    lines that start with [#] in version 3.

    The header of version 2 has a positive integer [width] and [height], and the
    times of its events are seconds from the start, each not before the one
    ahead of it. The header of version 3 has a [term] object with a positive
    integer ["cols"] and ["rows"], and the times of its events are seconds since
    the event ahead of it. A [theme], at the top of the header in version 2 and
    in [term] in version 3, has [fg] and [bg] colours and a ["palette"] of eight
    or sixteen colours separated by [:], each [#rrggbb], and may have the
    {!field-cursor} and {!field-cursor_accent} colours, which only k7 writes.
    With an [idle_time_limit] in the header, a longer interval between two
    events lasts that many seconds. An interval is rounded to the microsecond
    and a time to the nearest millisecond, half up.

    The [o], [i], [r] and [x] events are kept, an [r] event with [COLSxROWS]
    data and an [x] event with an integer; the other members of the header and
    the events of other codes, such as markers, are skipped. It is [Error msg]
    otherwise, with [msg] naming the line by its number from [1], and why. *)

val limit_idle : int -> t -> t
(** [limit_idle ms t] is [t] with each interval between two of its events, and
    before the first, that lasts more than [ms] milliseconds lasting [ms]. An
    [idle_time_limit] in the header differs in two ways: it counts the events
    {!of_string} skips, such as markers, and it cuts the intervals as written,
    to the microsecond, before the times are rounded. *)

val echoed : t -> t
(** [echoed t] is [t] with only the input events that the next event echoes: an
    output whose data is exactly what the input events since the event before
    them sent, at most 100 milliseconds after the last of them. *)

val retype : int -> t -> t
(** [retype ms t] is [t] typed at a steady pace: each output that echoes input
    events, as for {!echoed}, is one output per character, [ms] milliseconds
    apart, and the input events go at the time of the first. The first echo
    after another event keeps its time, and an echo that follows another one
    directly starts [ms] milliseconds after its last character, however long the
    keys took to come. Each other event keeps its interval with the event before
    it. *)

val utf_8 : (int * event) list -> (int * event) list
(** [utf_8 events] are [events] with the data of each [o] event in valid UTF-8:
    an incomplete UTF-8 sequence at its end is held until the next [o] event,
    which then starts with it, and a byte that is not UTF-8, the end of the last
    [o] event included, is U+FFFD. An [o] event left without data goes. *)

val to_string : ?idle:int -> t -> string
(** [to_string ~idle t] is [t] in asciicast v3, with the events [utf_8 t.events]
    and, if given, an [idle_time_limit] of [idle] milliseconds. {!of_string}
    reads it back, with each interval longer than [idle] shortened to it. *)
