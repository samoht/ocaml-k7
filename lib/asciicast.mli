(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Terminal recordings in
    {{:https://docs.asciinema.org/manual/asciicast/v2/}asciicast v2}. *)

type t = {
  columns : int;  (** The [width] of the header. *)
  rows : int;  (** The [height] of the header. *)
  output : (int * string) list;
      (** The data of the [o] events, at their times in milliseconds. *)
}
(** The type for recordings. *)

val pp : t Fmt.t
(** [pp] formats a recording for debugging: its size and its output events. *)

val of_string : string -> (t, string) result
(** [of_string s] is the recording [s]: a header object on its first line, with
    [version] 2 and a positive integer [width] and [height], then an event
    [[time, code, data]] on each line, with [time] in seconds, not negative and
    not before the time of the event ahead of it, and [code] and [data] strings.
    Lines of JSON whitespace only are skipped, and so are the other members of
    the header and the events whose [code] is not [o], such as input, markers
    and resizes. A time is rounded to the microsecond, then to the nearest
    millisecond, half up. It is [Error msg] otherwise, with [msg] naming the
    line by its number from [1], and why. *)
