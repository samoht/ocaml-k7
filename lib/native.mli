(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Playing tapes without VHS.

    A tape runs in bash on a pseudo-terminal whose output a {!Vte.t} reads, and
    its screens make an {!Svg} image. Time is counted by the tape: typing, a key
    and a sleep take the time the tape gives them, whatever the recorder spends
    around them, and a wait takes the time the program makes it wait. Output
    gets the time it arrives at within the action that runs, rounded down to a
    frame. *)

(** The type for the reasons a tape does not play. *)
type error =
  | Unsupported of string  (** A line only VHS plays, and why. *)
  | Timeout of { wait : string; last : string }
      (** A wait line did not hold in time; [last] is what it last read. *)
  | Not_found of string  (** An executable is not on the [PATH]. *)
  | Io of string  (** Any other failure. *)

type t
(** The type for tapes to play. *)

val of_lines : string list -> (t, error) result
(** [of_lines lines] is the tape of [lines], each one a command that
    {!Tape_syntax.check} accepts. It is [Error (Unsupported _)] on a line this
    module cannot play. *)

val play :
  clock:_ Eio.Time.Mono.t ->
  env:Eio.Process.Env.t ->
  cwd:string ->
  t ->
  (string, error) result
(** [play ~clock ~env ~cwd t] runs [t] in bash in the directory [cwd] and is the
    SVG image of what it showed. Bash gets [env] with the prompt VHS gives it,
    [TERM=xterm-256color] and the [Env] lines of [t]. *)

val replay : t -> (int * string) list -> string
(** [replay t output] is the SVG image of a terminal with the settings of [t]
    that receives each string of [output] at its time, in milliseconds. *)
