(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Playing tapes without VHS.

    A tape runs in bash or zsh on a pseudo-terminal whose output a {!Vte.t}
    reads, and its screens make an {!Svg} image. Time is counted by the tape:
    typing, a key and a sleep take the time the tape gives them, whatever the
    recorder spends around them, and a wait takes the time the program makes it
    wait. Output gets the time it arrives at within the action that runs,
    rounded down to a frame. *)

(** The type for the reasons a tape does not play, as [Vhs.error] names them. *)
type error =
  | Executable_not_found of string  (** An executable is not on the [PATH]. *)
  | Io of string  (** Any other I/O failure. *)
  | Unsupported of string  (** A line only VHS plays, and why. *)
  | Timeout of { wait : string; last : string }
      (** A wait line did not hold in time; [last] is what it last read. *)
  | Invalid of string
      (** A line of the tape of a [Source] line that VHS rejects or that holds a
          control character, and why. *)

type t
(** The type for tapes to play. *)

val of_lines :
  ?source:(string -> (string, string) result) ->
  string list ->
  (t, error) result
(** [of_lines ~source lines] is the tape of [lines], each one a command that
    {!Tape_syntax.check} accepts, with the lines of the tape [source path] reads
    in place of a [Source path] line, as VHS's parser inlines them; [source] is
    [Error msg] if it cannot read [path] (defaults to always [Error]). It is
    [Error (Unsupported _)] on a line this module cannot play, and
    [Error (Invalid _)] on a [Source] VHS refuses or whose tape holds a control
    character other than a tab or a carriage return. *)

val text_output : t -> string option
(** [text_output t] is the path of the text output of [t], its [Output] line
    with a [.txt], [.ascii] or [.test] extension, if any. *)

type recording = {
  svg : string;  (** The animated image. *)
  text : string;  (** The text output, in VHS's format. *)
  stills : (string * string) list;
      (** The [.svg] path and the image of each [Screenshot]. *)
}
(** The type for what a tape records. *)

val play :
  clock:_ Eio.Time.Mono.t ->
  env:Eio.Process.Env.t ->
  cwd:string ->
  t ->
  (recording, error) result
(** [play ~clock ~env ~cwd t] runs [t] in its shell in the directory [cwd] and
    is what it recorded. The text of {!text_output} is in VHS's format: the
    first lines of the buffer, as many as the screen has rows, after each
    command that runs while recording, from the first action or from the
    [Output] line that names it, each screen followed by a line of 80 U+2500
    characters. A [Screenshot] of [shot.png], which VHS writes as a PNG image of
    the next frame it records, is a still SVG image of that frame, without the
    cursor blinking, for [shot.svg]. The shell gets [env] with the prompt VHS
    gives it, [TERM=xterm-256color] and the [Env] lines of [t]. *)

val replay : t -> (int * string) list -> string
(** [replay t output] is the SVG image of a terminal with the settings of [t]
    that receives each string of [output] at its time, in milliseconds. *)
