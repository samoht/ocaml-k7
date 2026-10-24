(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Playing tapes and programs without VHS.

    A tape runs in bash, zsh or fish on a pseudo-terminal whose output a
    {!Vte.t} reads, and its screens make an {!Svg} image. Time is counted by the
    tape: typing, a key and a sleep take the time the tape gives them, whatever
    the recorder spends around them, and a wait takes the time the program makes
    it wait. Output gets the time it arrives at within the action that runs,
    rounded down to a frame. *)

(** The type for the reasons a tape does not play, as [K7.error] names them. *)
type error =
  | Executable_not_found of string  (** An executable is not on the [PATH]. *)
  | Io of string  (** Any other I/O failure. *)
  | Unsupported of string  (** A line only VHS plays, and why. *)
  | Timeout of {
      wait : string;
      line : int;
      file : string option;
      last : string;
    }
      (** The wait line [wait], line [line] from 1 of the tape, or of the tape
          [file] a [Source] line names, did not hold in time; [last] is what it
          last read. *)
  | Stuck of { wait : string; line : int; file : string option; last : string }
      (** The [stuck] of {!play} held while the wait line [wait], line [line] of
          [file] as for {!constructor-Timeout}, did not; [last] is what it last
          read. *)
  | Exited of {
      status : Eio.Process.exit_status;
      wait : string;
      line : int;
      file : string option;
      last : string;
    }
      (** The shell exited with [status] while the wait line [wait], line [line]
          of [file] as for {!constructor-Timeout}, did not hold; [last] is what
          it last read. *)
  | Invalid of string
      (** A line of the tape of a [Source] line that VHS rejects or that holds a
          control character, or an empty command, and why. *)

type t
(** The type for tapes to play. *)

val of_lines :
  ?source:(string -> (string, error) result) -> string list -> (t, error) result
(** [of_lines ~source lines] is the tape of [lines], each one a command that
    {!Tape_syntax.check} accepts, with the lines of the tape [source path] reads
    in place of a [Source path] line, as VHS's parser inlines them, and the
    image of a [Set MarginFill path] line; [source] is [Error e] if it cannot
    read [path] (defaults to always [Error (Io _)]). It is
    [Error (Unsupported _)] on a line this module cannot play, such as a
    [Screenshot] of a [.png] image, which VHS writes, and [Error (Invalid _)] on
    a [Source] VHS refuses or whose tape holds a control character other than a
    tab or a carriage return, and on a [Set MarginFill] image without the
    signature of a PNG, JPEG, GIF, WebP or BMP image. *)

val text_output : t -> string option
(** [text_output t] is the path of the text output of [t], its [Output] line
    with a [.txt], [.ascii] or [.test] extension, if any. *)

val stills : t -> string list
(** [stills t] are the paths of the [Screenshot] lines of [t], in order. *)

type recording = {
  svg : string;  (** The animated image. *)
  text : string;  (** The text output, in VHS's format. *)
  stills : (string * string) list;
      (** The path and the image of each [Screenshot]. *)
}
(** The type for what a tape records. *)

val play :
  ?interrupt:unit Eio.Promise.t ->
  ?stuck:(string -> bool) ->
  ?idle:int ->
  clock:_ Eio.Time.Mono.t ->
  env:Eio.Process.Env.t ->
  vars:(string * string) list ->
  cwd:string ->
  t ->
  (recording, error) result
(** [play ~interrupt ~stuck ~idle ~clock ~env ~vars ~cwd t] runs [t] in its
    shell in the directory [cwd] and is what it recorded, until the last action
    or until [interrupt] resolves. While a wait does not hold, [stuck out], with
    [out] the last 64 KiB the shell wrote since the previous wait held (defaults
    to [false]), fails it at once. In the image, a pause between two screens
    that lasts more than [idle] milliseconds lasts that long (defaults to no
    limit). The text of {!text_output} is in VHS's format: the first lines of
    the buffer, as many as the screen has rows, after each command that runs
    while recording, from the first action, each screen followed by a line of 80
    U+2500 characters. A [Screenshot] of [shot.svg] is a still SVG image of the
    next frame it records, without the cursor blinking. The shell gets [env]
    with [TERM=xterm-256color], then [vars], the prompt VHS gives it and the
    [Env] lines of [t]. *)

val check_argv : string list -> (unit, error) result
(** [check_argv argv] is [Error (Io _)], the error [execve] gives, if an
    argument of [argv] holds a NUL byte, and [Ok ()] otherwise. *)

val resolve : cwd:string -> Eio.Process.Env.t -> string -> string option
(** [resolve ~cwd env exe] is the file [exe] names, as [execvp] finds it: [exe]
    itself if it holds a slash, and otherwise the first executable file named
    [exe] in a directory of the [PATH] of [env], or the first such file if none
    is executable, so that running it reports why. A relative path is relative
    to [cwd]. It is [None] if there is no such file. *)

val size : t -> int * int
(** [size t] is the number of columns and rows of the terminal of [t]. *)

val palette : t -> Palette.t
(** [palette t] is the theme of the terminal of [t]. *)

val replay : t -> (int * Asciicast.event) list -> string * Screen.t
(** [replay t events] is the SVG image of a terminal with the settings of [t]
    that receives each event of [events] at its time, in milliseconds, and the
    last screen it shows. An output is fed to the terminal, a resize gives it
    the new size, at most {!size}, and an exit does nothing; the image lasts
    until the last event. *)

val capture :
  ?interrupt:unit Eio.Promise.t ->
  ?input:_ Eio.Flow.source ->
  ?echo:_ Eio.Flow.sink ->
  clock:_ Eio.Time.Mono.t ->
  env:Eio.Process.Env.t ->
  vars:(string * string) list ->
  cwd:string ->
  line:string ->
  t ->
  string list ->
  ((int * string) list * int * Eio.Process.exit_status, error) result
(** [capture ~interrupt ~input ~echo ~clock ~env ~vars ~cwd ~line t argv] runs
    the program [argv] in the directory [cwd] on a terminal with the settings of
    [t], until it exits, after [line], which the terminal reads first, and is
    what the program printed, at the times it arrived since the program started,
    in milliseconds, the time it exited and how. The program gets [env] with
    [TERM=xterm-256color], then [vars]. What [input] gives goes to the program
    as typed, and the terminal answers none of its queries, which the terminal
    that gives [input] answers instead. What the program prints is also written
    to [echo]. Once [interrupt] resolves, its process group gets [SIGTERM], then
    [SIGKILL] a second later by [clock]. It is [Error (Invalid _)] if [argv] is
    empty. *)
