(** Charmbracelet VHS terminal tapes.

    A VHS tape is a small, line-oriented program read by the external
    {{:https://github.com/charmbracelet/vhs}[vhs]} tool: settings such as size
    and theme, output files to render, and terminal actions such as typing text,
    pressing return, waiting for output and hiding setup steps.

    [Vhs] builds tapes as lists of {{!item}items}, prints them in the [*.tape]
    syntax, turns {{!Cram}cram tests} into tapes and {{!running}runs} [vhs] on
    them. Only {!render} performs I/O.

    {[
    let demo =
      [
        Vhs.output "demo.gif";
        Vhs.set (Width 1000);
        Vhs.set (Height 560);
        Vhs.type' "dune test";
        Vhs.key Enter;
        Vhs.wait Prompt;
      ]
    ]} *)

(** {1:tapes Tapes} *)

(** The type for VHS durations, in milliseconds. *)
type duration = Ms of int

(** The type for keys pressed in the virtual terminal. A key without an argument
    is the VHS key of the same name. *)
type key =
  | Enter
  | Tab
  | Space
  | Backspace
  | Delete
  | Insert
  | Escape
  | Up
  | Down
  | Left
  | Right
  | Page_up
  | Page_down
  | Scroll_up
  | Scroll_down
  | Ctrl of char
      (** [Ctrl c] holds control and presses [c]: an ASCII letter, [@], [^],
          [-], [.], a backslash or a square bracket. *)
  | Alt of char
      (** [Alt c] holds alt and presses [c]: an ASCII letter, [.] or a square
          bracket. *)

(** The type for terminal and renderer settings. *)
type setting =
  | Shell of string
  | Font_size of int
  | Font_family of string
  | Width of int
  | Height of int
  | Theme of string
  | Padding of int
  | Typing_speed of duration  (** Delay between typed characters. *)
  | Framerate of int

(** The type for waits on terminal output. A regular expression is
    {{:https://github.com/google/re2/wiki/Syntax}RE2} syntax without the
    surrounding slashes. *)
type wait =
  | Prompt  (** The cursor line shows the VHS shell prompt again. *)
  | Line of string  (** The cursor line matches. *)
  | Screen of string  (** Some line of the screen matches. *)

type item
(** The type for tape lines. A tape is an [item list]. *)

(** {1:items Items}

    These functions raise [Invalid_argument] if VHS cannot read the line they
    make, for instance from a string that holds a control character or all three
    VHS quote characters (["\""], ['\''] and [`]). *)

val comment : string -> item
(** [comment s] is the line [# s], or [#] if [s] is empty. *)

val blank : item
(** [blank] is an empty line. *)

val require : string -> item
(** [require program] makes VHS fail before running the tape if [program] is not
    on the [PATH]. *)

val output : string -> item
(** [output path] makes VHS render to [path], in the format of its extension, or
    to a directory of frames if [path] ends with a slash. *)

val env : string -> string -> item
(** [env name value] sets the environment variable [name] to [value] in the
    virtual terminal. [name] is letters, digits and underscores, not starting
    with a digit. *)

val set : setting -> item
(** [set s] applies the setting [s]. Integers must not be negative. *)

val type' : string -> item
(** [type' s] types [s]; use {!key} to press {!constructor-Enter} or
    {!constructor-Tab}. [s] can hold all three quote characters. *)

val key : key -> item
(** [key k] presses [k]. *)

val sleep : duration -> item
(** [sleep d] pauses the recording for [d]. *)

val wait : ?timeout:duration -> wait -> item
(** [wait ~timeout w] pauses the recording until [w] holds, and fails the
    recording after [timeout] (defaults to the VHS [WaitTimeout] setting). *)

val hide : item
(** [hide] stops rendering the terminal; its actions still run. *)

val show : item
(** [show] resumes rendering after {!val-hide}. *)

val screenshot : string -> item
(** [screenshot path] saves the current frame to the PNG file [path], which ends
    with [.png]. *)

val source : string -> item
(** [source path] runs the tape file [path]. *)

val raw : string -> item
(** [raw line] is [line], printed unchanged, for VHS commands and settings that
    have no function here. *)

(** {1:printing Printing} *)

val pp : item list Fmt.t
(** [pp] formats a tape in the [*.tape] syntax, one line per item. *)

(** {1:cram Cram tests} *)

(** Cram tests recorded as tapes.

    The [$ ] command lines become keystrokes, each followed by a wait for the
    prompt so the recording shows the command's output. Setup scripts are
    sourced, hidden, before the first command. *)
module Cram : sig
  type t
  (** The type for cram tests: a sequence of shell commands. *)

  val parse : string -> (t, string) result
  (** [parse src] is the cram test of source [src], or [Error msg] if a command
      holds a control character. A line [  $ cmd] starts a command and the
      [  > ] lines right after it continue it. Other lines, expected output and
      exit codes included, are dropped. Lines may end with ["\r\n"]. *)

  val items : ?setup:string list -> ?pause:duration -> t -> item list
  (** [items ~setup ~pause t] are the tape lines that record [t]. They source
      the shell scripts [setup] in order (defaults to [[]]) while the recording
      is hidden, then clear the screen. Each command of [t] is typed and
      submitted, then the tape waits for the {!constructor-Prompt} and pauses
      for [pause] (defaults to [Ms 1000]). *)
end

(** {1:running Running vhs} *)

(** The type for errors of {!render}. *)
type error =
  | Executable_not_found of string  (** No such executable on the [PATH]. *)
  | Failed of {
      vhs : string;
      status : [ `Exited of int | `Signaled of int ];
      stderr : string;
    }
      (** The executable [vhs] ran and did not exit with [0]; [stderr] is what
          it wrote on its standard error, such as the wait that timed out. *)
  | Io of string  (** Any other I/O failure, as a message. *)

val pp_error : error Fmt.t
(** [pp_error] formats an error for humans. *)

val render :
  proc:_ Eio.Process.mgr -> ?vhs:string -> item list -> (unit, error) result
(** [render ~proc ~vhs items] runs the executable [vhs] (defaults to ["vhs"])
    with the tape [items] on its standard input and its standard error captured
    for {!Failed}. [vhs] resolves the {!val-output} and {!val-source} paths of
    [items] against the current directory. *)
