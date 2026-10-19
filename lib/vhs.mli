(** Charmbracelet VHS terminal tapes.

    A VHS tape is a small, line-oriented program read by the external
    {{:https://github.com/charmbracelet/vhs}[vhs]} tool: settings such as size
    and theme, output files to render, and terminal actions such as typing text,
    pressing return, waiting for output and hiding setup steps.

    [Vhs] builds tapes as values, prints them in the [*.tape] syntax, turns
    {{!Cram}cram tests} into tapes and {{!running}runs} [vhs] on them. Only
    {!render} performs I/O.

    {[
    let demo =
      Vhs.tape
        [
          Vhs.output "demo.gif";
          Vhs.set (Width 1000);
          Vhs.set (Height 560);
          Vhs.type' "dune test";
          Vhs.enter ();
          Vhs.wait Prompt;
        ]
    ]} *)

(** {1:tapes Tapes} *)

(** The type for VHS durations, in milliseconds or seconds. Seconds are printed
    as a decimal with at most nine fractional digits. *)
type duration = Ms of int | S of float

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
  | Custom of string * string
      (** [Custom (name, value)] is [Set name value], printed unchanged. *)

(** The type for waits on terminal output. A regular expression is
    {{:https://github.com/google/re2/wiki/Syntax}RE2} syntax without the
    surrounding slashes. *)
type wait =
  | Prompt  (** The cursor line shows the VHS shell prompt again. *)
  | Line of string  (** The cursor line matches. *)
  | Screen of string  (** Some line of the screen matches. *)

type item
(** The type for tape lines. *)

type t
(** The type for VHS tapes. *)

val tape : item list -> t
(** [tape items] is the tape of [items], one line each, in order. *)

(** {1:items Items}

    These functions raise [Invalid_argument] if VHS cannot read the line they
    make, for instance from a string that holds a control character or all three
    VHS quote characters (["\""], ['\''] and [`]). *)

val comment : string -> item
(** [comment s] is the line [# s], or [#] if [s] is empty. *)

val blank : unit -> item
(** [blank ()] is an empty line. *)

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
(** [type' s] types [s] without pressing return; newlines and tabs in [s] are
    typed as {!constructor-Enter} and {!constructor-Tab}. [s] can hold all three
    quote characters. *)

val key : key -> item
(** [key k] presses [k]. *)

val enter : unit -> item
(** [enter ()] is [key Enter]. *)

val sleep : duration -> item
(** [sleep d] pauses the recording for [d]. *)

val wait : ?timeout:duration -> wait -> item
(** [wait ~timeout w] pauses the recording until [w] holds, and fails the
    recording after [timeout] (defaults to the VHS [WaitTimeout] setting). *)

val hide : unit -> item
(** [hide ()] stops rendering the terminal; its actions still run. *)

val show : unit -> item
(** [show ()] resumes rendering after {!val-hide}. *)

val screenshot : string -> item
(** [screenshot path] saves the current frame to the PNG file [path], which ends
    with [.png]. *)

val source : string -> item
(** [source path] runs the tape file [path]. *)

val raw : string -> item
(** [raw line] is [line], printed unchanged. *)

(** {1:printing Printing} *)

val pp : t Fmt.t
(** [pp] formats a tape in the [*.tape] syntax, one line per item. *)

(** {1:cram Cram tests} *)

type tape := t

(** Cram tests recorded as tapes.

    The [$ ] command lines become keystrokes, each followed by a wait for the
    prompt so the recording shows the command's output. Setup scripts are
    sourced, hidden, before the first command. *)
module Cram : sig
  type t
  (** The type for cram tests: a sequence of commands, each with its expected
      output and exit code. *)

  val parse : string -> (t, string) result
  (** [parse src] is the cram test of source [src], or [Error msg] if a command
      holds a control character other than a tab. A line [  $ cmd] starts a
      command, the [  > ] lines after it continue it, the other lines indented
      by two spaces are its expected output except an [[N]] line, which is its
      exit code. Other lines are dropped and lines may end with ["\r\n"]. *)

  val to_tape :
    ?settings:setting list ->
    ?setup_scripts:string list ->
    ?pause:duration ->
    ?final_pause:duration ->
    output:string ->
    t ->
    tape
  (** [to_tape ~output t] records the cram test [t] as a tape writing its GIF
      (or other format chosen by extension) to [output].

      Each command is typed and submitted, then the tape waits for the
      {!constructor-Prompt} to return before pausing. [settings] become
      {!val-set} lines and [setup_scripts] are sourced in order inside a hidden
      clearing block. [pause] is the reading pause after each command (defaults
      to [S 1.]) and [final_pause] holds the last screen before exit (defaults
      to [S 3.]). *)

  val pp : t Fmt.t
  (** [pp] formats a cram test as cram source, without prose. *)
end

(** {1:running Running vhs} *)

(** The type for errors of {!render}. *)
type error =
  | Executable_not_found of string  (** No such executable on the [PATH]. *)
  | Failed of { vhs : string; status : [ `Exited of int | `Signaled of int ] }
      (** The executable [vhs] ran and did not exit with [0]. *)
  | Io of string  (** Any other I/O failure, as a message. *)

val pp_error : error Fmt.t
(** [pp_error] formats an error for humans. *)

val render : proc:_ Eio.Process.mgr -> ?vhs:string -> t -> (unit, error) result
(** [render ~proc ~vhs t] runs the executable [vhs] (defaults to ["vhs"]) with
    [t] on its standard input. [vhs] resolves the {!val-output} and
    {!val-source} paths of [t] against the current directory. *)
