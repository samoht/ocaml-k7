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
          Vhs.wait (Wait_line "OK");
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

(** The type for waits on terminal output. The string is an
    {{:https://github.com/google/re2/wiki/Syntax}RE2} regular expression without
    the surrounding slashes; a slash in it is printed as [\x2f]. *)
type wait =
  | Wait of string  (** Matches in the default scope of VHS, the cursor line. *)
  | Wait_screen of string  (** Matches anywhere on the screen. *)
  | Wait_line of string  (** Matches the line of the cursor. *)

type item
(** The type for tape lines. *)

type t
(** The type for VHS tapes. *)

val tape : item list -> t
(** [tape items] is the tape of [items], one line each, in order. VHS expects
    {!val-output}, {!val-require} and {!val-set} lines before the first terminal
    action. *)

(** {1:items Items} *)

val comment : string -> item
(** [comment s] is the line [# s], or [#] if [s] is empty. *)

val blank : unit -> item
(** [blank ()] is an empty line. *)

val require : string -> item
(** [require program] makes VHS fail before running the tape if [program] is not
    on the [PATH]. *)

val output : string -> item
(** [output path] makes VHS render to [path], in the format of its extension. *)

val env : string -> string -> item
(** [env name value] sets the environment variable [name] to [value] in the
    virtual terminal. *)

val set : setting -> item
(** [set s] applies the setting [s]. *)

val type' : string -> item
(** [type' s] types [s] without pressing return; newlines and tabs in [s] are
    typed as {!constructor-Enter} and {!constructor-Tab}. *)

val key : key -> item
(** [key k] presses [k]. *)

val enter : unit -> item
(** [enter ()] is [key Enter]. *)

val sleep : duration -> item
(** [sleep d] pauses the recording for [d]. *)

val wait : wait -> item
(** [wait w] pauses the recording until [w] matches the terminal. *)

val hide : unit -> item
(** [hide ()] stops rendering the terminal; its actions still run. *)

val show : unit -> item
(** [show ()] resumes rendering after {!val-hide}. *)

val screenshot : string -> item
(** [screenshot path] saves the current frame to the PNG file [path]. *)

val source : string -> item
(** [source path] runs the tape file [path]. *)

val raw : string -> item
(** [raw line] is [line], printed unchanged. *)

(** {1:printing Printing}

    VHS strings have no escape sequences and end at a newline. A quoted string
    is printed between a quote character (["\""], ['\''] or [`]) it does not
    hold, and {!val-type'} cuts its text into several lines where needed. A tape
    is {e valid} unless:
    - a string other than the text of {!val-type'} holds a control character, or
      all three quote characters if it is quoted.
    - the text of {!val-type'} holds a control character other than a newline or
      a tab.
    - a {!val-require} program, an {!val-env} name or a {!constructor-Custom}
      setting name is empty or holds a space.
    - a {!constructor-Ctrl} or {!constructor-Alt} key is not one VHS accepts.
    - a duration is negative or not finite. *)

val validate : t -> (unit, string) result
(** [validate t] is [Ok ()] if [t] is valid and [Error msg] with [msg] naming
    the first invalid string otherwise. *)

val pp : t Fmt.t
(** [pp] formats a tape in the [*.tape] syntax, one line per item.

    @raise Invalid_argument
      if the tape is not valid, that is exactly when {!validate} is [Error _].
      Nothing is printed then. *)

(** {1:cram Cram tests} *)

type tape := t

(** Cram tests recorded as tapes.

    The [$ ] command lines become keystrokes and the expected-output lines
    become the waits that keep the recording in step with the checked output.
    Setup scripts are sourced, hidden, before the first command. *)
module Cram : sig
  type t
  (** The type for cram tests: a sequence of commands, each with its expected
      output and exit code. *)

  val parse : string -> t
  (** [parse src] is the cram test of source [src]. A line [  $ cmd] starts a
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

      Each command is typed and submitted; when it has expected output, the tape
      waits for the last non-empty output line before pausing, with regexp
      metacharacters escaped. [settings] become {!val-set} lines and
      [setup_scripts] are sourced in order inside a hidden clearing block.
      [pause] is the reading pause after each command (defaults to [S 1.]) and
      [final_pause] holds the last screen before exit (defaults to [S 3.]). *)

  val pp : t Fmt.t
  (** [pp] formats a cram test as cram source, without prose. *)
end

(** {1:running Running vhs} *)

(** The type for errors of {!render}. *)
type error =
  | Invalid_tape of string  (** The tape is not {{!validate}valid}. *)
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
