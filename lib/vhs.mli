(** Charmbracelet VHS terminal tapes.

    A VHS tape is a small, line-oriented program read by the external
    {{:https://github.com/charmbracelet/vhs}[vhs]} tool: settings such as size
    and theme, output files to render, and terminal actions such as typing text,
    pressing return, waiting for output and hiding setup steps.

    [Vhs] builds tapes as values, prints them in the [*.tape] syntax, turns
    {{!Cram}cram tests} into tapes and {{!running}runs} [vhs] on them. Only the
    functions of the last section perform I/O.

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

(** The type for keys pressed in the virtual terminal. *)
type key =
  | Enter
  | Tab
  | Space
  | Backspace
  | Escape
  | Left
  | Right
  | Up
  | Down
  | Ctrl of string  (** A control chord, for example [Ctrl "c"]. *)
  | Alt of string  (** An alt/meta chord. *)
  | Ctrl_alt of string  (** A combined control+alt chord. *)
  | Raw_key of string  (** An already-formed VHS key token. *)

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
      (** Name and raw value for unmodelled settings. *)

(** The type for waits on terminal output. The string is an
    {{:https://github.com/google/re2/wiki/Syntax}RE2} regular expression without
    the surrounding slashes; a slash in it is printed as [\x2f]. *)
type wait = Wait of string | Wait_screen of string | Wait_line of string

(** The type for terminal actions. *)
type command =
  | Type of string
      (** Types text without pressing return. Newlines and tabs are typed as
          {!constructor-Enter} and {!constructor-Tab}. *)
  | Key of key
  | Sleep of duration  (** Pauses the recording timeline. *)
  | Wait_for of wait
  | Hide  (** Hides subsequent input/output from the recording. *)
  | Show  (** Resumes rendering after {!constructor-Hide}. *)
  | Screenshot of string  (** Captures the current frame to a PNG file. *)
  | Source of string  (** Includes commands from another tape file. *)
  | Raw_command of string  (** A raw VHS command line. *)

(** The type for tape lines. *)
type item =
  | Comment of string
  | Blank
  | Require of string
      (** A command dependency; VHS fails early if the program is missing. *)
  | Output of string  (** An output target such as [demo.gif]. *)
  | Env of string * string  (** A variable visible to the virtual terminal. *)
  | Set of setting
  | Command of command
  | Raw_line of string  (** A raw tape line. *)

type t
(** The type for VHS tapes. *)

val tape : item list -> t
(** [tape items] is a VHS tape with [items] in output order. Emit
    {!constructor-Output}, {!constructor-Require} and {!constructor-Set} before
    commands to match VHS' own expectations. *)

val items : t -> item list
(** [items t] are the items of [t] in output order. *)

(** {1:items Items} *)

val comment : string -> item
(** [comment s] is the comment line [# s], or [#] if [s] is empty. *)

val blank : unit -> item
(** [blank ()] is {!constructor-Blank}, an empty line. *)

val require : string -> item
(** [require program] declares that [program] must be available before VHS runs
    the tape. *)

val output : string -> item
(** [output path] adds an output target; VHS chooses the renderer from the file
    extension. *)

val env : string -> string -> item
(** [env name value] is [Env (name, value)]. *)

val set : setting -> item
(** [set setting] changes the virtual terminal or renderer. Emit it before the
    first terminal action. *)

val command : command -> item
(** [command c] is [Command c]. *)

val raw_line : string -> item
(** [raw_line line] is the tape line [line], printed unchanged, for VHS syntax
    not modelled here. *)

val type' : string -> item
(** [type' s] is [Command (Type s)]: it types [s] without pressing return, so a
    shell command is usually [type' "..."; enter ()]. *)

val key : key -> item
(** [key k] presses [k] in the virtual terminal. *)

val enter : unit -> item
(** [enter ()] is [key Enter]. *)

val sleep : duration -> item
(** [sleep d] pauses the recording timeline for [d]. Use {!val-wait} to
    synchronize with command output instead. *)

val wait : wait -> item
(** [wait w] pauses until [w] matches terminal output. *)

val hide : unit -> item
(** [hide ()] starts a hidden region: its commands run but are not rendered. *)

val show : unit -> item
(** [show ()] ends a hidden region started by {!val-hide}. *)

val screenshot : string -> item
(** [screenshot path] is [Command (Screenshot path)]. *)

val source : string -> item
(** [source path] includes another tape. *)

val raw_command : string -> item
(** [raw_command line] is the command [line], printed unchanged, for VHS
    commands not modelled here. *)

(** {1:printing Printing}

    VHS strings have no escape sequences and end at a newline. The printers
    pick, for each quoted payload, a quote character (["\""], ['\''] or [`]) it
    does not hold, and cut a {!constructor-Type} payload into several commands
    where needed. They raise [Invalid_argument] if:
    - a quoted payload other than a {!constructor-Type} one holds a control
      character or all three quote characters.
    - a {!constructor-Type} payload holds a control character other than a
      newline or a tab.
    - a {!constructor-Require} program, an {!constructor-Env} name, a
      {!constructor-Custom} setting name or a {!constructor-Ctrl},
      {!constructor-Alt} or {!constructor-Ctrl_alt} key is empty or holds a
      space or a control character.
    - a {!constructor-Comment}, a {!constructor-Custom} setting value, a
      {!constructor-Raw_key}, a {!constructor-Raw_command} or a
      {!constructor-Raw_line} holds a newline or a carriage return.
    - a duration is negative or not finite.
    - a wait pattern holds a newline or a carriage return. *)

val pp_duration : duration Fmt.t
(** [pp_duration] formats a VHS duration. *)

val pp_key : key Fmt.t
(** [pp_key] formats a VHS key command. *)

val pp_setting : setting Fmt.t
(** [pp_setting] formats a VHS setting payload. *)

val pp_wait : wait Fmt.t
(** [pp_wait] formats a VHS wait command. *)

val pp_command : command Fmt.t
(** [pp_command] formats a VHS command line. *)

val pp_item : item Fmt.t
(** [pp_item] formats one tape item without forcing a trailing newline. *)

val pp : t Fmt.t
(** [pp] formats a complete tape with one line per item.

    @raise Invalid_argument if the tape is not {{!printing}printable}. *)

val to_string : t -> string
(** [to_string t] is the textual [*.tape] representation of [t].

    @raise Invalid_argument if [t] is not {{!printing}printable}. *)

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
      metacharacters escaped. [settings] become {!constructor-Set} lines and
      [setup_scripts] are sourced in order inside a hidden clearing block.
      [pause] is the reading pause after each command (defaults to [S 1.]) and
      [final_pause] holds the last screen before exit (defaults to [S 3.]). *)

  val pp : t Fmt.t
  (** [pp] formats a cram test as cram source, without prose. *)
end

(** {1:running Writing and running tapes}

    These functions never raise for an I/O failure: they return it as an
    {!type-error}. *)

(** The type for I/O and process errors. *)
type error =
  | Executable_not_found of string  (** No such executable on the PATH. *)
  | Child_error of { argv : string list; status : Eio.Process.exit_status }
      (** The executable ran and exited non-zero. *)
  | Io of string  (** Any other I/O failure, as a message. *)

val pp_error : error Fmt.t
(** [pp_error] formats errors for humans. *)

val write_file : path:string -> t -> (unit, error) result
(** [write_file ~path t] writes [to_string t] to [path].

    @raise Invalid_argument if [t] is not {{!printing}printable}. *)

val render_file :
  proc:_ Eio.Process.mgr -> ?vhs:string -> string -> (unit, error) result
(** [render_file ~proc ~vhs path] runs the executable [vhs] (defaults to
    ["vhs"]) on [path]. *)

val render :
  proc:_ Eio.Process.mgr ->
  ?vhs:string ->
  path:string ->
  t ->
  (unit, error) result
(** [render ~proc ?vhs ~path tape] is [write_file ~path tape] followed by
    [render_file ~proc ?vhs path].

    @raise Invalid_argument if [tape] is not {{!printing}printable}. *)

val publish :
  proc:_ Eio.Process.mgr -> ?vhs:string -> string -> (string, error) result
(** [publish ~proc ?vhs file] runs [vhs publish file] and is its standard
    output, holding the share links VHS prints there. *)
