(** VHS tape values.

    A VHS tape is a small, line-oriented program consumed by the external [vhs]
    tool: global settings such as size and theme, output files to render, and
    terminal actions such as typing text, pressing return, waiting for output,
    and hiding setup steps.

    This module is the format layer. It gives the [*.tape] language an OCaml
    representation and renders it back to text. It does not run [vhs]; the
    runner helpers live in {!Vhs}.

    The intended style is to locally open the module while constructing tapes:

    {[
    let demo =
      let open Vhs.Value in
      tape
        [
          output "demo.gif";
          set (Width 1000);
          set (Height 560);
          type' "dune test";
          enter ();
          wait (Wait_line "OK");
        ]
    ]} *)

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

(** The type for waits on terminal output. The string is the VHS
    regular-expression payload without surrounding slashes. Prefer waits to
    fixed sleeps when a command has variable runtime. *)
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
  | Show  (** Resumes rendering after {!Hide}. *)
  | Screenshot of string option  (** Captures the current frame. *)
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

val comment : string -> item
(** [comment s] emits [# s], or a bare [#] when [s] is empty. *)

val blank : unit -> item
(** [blank ()] emits an empty line. *)

val require : string -> item
(** [require program] declares that [program] must be available before VHS runs
    the tape. *)

val output : string -> item
(** [output path] adds an output target; VHS chooses the renderer from the file
    extension. *)

val env : string -> string -> item
(** [env key value] emits [Env key "value"]. *)

val set : setting -> item
(** [set setting] changes the virtual terminal or renderer. Emit it before the
    first terminal action. *)

val command : command -> item
(** [command c] lifts [c] into a tape item. *)

val raw_line : string -> item
(** [raw_line line] emits [line] unchanged, for VHS syntax not modelled here. *)

val type' : string -> item
(** [type' s] types [s] without pressing return, so a shell command is usually
    [type' "..."; enter ()]. *)

val key : key -> item
(** [key k] presses [k] in the virtual terminal. *)

val enter : unit -> item
(** [enter ()] emits {!constructor-Enter}. *)

val sleep : duration -> item
(** [sleep d] pauses the recording timeline for [d]. Use {!val-wait} to
    synchronize with command output instead. *)

val wait : wait -> item
(** [wait w] pauses until [w] matches terminal output. *)

val hide : unit -> item
(** [hide ()] starts a hidden region: its commands run but are not rendered. *)

val show : unit -> item
(** [show ()] ends a hidden region started by {!val-hide}. *)

val screenshot : ?path:string -> unit -> item
(** [screenshot ?path ()] emits {!constructor-Screenshot}. *)

val source : string -> item
(** [source path] includes another tape. *)

val raw_command : string -> item
(** [raw_command line] emits a command line unchanged, for VHS commands not
    modelled here. *)

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
    - a duration is negative or not finite. *)

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

val equal : t -> t -> bool
(** [equal a b] compares tapes by their generated textual representation. *)
