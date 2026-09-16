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

module Meta = Loc.Meta

type 'a node = 'a * Meta.t

type duration = Ms of int | S of float

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
  | Custom of string * string  (** Name and raw value for unmodelled settings. *)

(** The string is the VHS regular-expression payload without surrounding
    slashes. Prefer waits to fixed sleeps when a command has variable runtime. *)
type wait = Wait of string | Wait_screen of string | Wait_line of string

type command =
  | Type of string node  (** Types text without pressing return. *)
  | Key of key node
  | Sleep of duration node  (** Pauses the recording timeline. *)
  | Wait_for of wait node
  | Hide of unit node  (** Hides subsequent input/output from the recording. *)
  | Show of unit node  (** Resumes rendering after {!Hide}. *)
  | Screenshot of string option node  (** Captures the current frame. *)
  | Source of string node  (** Includes commands from another tape file. *)
  | Raw_command of string node  (** A raw VHS command line. *)

type item =
  | Comment of string node
  | Blank of unit node
  | Require of string node
      (** A command dependency; VHS fails early if the program is missing. *)
  | Output of string node  (** An output target such as [demo.gif]. *)
  | Env of (string * string) node
      (** A variable visible to the virtual terminal. *)
  | Set of setting node
  | Command of command node
  | Raw_line of string node  (** A raw tape line. *)

(** A complete VHS tape. *)
type t = Tape of item list node

val node : ?meta:Meta.t -> 'a -> 'a node
(** [node ?meta v] wraps [v] with [meta], defaulting to {!Meta.none}. *)

val tape : ?meta:Meta.t -> item list -> t
(** [tape items] is a VHS tape with [items] in output order. Emit
    {!constructor-Output}, {!constructor-Require} and {!constructor-Set} before
    commands to match VHS' own expectations. *)

val comment : ?meta:Meta.t -> string -> item
(** [comment s] emits [# s], or a bare [#] when [s] is empty. *)

val blank : ?meta:Meta.t -> unit -> item
(** [blank ()] emits an empty line. *)

val require : ?meta:Meta.t -> string -> item
(** [require program] declares that [program] must be available before VHS runs
    the tape. *)

val output : ?meta:Meta.t -> string -> item
(** [output path] adds an output target; VHS chooses the renderer from the file
    extension. *)

val env : ?meta:Meta.t -> string -> string -> item
(** [env key value] emits [Env key "value"]. *)

val set : ?meta:Meta.t -> setting -> item
(** [set setting] changes the virtual terminal or renderer. Emit it before the
    first terminal action. *)

val command : ?meta:Meta.t -> command -> item
(** [command c] lifts [c] into a tape item. *)

val raw_line : ?meta:Meta.t -> string -> item
(** [raw_line line] emits [line] unchanged, for VHS syntax not modelled here. *)

val type' : ?meta:Meta.t -> string -> item
(** [type' s] types [s] without pressing return, so a shell command is usually
    [type' "..."; enter ()]. *)

val key : ?meta:Meta.t -> key -> item
(** [key k] presses [k] in the virtual terminal. *)

val enter : ?meta:Meta.t -> unit -> item
(** [enter ()] emits {!constructor-Enter}. *)

val sleep : ?meta:Meta.t -> duration -> item
(** [sleep d] pauses the recording timeline for [d]. Use {!val-wait} to
    synchronize with command output instead. *)

val wait : ?meta:Meta.t -> wait -> item
(** [wait w] pauses until [w] matches terminal output. *)

val hide : ?meta:Meta.t -> unit -> item
(** [hide ()] starts a hidden region: its commands run but are not rendered. *)

val show : ?meta:Meta.t -> unit -> item
(** [show ()] ends a hidden region started by {!val-hide}. *)

val screenshot : ?meta:Meta.t -> ?path:string -> unit -> item
(** [screenshot ?path ()] emits {!constructor-Screenshot}. *)

val source : ?meta:Meta.t -> string -> item
(** [source path] includes another tape. *)

val raw_command : ?meta:Meta.t -> string -> item
(** [raw_command line] emits a command line unchanged, for VHS commands not
    modelled here. *)

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
(** [pp] formats a complete tape with one line per item. *)

val to_string : t -> string
(** [to_string t] is the textual [*.tape] representation of [t]. *)

val equal : t -> t -> bool
(** [equal a b] compares tapes by their generated textual representation. *)
