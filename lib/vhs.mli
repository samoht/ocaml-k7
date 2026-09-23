(** Generate and run Charmbracelet VHS terminal tapes.

    [Vhs] is the ergonomic entry point: it re-exports the tape value type and
    constructors from {!Value}, plus a small Eio runner for the external [vhs]
    executable. It writes reviewable [*.tape] files and delegates rendering and
    publishing to the installed VHS command-line tool. *)

module Loc = Loc
module Meta = Loc.Meta

module Value = Value
(** Tape values and their printers. *)

module Cram = Cram
(** Cram tests recorded as tapes. *)

(** {1:values Tape values} *)

type 'a node = 'a Value.node
(** See {!Value.type-node}. *)

(** See {!Value.type-duration}. *)
type duration = Value.duration = Ms of int | S of float

(** See {!Value.type-key}. *)
type key = Value.key =
  | Enter
  | Tab
  | Space
  | Backspace
  | Escape
  | Left
  | Right
  | Up
  | Down
  | Ctrl of string
  | Alt of string
  | Ctrl_alt of string
  | Raw_key of string

(** See {!Value.type-setting}. *)
type setting = Value.setting =
  | Shell of string
  | Font_size of int
  | Font_family of string
  | Width of int
  | Height of int
  | Theme of string
  | Padding of int
  | Typing_speed of duration
  | Framerate of int
  | Custom of string * string

(** See {!Value.type-wait}. *)
type wait = Value.wait =
  | Wait of string
  | Wait_screen of string
  | Wait_line of string

(** See {!Value.type-command}. *)
type command = Value.command =
  | Type of string node
  | Key of key node
  | Sleep of duration node
  | Wait_for of wait node
  | Hide of unit node
  | Show of unit node
  | Screenshot of string option node
  | Source of string node
  | Raw_command of string node

(** See {!Value.type-item}. *)
type item = Value.item =
  | Comment of string node
  | Blank of unit node
  | Require of string node
  | Output of string node
  | Env of (string * string) node
  | Set of setting node
  | Command of command node
  | Raw_line of string node

(** See {!Value.type-t}. *)
type t = Value.t = Tape of item list node

(** {1:items Items} *)

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

(** {1:rendering Rendering} *)

val pp : t Fmt.t
(** [pp] formats a complete tape with one line per item. *)

val to_string : t -> string
(** [to_string t] is the textual [*.tape] representation of [t]. *)

val write_file : path:string -> t -> unit
(** [write_file ~path t] writes the textual [*.tape] representation of [t] to
    [path].

    @raise Invalid_argument
      if [path] contains a relative segment such as ["."] or [".."]. *)

(** {1:running Running vhs} *)

(** The type for why running the [vhs] executable failed. *)
type error =
  | Executable_not_found of string  (** No such executable on the PATH. *)
  | Child_error of { argv : string list; status : Eio.Process.exit_status }
      (** The executable ran and exited non-zero. *)
  | Io of exn  (** Any other Eio I/O failure. *)

val pp_error : error Fmt.t
(** [pp_error] formats process and filesystem errors from the runners below. *)

val render_file :
  proc:_ Eio.Process.mgr -> ?vhs:string -> string -> (unit, error) result
(** [render_file ~proc ?vhs path] runs [vhs path]. [vhs] names the executable
    and defaults to ["vhs"]. *)

val render :
  proc:_ Eio.Process.mgr ->
  ?vhs:string ->
  path:string ->
  t ->
  (unit, error) result
(** [render ~proc ?vhs ~path tape] is [write_file ~path tape] followed by
    [render_file ~proc ?vhs path]. *)

val publish :
  proc:_ Eio.Process.mgr -> ?vhs:string -> string -> (string, error) result
(** [publish ~proc ?vhs file] runs [vhs publish file] and is its standard
    output, holding the share links VHS prints there. *)
