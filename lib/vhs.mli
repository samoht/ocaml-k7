(** Generate and run Charmbracelet VHS terminal tapes.

    [Vhs] is the ergonomic entry point: it re-exports the tape value type and
    constructors from {!Value}, plus a small Eio runner for the external [vhs]
    executable. It writes reviewable [*.tape] files and delegates rendering and
    publishing to the installed VHS command-line tool. *)

module Value = Value
(** Tape values and their printers. *)

module Cram = Cram
(** Cram tests recorded as tapes. *)

(** {1:values Tape values} *)

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
  | Type of string
  | Key of key
  | Sleep of duration
  | Wait_for of wait
  | Hide
  | Show
  | Screenshot of string option
  | Source of string
  | Raw_command of string

(** See {!Value.type-item}. *)
type item = Value.item =
  | Comment of string
  | Blank
  | Require of string
  | Output of string
  | Env of string * string
  | Set of setting
  | Command of command
  | Raw_line of string

type t = Value.t
(** See {!Value.type-t}. *)

(** {1:items Items} *)

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

(** {1:rendering Rendering} *)

val pp : t Fmt.t
(** [pp] is {!Value.pp}.

    @raise Invalid_argument if the tape is not {{!Value.printing}printable}. *)

val to_string : t -> string
(** [to_string t] is {!Value.to_string}[ t].

    @raise Invalid_argument if [t] is not {{!Value.printing}printable}. *)

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
