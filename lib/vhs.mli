(** Generate and run Charmbracelet VHS terminal tapes.

    This is the ergonomic entry point. It re-exports the tape value type and
    constructors from {!Value}, plus a small Eio runner for the external [vhs]
    executable.

    The library does not implement terminal rendering itself. It writes
    reviewable [*.tape] files and delegates rendering or publishing to the
    installed VHS command-line tool. *)

module Loc = Loc
module Meta = Loc.Meta
module Value = Value
module Cram = Cram

type 'a node = 'a Value.node
type duration = Value.duration = Ms of int | S of float

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

type wait = Value.wait =
  | Wait of string
  | Wait_screen of string
  | Wait_line of string

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

type item = Value.item =
  | Comment of string node
  | Blank of unit node
  | Require of string node
  | Output of string node
  | Env of (string * string) node
  | Set of setting node
  | Command of command node
  | Raw_line of string node

type t = Value.t = Tape of item list node

val tape : ?meta:Meta.t -> item list -> t
(** [tape items] is a VHS tape with [items] in output order. Put
    {!constructor-Output}, {!constructor-Require}, and {!constructor-Set} items
    before commands to match VHS' own expectations. *)

val comment : ?meta:Meta.t -> string -> item
(** [comment s] emits [# s]. *)

val blank : ?meta:Meta.t -> unit -> item
(** [blank ()] emits an empty line. *)

val require : ?meta:Meta.t -> string -> item
(** [require program] declares that [program] must be available before VHS runs
    the tape. Use it for demo dependencies such as [dune], [gum], or your CLI
    under test. *)

val output : ?meta:Meta.t -> string -> item
(** [output path] adds an output target. VHS chooses the renderer from the file
    extension, so callers can request [gif], [mp4], [webm], screenshots, or a
    frame directory. *)

val env : ?meta:Meta.t -> string -> string -> item
(** [env key value] emits [Env key "value"]. *)

val set : ?meta:Meta.t -> setting -> item
(** [set setting] changes the virtual terminal or renderer. Settings are global
    and should be emitted before the first terminal action. *)

val command : ?meta:Meta.t -> command -> item
(** [command c] lifts [c] into a tape item. *)

val raw_line : ?meta:Meta.t -> string -> item
(** [raw_line line] emits [line] unchanged. Use this for VHS syntax not yet
    modelled by the typed AST. *)

val type' : ?meta:Meta.t -> string -> item
(** [type' s] types [s] into the virtual terminal. It deliberately does not
    press return, so a shell command is usually [type' "..."; enter ()]. The
    trailing quote avoids OCaml's [type] keyword. *)

val key : ?meta:Meta.t -> key -> item
(** [key k] presses [k] in the virtual terminal. Use this for editing a command,
    accepting prompts, cancelling with [Ctrl "c"], or navigating TUIs. *)

val enter : ?meta:Meta.t -> unit -> item
(** [enter ()] emits {!constructor-Enter}. *)

val sleep : ?meta:Meta.t -> duration -> item
(** [sleep d] pauses the recording timeline for [d]. Use sleeps for deliberate
    reading time; use {!val-wait} for synchronization with command output. *)

val wait : ?meta:Meta.t -> wait -> item
(** [wait w] pauses until [w] matches terminal output. This is the preferred way
    to synchronize recordings with commands whose runtime varies. *)

val hide : ?meta:Meta.t -> unit -> item
(** [hide ()] starts a hidden region. Setup commands still run, but they are not
    rendered into the final recording. *)

val show : ?meta:Meta.t -> unit -> item
(** [show ()] ends a hidden region started by {!val-hide}. *)

val screenshot : ?meta:Meta.t -> ?path:string -> unit -> item
(** [screenshot ?path ()] emits {!constructor-Screenshot}, optionally with an
    output path. *)

val source : ?meta:Meta.t -> string -> item
(** [source path] includes another tape. Use it for shared settings or setup
    snippets. *)

val raw_command : ?meta:Meta.t -> string -> item
(** [raw_command line] emits a command line unchanged. Use this for VHS commands
    not yet modelled by {!type-command}. *)

val pp : t Fmt.t
(** [pp] formats a complete tape with one line per item. *)

val to_string : t -> string
(** [to_string t] is the textual [*.tape] representation of [t]. *)

val write_file : path:string -> t -> unit
(** [write_file ~path t] writes the textual [*.tape] representation of [t] to
    [path]. This is useful when callers want to inspect or commit the generated
    tape before rendering. *)

type error =
  | Executable_not_found of string
  | Child_error of { argv : string list; status : Eio.Process.exit_status }
  | Io of exn

val pp_error : error Fmt.t
(** [pp_error] formats process and filesystem errors from runner helpers. *)

val render_file :
  proc:_ Eio.Process.mgr -> ?vhs:string -> string -> (unit, error) result
(** [render_file ~proc path] runs [vhs path]. Use [?vhs] to point at a non-PATH
    executable or wrapper script. *)

val render :
  proc:_ Eio.Process.mgr ->
  ?vhs:string ->
  path:string ->
  t ->
  (unit, error) result
(** [render ~proc ~path tape] writes [tape] to [path], then runs [vhs path].
    This is the one-shot helper for generated demos. *)

val publish :
  proc:_ Eio.Process.mgr -> ?vhs:string -> string -> (string, error) result
(** [publish ~proc file] runs [vhs publish file] and returns stdout. VHS prints
    the share links there; this function does not parse them yet. *)
