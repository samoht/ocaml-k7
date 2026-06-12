(** VHS tape values.

    A VHS tape is a small, line-oriented program consumed by the external [vhs]
    tool. The program describes a virtual terminal: global settings such as size
    and theme, output files to render, and terminal actions such as typing text,
    pressing return, waiting for output, and hiding setup steps.

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
    ]}

    Programmatic constructors attach {!Loc.Meta.none}. The metadata is present
    so a future parser can report source locations and preserve layout without
    changing the AST. *)

module Meta = Loc.Meta

type 'a node = 'a * Meta.t
(** A format node: payload plus source metadata. *)

type duration =
  | Ms of int  (** Milliseconds, rendered as [Nms]. *)
  | S of float  (** Seconds, rendered as [Ns]. *)

type key =
  | Enter
      (** Press return. Use after {!val-type'} to submit a shell command. *)
  | Tab  (** Press tab. Useful for completion demos. *)
  | Space  (** Press the space bar. *)
  | Backspace  (** Delete the character before the cursor. *)
  | Escape  (** Press escape. *)
  | Left  (** Move the cursor left. *)
  | Right  (** Move the cursor right. *)
  | Up  (** Move through history or move the cursor up. *)
  | Down  (** Move through history or move the cursor down. *)
  | Ctrl of string  (** Press a control chord, for example [Ctrl "c"]. *)
  | Alt of string  (** Press an alt/meta chord. *)
  | Ctrl_alt of string  (** Press a combined control+alt chord. *)
  | Raw_key of string  (** Emit an already-formed VHS key token. *)

type setting =
  | Shell of string
      (** Shell used by the virtual terminal, for example ["bash"]. *)
  | Font_size of int  (** Rendered terminal font size in pixels. *)
  | Font_family of string  (** Font family used by the renderer. *)
  | Width of int  (** Rendered frame width in pixels. *)
  | Height of int  (** Rendered frame height in pixels. *)
  | Theme of string  (** VHS color theme name, for example ["Builtin Dark"]. *)
  | Padding of int  (** Padding around the terminal content in pixels. *)
  | Typing_speed of duration
      (** Delay between typed characters. Increase it for demos where the
          command itself matters; lower it when the output is what matters. *)
  | Framerate of int  (** Maximum output frame rate. *)
  | Custom of string * string
      (** Setting name and raw value for VHS features not yet modelled here. *)

type wait =
  | Wait of string
  | Wait_screen of string
  | Wait_line of string
      (** Wait conditions. The string is the VHS regular-expression payload
          without surrounding slashes.

          [Wait] waits for output, [Wait_screen] waits for the whole screen, and
          [Wait_line] waits for a matching line. Prefer waits to fixed sleeps
          when a command has variable runtime. *)

type command =
  | Type of string node
      (** Type text into the terminal. This does not press return; follow it
          with {!Enter} or {!val-enter} when the command should run. *)
  | Key of key node  (** Press a special key or key chord. *)
  | Sleep of duration node
      (** Pause the recording timeline. Use this to leave important output on
          screen long enough to read. *)
  | Wait_for of wait node
      (** Pause until terminal output matches a condition. This makes recordings
          less timing-sensitive than sleeps. *)
  | Hide of unit node
      (** Hide subsequent terminal input/output from the rendered recording.
          Typical use: cleanup, fixture creation, or cache warming. *)
  | Show of unit node  (** Resume rendering after {!Hide}. *)
  | Screenshot of string option node
      (** Capture the current frame, optionally to a named file. *)
  | Source of string node  (** Include commands from another tape file. *)
  | Raw_command of string node
      (** A raw VHS command line for syntax this library does not model yet. *)

type item =
  | Comment of string node
      (** A comment line. Comments make generated tapes reviewable. *)
  | Blank of unit node  (** A blank line. *)
  | Require of string node
      (** A command dependency. VHS fails early if the program is missing. *)
  | Output of string node
      (** An output target such as [demo.gif], [demo.mp4], or [frames/]. *)
  | Env of (string * string) node
      (** Environment variable visible to the virtual terminal. *)
  | Set of setting node
      (** A global VHS setting. Settings should appear before terminal actions.
      *)
  | Command of command node  (** A terminal action or VHS control command. *)
  | Raw_line of string node
      (** A raw tape line for syntax this library does not model yet. *)

type t = Tape of item list node  (** A complete VHS tape. *)

val node : ?meta:Meta.t -> 'a -> 'a node
(** [node ?meta v] wraps [v] with [meta], defaulting to {!Meta.none}. *)

val tape : ?meta:Meta.t -> item list -> t
(** [tape items] is a VHS tape with [items] in output order. Put [Output],
    [Require], and [Set] items before commands to match VHS' own expectations.
*)

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
(** [enter ()] emits [Enter]. *)

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
(** [screenshot ?path ()] emits [Screenshot], optionally with an output path. *)

val source : ?meta:Meta.t -> string -> item
(** [source path] includes another tape. Use it for shared settings or setup
    snippets. *)

val raw_command : ?meta:Meta.t -> string -> item
(** [raw_command line] emits a command line unchanged. Use this for VHS commands
    not yet modelled by {!type-command}. *)

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
