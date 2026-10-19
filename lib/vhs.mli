(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Terminal recordings with VHS.

    {{:https://github.com/charmbracelet/vhs}VHS}, by Charm, records a terminal
    session as a GIF or a video from a tape: a script of settings, followed by
    actions such as typing text, pressing keys and waiting for output.

    [Vhs] builds the actions as values, checks them once when {!val-tape}
    assembles a tape, prints tapes in the [.tape] syntax and {{!running}runs}
    [vhs] on them. Only {!render} performs I/O.

    {[
    let demo =
      Vhs.tape
        ~settings:[ Width 1000; Height 560 ]
        ~outputs:[ "demo.gif" ]
        [ Vhs.type' "ls\n"; Vhs.wait Prompt ]
    ]} *)

(** {1:actions Actions} *)

(** The type for durations, in milliseconds. A tape accepts [Ms 0] to
    [Ms 9_223_372_036_854], the range of Go's [time.Duration]. *)
type duration = Ms of int

(** The type for keys. A key without an argument is the VHS key of the same
    name. *)
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

(** The type for conditions on the terminal. *)
type wait =
  | Prompt
      (** The line under the cursor ends with [>]. VHS starts each shell with
          the prompt ["> "], so this holds when the shell is ready for the next
          command. It never holds once something changes the prompt, such as a
          sourced script that sets [PS1], and it holds early when a command
          prints a line ending with [>] and waits. *)
  | Screen of string
      (** The screen, its lines joined by newlines, matches the regular
          expression, in the {{:https://github.com/google/re2/wiki/Syntax}RE2}
          syntax of Go and without surrounding slashes. *)

type action
(** The type for terminal actions. *)

val type' : string -> action
(** [type' s] types the text [s]. A tab in [s] presses {!constructor-Tab} and a
    newline presses {!constructor-Enter}. *)

val key : key -> action
(** [key k] presses [k]. *)

val sleep : duration -> action
(** [sleep d] pauses for [d] while the recording goes on. *)

val wait : ?timeout:duration -> wait -> action
(** [wait ~timeout w] pauses until [w] holds and fails the recording if it does
    not hold within [timeout] (defaults to 15 seconds, VHS's [WaitTimeout]). *)

val hidden : action list -> action
(** [hidden actions] runs [actions] without recording them. A [hidden] inside
    [actions] is the same as its own actions. *)

val raw : string -> action
(** [raw line] is the tape line [line], printed as is, for the VHS commands
    without a builder here, such as [Require], [Env], [Source], [Screenshot] and
    the settings missing from {!setting}. Quoting the arguments of [line] is the
    caller's task. *)

(** {1:tapes Tapes} *)

(** The type for the shells VHS can start. *)
type shell =
  | Bash
  | Zsh
  | Fish
  | Powershell
  | Pwsh
  | Cmd
  | Nushell
  | Osh
  | Xonsh

(** The type for settings, applied before the first action. Their integers are
    not negative. *)
type setting =
  | Shell of shell  (** The shell to run (defaults to {!constructor-Bash}). *)
  | Font_size of int  (** The font size (defaults to [22]). *)
  | Font_family of string  (** A font list. *)
  | Width of int  (** The width in pixels (defaults to [1200]). *)
  | Height of int  (** The height in pixels (defaults to [600]). *)
  | Theme of string  (** A theme name, as [vhs themes] lists them, or JSON. *)
  | Padding of int  (** The padding in pixels (defaults to [60]). *)
  | Typing_speed of duration
      (** The pause after each typed character (defaults to [Ms 50]). *)
  | Framerate of int  (** The frame rate (defaults to [50]). *)

type t
(** The type for tapes VHS reads without error and without ignoring a line. *)

val tape :
  ?settings:setting list ->
  outputs:string list ->
  action list ->
  (t, string) result
(** [tape ~settings ~outputs actions] is the tape that applies [settings]
    (defaults to [[]]), runs [actions] and writes the recording to each path of
    [outputs], in the format of its extension: [.gif], [.mp4], [.webm], [.txt]
    or [.ascii] for the screen as text, and PNG frames for a directory, written
    with a trailing slash. It is [Error msg], with [msg] naming the first
    offending item, if:
    - [outputs] is empty, or one has neither an extension nor a trailing slash;
    - a string holds a control character (the tabs and newlines of {!type'}
      aside), all three quote characters (['"'], ['\''] and ['`']) where VHS
      quotes it, or, in typed text, invalid UTF-8;
    - a duration is out of range, or a wait timeout is [Ms 0];
    - a {!Screen} pattern is not valid Go regular expression syntax;
    - a key or a setting breaks the condition its documentation states;
    - a {!raw} line is [Hide] or [Show] (use {!hidden}), or is [Require] or
      [Set], other than [Set TypingSpeed], after an action, where VHS ignores
      it. *)

val pp : t Fmt.t
(** [pp] formats a tape in the [.tape] syntax: the [Output] lines, then the
    [Set] lines, then the actions, one or more lines each. A {!type'} with all
    three quote characters is several [Type] lines. *)

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
  | Io of string  (** Any other I/O failure. *)

val pp_error : error Fmt.t
(** [pp_error] formats an error for humans. *)

val render :
  ?vhs:string ->
  proc:_ Eio.Process.mgr ->
  cwd:_ Eio.Path.t ->
  t ->
  (unit, error) result
(** [render ~vhs ~proc ~cwd t] runs the executable [vhs] (defaults to ["vhs"])
    in the directory [cwd], with the tape [t] on its standard input and its
    standard error captured for {!Failed}. The outputs of [t] are relative to
    [cwd]. *)
