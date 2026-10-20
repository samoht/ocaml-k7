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
      (** [Alt c] holds alt and presses [c]: an ASCII letter other than [m] and
          [s], which VHS reads as time units, [.] or a square bracket. *)

(** The type for conditions on the terminal. *)
type wait =
  | Prompt
      (** The line under the cursor ends with [>], or matches the pattern of a
          {!raw} [Set WaitPattern] if the tape has one. VHS starts each shell
          with the prompt ["> "], so this holds when the shell is ready for the
          next command. It never holds once something changes the prompt, such
          as a sourced script that sets [PS1], and it holds early when a command
          prints a line ending with [>] and waits. *)
  | Line of string
      (** The line under the cursor, without its trailing spaces, matches the
          regular expression, as for {!Screen}. *)
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
    not hold within [timeout] (defaults to 15 seconds, or to the duration of a
    {!raw} [Set WaitTimeout] if the tape has one). *)

val hidden : action list -> action
(** [hidden actions] runs [actions] without recording them. A [hidden] inside
    [actions] is the same as its own actions. *)

val raw : string -> action
(** [raw line] is the tape line [line], printed as is, with its arguments quoted
    by the caller. {!val-tape} checks it as its documentation states; VHS checks
    the rest, such as the value of a setting {!setting} has no constructor for
    or the tape of a [Source] line, only when it runs. *)

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

(** The type for settings, applied before the first action. *)
type setting =
  | Shell of shell  (** The shell to run (defaults to {!constructor-Bash}). *)
  | Font_size of int  (** A positive size (defaults to [22]). *)
  | Font_family of string
      (** A font list, which holds no ['] and no backslash. *)
  | Width of int
      (** The width in pixels, at least twice the padding (defaults to [1200]).
      *)
  | Height of int
      (** The height in pixels, at least twice the padding (defaults to [600]).
      *)
  | Theme of string
      (** The name of a theme of VHS 0.12.1, as [vhs themes] lists them; a theme
          in JSON, which starts with [{] and which VHS checks only when it runs;
          or only spaces, for the default theme. *)
  | Padding of int
      (** The padding in pixels, not negative (defaults to [60]). *)
  | Typing_speed of duration
      (** The pause after each typed character (defaults to [Ms 50]). *)
  | Framerate of int  (** A positive frame rate (defaults to [50]). *)

type t
(** The type for tapes VHS reads without error and without ignoring a line. *)

val tape :
  ?settings:setting list ->
  outputs:string list ->
  action list ->
  (t, string) result
(** [tape ~settings ~outputs actions] is the tape that applies [settings]
    (defaults to [[]]), runs [actions] and writes the recording to each path of
    [outputs], in the format of its extension: [.gif], [.mp4], [.webm], [.txt],
    [.ascii] or [.test] for the screen as text, and a directory of PNG frames
    for [.png] or a trailing slash. It is [Error msg], with [msg] naming the
    first offending item, if:
    - [outputs] is empty, or one has another extension, none and no trailing
      slash, or starts with [-], which ffmpeg reads as an option;
    - two outputs, {!raw} [Output] lines included, are of the same kind (GIF,
      MP4, WebM, text, or PNG frames), of which VHS writes only the last;
    - a string holds a control character, C1 controls included (the tabs and
      newlines of {!type'} aside), all three quote characters (['"'], ['\''] and
      ['`']) where VHS quotes it, or, in typed text, invalid UTF-8;
    - a duration is out of range, or a wait timeout is [Ms 0];
    - a {!Line} or {!Screen} pattern is not valid Go regular expression syntax;
    - a key or a setting breaks the condition its documentation states;
    - a {!raw} line is not exactly one command VHS's parser accepts, ends inside
      a string, a regular expression or a JSON value, or lacks the argument of
      its command;
    - a {!raw} [Set] has a value its setting cannot parse: a duration, an
      integer, a float or a regular expression in Go's syntax, or a positive
      integer for [Rows] and [Columns];
    - a {!raw} line is [Hide] or [Show] (use {!hidden}), [Set] for a setting of
      {!setting} other than [TypingSpeed] (use the setting), an [Output] that
      breaks the rules of [outputs], or is [Require] or [Set], other than
      [Set TypingSpeed], after an action that prints a line, where VHS ignores
      it. *)

val to_string : t -> string
(** [to_string t] is [t] in the [.tape] syntax: the [Output] lines, then the
    [Set] lines, then the actions. An action is one or more lines, except
    [type' ""] and a {!hidden} of such actions, which are none. A {!type'} with
    all three quote characters is several [Type] lines. *)

val pp : t Fmt.t
(** [pp ppf t] is [Fmt.string ppf (to_string t)]. *)

(** {1:running Running vhs} *)

(** The type for errors of {!render}. *)
type error =
  | Executable_not_found of string  (** No such executable on the [PATH]. *)
  | Failed of {
      vhs : string;
      status : [ `Exited of int | `Signaled of int ];
      output : string;
    }
      (** The executable [vhs] ran and did not exit with [0]; [output] is the
          end of what it printed, at most 64 KiB, such as the wait that timed
          out. *)
  | Io of string  (** Any other I/O failure. *)

val pp_error : error Fmt.t
(** [pp_error] formats an error for humans: one line, followed for {!Failed} by
    the last ten lines [vhs] printed, indented. Escape sequences and control
    characters are removed from every string it prints, and invalid UTF-8 is
    replaced by U+FFFD. *)

val render :
  ?vhs:string ->
  clock:_ Eio.Time.Mono.t ->
  proc:_ Eio.Process.mgr ->
  cwd:_ Eio.Path.t ->
  t ->
  (unit, error) result
(** [render ~vhs ~clock ~proc ~cwd t] runs [vhs -q] (defaults to ["vhs"]) in the
    directory [cwd] with the tape [t] on its standard input, so the outputs of
    [t] are relative to [cwd]. [vhs] gets the environment of [proc] without
    [VHS_PUBLISH], which would upload the recording, [rod], which configures the
    browser VHS drives, and [PS1], [PROMPT] and [PROMPT_COMMAND], which would
    replace the prompt {!Prompt} waits for. What [vhs] prints is kept for
    {!Failed} and dropped on success. If the calling fiber is cancelled,
    [render] sends [vhs] [SIGTERM], which makes it stop the terminal and the
    browser it started, and waits for it to exit. If [vhs] still runs ten
    seconds later by [clock], [render] sends it [SIGKILL], which can leave the
    terminal and the browser running, and waits for it to exit. *)
