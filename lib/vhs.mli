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
    [vhs] on them. It also {{!native}records} a tape itself, as an animated SVG
    image, without [vhs]. Only {!render} and {!record} perform I/O.

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
    by the caller. {!val-tape} checks it as its documentation states. VHS checks
    the rest only when it runs, such as the tape of a [Source] line, the program
    of a [Require] line, the image a [Set MarginFill] names when it is not a
    colour, and what ffmpeg makes of a [Set PlaybackSpeed] or [Set LoopOffset]
    value. *)

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
      (** The width in pixels, at least twice the padding and the margin of a
          {!raw} [Set Margin], and even with an MP4 output (defaults to [1200]).
          The browser VHS drives takes what is left after the padding, and the
          margin unless a {!raw} [Set MarginFill] is empty, as its viewport, of
          at most 10,000,000 pixels. *)
  | Height of int
      (** The height in pixels, at least twice the padding and the margin, plus
          the size of the window bar of a {!raw} [Set WindowBar], and even with
          an MP4 output (defaults to [600]). Its viewport, what is left after
          them, is at most 10,000,000 pixels, as for {!Width}. *)
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
    [.ascii] or [.test] for the screen as text, a directory of PNG frames for
    [.png] or a trailing slash, and [.svg] for the animated SVG image that
    {!record} writes and VHS cannot. It is [Error msg], with [msg] naming the
    first offending item, if:
    - [outputs] is empty, or one has another extension, none and no trailing
      slash, or starts with [-], which ffmpeg reads as an option;
    - two outputs, {!raw} [Output] lines included, are of the same kind (GIF,
      MP4, WebM, text, PNG frames or SVG), of which VHS writes only the last;
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
    - a {!raw} [Set Rows] or [Set Columns] sizes the frame in the direction that
      {!Height} or {!Width} does;
    - a {!raw} line is [Hide] or [Show] (use {!hidden}), [Set] for a setting of
      {!setting} other than [TypingSpeed] (use the setting), an [Output] that
      breaks the rules of [outputs] or ends with [.svg], or is [Require] or
      [Set], other than [Set TypingSpeed], after an action that prints a line,
      where VHS ignores it;
    - no action prints a line outside {!hidden}, so VHS records no frame and
      fails; it also fails if the actions it records all end within one frame,
      such as a lone [sleep (Ms 0)], which [tape] does not check. *)

val to_string : t -> string
(** [to_string t] is [t] in the [.tape] syntax: the [Output] lines, but those of
    the [.svg] outputs, then the [Set] lines, then the actions. An action is one
    or more lines, except [type' ""] and a {!hidden} of such actions, which are
    none. A {!type'} with all three quote characters is several [Type] lines. *)

val pp : t Fmt.t
(** [pp ppf t] is [Fmt.string ppf (to_string t)]. *)

(** {1:running Running vhs} *)

(** The type for errors of {!render} and {!record}. *)
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
  | Unsupported of string
      (** A setting or a command that {!record} cannot play, named, and why:
          {!render} plays it. *)
  | Timeout of { wait : string; last : string }
      (** The wait line [wait] of {!record} did not hold in time; [last] is the
          line or the screen it last read. *)

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
    {!Failed} and dropped on success. [render] writes neither the [.svg] outputs
    of [t] nor anything else if they are its only outputs. If the calling fiber
    is cancelled, [render] sends [vhs] [SIGTERM], which makes it stop the
    terminal and the browser it started, and waits for it to exit. If [vhs]
    still runs ten seconds later by [clock], [render] sends it [SIGKILL], which
    can leave the terminal and the browser running, and waits for it to exit. *)

(** {1:native Recording natively}

    {!record} plays a tape in bash on a pseudo-terminal, reads what bash prints
    with a terminal emulator and draws the screens it shows as an animated SVG
    image. It needs no [vhs], browser or [ffmpeg]. The image holds text, no
    script and no external resource, and animates with CSS only, so it plays in
    an HTML [<img>].

    The terminal has the size VHS gives it with JetBrains Mono, VHS's first
    font: [Width], [Height], [Font_size], [Padding] and the raw [Set Margin],
    [Set LetterSpacing], [Set LineHeight], [Set Rows] and [Set Columns] size it.
    VHS sizes it with the fonts of the machine it runs on, so it can show more
    rows than the image elsewhere. The image draws each cell on a grid, in a
    monospace font of the viewer's machine, in the colours of the [Theme]. The
    terminal keeps a colour as RGB, so a 256-colour or true colour equal to one
    of xterm's sixteen ANSI colours takes the theme's colour too. It drops
    zero-width characters and combining marks, which VHS shows.

    Time is the tape's: a typed character, a key and a sleep last what the tape
    says, a wait lasts what the program makes it last, and output shows at the
    time it arrives within the action that runs, rounded down to a frame of
    {!Framerate}. A tape whose program prints the same bytes within the same
    frames gives the same image, byte for byte; the time a command takes to
    print, including how the machine schedules it, changes the frames its output
    lands in.

    {!record} plays the actions of {!action}, the raw [Type], key, [Ctrl],
    [Alt], [Shift], [Sleep], [Wait], [Copy] and [Paste] lines ([Paste] types the
    last [Copy]), [Env], [Require], and every [Set] but those below. It does not
    play, and is {!Unsupported} on:
    - a {!shell} other than {!Bash};
    - a JSON [Theme], [Set WindowBar] with a bar, and [Set MarginFill] with an
      image;
    - [Screenshot] and [Source];
    - a wait pattern with a Unicode class ([\p] or [\P]).

    Its patterns match as Go's do, except that [(?i)] folds ASCII letters only
    and [\b] counts Latin-1 letters as word characters next to a non-ASCII
    character. [Set CursorBlink], [Set BorderRadius], [Set PlaybackSpeed] and
    [Set LoopOffset] change the animation. A size beyond 100000 pixels, or a
    terminal beyond 2000 cells a side, is {!Unsupported}. *)

val record :
  clock:_ Eio.Time.Mono.t ->
  proc:_ Eio.Process.mgr ->
  cwd:_ Eio.Path.t ->
  t ->
  (string, error) result
(** [record ~clock ~proc ~cwd t] plays [t] in bash in the directory [cwd], with
    the environment {!render} gives [vhs], and is the SVG image of what it
    showed. [record] also writes the image to each [.svg] output of [t],
    relative to [cwd], and writes no other output. The waits of [t] time out by
    [clock]. It is {!Executable_not_found} if [bash] or the executable of a
    [Require] line is not on the [PATH]. *)

val svg :
  ?settings:setting list -> (duration * string) list -> (string, error) result
(** [svg ~settings output] is the SVG image of a terminal with [settings]
    (defaults to [[]]) that receives each string of [output] at its time, which
    increases. It is [Error (Unsupported _)] on a JSON {!Theme} and on the sizes
    {!record} refuses. *)
