(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Terminal recordings with VHS.

    {{:https://github.com/charmbracelet/vhs}VHS}, by
    {{:https://charm.land/}Charm}, records a terminal session as a GIF or a
    video from a tape: a script of settings, followed by actions such as typing
    text, pressing keys and waiting for output.

    [Vhs] builds the actions as values, {{!checks}checks} them once when
    {!val-tape} assembles a tape, prints tapes in the [.tape] syntax and
    {{!running}runs} them, with [vhs] or, for an animated SVG image, without it.
    Only {!render} and {!record} perform I/O.

    {[
    let demo =
      Vhs.tape
        ~settings:Vhs.[ Width 1000; Height 560 ]
        ~outputs:[ "demo.gif" ]
        Vhs.[ type' "ls\n"; wait Prompt ]
    ]} *)

(** {1:actions Actions} *)

(** The type for durations, in milliseconds. A tape accepts [Ms 0] to
    [Ms 9_223_372_036_854], the range of Go's [time.Duration]. *)
type duration = Ms of int

(** The type for keys. A key without an argument is the VHS key of the same name
    without the underscore. Constructors may be added as VHS gains keys. *)
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

(** The type for conditions on the terminal. Constructors may be added. *)
type wait =
  | Prompt
      (** The line under the cursor ends with [>], or matches the pattern of a
          {!raw} [Set WaitPattern] if the tape has one. VHS starts each shell
          with the prompt ["> "], so this holds when the shell is ready for the
          next command. It never holds once something changes the prompt, such
          as a sourced script that sets [PS1].

          {b Warning.} It also holds while a command prints a line ending with
          [>] and waits. Wait for such a command with {!constructor-Line} or
          {!constructor-Screen}. *)
  | Line of string
      (** The line under the cursor, without its trailing spaces, matches the
          regular expression, as for {!constructor-Screen}. *)
  | Screen of string
      (** The screen, its lines joined by newlines, matches the regular
          expression, in the {{:https://github.com/google/re2/wiki/Syntax}RE2}
          syntax of Go and without surrounding slashes. VHS reads as many lines
          as the terminal has rows from the top of its buffer, so once output
          scrolls they are the first lines of its scrollback: at most a
          thousand, since the last [clear]. *)

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
    not hold within [timeout]. Without [timeout], the wait takes the duration of
    a {!raw} [Set WaitTimeout] if the tape has one, and 15 seconds otherwise. *)

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

(** The type for the shells VHS can start. Constructors may be added as VHS
    starts more. *)
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

(** The type for settings, applied before the first action. Constructors may be
    added for more of VHS's settings. *)
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
          them, is at most 10,000,000 pixels, as for {!constructor-Width}. *)
  | Theme of string
      (** The name of a theme of VHS 0.12.1, as [vhs themes] lists them; a theme
          in JSON, which starts with [{], an object whose fields VHS reads as
          strings, matching keys ignoring case, and xterm.js as colours, its
          default for a missing or empty one; or only spaces, for the default
          theme. A JSON theme nests at most 10000 levels, as Go reads it, and is
          valid UTF-8 without an unpaired surrogate escape such as [\ud800],
          which VHS reads as U+FFFD but this library does not. *)
  | Padding of int
      (** The padding in pixels, not negative (defaults to [60]). *)
  | Typing_speed of duration
      (** The pause after each typed character (defaults to [Ms 50]). *)
  | Framerate of int  (** A positive frame rate (defaults to [50]). *)

type t
(** The type for tapes VHS reads without error and without ignoring a line. VHS
    can still fail as it records, see {{!checks}checks}. *)

(** The type for errors. Constructors may be added, so a match on [error] ends
    with a case for the others. *)
type error =
  | Executable_not_found of string  (** No such executable on the [PATH]. *)
  | Failed of {
      vhs : string;
      status : Eio.Process.exit_status;
      output : string;
    }
      (** The executable [vhs] ran and did not exit with [0]; [output] is the
          end of what it printed, at most 64 KiB, such as the wait that timed
          out. *)
  | Io of string  (** Any other I/O failure. *)
  | Unsupported of string
      (** An output, a setting or a command that the function cannot play or
          write, named, and why: an output that only the other of {!render} and
          {!record} writes, or what the {{!native_details}native recorder} does
          not play. *)
  | Timeout of { wait : string; last : string }
      (** The wait line [wait] of {!record} did not hold in time; [last] is the
          line or the screen it last read. Only {!record} returns it: {!render}
          reports a wait that times out as {!constructor-Failed}, with the
          message of [vhs] in [output]. *)
  | Invalid of string
      (** A tape VHS rejects, or one of whose lines it ignores, naming the first
          offending item, and why. *)

val pp_error : error Fmt.t
(** [pp_error] formats an error for humans: one line, followed for
    {!constructor-Failed} by the last ten lines [vhs] printed, indented. Escape
    sequences and control characters are removed from every string it prints,
    and so are the format characters (Unicode category [Cf]) other than the
    joiners U+200C and U+200D, the prepended concatenation marks, such as
    U+0605, and the tag characters of a subdivision flag, such as Scotland's,
    which show; invalid UTF-8 is replaced by U+FFFD. *)

val tape :
  ?settings:setting list ->
  outputs:string list ->
  action list ->
  (t, error) result
(** [tape ~settings ~outputs actions] is the tape that applies [settings]
    (defaults to [[]]), runs [actions] and writes the recording to each path of
    [outputs], in the format of its extension: [.gif], [.mp4], [.webm], [.txt],
    [.ascii] or [.test] for the screen as text, a directory of PNG frames for
    [.png] or a trailing slash, and [.svg] for the animated SVG image that
    {!record} writes and VHS cannot. {!render} writes all but the [.svg] outputs
    and {!record} only the [.svg] and text outputs, so a recording to both a GIF
    and an SVG image takes a tape for each. It is [Error (Invalid msg)], with
    [msg] naming the first offending item, if the tape breaks one of the
    {{!checks}checks}. *)

val to_string : t -> string
(** [to_string t] is [t] in the [.tape] syntax: the [Output] lines, but those of
    the [.svg] outputs, then the [Set] lines, then the actions. An action is one
    or more lines, except [type' ""] and a {!hidden} of such actions, which are
    none. A {!type'} with all three quote characters is several [Type] lines. *)

val pp : t Fmt.t
(** [pp ppf t] is [Fmt.string ppf (to_string t)]. *)

(** {1:running Running tapes}

    Two engines play a tape, with one contract. {!render} runs [vhs], which
    records a GIF, a video or PNG frames with a browser and [ffmpeg]. {!record}
    plays the tape itself in bash, zsh or fish on a pseudo-terminal and draws
    the screens it shows as an animated SVG image; it needs no [vhs], browser or
    [ffmpeg]. Each one writes the outputs of the tape, relative to its [cwd] and
    anywhere else an absolute or [..] path leads, and refuses, as
    {!constructor-Unsupported} and before it plays anything, an output that only
    the other one writes. Both write the text outputs and the screenshots.
    {!svg} draws the image of output that is already there, with no tape and no
    shell. *)

val render :
  ?vhs:string ->
  clock:_ Eio.Time.Mono.t ->
  proc:_ Eio.Process.mgr ->
  cwd:_ Eio.Path.t ->
  t ->
  (unit, error) result
(** [render ~vhs ~clock ~proc ~cwd t] runs the executable [vhs] (defaults to
    ["vhs"]) as [vhs -q] in the directory [cwd], with the tape [t] on its
    standard input, so that it writes the outputs of [t]. It is
    [Error (Unsupported _)] if [t] has an [.svg] output, which {!record} writes.
    [vhs] gets the environment of [proc] without five variables: [VHS_PUBLISH],
    which would upload the recording; [rod], which configures the browser VHS
    drives; and [PS1], [PROMPT] and [PROMPT_COMMAND], which would replace the
    prompt {!constructor-Prompt} waits for. What [vhs] prints is kept for
    {!constructor-Failed} and dropped on success. If the calling fiber is
    cancelled, [render] sends [vhs] [SIGTERM], which makes it stop the terminal
    and the browser it started, and waits for it to exit. If [vhs] still runs
    ten seconds later by [clock], [render] sends it [SIGKILL], which can leave
    the terminal and the browser running, and waits for it to exit. *)

val record :
  clock:_ Eio.Time.Mono.t ->
  proc:_ Eio.Process.mgr ->
  cwd:_ Eio.Path.t ->
  t ->
  (unit, error) result
(** [record ~clock ~proc ~cwd t] plays [t] in its shell in the directory [cwd],
    with the environment {!render} gives [vhs], and writes the outputs of [t] as
    {!render} does: the SVG image of what the terminal showed to the [.svg]
    output, the {{!native_details}text output} to the [.txt], [.ascii] or
    [.test] output and the {{!native_details}screenshots}. The waits of [t] time
    out by [clock]. When the recording ends or fails, or the calling fiber is
    cancelled, [record] sends the shell [SIGTERM], hangs up its terminal, sends
    it [SIGKILL] if it still runs a second later by [clock], and waits for it to
    exit. It is:
    - [Error (Unsupported _)] on a [.gif], [.mp4], [.webm] or PNG frames output,
      which {!render} writes, and on what the {{!native_details}native recorder}
      does not play, before it plays anything;
    - [Error (Invalid _)] on a [Source] line it refuses;
    - [Error (Timeout _)] if a wait does not hold in time;
    - [Error (Executable_not_found _)] if the shell or the executable of a
      [Require] line is not on the [PATH];
    - [Error (Io _)] if [cwd] is not a directory of the native file system, and
      on any other I/O failure, such as an output it cannot write. *)

val svg :
  ?settings:setting list -> (duration * string) list -> (string, error) result
(** [svg ~settings output] is the SVG image of a terminal with [settings]
    (defaults to [[]]) that receives each string of [output] at its time, as
    {!record} draws it. [svg] starts no shell, so it ignores
    {!constructor-Shell}. It is [Error (Invalid _)] if a setting is one
    {!val-tape} refuses, or if a time is out of the range of a {!duration} or
    before the one ahead of it, and [Error (Unsupported _)] on what the
    {{!native_details}native recorder} refuses: the {!constructor-Theme} colours
    and the sizes listed there. *)

(** {1:checks Checks}

    [tape ~settings ~outputs actions] is [Error (Invalid msg)], with [msg]
    naming the first offending item, if:
    - [outputs] is empty, or one has another extension, none and no trailing
      slash, or starts with [-], which ffmpeg reads as an option;
    - two outputs, {!raw} [Output] lines included, are of the same kind (GIF,
      MP4, WebM, text, PNG frames or SVG): VHS writes only the last of each kind
      but SVG, which it never reads, and {!record} writes one SVG image;
    - a string holds a control character, C1 controls included (the tabs and
      newlines of {!type'} aside), or, in typed text, invalid UTF-8;
    - an output path or a {!constructor-Theme} holds all three quote characters
      (['"'], ['\''] and ['`']), which no VHS string can quote; a {!type'} can
      hold them, see {!to_string};
    - a duration is out of range, or a wait timeout is [Ms 0];
    - a {!constructor-Line} or {!constructor-Screen} pattern is not valid Go
      regular expression syntax;
    - a key or a setting breaks the condition its documentation states;
    - a {!raw} line is not exactly one command VHS's parser accepts, ends inside
      a string, a regular expression or a JSON value, or lacks the argument of
      its command;
    - a {!raw} [Set] has a value its setting cannot parse: a duration, an
      integer, a float or a regular expression in Go's syntax, or a positive
      integer for [Rows] and [Columns];
    - a {!raw} [Set Rows] or [Set Columns] sizes the frame in the direction that
      {!constructor-Height} or {!constructor-Width} does, or asks for more rows
      or columns than 32 times the browser's viewport has pixels: VHS measures
      the cells in the viewport doubled up to five times, and fails with "could
      not fit" if the terminal there is still smaller. Fewer can still not fit
      with a large font, which [tape] does not check;
    - the {!constructor-Width} or {!constructor-Height} of a GIF output is
      beyond 65535 pixels, of an MP4 output beyond 16384 or of a WebM output
      beyond 65536, the sizes the encoders of ffmpeg 9 take, or the frame of
      such an output has more pixels than ffmpeg's image check takes, about 268
      million: (8 width + 1024) (height + 128) must be below 2{^ 31};
    - a {!raw} line is [Hide] or [Show] (use {!hidden}), [Set] for a setting of
      {!setting} other than [TypingSpeed] (use the setting), an [Output] that
      breaks the rules of [outputs] or ends with [.svg], or is [Require] or
      [Set], other than [Set TypingSpeed], after an action that prints a line,
      where VHS ignores it;
    - every action outside {!hidden} is a [type' ""] or a {!raw} [Output],
      [Set], [Require], empty or comment line, which VHS does not record, so it
      records no frame and fails. VHS also fails if the actions it records all
      end within one frame, such as a lone [sleep (Ms 0)], which [tape] does not
      check. *)

(** {1:native_details Native recordings}

    The terminal has the size VHS gives it with JetBrains Mono, VHS's first
    font: [Width], [Height], [Font_size], [Padding] and the raw [Set Margin],
    [Set LetterSpacing], [Set LineHeight], [Set Rows] and [Set Columns] size it.
    VHS sizes it with the fonts of the machine it runs on, so it can show more
    rows than the image elsewhere. The image draws each cell on a grid, in a
    monospace font of the viewer's machine, in the colours of the [Theme]. The
    terminal keeps a colour as RGB, so a 256-colour or true colour equal to one
    of xterm's sixteen ANSI colours takes the theme's colour too. The terminal
    reads what the shell prints as xterm does, where VHS's xterm.js departs from
    it in a few controls, such as an erasure in the last column or the row a
    cursor restore returns to. As in xterm.js, a character of no width, such as
    a combining mark or a zero-width joiner, joins the cell before it.

    Time is the tape's: a typed character, a key and a sleep last what the tape
    says, a wait lasts what the program makes it last, and output shows at the
    time it arrives within the action that runs, rounded down to a frame of
    {!constructor-Framerate}, which lasts at least a millisecond. A tape whose
    program prints the same bytes within the same frames gives the same image,
    byte for byte; the time a command takes to print, including how the machine
    schedules it, changes the frames its output lands in.

    The text output, [.txt], [.ascii] or [.test], is VHS's: after each command
    that runs while recording, the first lines of the terminal's buffer, as many
    as it has rows, as a {!constructor-Screen} wait reads them, then a line of
    80 U+2500 characters. VHS also adds a screen for each [Output] and [Set]
    line it reads before it sizes the terminal, from the text output's line on,
    whose rows depend on its browser's window; the native output leaves them
    out.

    A [Screenshot] of [shot.png], which VHS writes as a PNG image of the next
    frame it records, writes a still SVG image of that frame to [shot.svg],
    without the cursor blinking.

    {!record} plays the actions of {!action}, the raw [Type], key, [Ctrl],
    [Alt], [Shift], [Sleep], [Wait], [Copy], [Paste] ([Paste] types the last
    [Copy]), [Screenshot] and [Output] lines, [Source] lines, whose tape it
    reads relative to the [cwd] of {!record} and refuses, as
    {!constructor-Invalid}, where VHS does and on a control character other than
    a tab or a carriage return, which {!val-tape} refuses in a {!raw} line,
    [Env], [Require], and every [Set] but those below. It does not play, and is
    {!constructor-Unsupported} on:
    - a {!shell} other than {!constructor-Bash}, {!constructor-Zsh} and
      {!constructor-Fish}, which start as in VHS;
    - a colour of a JSON [Theme] other than [#RGB], [#RGBA], [#RRGGBB],
      [#RRGGBBAA], [rgb()] or [rgba()], such as a colour name, which xterm.js
      reads with the browser;
    - [Set MarginFill] with an image other than PNG, JPEG, GIF, WebP, BMP or
      SVG, by its extension.

    Its patterns match as Go's do, case folding, ASCII word boundaries and the
    Unicode classes of Go's tables included. After a line is submitted, by
    {!constructor-Enter} or the newline of a {!type'}, a {!constructor-Prompt}
    or {!constructor-Line} wait holds only once the terminal has started another
    line, or changed the line so that it does not hold, so it never holds on the
    prompt the command was typed at, however fast it is typed.
    [Set CursorBlink], [Set BorderRadius], [Set PlaybackSpeed] and
    [Set LoopOffset] change the animation, and [Set WindowBar] and
    [Set WindowBarSize] draw VHS's window bar. For a reader who prefers reduced
    motion, the image shows its last screen, still, and the cursor does not
    blink. The image leaves out the invisible format characters, as {!pp_error}
    does. Its title is "Terminal recording" and, unless its last screen is
    blank, its description, its text alternative, is the text that screen shows,
    without concealed text. The image of a [Set MarginFill], read relative to
    the [cwd] of {!record}, is embedded, stretched to the frame as ffmpeg
    stretches it. A size beyond 100000 pixels, a terminal beyond 2000 cells a
    side, a [Set LetterSpacing] or [Set LineHeight] beyond 100000 and a positive
    [Set PlaybackSpeed] below 0.001 are {!constructor-Unsupported}. *)
