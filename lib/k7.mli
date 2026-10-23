(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Terminal recordings as animated SVG images, and tapes for VHS.

    [K7] draws a terminal session as an SVG image animated with CSS alone,
    without a browser. {!record} plays a tape in a shell on a pseudo-terminal,
    {!run} runs a program there, {!capture} records a program there as an
    asciicast recording, {!cram} gives the cram test of a command, {!svg} draws
    output given with its times and {!svg_of_casts} draws asciicast recordings.

    A tape is the script of {{:https://github.com/charmbracelet/vhs}VHS}, by
    {{:https://charm.land/}Charm}, which records a terminal session as a GIF or
    a video. The script lists settings, then actions such as typing text or
    waiting for output. [K7] builds the actions as values and {{!checks}checks}
    them once, when {!val-tape} assembles a tape. It reads and prints tapes in
    the [.tape] syntax and {{!running}runs} them, with [vhs] for a GIF or a
    video. Only {!record}, {!check}, {!run}, {!capture} and {!cram} perform I/O.

    {[
    let demo =
      K7.tape
        ~settings:K7.[ Columns 80; Rows 12 ]
        ~outputs:[ "demo.svg" ]
        K7.[ type' "ls\n"; wait Prompt ]
    ]}

    {!record} writes [demo.svg] without [vhs]: it picks the {!engine} that
    writes the outputs of a tape, as [k7 play] does. *)

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
          [-], [.], a backslash or a square bracket. As in VHS, [@], [-] and [.]
          send nothing. *)
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
    not hold within [timeout]. Without [timeout], the wait takes the
    {!constructor-Wait_timeout} of the tape. *)

val hidden : action list -> action
(** [hidden actions] runs [actions] without recording them. A [hidden] inside
    [actions] is the same as its own actions. *)

val raw : string -> action
(** [raw line] is the tape line [line], printed as is, with its arguments quoted
    by the caller. {!val-tape} checks it as its documentation states. Only VHS,
    as it runs, checks the tape of a [Source] line, the program of a [Require]
    line, the image a [Set MarginFill] names when it is not a colour, and what
    ffmpeg makes of a [Set PlaybackSpeed] or [Set LoopOffset] value. *)

(** {1:errors Errors} *)

(** The type for errors. Constructors may be added, so a match on [error] ends
    with a case for the others. *)
type error =
  | Executable_not_found of string  (** No such executable on the [PATH]. *)
  | Vhs_failed of {
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
          write, named, and why. *)
  | Timeout of {
      wait : string;
      line : int;
      file : string option;
      last : string;
    }
      (** A wait of the {!constructor-Native} engine did not hold in time.
          [line] is its line, counted from 1, in the source {!of_string} read
          the tape from, or in the tape as {!to_string} prints it for a tape
          built otherwise, and [wait] is that line, such as [Wait+Line /re/] or
          [Wait@2000ms]. For a wait in the tape of a [Source] line, [file] is
          that tape, as the line names it, and [line] is the line there; [file]
          is [None] otherwise. [last] is the line or the screen the wait last
          read. Before the first action, the engine waits for the shell's first
          prompt, as VHS does, within the {!constructor-Wait_timeout} of the
          tape; [line] is then [0] and [wait] says so. Only the
          {!constructor-Native} engine returns it, and the {!constructor-Vhs}
          engine reports a wait that times out as {!constructor-Vhs_failed},
          with the message of [vhs] in [output]. *)
  | Stuck of { wait : string; line : int; file : string option; last : string }
      (** The [stuck] of {!record} held while the wait [wait] on line [line] of
          [file], as for {!constructor-Timeout}, did not hold, so {!record}
          failed at once rather than when the wait would time out. *)
  | Exited of {
      status : Eio.Process.exit_status;
      wait : string;
      line : int;
      file : string option;
      last : string;
    }
      (** The shell of the {!constructor-Native} engine exited with [status]
          while the wait [wait] on line [line] of [file], as for
          {!constructor-Timeout}, did not hold. The wait then never holds, so
          {!record} fails at once rather than when the wait times out. *)
  | Invalid of string
      (** A tape, a setting or a time that VHS rejects or partly ignores, or
          that {!record}, {!svg} or {!svg_of_casts} refuses as their
          documentation states, naming the first offending item, and why. *)

val pp_error : error Fmt.t
(** [pp_error] formats an error for humans, on one line. For
    {!constructor-Vhs_failed}, the last ten lines that [vhs] printed and that
    are not blank follow it, trimmed and indented. It removes escape sequences
    and control characters from every string it prints. It also removes the
    format characters (Unicode category [Cf]), but for the joiners U+200C and
    U+200D and the prepended concatenation marks such as U+0605. It keeps the
    tag characters of a subdivision flag such as Scotland's. A variation
    selector stays after a character that shows and goes elsewhere. Invalid
    UTF-8 becomes U+FFFD. *)

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
          at most ten million pixels. *)
  | Height of int
      (** The height in pixels, at least twice the padding and the margin, plus
          the size of the window bar of a {!raw} [Set WindowBar], and even with
          an MP4 output (defaults to [600]). Its viewport, what is left after
          them, is at most ten million pixels, as for {!constructor-Width}. *)
  | Columns of int
      (** The terminal's columns, a positive number, in place of
          {!constructor-Width}. The frame is as wide as they need, given the
          other settings. *)
  | Rows of int
      (** The terminal's rows, a positive number, in place of
          {!constructor-Height}, as for {!constructor-Columns}. *)
  | Theme of string
      (** The name of a theme of VHS 0.12.1, such as ["Dracula"], as
          [vhs themes] lists them, an empty or blank string for the default
          theme, or a theme in JSON, which starts with [{]. A JSON theme is an
          object whose fields VHS reads as strings, matching keys ignoring case,
          and xterm.js reads as colours, with its default for a missing or empty
          one. It nests at most 10000 levels, as Go reads it. It is valid UTF-8
          and holds no unpaired surrogate escape such as [\ud800]; VHS reads
          both as U+FFFD, and {!val-tape} refuses them. *)
  | Padding of int
      (** The padding in pixels, not negative (defaults to [60]). *)
  | Typing_speed of duration
      (** The pause after each typed character (defaults to [Ms 50]). *)
  | Framerate of int  (** A positive frame rate (defaults to [50]). *)
  | Wait_timeout of duration
      (** How long a {!val-wait} without a timeout waits (defaults to 15
          seconds). *)

type t
(** The type for tapes VHS reads without error and without ignoring a line, but
    for a {!raw} [Screenshot] of an [.svg] image, which only the
    {!constructor-Native} engine takes. VHS can still fail as it records, see
    {{!checks}checks}. *)

val tape :
  ?settings:setting list ->
  outputs:string list ->
  action list ->
  (t, error) result
(** [tape ~settings ~outputs actions] is the tape that applies [settings]
    (defaults to [[]]), runs [actions] and writes the recording to each path of
    [outputs], in the format of its extension. The extensions are [.gif], [.mp4]
    and [.webm]; [.txt], [.ascii] and [.test] for the screen as text; [.png], or
    a trailing slash, for a directory of PNG frames; and [.svg] for the animated
    SVG image that the {!constructor-Native} engine writes and VHS cannot.
    {{!running}Running tapes} says which engine writes which output. It is
    [Error (Invalid msg)], with [msg] naming the first offending item, if the
    tape breaks one of the {{!checks}checks}. *)

val settings : t -> setting list
(** [settings t] are the settings of [t], as {!val-tape}, {!of_string} or
    {!with_settings} gave them. *)

val outputs : t -> string list
(** [outputs t] are the outputs of [t], as {!val-tape}, {!of_string} or
    {!with_outputs} gave them. *)

val with_settings : setting list -> t -> (t, error) result
(** [with_settings settings t] is [t] with [settings] after its own settings, in
    place of those of the same constructors and of those that size the frame in
    the same direction: {!constructor-Columns} replaces {!constructor-Width} and
    {!constructor-Rows} replaces {!constructor-Height}, and the reverse. A
    {!raw} [Set] of the same setting before the first action, such as a
    fractional [Set TypingSpeed] that {!of_string} keeps raw, is replaced too.
    It is [Error (Invalid _)] as {!val-tape} is. *)

val with_outputs : string list -> t -> (t, error) result
(** [with_outputs outputs t] is [t] that writes to [outputs] in place of its own
    outputs. It is [Error (Invalid _)] as {!val-tape} is. *)

val of_string : string -> (t, error) result
(** [of_string s] is the tape of the [.tape] source [s], one command per line,
    each line ending with ["\n"] or ["\r\n"]. Its [Output] lines, wherever they
    are, are its outputs, and the [Set] lines before the first action that a
    {!setting} writes are its settings; a tape may have no output, which
    {!with_outputs} gives it. The lines between [Hide] and the next [Show], or
    the end, are a {!hidden} action, and a [Show] outside them is dropped. Every
    other line is a {!raw} action. It is [Error (Invalid msg)] if a line is not
    one VHS's parser accepts, or is a [Set WaitTimeout] that is not a whole
    number of milliseconds, with [msg] naming the line by its number from [1],
    or if {!val-tape} refuses the tape. For a tape [x],
    [Result.map to_string (of_string (to_string x))] is [Ok (to_string x)].

    {b Note.} {!to_string} prints the outputs first, so a text output that an
    [Output] line after the first action names holds the screens from the first
    action on, where VHS starts at that line. *)

val to_string : t -> string
(** [to_string t] is [t] in the [.tape] syntax. The [Output] lines come first,
    without those of the [.svg] outputs, then the [Set] lines and the actions.
    An action is one or more lines, except [type' ""] and a {!hidden} of such
    actions, which are none. A {!hidden} is [Hide], its actions and [Show], with
    no [Show] when it is the last action, as VHS records nothing after a last
    [Hide]. A {!type'} with all three quote characters is several [Type] lines.
*)

val pp : t Fmt.t
(** [pp ppf t] is [Fmt.string ppf (to_string t)]. *)

(** {1:running Running tapes}

    Two engines record a tape, with one contract. The {!constructor-Vhs} engine
    runs [vhs], which records with a browser and [ffmpeg]. The
    {!constructor-Native} engine plays the tape itself in bash, zsh or fish on a
    pseudo-terminal and draws the screens it shows; it needs no [vhs], browser
    or [ffmpeg].

    The {!constructor-Vhs} engine writes [.gif], [.mp4] and [.webm] outputs and
    PNG frames, the {!constructor-Native} engine writes [.svg] outputs, and both
    write [.txt], [.ascii] and [.test] outputs. A [Screenshot] is a [.png] image
    for the {!constructor-Vhs} engine and an [.svg] image for the
    {!constructor-Native} engine. Each one refuses, as
    {!constructor-Unsupported} and before it plays anything, an output or a
    [Screenshot] that only the other one writes, so a GIF and an SVG image of
    the same actions take a tape each. Each one is [Error (Invalid _)] on a tape
    without an output, such as one {!of_string} read from a source without an
    [Output] line.

    Each one writes at the paths the tape names, relative to its [cwd] and
    anywhere else an absolute or [..] path leads. It creates the missing
    directories of an output, as VHS does, but not those of PNG frames or of a
    screenshot. Before it plays anything, it reports as {!constructor-Io} an
    output or a screenshot that is a directory, but PNG frames, and an output
    whose directory it cannot write in or create.

    {!svg} draws the image the {!constructor-Native} engine would from output
    given with its times, such as bytes a test captured, with no tape, no shell
    and no file written: it returns the image. {!svg_of_casts} draws the output
    of asciicast recordings the same way.

    {[
    let image =
      K7.svg
        ~settings:K7.[ Width 600; Height 200 ]
        K7.[ (Ms 0, "$ ls\r\n"); (Ms 500, "a.txt  b.txt\r\n") ]
    ]} *)

(** The type for the engines that record a tape. *)
type engine =
  | Native
      (** k7 plays the tape in a shell on a pseudo-terminal, without [vhs]. *)
  | Vhs  (** [vhs] records the tape, with a browser and [ffmpeg]. *)

val record :
  ?engine:engine ->
  ?vhs:string ->
  ?env:(string * string) list ->
  ?stop:unit Eio.Promise.t ->
  ?stuck:(string -> bool) ->
  clock:_ Eio.Time.Mono.t ->
  proc:_ Eio_unix.Process.mgr ->
  cwd:_ Eio.Path.t ->
  t ->
  (unit, error) result
(** [record ~engine ~vhs ~env ~stop ~stuck ~clock ~proc ~cwd t] writes the
    outputs of [t] with [engine], which defaults to {!constructor-Vhs} if [t]
    has an output or a [Screenshot] that only {!constructor-Vhs} writes, and to
    {!constructor-Native} otherwise, which needs no [vhs]. It is
    [Error (Invalid _)], before it runs anything, if a name of [env] is empty or
    holds [=], or a name or a value holds a NUL byte.

    The environment of the shell is, in this order, each variable replacing one
    of the same name before it:
    - the environment of [proc] without seven variables: [VHS_PUBLISH], which
      would upload the recording; [rod], which configures the browser VHS
      drives; [PS1], [PROMPT] and [PROMPT_COMMAND], which would replace the
      prompt {!constructor-Prompt} waits for; and [COLUMNS] and [LINES], which
      would replace the size of the terminal;
    - [TERM=xterm-256color], as ttyd sets it under [vhs];
    - the variables of [env] (defaults to [[]]);
    - the prompt VHS gives the shell and the [Env] lines of [t].

    With {!constructor-Vhs}, [vhs] gets the first and the third, and gives the
    shell the others itself, so ttyd's [TERM] replaces one of [env].

    With {!constructor-Vhs}, [record] runs the executable [vhs] (defaults to
    ["vhs"]), looked up in the [PATH] it gets unless it holds a slash, as
    [vhs -q] in the directory [cwd], with the tape [t] on its standard input, so
    that it writes the outputs of [t]. What [vhs] prints is kept for
    {!constructor-Vhs_failed} and dropped on success. If the calling fiber is
    cancelled, [record] sends [vhs] [SIGTERM], which makes it stop the terminal
    and the browser it started, and waits for it to exit. If [vhs] still runs
    ten seconds later by [clock], [record] sends it [SIGKILL], which can leave
    the terminal and the browser running, and waits for it to exit. It does the
    same once [stop] resolves, and is then [Error (Vhs_failed _)] unless [vhs]
    has exited with [0]. It is:
    - [Error (Unsupported _)] on an [.svg] output or a {!raw} [Screenshot] of an
      [.svg] image, which {!constructor-Native} writes, before it runs anything;
    - [Error (Invalid _)], before it runs anything, on a [Screenshot] whose
      path, in [cwd] and with symbolic links followed, is that of an output or
      of another [Screenshot];
    - [Error (Io _)] if the directory of an output cannot be written, before it
      runs anything, and on any other I/O failure;
    - [Error (Executable_not_found _)] if [vhs] is not found;
    - [Error (Vhs_failed _)] if [vhs] exits with another status than [0], a wait
      that times out included.

    With {!constructor-Native}, [record] plays [t] in its shell in the directory
    [cwd]. The [.svg] output is the SVG image of what the terminal showed, the
    [.txt], [.ascii] or [.test] output is the {{!native_details}text output},
    and the {{!native_details}screenshots} are written too. Once [stop]
    resolves, [record] runs no more actions and writes the outputs of what it
    recorded so far. The waits of [t] time out by [clock]. While a wait does not
    hold, [stuck out] (defaults to [false]), with [out] the last 64 KiB the
    shell wrote since the previous wait held, escape sequences included, fails
    the recording at once with {!constructor-Stuck}; {!constructor-Vhs} ignores
    [stuck]. When the recording ends or fails, or the calling fiber is
    cancelled, [record] sends the shell and its process group [SIGTERM], hangs
    up its terminal, sends them [SIGKILL] if one still runs a second later by
    [clock], and waits for the shell to exit. It is:
    - [Error (Unsupported _)] on a [.gif], [.mp4], [.webm] or PNG frames output,
      or a [Screenshot] of a [.png] image, which {!constructor-Vhs} writes, and
      on what the {{!native_details}native recorder} does not play, before it
      plays anything;
    - [Error (Invalid _)] on a [Source] line or a [Set MarginFill] image it
      refuses, and on a [Screenshot], of [t] or of a sourced tape, whose path,
      in [cwd] and with symbolic links followed, is that of an output or of
      another [Screenshot], before it plays anything;
    - [Error (Timeout _)] if a wait does not hold in time, or the shell shows no
      prompt within the {!constructor-Wait_timeout} of the tape;
    - [Error (Exited _)] as soon as the shell exits while a wait does not hold,
      or before its first prompt;
    - [Error (Executable_not_found _)] if the shell or the executable of a
      [Require] line is not on the [PATH] the shell gets;
    - [Error (Io _)] if [cwd] is not a directory of the native file system, or
      if the directory of an output cannot be written, before it plays anything,
      and on any other I/O failure. *)

val check :
  ?engine:engine ->
  ?env:(string * string) list ->
  cwd:_ Eio.Path.t ->
  t ->
  (unit, error) result
(** [check ~engine ~env ~cwd t] is the error that [record ~engine ~env ~cwd t]
    returns before it runs anything, if any, and [Ok ()] otherwise: it checks
    [env], the outputs and the [Screenshot] paths of [t] against [engine] and
    their directories, and, for {!constructor-Native}, the [Source] and
    [Set MarginFill] files of [t]. It starts no shell and no [vhs] and writes
    nothing, so it does not look for [vhs], the shell or the executable of a
    [Require] line. *)

val svg :
  ?settings:setting list -> (duration * string) list -> (string, error) result
(** [svg ~settings output] is the SVG image of a terminal with [settings]
    (defaults to [[]]) that receives each string of [output] at its time, as the
    {!constructor-Native} engine draws it. The strings are raw terminal output,
    so a line ends with ["\r\n"]: ["a\nb"] moves down without returning to the
    first column. [svg] starts no shell and types nothing, so
    {!constructor-Shell}, {!constructor-Typing_speed} and
    {!constructor-Wait_timeout} have no effect. It is [Error (Invalid _)] if a
    setting is one {!val-tape} refuses, or if a time is out of the range of a
    {!duration} or before the one ahead of it. It is [Error (Unsupported _)] on
    the {!constructor-Theme} colours and the sizes that the
    {{!native_details}native recorder} refuses. *)

(** {1:casts Asciicast recordings}

    An asciicast recording is what a program printed, with its times, in the
    format of {{:https://asciinema.org/}asciinema}. {!capture} records one,
    {!svg_of_casts} draws recordings as an image and {!text_of_casts} gives the
    last screen they show, so a recording can be drawn later with other
    settings. *)

type cast
(** The type for asciicast recordings. *)

val cast_of_string : string -> (cast, error) result
(** [cast_of_string s] is the recording [s], in
    {{:https://docs.asciinema.org/manual/asciicast/v2/}asciicast v2} or
    {{:https://docs.asciinema.org/manual/asciicast/v3/}asciicast v3}, as
    asciinema 2 and 3 write it. It keeps the size of the terminal, the
    [command], the [theme], with the [cursor] and [cursor_accent] colours of
    {!string_of_cast}, and, after the [idle_time_limit] of the header, the
    output, resize and exit events at their times, each rounded to the
    microsecond, then to the nearest millisecond, half up, so the same [s] gives
    the same image, byte for byte. It ignores the other members of the header,
    input events, markers and events of other codes.

    It is [Error (Invalid msg)] if [s] is not such a recording, with [msg]
    naming the line, from [1], and why: a header that is not a JSON object with
    [version] [2] and a positive integer [width] and [height], or [version] [3]
    and a [term] object with a positive integer [cols] and [rows]; a theme whose
    colours are not [#rrggbb] or whose palette does not have eight or sixteen of
    them; a line that is not an event [[time, code, data]] of a number and two
    strings; a negative time or interval, or in version 2 a time before the one
    ahead of it; a resize whose data is not [COLSxROWS], or an exit whose data
    is not an integer. *)

val string_of_cast : cast -> string
(** [string_of_cast c] is [c] in asciicast v3, its times as intervals in
    milliseconds and without an [idle_time_limit]: {!cast_of_string} reads it
    back as [c]. Beyond asciicast v3, its theme holds the [cursor] colour if it
    is not the foreground and the [cursor_accent] colour if it is not the one
    {!svg} derives, so that a recording {!capture} made with a
    {!constructor-Theme} draws with that theme's colours. *)

val svg_of_casts :
  ?settings:setting list ->
  ?show:string list ->
  ?delay:duration ->
  cast list ->
  (string, error) result
(** [svg_of_casts ~settings ~show ~delay casts] is the SVG image of the
    recordings [casts], one after the other on one terminal, as {!svg} draws the
    data of their output events at their times. A recording's first line is [$ ]
    then its command, as for {!run}, and its output shows [delay] (defaults to
    [Ms 500]) later. If [show] (defaults to [[]]) holds one string, that string
    is the command of every recording; if it holds one string per recording, its
    [n]th string is the command of the [n]th recording. Otherwise a recording's
    command is the [command] of its header, if any, its control characters but
    newlines shown as spaces. A newline in a command continues it on a line that
    starts with [> ], as in a shell. An empty command shows no line and no
    delay. The last screen of each recording holds for a second after its last
    event, an exit included, and the next recording starts then.

    The terminal has the size of the header of each recording from its start,
    and of each resize, on an image of the largest of them, unless [settings]
    (defaults to [[]]) size the terminal in that direction: it then keeps that
    size. Its colours are the theme of the first recording's header, if any,
    with each colour of an eight-colour palette as its bright colour too, unless
    [settings] give a {!constructor-Theme}. [settings] apply after them. So, for
    a recording [c] without a command, a theme or a resize, whose header has the
    size [w] and [h], whose output events give [output], their data at their
    milliseconds, whose last event is at [last], and for [settings] that do not
    size the terminal:

    {@ocaml skip[
    svg_of_casts ~settings [ c ]
    = svg
        ~settings:(Columns w :: Rows h :: settings)
        (output @ [ (Ms (last + 1000), "") ])
    ]}

    It is [Error (Invalid _)] on an empty [casts], on a [show] of several
    strings but not one per recording, on what {!run} refuses in [show] and
    [delay], and the error of {!svg} on [settings] that it refuses. *)

val text_of_casts :
  ?settings:setting list ->
  ?show:string list ->
  cast list ->
  (string, error) result
(** [text_of_casts ~settings ~show casts] is the text of the last screen of
    [svg_of_casts ~settings ~show casts]: each row with a space for each column
    that SGR 8 conceals and without its trailing white space, as {!svg} reads
    it, followed by a newline, up to the last row that is not blank. It is the
    error of {!svg_of_casts} otherwise. *)

val run :
  ?settings:setting list ->
  ?env:(string * string) list ->
  ?show:string ->
  ?delay:duration ->
  ?stop:unit Eio.Promise.t ->
  clock:_ Eio.Time.Mono.t ->
  proc:_ Eio_unix.Process.mgr ->
  cwd:_ Eio.Path.t ->
  string list ->
  (string * Eio.Process.exit_status, error) result
(** [run ~settings ~env ~show ~delay ~stop ~clock ~proc ~cwd argv] runs the
    program [argv] in the directory [cwd] on a pseudo-terminal, with no shell
    and nothing typed. It is the SVG image of what the terminal showed, as
    {!svg} draws a terminal with [settings] (defaults to [[]]), paired with how
    the program exited. Each output shows at the time it arrived since the
    program started, rounded down to a frame, and the last screen holds for a
    second after the program exits. The image's first line is [$ ] in the colour
    of VHS's prompt, then [show] (defaults to [argv], each argument shell-quoted
    where needed, for POSIX sh), and the program's output starts on the next
    line; an empty [show] shows no line. A line that shows stays alone for
    [delay] (defaults to [Ms 500]), as if typed, and the program's output shows
    that much later. The first element of [argv] is the executable, looked up in
    the [PATH] the program gets unless it holds a slash, and relative to [cwd]
    if it does. The program gets the environment {!record} gives a shell up to
    the variables of [env] (defaults to [[]]), without the prompt and the [Env]
    lines.

    A program that exits with another status than [0], or is killed, still gives
    its image: the status says how it exited. [run] returns once the program has
    exited and what it printed is read, so a program that waits for input waits
    until the calling fiber is cancelled or [stop] resolves. Once [stop]
    resolves, [run] sends the program's process group [SIGTERM], then [SIGKILL]
    a second later by [clock], and returns the image of what the terminal showed
    until the program exited. The program leads a process group. When it exits,
    or if the calling fiber is cancelled, [run] sends the group [SIGTERM], hangs
    up its terminal, sends the group [SIGKILL] if one of it still runs a second
    later by [clock], and waits for the program to exit. The image is that of
    {!capture}'s recording as {!svg_of_casts} draws it, with [settings] and
    [delay]. It is:
    - [Error (Invalid _)] if [argv] is empty, if a setting is one {!svg}
      refuses, if [show] holds a control character, if [delay] is out of the
      range of a {!duration}, or if a name of [env] is empty or holds [=], or a
      name or a value holds a NUL byte;
    - [Error (Unsupported _)] on what {!svg} refuses;
    - [Error (Executable_not_found _)] if the executable is not found;
    - [Error (Io _)] if an argument of [argv] holds a NUL byte, which [execve]
      refuses, if [cwd] is not a directory of the native file system or cannot
      be entered, if the executable cannot run, with the reason, and on any
      other I/O failure. *)

val capture :
  ?settings:setting list ->
  ?env:(string * string) list ->
  ?show:string ->
  ?stop:unit Eio.Promise.t ->
  clock:_ Eio.Time.Mono.t ->
  proc:_ Eio_unix.Process.mgr ->
  cwd:_ Eio.Path.t ->
  string list ->
  (cast * Eio.Process.exit_status, error) result
(** [capture ~settings ~env ~show ~stop ~clock ~proc ~cwd argv] runs the program
    [argv] as {!run} does and is the recording of what it printed, paired with
    how it exited. The recording has the size of the terminal of [settings], the
    colours of their {!constructor-Theme}, if any, as its theme, [show] as its
    [command] (defaults to [argv], each argument shell-quoted where needed; none
    if empty), each output at the millisecond it arrived since the program
    started, in valid UTF-8 (a character split across two outputs goes in the
    second, and a byte that is not UTF-8, an incomplete character at the end
    included, is U+FFFD), and the exit, with the exit code or [128] plus the
    number of the signal that killed the program. The terminal reads the command
    line first, as in the image of {!run}, so the program's queries get the
    answers they would there. It is the error of {!run} otherwise. *)

val cram :
  ?env:(string * string) list ->
  ?stop:unit Eio.Promise.t ->
  clock:_ Eio.Time.Mono.t ->
  proc:_ Eio_unix.Process.mgr ->
  cwd:_ Eio.Path.t ->
  string list ->
  (string * Eio.Process.exit_status, error) result
(** [cram ~env ~stop ~clock ~proc ~cwd argv] runs the program [argv] as a cram
    test runs a command, and is that cram test, paired with how the command
    exited. [cram] runs [sh -c] with the command line, [argv] with each argument
    shell-quoted where needed, in the directory [cwd], on pipes: an empty
    standard input, and one pipe for both its standard output and standard
    error. It gets the environment of [proc] with the variables of [env]
    (defaults to [[]]). [sh] leads a process group. Once [sh] exits, or once
    [stop] resolves, [cram] sends the group [SIGTERM], then [SIGKILL] a second
    later by [clock], and returns once [sh] has exited and what the group
    printed is read.

    The cram test is the line [  $ ] and the command line, which continues on a
    line [  > ] after each newline of an argument, then each line of what the
    command printed, indented by two spaces, as dune writes them: without the
    bytes from an escape character up to the next [m], without the carriage
    return before a line feed, and with a last line that does not end with a
    line feed; then [  [N]] if the exit code [N], or [128] plus the number of
    the signal that killed the command, is not [0]. dune then reads the test
    back unchanged, but for a line dune would read as a command: one that starts
    with [$ ], or a first line that is [>] or starts with [> ]. dune writes it
    as it is, and runs it; [cram] indents it by one more space, so that it stays
    output, which dune reports as a difference. It is [Error (Invalid _)] if
    [argv] is empty or a name of [env] is one {!run} refuses, and [Error (Io _)]
    if an argument of [argv] holds a NUL byte, as for {!run}, before anything
    runs, or on an I/O failure. *)

(** {1:checks Checks}

    [tape ~settings ~outputs actions] is [Error (Invalid msg)], with [msg]
    naming the first offending item, if:
    - an output has another extension, none and no trailing slash, or starts
      with [-], which ffmpeg reads as an option;
    - two outputs are of the same kind (GIF, MP4, WebM, text, PNG frames or
      SVG). VHS writes only the last of each kind but SVG, which it never reads,
      and {!record} writes one SVG image;
    - a {!raw} [Screenshot] has the path of an output or of another [Screenshot]
      once [.] segments, [..] segments after a name and repeated slashes are
      removed;
    - a string holds a control character, C1 controls included (the tabs and
      newlines of {!type'} aside), or, in typed text, invalid UTF-8;
    - an output path or a {!constructor-Theme} holds all three quote characters
      (['"'], ['\''] and ['`']), which no VHS string can quote; a {!type'} can
      hold them, see {!to_string};
    - a duration is out of range, or the timeout of a {!val-wait} is [Ms 0];
    - a {!constructor-Line} or {!constructor-Screen} pattern is not valid Go
      regular expression syntax;
    - a key or a setting breaks the condition its documentation states;
    - a {!raw} line is not exactly one command VHS's parser accepts, ends inside
      a string, a regular expression or a JSON value, or lacks the argument of
      its command;
    - a {!raw} [Env] names a variable with an empty name or a name with [=],
      which no environment takes;
    - a {!raw} [Set] has a value its setting cannot parse as a duration, an
      integer, a float or a regular expression in Go's syntax;
    - {!constructor-Rows} or {!constructor-Columns} sizes the frame in the
      direction that {!constructor-Height} or {!constructor-Width} does, or asks
      for more rows or columns than 32 times the browser's viewport has pixels.
      VHS measures the cells in the viewport doubled up to five times, and fails
      with "could not fit" if the terminal there is still smaller. Fewer can
      still not fit with a large font, which [tape] does not check;
    - the {!constructor-Width} or {!constructor-Height} of a GIF output is
      beyond 65535 pixels, of an MP4 output beyond 16384 or of a WebM output
      beyond 65536, the sizes the encoders of ffmpeg 9 take. The same holds if
      the frame of such an output has more pixels than ffmpeg's image check
      takes, about 268 million, where (8 width + 1024) (height + 128) must be
      below 2{^ 31};
    - a {!raw} line is [Hide] or [Show] (use {!hidden}), [Set] for a setting of
      {!setting} other than [TypingSpeed] (use the setting), or [Output] (use
      [outputs]);
    - a {!raw} [Require] or [Set], other than [Set TypingSpeed], comes after an
      action that prints a line, where VHS ignores it;
    - every action outside {!hidden} is a [type' ""] or a {!raw} [Set],
      [Require], empty or comment line, which VHS does not record, so it records
      no frame and fails. VHS also fails if the actions it records all end
      within one frame, such as a lone [sleep (Ms 0)], which [tape] does not
      check. *)

(** {1:native_details Native recordings}

    The terminal has the size VHS gives it with JetBrains Mono, VHS's first
    font. [Width] or [Columns], [Height] or [Rows], [Font_size], [Padding] and
    the raw [Set Margin], [Set LetterSpacing] and [Set LineHeight] size it. VHS
    sizes it with the fonts of the machine it runs on, so it can show more rows
    than the image elsewhere. The image draws each cell on a grid, in a
    monospace font of the viewer's machine, in the colours of the [Theme] and
    those a program sets with OSC 4, which recolour the cells on the screen. The
    terminal keeps a colour as RGB, so a 256-colour or true colour equal to one
    of xterm's sixteen ANSI colours takes the theme's colour too. The terminal
    reads what the shell prints as xterm does. VHS's xterm.js departs from xterm
    in a few controls, such as an erasure in the last column or the row a cursor
    restore returns to. As in xterm.js, a character of no width, such as a
    combining mark or a zero-width joiner, joins the cell before it.

    Time is the tape's. A typed character, a key and a sleep last what the tape
    says, and a wait lasts what the program makes it last. Output shows at the
    time it arrives within the action that runs, rounded down to a frame of
    {!constructor-Framerate}, which lasts at least a millisecond. A tape whose
    program prints the same bytes within the same frames gives the same image,
    byte for byte. The time a command takes to print, including how the machine
    schedules it, changes the frames its output lands in. While a program has
    set synchronized output (DEC private mode 2026), the image keeps its last
    frame, and shows the screen once the program resets the mode or, as
    terminals do, 150ms after it set it; waits and the text output read the
    screen as it is. {!svg} and the asciicast recordings are drawn so too.

    The text output, [.txt], [.ascii] or [.test], is the one VHS writes. After
    each command that runs while recording, it holds the lines a
    {!constructor-Screen} wait reads, then a line of 80 U+2500 characters. VHS
    also adds a screen for each [Output] and [Set] line it reads before it sizes
    the terminal, from the text output's line on. Their rows depend on its
    browser's window, and the native output leaves them out.

    A [Screenshot] of [shot.svg] writes a still SVG image of the next frame it
    records to [shot.svg], without the cursor blinking, where VHS writes a
    [Screenshot] of [shot.png] as a PNG image.

    The {!constructor-Native} engine plays the actions of {!action} and the raw
    lines [Type], key, [Ctrl], [Alt], [Shift], [Sleep], [Wait], [Copy], [Paste],
    [Screenshot], [Source], [Env], [Require] and every [Set] but those below.
    [Paste] types the last [Copy]. The tape of a [Source] line is read relative
    to the [cwd] of {!record}, and is {!constructor-Invalid} where VHS refuses
    it and on a control character other than a tab or a carriage return, which
    {!val-tape} refuses in a {!raw} line. It does not play, and is
    {!constructor-Unsupported} on:
    - a {!shell} other than {!constructor-Bash}, {!constructor-Zsh} and
      {!constructor-Fish}, which start as in VHS;
    - a colour of a JSON [Theme] other than [#RGB], [#RGBA], [#RRGGBB],
      [#RRGGBBAA], [rgb()] or [rgba()], such as a colour name, which xterm.js
      reads with the browser.

    Its patterns match as Go's do, case folding, ASCII word boundaries and the
    Unicode classes of Go's tables included. After a line is submitted, by
    {!constructor-Enter} or the newline of a {!type'}, a {!constructor-Prompt}
    or {!constructor-Line} wait holds only once the terminal has started another
    line, or has changed the line so that the wait does not hold, or the line
    did not hold when it was submitted. It never holds on the prompt the command
    was typed at, however fast it is typed.

    [Set CursorBlink], [Set BorderRadius], [Set PlaybackSpeed] and
    [Set LoopOffset] change the animation, and [Set WindowBar] and
    [Set WindowBarSize] draw VHS's window bar. A frame of
    {!constructor-Framerate} plays for at least 10 milliseconds, so a
    [Set PlaybackSpeed] above 2 plays as 2 at the default 50 frames a second,
    and a recording at 100 frames a second or more plays at most in real time.
    For a reader who prefers reduced motion, the image shows its last screen,
    still, and the cursor does not blink. The image leaves out the format
    characters and the stray variation selectors that {!pp_error} removes. Its
    title is "Terminal recording". Its ids, classes and animations are named
    after a digest of the image, so that two images inlined in one HTML page
    keep their own, and each style rule selects one class, never an id. The
    default foreground, the background and the cursor are attributes, so its
    text stays readable where its style sheet is dropped. Unless its last screen
    is blank, its description, the text alternative, is the text that screen
    shows, without concealed text.

    The image of a [Set MarginFill], read relative to the [cwd] of {!record}, is
    embedded, stretched to the frame as ffmpeg stretches it. It is a regular
    file of at most 10 MiB, a symlink to one included, that starts with the
    signature of a PNG, JPEG, GIF, WebP or BMP image. The tape of a [Source]
    line is a regular file of at most 10 MiB. Anything else, such as a FIFO, a
    directory or a text file, is {!constructor-Invalid} before anything plays. A
    size beyond 100000 pixels, a terminal beyond 2000 cells a side, a
    [Set LetterSpacing] or [Set LineHeight] beyond 100000 and a
    [Set PlaybackSpeed] beyond 100000, or positive and below 0.001, are
    {!constructor-Unsupported}. *)
