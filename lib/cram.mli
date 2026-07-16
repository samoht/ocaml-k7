(** Record a cram test as a VHS tape.

    The [$ ] command lines become keystrokes and the expected-output lines
    become the waits that keep the recording in step with the checked output, so
    one cram test drives both the check and the GIF. Setup scripts are sourced,
    hidden, before the first command, matching the cram stanza's
    [setup_scripts]. *)

type step = {
  command : string list;
      (** The command, one entry per input line. A single-line command is a
          singleton; continuation ([> ]) lines add further entries. The [$ ]
          prompt and [> ] markers are removed. *)
  output : string list;
      (** Expected output, with the two-space indent removed. *)
  exit_code : int option;
      (** The trailing [[N]] annotation, when the command exits non-zero. *)
}
(** One command block of a cram test. *)

type t = step list
(** A parsed cram test: its command blocks in order. *)

val parse : string -> t
(** [parse src] returns the command blocks of cram source [src] in order. Prose
    (lines not indented by two spaces) and blank lines are dropped; only [$ ]
    command blocks are returned. *)

val to_tape :
  ?settings:Value.setting list ->
  ?setup_scripts:string list ->
  ?pause:Value.duration ->
  ?final_pause:Value.duration ->
  output:string ->
  t ->
  Value.t
(** [to_tape ~output t] records the cram test [t] as a tape writing its GIF (or
    other format chosen by extension) to [output].

    Each command is typed and submitted. When a command has expected output, the
    tape waits for the last non-empty output line to appear on screen before
    pausing, so the recording tracks real output instead of guessing with fixed
    sleeps; the wait pattern is the output line with regexp metacharacters
    escaped. When a command has no expected output, the tape only pauses.

    [settings] become [Set] lines, emitted after [Output] and before the first
    command. [setup_scripts] are sourced, in order, inside a hidden block that
    also clears the screen, so their output never reaches the recording. [pause]
    is the reading pause after each command (default one second), while
    [final_pause] holds the last screen before exit (default three seconds). *)

val pp : t Fmt.t
(** [pp] formats a parsed cram test as its commands and expected output. *)
