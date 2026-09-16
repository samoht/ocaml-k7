(** Record a cram test as a VHS tape.

    The [$ ] command lines become keystrokes and the expected-output lines
    become the waits that keep the recording in step with the checked output.
    Setup scripts are sourced, hidden, before the first command. *)

type step = {
  command : string list;
      (** One entry per input line; continuation ([> ]) lines add entries, with
          the [$ ] prompt and [> ] markers removed. *)
  output : string list;
      (** Expected output, with the two-space indent removed. *)
  exit_code : int option;
      (** The trailing [[N]] annotation, when the command exits non-zero. *)
}
(** One command block of a cram test. *)

type t = step list
(** A parsed cram test: its command blocks in order. *)

val parse : string -> t
(** [parse src] is the command blocks of cram source [src] in order. Prose and
    blank lines are dropped. *)

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

    Each command is typed and submitted; when it has expected output, the tape
    waits for the last non-empty output line before pausing, with regexp
    metacharacters escaped. [settings] become [Set] lines and [setup_scripts]
    are sourced in order inside a hidden clearing block. [pause] is the reading
    pause after each command (default one second) and [final_pause] holds the
    last screen before exit (default three seconds). *)

val pp : t Fmt.t
(** [pp] formats a parsed cram test as its commands and expected output. *)
