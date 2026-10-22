(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Reading what a program writes to a pseudo-terminal until it exits. *)

val run :
  read:(bytes -> int -> int -> int) ->
  ready:(unit -> unit) ->
  pause:(unit -> unit) ->
  exited:(unit -> bool) ->
  feed:(bytes -> int -> unit) ->
  bytes ->
  unit
(** [run ~read ~ready ~pause ~exited ~feed buf] reads into [buf] with
    [read buf 0 (Bytes.length buf)] and gives the [n] bytes of each read to
    [feed buf n], until the program has exited and what it wrote is read. [read]
    raises [Unix.Unix_error (EAGAIN, _, _)] when there is nothing to read yet,
    and [ready ()] returns once there may be; it returns [0] or raises another
    [Unix.Unix_error] when the terminal is closed, which macOS can report while
    the program runs, and [pause ()] waits before the next try. [exited ()] is
    [true] once the program has exited. *)
