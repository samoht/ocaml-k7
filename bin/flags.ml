(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Cmdliner

let decimal s =
  if s <> "" && String.for_all Char.Ascii.is_digit s then int_of_string_opt s
  else None

let positive what s =
  match decimal s with
  | Some n when n > 0 -> Ok n
  | _ -> Fmt.error "%a is not a positive %s" Quote.pp s what

let count ~docv what =
  Arg.Conv.make ~docv ~parser:(positive what) ~pp:Fmt.int ()

let settings =
  let cols =
    let doc =
      "Give the terminal $(docv) columns, and the image the width they need \
       (defaults to as many as a frame of 1200 pixels holds, as in VHS)."
    in
    Arg.(
      value
      & opt (some (count ~docv:"N" "number of columns")) None
      & info [ "cols" ] ~docv:"N" ~doc)
  and rows =
    let doc =
      "Give the terminal $(docv) rows, and the image the height they need \
       (defaults to as many as a frame of 600 pixels holds, as in VHS)."
    in
    Arg.(
      value
      & opt (some (count ~docv:"N" "number of rows")) None
      & info [ "rows" ] ~docv:"N" ~doc)
  and font_size =
    let doc = "Draw the text $(docv) pixels high (defaults to 22)." in
    Arg.(
      value
      & opt (some (count ~docv:"PX" "font size")) None
      & info [ "font-size" ] ~docv:"PX" ~doc)
  and theme =
    let doc =
      "Draw the terminal in the colours of the VHS theme $(docv), as $(b,vhs \
       themes) lists them, or of a theme in JSON."
    in
    Arg.(value & opt (some string) None & info [ "theme" ] ~docv:"NAME" ~doc)
  in
  let make cols rows font_size theme =
    List.filter_map Fun.id
      [
        Option.map (fun n -> K7.Columns n) cols;
        Option.map (fun n -> K7.Rows n) rows;
        Option.map (fun n -> K7.Font_size n) font_size;
        Option.map (fun t -> K7.Theme t) theme;
      ]
  in
  Term.(const make $ cols $ rows $ font_size $ theme)

let binding s =
  match String.index_opt s '=' with
  | Some i when i > 0 ->
      Ok (String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1))
  | _ -> Fmt.error "%a is not VAR=VALUE" Quote.pp s

let env =
  let doc =
    "Set the variable $(i,VAR) to $(i,VALUE) in the environment of the \
     recorded program. Repeatable."
  in
  let pp ppf (k, v) = Fmt.pf ppf "%s=%s" k v in
  Arg.(
    value
    & opt_all (Arg.Conv.make ~docv:"VAR=VALUE" ~parser:binding ~pp ()) []
    & info [ "env" ] ~docv:"VAR=VALUE" ~doc)

(* Go's longest duration, in seconds. *)
let max_seconds = 9_223_372_036

let seconds s =
  let too_large () =
    Fmt.error "%a is too large: a wait lasts at most %d seconds" Quote.pp s
      max_seconds
  in
  match decimal s with
  | Some n when n > 0 && n <= max_seconds -> Ok n
  | Some n when n > 0 -> too_large ()
  (* Digits [int_of_string] cannot read are beyond [max_int]. *)
  | None when s <> "" && String.for_all Char.Ascii.is_digit s -> too_large ()
  | _ -> Fmt.error "%a is not a positive number of seconds" Quote.pp s

let wait_timeout =
  let doc =
    "Fail the recording when a command has not given the prompt back after \
     $(docv) seconds (defaults to VHS's 15)."
  in
  Arg.(
    value
    & opt
        (some (Arg.Conv.make ~docv:"SECONDS" ~parser:seconds ~pp:Fmt.int ()))
        None
    & info [ "wait-timeout" ] ~docv:"SECONDS" ~doc)

let vhs =
  let doc = "The $(b,vhs) executable to run." in
  Arg.(value & opt string "vhs" & info [ "vhs" ] ~docv:"PATH" ~doc)

let print =
  let doc =
    "Print the tape on standard output instead of recording it. An $(b,.svg) \
     output, which VHS cannot write, has no $(b,Output) line."
  in
  Arg.(value & flag & info [ "print" ] ~doc)

let exits =
  Cmd.Exit.
    [
      info ok
        ~doc:
          "on success, also when the reader of $(b,--print) closes the pipe \
           early.";
      info some_error
        ~doc:
          "when the recording fails, when $(b,vhs) cannot run or fails, or \
           when the tape of $(b,--print) cannot be written; the reason is on \
           standard error.";
      info 130
        ~doc:"when interrupted by SIGINT, once what was recorded is written.";
      info 143 ~doc:"when interrupted by SIGTERM, likewise.";
      info cli_error ~doc:"on command line parsing errors.";
      info internal_error ~doc:"on unexpected internal errors (bugs).";
    ]

let max_load = 10 * 1024 * 1024

let load env path =
  let read flow =
    Eio.Buf_read.parse ~max_size:max_load Eio.Buf_read.take_all flow
  in
  match Eio.Path.(with_open_in (Eio.Stdenv.fs env / path)) read with
  | Ok src -> Ok src
  | Error (`Msg _) -> Fmt.error "%s: larger than 10 MiB" path
  | exception Eio.Io (Eio.Fs.E (Not_found _), _) ->
      Fmt.error "%s: no such file" path
  | exception Eio.Io (Eio.Fs.E (Permission_denied _), _) ->
      Fmt.error "%s: permission denied" path
  | exception Eio.Io (err, _) -> Fmt.error "%s: %a" path Eio.Exn.pp_err err

let write flow s =
  match Eio.Flow.copy_string s flow with
  | () -> Ok ()
  | exception
      Eio.Io
        (Eio.Net.E (Connection_reset (Eio_unix.Unix_error (EPIPE, _, _))), _) ->
      Error None
  | exception Eio.Io (Eio.Exn.X (Eio_unix.Unix_error (code, _, _)), _) ->
      Error (Some (Unix.error_message code))
  | exception Eio.Io (err, _) ->
      Error (Fmt.kstr Option.some "%a" Eio.Exn.pp_err err)

(* As under SIGPIPE, which Eio ignores: [k7 cram --print | head] is quiet. *)
let print_tape env t =
  match write (Eio.Stdenv.stdout env) (K7.to_string t) with
  | Ok () | Error None -> Ok ()
  | Error (Some msg) -> Fmt.error "standard output: %s" msg

let shown s = Fmt.str "%a" K7.pp_error (Io s)

let replace ~sub ~by s =
  let n = String.length sub in
  let b = Buffer.create (String.length s) in
  let rec go i =
    if i > String.length s - n then
      Buffer.add_substring b s i (String.length s - i)
    else if String.sub s i n = sub then (
      Buffer.add_string b by;
      go (i + n))
    else (
      Buffer.add_char b s.[i];
      go (i + 1))
  in
  go 0;
  Buffer.contents b

(* The library names its functions; the CLI names what runs. *)
let library_names =
  [
    ("which K7.record records", "which k7 records without vhs");
    ("which K7.record takes", "which k7 records without vhs");
    ("which K7.render records", "which vhs records");
    ("which K7.render takes", "which vhs records");
  ]

let pp_error ppf = function
  | K7.Timeout { wait; line; last } ->
      Fmt.pf ppf
        "timed out waiting for line %d, %s%s; give the wait a longer timeout, \
         as in Wait@30s, or the tape a Set WaitTimeout"
        line (shown wait)
        (if last = "" then "" else "; last value was: " ^ shown last)
  | K7.Unsupported _ as e ->
      Fmt.string ppf
        (List.fold_left
           (fun s (sub, by) -> replace ~sub ~by s)
           (Fmt.str "%a" K7.pp_error e)
           library_names)
  | e -> K7.pp_error ppf e

let pp_play_error ~vhs ppf = function
  | K7.Executable_not_found exe as e when exe = vhs ->
      Fmt.pf ppf
        "%a; a GIF or a video needs Charm's VHS, see \
         https://github.com/charmbracelet/vhs#installation, and an .svg output \
         does not"
        K7.pp_error e
  | e -> pp_error ppf e

let interruptible f =
  let received = Atomic.make None and signalled = Eio.Condition.create () in
  let handle s =
    Atomic.set received (Some s);
    Eio.Condition.broadcast signalled
  in
  let signals = [ Sys.sigint; Sys.sigterm ] in
  let previous =
    List.map (fun s -> Sys.signal s (Sys.Signal_handle handle)) signals
  in
  Fun.protect ~finally:(fun () -> List.iter2 Sys.set_signal signals previous)
  @@ fun () ->
  let stop, stopped = Eio.Promise.create () in
  let result =
    Eio.Fiber.first
      (fun () -> f stop)
      (fun () ->
        ignore
          (Eio.Condition.loop_no_mutex signalled (fun () -> Atomic.get received));
        Eio.Promise.resolve stopped ();
        Eio.Fiber.await_cancel ())
  in
  (result, Atomic.get received)

let fail env msg =
  (* With standard error closed, the exit code still reports. *)
  ignore (write (Eio.Stdenv.stderr env) (Fmt.str "k7: %s\n" msg));
  Cmd.Exit.some_error

let exit env ~ok = function
  | Ok x, None -> ok x
  | Error msg, None -> fail env msg
  | Ok _, Some s -> 128 + Sys.signal_to_int s
  | Error msg, Some s ->
      ignore (fail env msg);
      128 + Sys.signal_to_int s
