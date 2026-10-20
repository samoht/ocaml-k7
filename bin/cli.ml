(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let ( let* ) = Result.bind

let tape ~setup ~output ~name src =
  let* cram = Cram.parse src |> Result.map_error (Fmt.str "%s: %s" name) in
  match
    (Cram.is_empty cram, List.find_opt (Fun.negate Cram.is_typeable) setup)
  with
  | true, _ -> Fmt.error "%s: no command" name
  | false, Some path ->
      Fmt.error "setup %S: holds invalid UTF-8 or a control character" path
  | false, None ->
      Vhs.tape ~settings:[ Shell Bash ] ~outputs:[ output ]
        (Cram.actions ~setup cram)

let load_error name = function
  | Eio.Fs.E (Not_found _) -> Fmt.str "%s: no such file" name
  | Eio.Fs.E (Permission_denied _) -> Fmt.str "%s: permission denied" name
  | err -> Fmt.str "%s: %a" name Eio.Exn.pp_err err

(* [write flow s] writes [s] to [flow]. It is [Error None] if the reader
   closed the pipe, and [Error (Some msg)] on another failure. *)
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

let record env ~print ~vhs ~setup ~output input =
  let* src =
    match Eio.Path.(load (Eio.Stdenv.fs env / input)) with
    | src -> Ok src
    | exception Eio.Io (err, _) -> Error (load_error input err)
  in
  let* tape = tape ~setup ~output ~name:input src in
  if print then
    (* Like a process killed by SIGPIPE, which Eio ignores, [--print] stops
       quietly when the reader goes away, as in [vhs-cram --print | head]. *)
    match write (Eio.Stdenv.stdout env) (Vhs.to_string tape) with
    | Ok () | Error None -> Ok ()
    | Error (Some msg) -> Fmt.error "standard output: %s" msg
  else
    Vhs.render ~vhs
      ~clock:(Eio.Stdenv.mono_clock env)
      ~proc:(Eio.Stdenv.process_mgr env)
      ~cwd:(Eio.Stdenv.cwd env) tape
    |> Result.map_error (Fmt.str "%a" Vhs.pp_error)

let run env ~print ~vhs ~setup ~output input =
  match record env ~print ~vhs ~setup ~output input with
  | Ok () -> 0
  | Error msg ->
      (* A closed standard error leaves the exit code to report. *)
      ignore (write (Eio.Stdenv.stderr env) (Fmt.str "vhs-cram: %s\n" msg));
      Cmdliner.Cmd.Exit.some_error

open Cmdliner

let output =
  let doc =
    "Write the recording to $(docv); VHS picks the format from its extension."
  in
  Arg.(
    required & opt (some string) None & info [ "o"; "output" ] ~docv:"FILE" ~doc)

let setup =
  let doc =
    "Source $(docv) before the first command, without recording it. Repeatable."
  in
  Arg.(value & opt_all non_dir_file [] & info [ "setup" ] ~docv:"FILE" ~doc)

let vhs =
  let doc = "The $(b,vhs) executable to run." in
  Arg.(value & opt string "vhs" & info [ "vhs" ] ~docv:"PATH" ~doc)

let print =
  let doc = "Print the tape on standard output instead of running $(b,vhs)." in
  Arg.(value & flag & info [ "print" ] ~doc)

let input =
  let doc = "The cram test to record." in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"CRAM" ~doc)

let man =
  [
    `S Manpage.s_description;
    `P
      "$(tname) types each $(b,\\$) command of the cram test $(i,CRAM) into a \
       VHS terminal running bash, waits for the prompt to come back, and \
       records the session with $(b,vhs). Expected output and exit codes are \
       ignored; other settings are the VHS defaults.";
    `P
      "Before the first command, and after the setup scripts, $(tname) makes \
       the last command of $(b,PROMPT_COMMAND) set $(b,PS1) to the VHS prompt \
       followed by a zero-width space, U+200B, which tells the prompt from \
       output that ends with $(b,>). The space does not show in a GIF or a \
       video, but the text outputs ($(b,.txt), $(b,.ascii) and $(b,.test)) \
       hold it after each prompt. The prompt hooks of the setup scripts still \
       run first; a command that replaces $(b,PROMPT_COMMAND) makes the next \
       wait time out.";
    `S Manpage.s_examples;
    `Pre
      "$(cmd) --output demo.gif demo.t\n\
       $(cmd) --print --output demo.gif demo.t   # show the tape";
  ]

let exits =
  Cmd.Exit.
    [
      info ok
        ~doc:
          "on success, also when the reader of $(b,--print) closes the pipe \
           early.";
      info some_error
        ~doc:
          "on a cram test that cannot be recorded, when $(b,vhs) cannot run or \
           fails, or when the tape of $(b,--print) cannot be written; the \
           reason is on standard error.";
      info cli_error ~doc:"on command line parsing errors.";
      info internal_error ~doc:"on unexpected internal errors (bugs).";
    ]

let cmd =
  let doc = "record a cram test as a VHS terminal recording" in
  let run print vhs setup output input =
    Eio_main.run @@ fun env -> run env ~print ~vhs ~setup ~output input
  in
  Cmd.make
    (Cmd.info "vhs-cram" ~doc ~man ~exits)
    Term.(const run $ print $ vhs $ setup $ output $ input)
