(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let ( let* ) = Result.bind

(* The image's directory is checked before the program runs. *)
let check_output output =
  let dir = Filename.dirname output in
  if Filename.extension output <> ".svg" then
    Fmt.error "%a: not .svg, the animated SVG image k7 run writes" Quote.pp
      output
  else if not (Sys.file_exists dir && Sys.is_directory dir) then
    Fmt.error "%a: no directory %a" Quote.pp output Quote.pp dir
  else Ok ()

let exit_code = function
  | `Exited n -> n
  | `Signaled s -> 128 + Sys.signal_to_int s

let record stdenv ~settings ~env ~output argv =
  let* () = check_output output in
  let* svg, status =
    Vhs.run ~settings ~env
      ~clock:(Eio.Stdenv.mono_clock stdenv)
      ~proc:(Eio.Stdenv.process_mgr stdenv)
      ~cwd:(Eio.Stdenv.cwd stdenv) argv
    |> Result.map_error (Fmt.to_to_string Vhs.pp_error)
  in
  match
    Eio.Path.(save ~create:(`Or_truncate 0o644) (Eio.Stdenv.fs stdenv / output))
      svg
  with
  | () -> Ok status
  | exception Eio.Io (err, _) -> Fmt.error "%s: %a" output Eio.Exn.pp_err err

let run stdenv ~settings ~env ~output argv =
  match record stdenv ~settings ~env ~output argv with
  | Ok status -> exit_code status
  | Error msg -> Flags.fail stdenv msg

open Cmdliner

let output =
  let doc = "Write the animated SVG image to $(docv), which ends with .svg." in
  Arg.(
    required & opt (some string) None & info [ "o"; "output" ] ~docv:"FILE" ~doc)

let command =
  let doc = "The program to record, then its arguments, after $(b,--)." in
  Arg.(non_empty & pos_all string [] & info [] ~docv:"COMMAND" ~doc)

let man =
  [
    `S Manpage.s_description;
    `P
      "$(tname) runs $(i,COMMAND) on a pseudo-terminal, with no shell and \
       nothing typed, and writes what the terminal showed as an animated SVG \
       image, which plays in an HTML $(i,img) element. Each screen shows at \
       the time it arrived since the program started, and the last one holds \
       for a second after the program exits. The image shows only the \
       program's own output.";
    `P
      "The program is looked up in $(b,PATH) unless its name holds a slash. It \
       gets the environment of $(tname), with $(b,TERM=xterm-256color), then \
       the $(b,--env) variables. Its standard input is the terminal, where \
       nothing is typed: a program that waits for input waits until $(tname) \
       is interrupted.";
    `S Manpage.s_examples;
    `Pre
      "$(cmd) -o ls.svg -- ls -l --color=always\n\
       $(cmd) -o columns.svg --cols 100 -- dune exec -- ./main.exe";
  ]

let exits =
  Cmd.Exit.
    [
      info 0 ~max:255
        ~doc:
          "the exit code of $(i,COMMAND), once the image is written, or 128 \
           plus the number of the signal that killed it.";
      info some_error
        ~doc:
          "also when the recording fails or the image cannot be written; the \
           reason is on standard error.";
      info cli_error ~doc:"on command line parsing errors.";
      info internal_error ~doc:"on unexpected internal errors (bugs).";
    ]

let cmd =
  let doc = "record a program" in
  let run settings env output argv =
    Eio_main.run @@ fun stdenv -> run stdenv ~settings ~env ~output argv
  in
  Cmd.make
    (Cmd.info "run" ~doc ~man ~exits)
    Term.(const run $ Flags.settings $ Flags.env $ output $ command)
