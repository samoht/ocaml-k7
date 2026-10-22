(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let ( let* ) = Result.bind

(* As K7.record, k7 rec creates the missing directories of its output,
   below the nearest one that exists. *)
let check_output output =
  let rec nearest d =
    let up = Filename.dirname d in
    if Sys.file_exists d || up = d then d else nearest up
  in
  let dir = nearest (Filename.dirname output) in
  if Filename.extension output <> ".svg" then
    Fmt.error "%a: not .svg, the animated SVG image k7 rec writes" Quote.pp
      output
  else if not (Sys.is_directory dir) then
    Fmt.error "%a: %a is not a directory" Quote.pp output Quote.pp dir
  else Ok ()

let exit_code = function
  | `Exited n -> n
  | `Signaled s -> 128 + Sys.signal_to_int s

let record ?show ?delay ?stop stdenv ~settings ~env ~output argv =
  let* () = check_output output in
  let* svg, status =
    K7.run ~settings ~env ?show ?delay ?stop
      ~clock:(Eio.Stdenv.mono_clock stdenv)
      ~proc:(Eio.Stdenv.process_mgr stdenv)
      ~cwd:(Eio.Stdenv.cwd stdenv) argv
    |> Result.map_error (Fmt.to_to_string Flags.pp_error)
  in
  let file = Eio.Path.(Eio.Stdenv.fs stdenv / output) in
  match
    Option.iter
      (fun (dir, _) -> Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 dir)
      (Eio.Path.split file);
    Eio.Path.save ~create:(`Or_truncate 0o644) file svg
  with
  | () -> Ok status
  | exception Eio.Io (err, _) -> Fmt.error "%s: %a" output Eio.Exn.pp_err err

let run ?show ?delay stdenv ~settings ~env ~output argv =
  Flags.interruptible (fun stop ->
      record ?show ?delay ~stop stdenv ~settings ~env ~output argv)
  |> Flags.exit stdenv ~ok:exit_code

open Cmdliner

let output =
  let doc =
    "Write the animated SVG image to $(docv), which ends with .svg, creating \
     its missing directories."
  in
  Arg.(
    required & opt (some string) None & info [ "o"; "output" ] ~docv:"FILE" ~doc)

let show =
  let doc =
    "Show $(docv) after the $(b,\\$) of the image's first line instead of \
     $(i,COMMAND); an empty $(docv) shows no line."
  in
  Arg.(value & opt (some string) None & info [ "show" ] ~docv:"TEXT" ~doc)

(* Go's longest duration, in milliseconds. *)
let max_ms = 9_223_372_036_854

let milliseconds s =
  match
    if s <> "" && String.for_all Char.Ascii.is_digit s then int_of_string_opt s
    else None
  with
  | Some n when n <= max_ms -> Ok n
  | _ ->
      Fmt.error "%a is not a number of milliseconds from 0 to %d" Quote.pp s
        max_ms

let delay =
  let doc =
    "Show the command line alone for $(docv) milliseconds, as if typed, before \
     the program's output; 0 shows the output at once."
  in
  let ms = Arg.Conv.make ~docv:"MS" ~parser:milliseconds ~pp:Fmt.int () in
  Arg.(value & opt ms 500 & info [ "delay" ] ~docv:"MS" ~doc)

let command =
  let doc = "The program to record, then its arguments, after $(b,--)." in
  Arg.(non_empty & pos_all string [] & info [] ~docv:"COMMAND" ~doc)

let man =
  [
    `S Manpage.s_description;
    `P
      "$(cmd) runs $(i,COMMAND) on a pseudo-terminal, with no shell and \
       nothing typed, and writes what the terminal showed as an animated SVG \
       image, which plays in an HTML $(i,img) element. Each screen shows at \
       the time it arrived since the program started, and the last one holds \
       for a second after the program exits. The first line of the image shows \
       $(b,\\$) and $(i,COMMAND), each argument shell-quoted where needed, and \
       the program's output starts on the next line. Output beyond the last \
       row of the terminal scrolls its first rows away.";
    `P
      "Without $(b,--columns) and $(b,--rows), the terminal has as many \
       columns and rows as a frame of 1200 by 600 pixels holds, as in VHS, \
       with characters $(b,--font-size) pixels high: 75 columns and 16 rows at \
       the default 22 pixels, and 150 columns and 33 rows at 11. With them, \
       the image is as wide and as high as they need at that font size.";
    `P
      "The program is looked up in the $(b,PATH) it gets unless its name holds \
       a slash. It gets the environment of $(cmd) without $(b,COLUMNS) and \
       $(b,LINES), so that it reads the size of the terminal, with \
       $(b,TERM=xterm-256color), then the $(b,--env) variables. Its standard \
       input is the terminal, where nothing is typed, so a program that waits \
       for input waits until $(cmd) is interrupted.";
    `P
      "On SIGINT or SIGTERM, $(cmd) sends the program and its descendants \
       SIGTERM, then SIGKILL a second later, writes the image of what the \
       terminal showed until the program exited, and exits 130 or 143.";
    `S Manpage.s_examples;
    `Pre
      "$(cmd) -o ls.svg -- ls -l --color=always\n\
       $(cmd) -o columns.svg --columns 100 -- dune exec -- ./main.exe";
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
          "also when k7 itself fails, such as when the program cannot start or \
           the image cannot be written, after printing $(b,k7:) and the reason \
           on standard error. A program can exit with 123, 124 or 125 too; \
           only k7's own errors print that line.";
      info 130 ~doc:"also when $(cmd) is interrupted by SIGINT.";
      info 143 ~doc:"also when $(cmd) is interrupted by SIGTERM.";
      info cli_error ~doc:"on command line parsing errors.";
      info internal_error ~doc:"on unexpected internal errors (bugs).";
    ]

let cmd =
  let doc = "run a program on a pseudo-terminal and record its screens" in
  let run settings env show delay output argv =
    Eio_main.run @@ fun stdenv ->
    run ?show ~delay:(Ms delay) stdenv ~settings ~env ~output argv
  in
  Cmd.make
    (Cmd.info "rec" ~doc ~man ~exits)
    Term.(
      const run $ Flags.settings $ Flags.env $ show $ delay $ output $ command)
