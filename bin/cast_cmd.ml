(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let ( let* ) = Result.bind

let draw ?show ?delay stdenv ~settings ~output input =
  let* () = Flags.check_svg ~cmd:"k7 cast" output in
  let* src = Flags.load stdenv input in
  let error e = Fmt.str "%s: %a" input K7.pp_error e in
  let* svg =
    K7.svg_of_cast ~settings ?show ?delay src |> Result.map_error error
  in
  Flags.save stdenv output svg

let run ?show ?delay stdenv ~settings ~output input =
  match draw ?show ?delay stdenv ~settings ~output input with
  | Ok () -> 0
  | Error msg -> Flags.fail stdenv msg

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
    "Show a first line with $(b,\\$) and $(docv), as $(b,k7 rec) does, before \
     the recording."
  in
  Arg.(value & opt (some string) None & info [ "show" ] ~docv:"TEXT" ~doc)

let delay =
  let doc =
    "Show the line of $(b,--show) alone for $(docv) milliseconds, as if typed, \
     before the recording; 0 starts the recording at once."
  in
  Arg.(value & opt Flags.milliseconds 500 & info [ "delay" ] ~docv:"MS" ~doc)

let input =
  let doc = "The recording to draw, in asciicast v2." in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"FILE" ~doc)

let settings =
  Flags.settings_for ~columns:"the $(b,width) of the header of $(i,FILE)"
    ~rows:"the $(b,height) of the header of $(i,FILE)"

let man =
  [
    `S Manpage.s_description;
    `P
      "$(cmd) draws the terminal recording $(i,FILE), in asciinema's asciicast \
       v2 format, as an animated SVG image, which plays in an HTML $(i,img) \
       element. It runs nothing: each output event shows at its time, rounded \
       to the millisecond, so the same $(i,FILE) gives the same image, byte \
       for byte, and the last screen holds for a second.";
    `P
      "The terminal has the $(b,width) and $(b,height) of the header of \
       $(i,FILE), which $(b,--columns) and $(b,--rows) replace. $(cmd) ignores \
       the input events, the markers and the resizes of $(i,FILE), and the \
       other members of its header.";
    `P
      "A program that uses console writes such a recording, at the times of \
       its virtual clock, with $(b,CONSOLE_DETERMINISTIC_CLOCK=1) and \
       $(b,CONSOLE_CAST=)$(i,FILE).";
    `S Manpage.s_examples;
    `Pre
      "$(cmd) -o demo.svg demo.cast\n\
       $(cmd) --show 'dune exec -- ./main.exe' -o main.svg main.cast";
  ]

let exits =
  Cmd.Exit.
    [
      info ok ~doc:"on success.";
      info some_error
        ~doc:
          "when $(i,FILE) cannot be read or is not asciicast v2, or the image \
           cannot be written; the reason is on standard error.";
      info cli_error ~doc:"on command line parsing errors.";
      info internal_error ~doc:"on unexpected internal errors (bugs).";
    ]

let cmd =
  let doc = "draw an asciicast recording as an animated SVG image" in
  let run settings show delay output input =
    Eio_main.run @@ fun stdenv ->
    run ?show ~delay:(Ms delay) stdenv ~settings ~output input
  in
  Cmd.make
    (Cmd.info "cast" ~doc ~man ~exits)
    Term.(const run $ settings $ show $ delay $ output $ input)
