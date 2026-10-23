(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Cmdliner

let man =
  [
    `S Manpage.s_description;
    `P
      "$(tool) records terminal sessions as animated SVG images, without a \
       browser. For a GIF or a video, $(b,k7 play) runs Charm's $(b,vhs).";
    `P
      "Every subcommand takes $(b,--columns), $(b,--rows), $(b,--font-size), \
       $(b,--theme) and $(b,--framerate), and $(b,k7 play) and $(b,k7 rec) \
       take $(b,--env). Run $(b,k7) $(i,COMMAND) $(b,--help) for the manual of \
       a subcommand.";
    `S "PLAYING A TAPE OR A CRAM TEST";
    `P
      "$(b,k7 play) $(i,FILE) plays a tape in VHS's $(b,.tape) syntax to its \
       outputs, or to those of $(b,-o). $(b,k7 play) $(b,-o) $(i,OUTPUT) \
       $(i,FILE.t) types each command of the cram test $(i,FILE.t) into bash \
       and waits for the prompt before the next. k7 writes an $(b,.svg) output \
       itself, as an animated SVG image, and the text outputs $(b,.txt), \
       $(b,.ascii) and $(b,.test), as VHS does, and runs $(b,vhs) for the \
       other extensions.";
    `S "RECORDING A PROGRAM";
    `P
      "$(b,k7 rec) $(b,-o) $(i,FILE.svg) $(b,--) $(i,COMMAND) runs \
       $(i,COMMAND) on a pseudo-terminal, with no shell and nothing typed, and \
       writes its screens to $(i,FILE.svg), each at the time it arrived. It \
       exits with the exit code of $(i,COMMAND); an error of k7 itself prints \
       $(b,k7:) and the reason on standard error.";
    `S "DRAWING A RECORDING";
    `P
      "$(b,k7 cast) $(b,-o) $(i,FILE.svg) $(i,FILE.cast) draws a recording in \
       asciinema's asciicast v2 format, such as console writes under its \
       virtual clock, without running anything: the same recording gives the \
       same image, byte for byte.";
  ]

let exits =
  Cmd.Exit.
    [
      info ok ~doc:"on success.";
      info some_error
        ~doc:
          "when a subcommand fails, after printing $(b,k7:) and the reason on \
           standard error. $(b,k7 rec) exits with the code of its program \
           otherwise, which can be 123, 124 or 125 too.";
      info 130 ~doc:"when interrupted by SIGINT.";
      info 143 ~doc:"when interrupted by SIGTERM.";
      info cli_error ~doc:"on command line parsing errors.";
      info internal_error ~doc:"on unexpected internal errors (bugs).";
    ]

let cmd =
  let doc = "record terminal sessions as animated SVG images" in
  Cmd.group
    (Cmd.info "k7" ~version:"%%VERSION%%" ~doc ~man ~exits)
    [ Play_cmd.cmd; Rec_cmd.cmd; Cast_cmd.cmd ]
