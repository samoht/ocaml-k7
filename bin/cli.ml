(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Cmdliner

let man =
  [
    `S Manpage.s_description;
    `P
      "$(tool) records terminal sessions and draws them as animated SVG \
       images, without a browser. $(b,k7 rec) records any program, as an \
       image, an asciicast recording, the text of its last screen or a cram \
       test. $(b,k7 play) draws asciicast recordings, its own or asciinema's, \
       and plays cram tests and the tapes of Charm's VHS; for a GIF or a video \
       it runs $(b,vhs).";
    `P
      "Both subcommands take $(b,--columns), $(b,--rows), $(b,--font-size), \
       $(b,--theme) and $(b,--framerate), and $(b,--env) for what they run. \
       Run $(b,k7) $(i,COMMAND) $(b,--help) for the manual of a subcommand.";
    `S "PLAYING A FILE";
    `P
      "$(b,k7 play) $(i,FILE) plays a file by the kind its extension names. A \
       tape in VHS's $(b,.tape) syntax plays to its outputs, or to those of \
       $(b,-o). $(b,k7 play) $(b,-o) $(i,OUTPUT) $(i,FILE.t) types each \
       command of the cram test $(i,FILE.t) into bash and waits for the prompt \
       before the next. k7 writes an $(b,.svg) output itself, as an animated \
       SVG image, and the text outputs $(b,.txt), $(b,.ascii) and $(b,.test), \
       as VHS does, and runs $(b,vhs) for the other extensions.";
    `P
      "$(b,k7 play) $(b,-o) $(i,FILE.svg) $(i,FILE.cast) draws a recording in \
       asciinema's asciicast v2 or v3 format, such as asciinema or console \
       writes, without running anything: the same recording gives the same \
       image, byte for byte.";
    `S "RECORDING A PROGRAM";
    `P
      "$(b,k7 rec) $(b,-o) $(i,FILE.svg) $(b,--) $(i,COMMAND) runs \
       $(i,COMMAND) on a pseudo-terminal, with no shell and nothing typed, and \
       writes its screens to $(i,FILE.svg), each at the time it arrived. A \
       $(b,.cast) output keeps the recording, for $(b,k7 play) to draw later, \
       a $(b,.txt) output is the text of the last screen, and a $(b,.t) output \
       is a cram test of $(i,COMMAND) as dune runs it. It exits with the exit \
       code of $(i,COMMAND); an error of k7 itself prints $(b,k7:) and the \
       reason on standard error.";
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
  let doc = "record terminal sessions and draw them as animated SVG images" in
  Cmd.group
    (Cmd.info "k7" ~version:"%%VERSION%%" ~doc ~man ~exits)
    [ Play_cmd.cmd; Rec_cmd.cmd ]
