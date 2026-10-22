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
       browser. For a GIF or a video, $(b,k7 cram) and $(b,k7 tape) run \
       Charm's $(b,vhs).";
    `P
      "Every subcommand takes $(b,--cols), $(b,--rows), $(b,--font-size), \
       $(b,--theme) and $(b,--env). Run $(b,k7) $(i,COMMAND) $(b,--help) for \
       the manual of a subcommand.";
    `S "RECORDING A CRAM TEST";
    `P
      "$(b,k7 cram) $(b,-o) $(i,FILE) $(i,CRAM) types each command of the cram \
       test $(i,CRAM) into bash and waits for the prompt before the next. It \
       writes the session to $(i,FILE) as an animated SVG image for $(b,.svg), \
       as the text VHS writes for $(b,.txt), $(b,.ascii) and $(b,.test), and \
       through $(b,vhs) for the other extensions.";
    `S "RECORDING A PROGRAM";
    `P
      "$(b,k7 run) $(b,-o) $(i,FILE.svg) $(b,--) $(i,COMMAND) runs \
       $(i,COMMAND) on a pseudo-terminal, with no shell and nothing typed, and \
       writes its screens to $(i,FILE.svg), each at the time it arrived. It \
       exits with the exit code of $(i,COMMAND).";
    `S "RECORDING A TAPE";
    `P
      "$(b,k7 tape) $(i,TAPE) records a tape in VHS's $(b,.tape) syntax to its \
       outputs, or to those of $(b,-o). It records $(b,.svg) and the text \
       outputs itself, and runs $(b,vhs) for the others.";
  ]

let cmd =
  let doc = "record terminal sessions as animated SVG images" in
  Cmd.group
    (Cmd.info "k7" ~doc ~man ~exits:Flags.exits)
    [ Cram_cmd.cmd; Run_cmd.cmd; Tape_cmd.cmd ]
