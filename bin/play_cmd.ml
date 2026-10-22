(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type kind = Tape | Cram

(* Ignoring case, as macOS's disk does. *)
let kind input =
  match String.lowercase_ascii (Filename.extension input) with
  | ".tape" -> Ok Tape
  | ".t" -> Ok Cram
  | _ -> Fmt.error "%s: neither a tape (.tape) nor a cram test (.t)" input

let cram_only ~setup ~dir ~wait_timeout =
  List.find_map
    (fun (given, flag) -> if given then Some flag else None)
    [
      (setup <> [], "--setup");
      (Option.is_some dir, "--dir");
      (Option.is_some wait_timeout, "--wait-timeout");
    ]

let run stdenv ?engine ~dry_run ~print ~vhs ~settings ~env ~outputs ~setup ~dir
    ~wait_timeout input =
  match kind input with
  | Error msg -> Flags.fail stdenv msg
  | Ok _ when dry_run && print ->
      Flags.fail stdenv
        "--dry-run checks a file and --print prints its tape; give one"
  | Ok Cram ->
      Play_cram.run stdenv ?engine ~dry_run ~print ~vhs ~setup ?dir ~settings
        ~env ?wait_timeout_s:wait_timeout ~outputs input
  | Ok Tape -> (
      match cram_only ~setup ~dir ~wait_timeout with
      | Some flag ->
          Flags.fail stdenv
            (Fmt.str "%s: %s applies to a cram test (.t), not to a tape" input
               flag)
      | None ->
          Play_tape.run stdenv ?engine ~dry_run ~print ~vhs ~settings ~env
            ~outputs input)

open Cmdliner

let outputs =
  let doc =
    "Write the recording to $(docv), in the format of its extension, in place \
     of the $(b,Output) lines of a tape. Repeatable. $(cmd) itself records an \
     $(b,.svg) image and the text outputs $(b,.txt), $(b,.ascii) and \
     $(b,.test), and $(b,vhs) records the others."
  in
  Arg.(value & opt_all string [] & info [ "o"; "output" ] ~docv:"FILE" ~doc)

let engine =
  let doc =
    "Record with $(docv): $(b,native), k7 itself, or $(b,vhs), in place of the \
     one that writes the outputs."
  in
  let engines = [ ("native", K7.Native); ("vhs", K7.Vhs) ] in
  Arg.(
    value
    & opt (some (enum engines)) None
    & info [ "engine" ] ~docv:"ENGINE" ~doc)

let dry_run =
  let doc =
    "Check $(i,FILE), the options and the outputs as a recording would, \
     without recording anything, and without starting a shell or $(b,vhs). It \
     does not go with $(b,--print)."
  in
  Arg.(value & flag & info [ "dry-run" ] ~doc)

let setup =
  let doc =
    "Source $(docv) before the first command of a cram test, without recording \
     it. Repeatable."
  in
  Arg.(value & opt_all non_dir_file [] & info [ "setup" ] ~docv:"FILE" ~doc)

let dir =
  let doc =
    "Run the commands of a cram test in $(docv) instead of a new empty \
     temporary directory."
  in
  Arg.(value & opt (some dir) None & info [ "dir" ] ~docv:"DIR" ~doc)

let input =
  let doc =
    "The file to play: a tape in VHS's $(b,.tape) syntax, or a cram test, a \
     $(b,.t) file."
  in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"FILE" ~doc)

let man =
  [
    `S Manpage.s_description;
    `P
      "$(cmd) plays $(i,FILE) and records it: a tape in VHS's $(b,.tape) \
       syntax, or a cram test, the $(b,.t) format that dune runs, whose \
       commands it types into bash.";
    `P
      "When every output is an $(b,.svg) image or a text output ($(b,.txt), \
       $(b,.ascii) or $(b,.test)), $(cmd) plays $(i,FILE) itself, in a shell \
       on a pseudo-terminal, and draws its screens as an animated SVG image, \
       which plays in an HTML $(i,img) element, or writes their text as VHS \
       does. Otherwise $(b,vhs), which needs $(b,ttyd), $(b,ffmpeg) and a \
       browser, records it. A file cannot ask for both an $(b,.svg) image and \
       an output only $(b,vhs) writes. $(b,--engine) picks the recorder \
       instead, such as $(b,vhs) for a text output of a tape whose shell \
       $(cmd) does not start itself.";
    `P
      "$(b,--dry-run) checks $(i,FILE) as $(b,vhs validate) does, and also \
       that the chosen recorder writes the outputs, with the other options \
       applied, in directories it can write in; it exits 0 when they pass.";
    `P
      "On SIGINT or SIGTERM, $(cmd) plays no more actions, writes what it \
       recorded so far, and exits 130 or 143.";
    `S "TAPES";
    `P
      "The $(b,Output) lines of a tape, wherever they are, are its outputs, \
       which $(b,-o) replaces. The $(b,Set) lines of a tape before its first \
       action are its settings, which $(b,--columns), $(b,--rows), \
       $(b,--font-size), $(b,--theme) and $(b,--framerate) replace. \
       $(b,--setup), $(b,--dir) and $(b,--wait-timeout) apply to cram tests \
       only.";
    `S "CRAM TESTS";
    `P
      "$(cmd) types each $(b,\\$) command of a cram test into a terminal \
       running bash and waits for the prompt to come back before the next. It \
       ignores the expected output and exit codes. As dune does, it runs the \
       commands in a new empty temporary directory, which it removes \
       afterwards, unless $(b,--dir) names another one. A cram test needs \
       $(b,-o).";
    `P
      "The prompt hooks of the setup scripts still run first, and a command \
       that replaces $(b,PROMPT_COMMAND) makes the next wait time out. So does \
       a command that leaves bash waiting for more input, such as an \
       unterminated quote or here-document, and $(cmd) then names that \
       command. In a native recording, a wait that times out names the command \
       whose prompt did not come back, and a shell that exits ends the \
       recording at once.";
    `P
      "Before the first command, and after the setup scripts, $(cmd) makes the \
       last command of $(b,PROMPT_COMMAND) set $(b,PS1) to the VHS prompt \
       followed by a mark. The mark is sixteen zero-width characters, U+200B \
       and U+2060, that spell a digest of the setup scripts and the commands. \
       It tells the prompt from output, even output that ends with $(b,>) or \
       with a zero-width space, and does not show in a GIF or a video. The \
       text outputs hold it after each prompt.";
    `S Manpage.s_examples;
    `Pre
      "$(cmd) demo.tape\n\
       $(cmd) --columns 100 -o demo.svg demo.tape\n\
       $(cmd) -o demo.svg demo.t\n\
       $(cmd) -o demo.gif demo.t\n\
       $(cmd) --print -o demo.gif demo.t   # show the tape";
  ]

let cmd =
  let doc = "play a tape or a cram test and record it" in
  let run engine dry_run print vhs settings env outputs setup dir wait_timeout
      input =
    Eio_main.run @@ fun stdenv ->
    run stdenv ?engine ~dry_run ~print ~vhs ~settings ~env ~outputs ~setup ~dir
      ~wait_timeout input
  in
  Cmd.make
    (Cmd.info "play" ~doc ~man ~exits:Flags.exits)
    Term.(
      const run $ engine $ dry_run $ Flags.print $ Flags.vhs $ Flags.settings
      $ Flags.env $ outputs $ setup $ dir $ Flags.wait_timeout $ input)
