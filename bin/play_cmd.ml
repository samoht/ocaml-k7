(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type kind = Tape | Cram | Cast

(* Ignoring case, as macOS's disk does. *)
let kind input =
  match String.lowercase_ascii (Filename.extension input) with
  | ".tape" -> Ok Tape
  | ".t" -> Ok Cram
  | ".cast" -> Ok Cast
  | _ ->
      Fmt.error
        "%s: not a tape (.tape), a cram test (.t) or an asciicast recording \
         (.cast)"
        input

let name = function
  | Tape -> "a tape"
  | Cram -> "a cram test"
  | Cast -> "an asciicast recording"

let names = function
  | [ Cram ] -> "a cram test (.t)"
  | [ Cast ] -> "an asciicast recording (.cast)"
  | _ -> "a tape or a cram test"

let misplaced kind ~engine ~print ~vhs ~env ~setup ~dir ~wait_timeout ~show
    ~delay =
  List.find_map
    (fun (given, flag, kinds) ->
      if given && not (List.mem kind kinds) then Some (flag, kinds) else None)
    [
      (Option.is_some engine, "--engine", [ Tape; Cram ]);
      (env <> [], "--env", [ Tape; Cram ]);
      (print, "--print", [ Tape; Cram ]);
      (Option.is_some vhs, "--vhs", [ Tape; Cram ]);
      (setup <> [], "--setup", [ Cram ]);
      (Option.is_some dir, "--dir", [ Cram ]);
      (Option.is_some wait_timeout, "--wait-timeout", [ Cram ]);
      (show <> [], "--show", [ Cast ]);
      (Option.is_some delay, "--delay", [ Cast ]);
    ]

let is_cast input = match kind input with Ok Cast -> true | _ -> false

(* Several files play one after the other only as recordings. *)
let kinds = function
  | [ input ] -> kind input
  | inputs -> (
      match List.find_opt (Fun.negate is_cast) inputs with
      | Some input ->
          Fmt.error
            "%s: several files play only as asciicast recordings (.cast)" input
      | None -> Ok Cast)

(* Two seconds but for a tape, whose sleeps are what its author wrote. *)
let idle_limit kind idle =
  let default = match kind with Tape -> None | Cram | Cast -> Some 2000 in
  Option.map (fun ms -> K7.Ms ms) (Option.value idle ~default)

let idle_time_limit input idle =
  idle_limit (Result.value (kind input) ~default:Cast) idle

let run stdenv ?engine ~dry_run ~print ?vhs ~settings ~env ~outputs ~setup ~dir
    ~wait_timeout ?(show = []) ?delay ?idle inputs =
  let input = String.concat " " inputs in
  let play kind =
    let idle_time_limit = idle_limit kind idle in
    match kind with
    | Cram ->
        Play_cram.run stdenv ?engine ?idle_time_limit ~dry_run ~print
          ~vhs:(Option.value vhs ~default:"vhs")
          ~setup ?dir ~settings ~env ?wait_timeout_s:wait_timeout ~outputs input
    | Tape ->
        Play_tape.run stdenv ?engine ?idle_time_limit ~dry_run ~print
          ~vhs:(Option.value vhs ~default:"vhs")
          ~settings ~env ~outputs input
    | Cast ->
        Play_cast.run stdenv ~dry_run ~settings ~show ?delay ?idle_time_limit
          ~outputs inputs
  in
  match kinds inputs with
  | Error msg -> Flags.fail stdenv msg
  | Ok _ when dry_run && print ->
      Flags.fail stdenv
        "--dry-run checks a file and --print prints its tape; give one"
  | Ok kind -> (
      match
        misplaced kind ~engine ~print ~vhs ~env ~setup ~dir ~wait_timeout ~show
          ~delay
      with
      | Some (flag, kinds) ->
          Flags.fail stdenv
            (Fmt.str "%s: %s applies to %s, not to %s" input flag (names kinds)
               (name kind))
      | None -> play kind)

open Cmdliner

let outputs =
  let doc =
    "Write the recording to $(docv), in the format of its extension, in place \
     of the $(b,Output) lines of a tape. Repeatable. $(cmd) itself records an \
     $(b,.svg) image and the text outputs $(b,.txt), $(b,.ascii) and \
     $(b,.test), and $(b,vhs) records the others. An asciicast recording takes \
     $(b,.svg) images and $(b,.txt) for its last screen."
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
     without recording or writing anything, and without starting a shell or \
     $(b,vhs). It does not go with $(b,--print)."
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

let show =
  let doc =
    "Show a first line with $(b,\\$) and $(docv), as $(b,k7 rec) does, before \
     an asciicast recording, in place of the $(b,command) of its header; an \
     empty $(docv) shows no line. Given once per recording, the $(i,n)th \
     applies to the $(i,n)th recording."
  in
  Arg.(value & opt_all string [] & info [ "show" ] ~docv:"TEXT" ~doc)

let delay =
  let doc =
    "Show the line of $(b,--show) alone for $(docv) milliseconds (defaults to \
     500), as if typed, before an asciicast recording; 0 starts the recording \
     at once."
  in
  Arg.(
    value
    & opt (some Flags.milliseconds) None
    & info [ "delay" ] ~docv:"MS" ~doc)

let settings =
  Flags.settings_for
    ~columns:
      "the size of the header of an asciicast recording, and otherwise as many \
       as a frame of 1200 pixels holds, as in VHS: 75 at the default font size"
    ~rows:
      "the size of the header of an asciicast recording, and otherwise as many \
       as a frame of 600 pixels holds, as in VHS: 16 at the default font size"

let input =
  let doc =
    "The file to play: a tape ($(b,.tape)), a cram test ($(b,.t)) or an \
     asciicast recording ($(b,.cast)). Several asciicast recordings play one \
     after the other in one image."
  in
  Arg.(non_empty & pos_all non_dir_file [] & info [] ~docv:"FILE" ~doc)

let man =
  [
    `S Manpage.s_description;
    `P
      "$(cmd) plays $(i,FILE) by the kind its extension names, in any case. A \
       tape ($(b,.tape)), in VHS's syntax, plays in a shell and is recorded; \
       so does a cram test ($(b,.t)), the format that dune runs, whose \
       commands $(cmd) types into bash. An asciicast recording ($(b,.cast)), \
       in asciinema's v2 or v3 format, is output already recorded: $(cmd) runs \
       nothing and draws it as an $(b,.svg) image, or writes its last screen \
       as $(b,.txt). An option that does not apply to the kind of $(i,FILE) is \
       an error.";
    `P
      "When every output of a tape or a cram test is an $(b,.svg) image or a \
       text output ($(b,.txt), $(b,.ascii) or $(b,.test)), $(cmd) plays \
       $(i,FILE) itself, in a shell on a pseudo-terminal, and draws its \
       screens as an animated SVG image, which plays in an HTML $(i,img) \
       element, or writes their text as VHS does. Otherwise $(b,vhs), which \
       needs $(b,ttyd), $(b,ffmpeg) and a browser, records it. A file cannot \
       ask for both an $(b,.svg) image and an output only $(b,vhs) writes. \
       $(b,--engine) picks the recorder instead, such as $(b,vhs) for a text \
       output of a tape whose shell $(cmd) does not start itself.";
    `P
      "$(b,--dry-run) checks $(i,FILE) as $(b,vhs validate) does, and also \
       that the chosen recorder writes the outputs, with the other options \
       applied, in directories it can write in; it exits 0 when they pass. On \
       an asciicast recording, it draws the image and writes nothing.";
    `P
      "On SIGINT or SIGTERM, $(cmd) plays no more actions, writes what it \
       recorded so far, and exits 130 or 143.";
    `S "TAPES";
    `P
      "The $(b,Output) lines of a tape, wherever they are, are its outputs, \
       which $(b,-o) replaces. A relative path, there or in $(b,-o), is \
       relative to the current directory, not to the tape's, as in VHS. The \
       $(b,Set) lines of a tape before its first action are its settings, \
       which $(b,--columns), $(b,--rows), $(b,--font-size), $(b,--theme) and \
       $(b,--framerate) replace. $(b,--setup), $(b,--dir) and \
       $(b,--wait-timeout) apply to cram tests only. An $(b,.svg) image of a \
       tape keeps every pause, its $(b,Sleep) lines included, unless \
       $(b,--idle-time-limit) is given; $(b,vhs) ignores the option.";
    `S "CRAM TESTS";
    `P
      "$(cmd) types each $(b,\\$) command of a cram test into a terminal \
       running bash and waits for the prompt to come back before the next. It \
       ignores the expected output and exit codes. As dune does, it runs the \
       commands in a new empty temporary directory, which it removes \
       afterwards, unless $(b,--dir) names another one. A cram test needs \
       $(b,-o). In its $(b,.svg) image, a pause longer than \
       $(b,--idle-time-limit) seconds, 2 unless given, such as a build that \
       prints nothing for a while, lasts that long.";
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
    `S "ASCIICAST RECORDINGS";
    `P
      "Each output event of an asciicast recording shows at its time, rounded \
       to the millisecond, so the same recording gives the same image, byte \
       for byte, and the last screen holds for a second after the last event, \
       its exit included. A pause longer than the $(b,idle_time_limit) of the \
       header lasts that limit, and then one longer than \
       $(b,--idle-time-limit) seconds, 2 unless given, lasts that long. The \
       terminal has the size of the header and of each resize, on an image of \
       the largest, and $(b,--columns) and $(b,--rows) fix it instead; the \
       theme of the header gives its colours unless $(b,--theme) does. $(cmd) \
       ignores the input events, the markers and the other members of the \
       header. A recording needs $(b,-o), an $(b,.svg) image or a $(b,.txt) \
       text of its last screen.";
    `P
      "The first line of the image is $(b,\\$) and the $(b,command) of the \
       header, if any, as $(b,k7 rec) shows its command, and the output starts \
       half a second later. $(b,--show) replaces the command, or hides it when \
       empty, and $(b,--delay) sets the pause; both apply to asciicast \
       recordings only.";
    `P
      "Several recordings play one after the other in one image, on one \
       terminal, as commands typed in turn: each shows its command line, and \
       the next starts once the last screen of the one before has held for a \
       second. One $(b,--show) then replaces the command of each, one per \
       recording replaces each command in turn, and the theme is that of the \
       first.";
    `P
      "asciinema writes such recordings, and so does $(b,k7 rec) $(b,-o) \
       $(i,FILE.cast). A program that uses console writes one at the times of \
       its virtual clock, with $(b,CONSOLE_DETERMINISTIC_CLOCK=1) and \
       $(b,CONSOLE_CAST=)$(i,FILE).";
    `S Manpage.s_examples;
    `Pre
      "$(cmd) demo.tape\n\
       $(cmd) --columns 100 -o demo.svg demo.tape\n\
       $(cmd) -o demo.svg demo.t\n\
       $(cmd) -o demo.gif demo.t\n\
       $(cmd) --print -o demo.gif demo.t   # show the tape\n\
       $(cmd) --show 'dune exec -- ./main.exe' -o main.svg main.cast\n\
       $(cmd) -o themes.svg dark.cast light.cast\n\
       $(cmd) --show 'demo' --show 'demo --theme light' -o themes.svg \
       dark.cast light.cast";
  ]

let cmd =
  let doc = "play a tape, a cram test or asciicast recordings" in
  let run engine dry_run print vhs settings env outputs setup dir wait_timeout
      show delay idle inputs =
    Eio_main.run @@ fun stdenv ->
    run stdenv ?engine ~dry_run ~print ?vhs ~settings ~env ~outputs ~setup ~dir
      ~wait_timeout ~show
      ?delay:(Option.map (fun ms -> K7.Ms ms) delay)
      ?idle inputs
  in
  Cmd.make
    (Cmd.info "play" ~doc ~man ~exits:Flags.exits)
    Term.(
      const run $ engine $ dry_run $ Flags.print $ Flags.vhs $ settings
      $ Flags.env $ outputs $ setup $ dir $ Flags.wait_timeout $ show $ delay
      $ Flags.idle_time_limit
          ~default:
            "2 for a cram test or an asciicast recording, and none for a tape"
      $ input)
