(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let ( let* ) = Result.bind
let has_all_quotes s = List.for_all (String.contains s) [ '"'; '\''; '`' ]

let check_env (k, v) =
  let binding = k ^ "=" ^ v in
  if not (Cram.is_typeable k && Cram.is_typeable v) then
    Fmt.error "--env %a: holds invalid UTF-8 or a control character" Quote.pp
      binding
  else if has_all_quotes k || has_all_quotes v then
    Fmt.error "--env %a: holds all three quote characters" Quote.pp binding
  else Ok ()

let tape ~setup ?(settings = []) ?(env = []) ~output ?wait_timeout_s ~name src =
  let* cram =
    Cram.parse src |> Result.map_error (fun e -> Fmt.str "%s: %s" name e)
  in
  let* () =
    List.fold_left
      (fun acc b -> Result.bind acc (fun () -> check_env b))
      (Ok ()) env
  in
  match
    (Cram.is_empty cram, List.find_opt (Fun.negate Cram.is_typeable) setup)
  with
  | true, _ -> Fmt.error "%s: no command: no line starts with \"  $ \"" name
  | false, Some path ->
      Fmt.error "setup path %a: holds invalid UTF-8 or a control character"
        Quote.pp path
  | false, None ->
      let timeout =
        Option.map (fun s -> K7.Wait_timeout (Ms (s * 1000))) wait_timeout_s
      in
      K7.tape
        ~settings:((K7.Shell Bash :: settings) @ Option.to_list timeout)
        ~outputs:[ output ]
        (Cram.actions ~env ~setup cram)
      |> Result.map_error (fun e -> Fmt.str "%a" Flags.pp_error e)

let shown = Flags.shown
let last_value = function "" -> "" | last -> "; last value was: " ^ shown last

(* A cram tape waits after its setup, then after each command. *)
let after cram tape line =
  let waits =
    String.split_on_char '\n' (K7.to_string tape)
    |> List.filteri (fun i l -> i < line && String.starts_with ~prefix:"Wait" l)
    |> List.length
  in
  match Cram.command cram (waits - 1) with
  | Some first ->
      Fmt.str "the prompt after command %d, %a" (waits - 1) Quote.pp first
  | None -> "the prompt after the setup scripts"

let rest_after sub l =
  let n = String.length sub in
  let rec go i =
    match String.index_from_opt l i sub.[0] with
    | Some j when j + n <= String.length l ->
        if String.sub l j n = sub then
          Some (String.sub l (j + n) (String.length l - j - n))
        else go (j + 1)
    | _ -> None
  in
  go 0

let vhs_timeout output =
  List.find_map
    (fun l ->
      Option.map
        (fun rest ->
          Option.value (rest_after "last value was: " rest) ~default:"")
        (rest_after "timeout waiting for" l))
    (String.split_on_char '\n' output)

let seconds_of n = Fmt.str "%d second%s" n (if n = 1 then "" else "s")

(* A command left at bash's continuation prompt times the next wait out. *)
let render_error ~vhs ~setup ~wait ~name ~tape src e =
  let cram = Result.to_option (Cram.parse src) in
  let unfinished output =
    Option.bind cram (fun t -> Cram.unfinished ~setup t output)
  in
  let longer = "; --wait-timeout gives it longer" in
  let output =
    match e with
    | K7.Vhs_failed { output; _ } -> Some output
    | Timeout { last; _ } | Exited { last; _ } ->
        Some ("last value was: " ^ last)
    | _ -> None
  in
  match (Option.bind output unfinished, e, cram) with
  | Some (n, line), _, _ ->
      Fmt.str
        "%s: command %d, %a, left bash waiting for more input, such as the end \
         of a quote or of a here-document"
        name n Quote.pp line
  | None, Timeout { line; last; _ }, Some cram ->
      Fmt.str "%s: timed out after %s waiting for %s%s%s" name (seconds_of wait)
        (after cram tape line) (last_value last) longer
  | None, Exited { status; line; last; _ }, Some cram ->
      let how =
        match status with
        | `Exited n -> Fmt.str "exited (status %d)" n
        | `Signaled n -> Fmt.str "was killed by signal %d" (Sys.signal_to_int n)
      in
      Fmt.str "%s: the shell %s while waiting for %s%s" name how
        (after cram tape line) (last_value last)
  | None, Vhs_failed { output; _ }, _ when Option.is_some (vhs_timeout output)
    ->
      Fmt.str
        "%s: vhs timed out after %s waiting for the prompt after a command%s%s"
        name (seconds_of wait)
        (last_value (Option.get (vhs_timeout output)))
        longer
  | None, _, _ -> Fmt.str "%s: %a" name (Flags.pp_play_error ~vhs) e

let absolute path =
  if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
  else path

let in_dir env dir f =
  let fs = Eio.Stdenv.fs env in
  match dir with
  | Some dir -> f Eio.Path.(fs / absolute dir)
  | None -> (
      match Filename.temp_dir "k7-cram-" "" with
      | exception Sys_error msg -> Error msg
      | dir ->
          let dir = Eio.Path.(fs / dir) in
          Fun.protect
            ~finally:(fun () ->
              try Eio.Path.rmtree ~missing_ok:true dir with Eio.Io _ -> ())
            (fun () -> f dir))

let record ?stop env ~print ~vhs ~setup ?dir ?settings ?env:vars ?wait_timeout_s
    ~output input =
  let* src = Flags.load env input in
  let tape ~setup ~output =
    tape ~setup ?settings ?env:vars ~output ?wait_timeout_s ~name:input src
  in
  let* given = tape ~setup ~output in
  if print then Flags.print_tape env given
  else
    let* tape =
      tape ~setup:(List.map absolute setup) ~output:(absolute output)
    in
    let run_vhs = if String.contains vhs '/' then absolute vhs else vhs in
    let named = function
      | K7.Vhs_failed f -> K7.Vhs_failed { f with vhs }
      | K7.Executable_not_found exe when exe = run_vhs ->
          K7.Executable_not_found vhs
      | e -> e
    in
    in_dir env dir @@ fun cwd ->
    K7.record ?stop ~vhs:run_vhs
      ~clock:(Eio.Stdenv.mono_clock env)
      ~proc:(Eio.Stdenv.process_mgr env)
      ~cwd tape
    |> Result.map_error (fun e ->
        render_error ~vhs ~setup
          ~wait:(Option.value wait_timeout_s ~default:15)
          ~name:input ~tape:given src (named e))

let run env ~print ~vhs ~setup ?dir ?settings ?env:vars ?wait_timeout_s ~output
    input =
  Flags.interruptible (fun stop ->
      record ~stop env ~print ~vhs ~setup ?dir ?settings ?env:vars
        ?wait_timeout_s ~output input)
  |> Flags.exit env ~ok:(fun () -> 0)

open Cmdliner

let output =
  let doc =
    "Write the recording to $(docv), in the format of its extension, in lower \
     case. $(cmd) itself records an $(b,.svg) image and the text outputs \
     $(b,.txt), $(b,.ascii) and $(b,.test), and $(b,vhs) records the others."
  in
  Arg.(
    required & opt (some string) None & info [ "o"; "output" ] ~docv:"FILE" ~doc)

let setup =
  let doc =
    "Source $(docv) before the first command, without recording it. Repeatable."
  in
  Arg.(value & opt_all non_dir_file [] & info [ "setup" ] ~docv:"FILE" ~doc)

let dir =
  let doc =
    "Run the commands in $(docv) instead of a new empty temporary directory."
  in
  Arg.(value & opt (some dir) None & info [ "dir" ] ~docv:"DIR" ~doc)

let input =
  let doc = "The cram test to record." in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"CRAM" ~doc)

let man =
  [
    `S Manpage.s_description;
    `P
      "$(cmd) records the cram test $(i,CRAM). It types each $(b,\\$) command \
       into a terminal running bash and waits for the prompt to come back \
       before the next. It ignores the expected output and exit codes. As dune \
       does, it runs the commands in a new empty temporary directory, which it \
       removes afterwards, unless $(b,--dir) names another one.";
    `P
      "$(cmd) records an $(b,.svg) output and the text outputs without \
       $(b,vhs). It runs bash on a pseudo-terminal itself and draws its \
       screens as an animated SVG image, which plays in an HTML $(i,img) \
       element, or writes their text as VHS does. $(b,vhs) records the other \
       outputs, and needs $(b,ttyd), $(b,ffmpeg) and a browser.";
    `P
      "The prompt hooks of the setup scripts still run first, and a command \
       that replaces $(b,PROMPT_COMMAND) makes the next wait time out. So does \
       a command that leaves bash waiting for more input, such as an \
       unterminated quote or here-document, and $(cmd) then names that \
       command. In a native recording, a wait that times out names the command \
       whose prompt did not come back, and a shell that exits ends the \
       recording at once.";
    `P
      "Each $(b,--env) variable is set with an $(b,Env) line of the tape, \
       before bash starts.";
    `P
      "On SIGINT or SIGTERM, $(cmd) types no more commands, writes what it \
       recorded so far, and exits 130 or 143.";
    `S "THE PROMPT MARK";
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
      "$(cmd) --output demo.svg demo.t\n\
       $(cmd) --cols 100 --output demo.svg demo.t\n\
       $(cmd) --output demo.gif demo.t\n\
       $(cmd) --print --output demo.gif demo.t   # show the tape";
  ]

let cmd =
  let doc = "type a cram test into bash and record the session" in
  let run print vhs setup dir settings env wait_timeout_s output input =
    Eio_main.run @@ fun stdenv ->
    run stdenv ~print ~vhs ~setup ?dir ~settings ~env ?wait_timeout_s ~output
      input
  in
  Cmd.make
    (Cmd.info "cram" ~doc ~man ~exits:Flags.exits)
    Term.(
      const run $ Flags.print $ Flags.vhs $ setup $ dir $ Flags.settings
      $ Flags.env $ Flags.wait_timeout $ output $ input)
