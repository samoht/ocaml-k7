(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let ( let* ) = Result.bind
let has_all_quotes s = List.for_all (String.contains s) [ '"'; '\''; '`' ]

(* The tape's [Env] lines quote each name and value. *)
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
        match wait_timeout_s with
        | None -> []
        | Some s -> [ Fmt.kstr K7.raw "Set WaitTimeout %ds" s ]
      in
      K7.tape ~settings:(Shell Bash :: settings) ~outputs:[ output ]
        (timeout @ Cram.actions ~env ~setup cram)
      |> Result.map_error (fun e -> Fmt.str "%a" K7.pp_error e)

(* [shown s] is [s] as [K7.pp_error] prints it: without escape sequences,
   controls and the invisible characters of the prompt mark. *)
let shown s = Fmt.str "%a" K7.pp_error (Io s)
let last_value = function "" -> "" | last -> "; last value was: " ^ shown last

(* [after cram tape line] is the prompt that the wait on line [line] of
   [tape] waits for, in words. The waits of a cram tape are the setup's,
   then one after each command. *)
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

(* [rest_after sub l] is what follows the first [sub] in [l], if any. *)
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

(* What [vhs] last read in a wait that timed out, from the line of its
   message, if a wait timed out. *)
let vhs_timeout output =
  List.find_map
    (fun l ->
      Option.map
        (fun rest ->
          Option.value (rest_after "last value was: " rest) ~default:"")
        (rest_after "timeout waiting for" l))
    (String.split_on_char '\n' output)

let seconds_of n = Fmt.str "%d second%s" n (if n = 1 then "" else "s")

(* A command that leaves bash at its continuation prompt makes the next
   wait time out; the error then names it. A timeout otherwise names the
   prompt it waited for, in words, without the prompt mark of the tape's
   pattern, and the option that waits longer. *)
let render_error ~setup ~wait ~name ~tape src e =
  let cram = Result.to_option (Cram.parse src) in
  let unfinished output =
    Option.bind cram (fun t -> Cram.unfinished ~setup t output)
  in
  let longer = "; --wait-timeout gives it longer" in
  let output =
    match e with
    | K7.Failed { output; _ } -> Some output
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
        | `Signaled n -> Fmt.str "was killed by signal %d" n
      in
      Fmt.str "%s: the shell %s while waiting for %s%s" name how
        (after cram tape line) (last_value last)
  | None, Failed { output; _ }, _ when Option.is_some (vhs_timeout output) ->
      Fmt.str
        "%s: vhs timed out after %s waiting for the prompt after a command%s%s"
        name (seconds_of wait)
        (last_value (Option.get (vhs_timeout output)))
        longer
  | None, _, _ -> Fmt.str "%s: %a" name K7.pp_error e

let record env ~print ~vhs ~setup ?settings ?env:vars ?wait_timeout_s ~output
    input =
  let* src = Flags.load env input in
  let* tape =
    tape ~setup ?settings ?env:vars ~output ?wait_timeout_s ~name:input src
  in
  if print then Flags.print_tape env tape
  else
    K7.play ~vhs
      ~clock:(Eio.Stdenv.mono_clock env)
      ~proc:(Eio.Stdenv.process_mgr env)
      ~cwd:(Eio.Stdenv.cwd env) tape
    |> Result.map_error
         (render_error ~setup
            ~wait:(Option.value wait_timeout_s ~default:15)
            ~name:input ~tape src)

let run env ~print ~vhs ~setup ?settings ?env:vars ?wait_timeout_s ~output input
    =
  match
    record env ~print ~vhs ~setup ?settings ?env:vars ?wait_timeout_s ~output
      input
  with
  | Ok () -> 0
  | Error msg -> Flags.fail env msg

open Cmdliner

let output =
  let doc =
    "Write the recording to $(docv), in the format of its extension, in lower \
     case. $(tname) itself records an $(b,.svg) image and the text outputs \
     $(b,.txt), $(b,.ascii) and $(b,.test), and $(b,vhs) records the others."
  in
  Arg.(
    required & opt (some string) None & info [ "o"; "output" ] ~docv:"FILE" ~doc)

let setup =
  let doc =
    "Source $(docv) before the first command, without recording it. Repeatable."
  in
  Arg.(value & opt_all non_dir_file [] & info [ "setup" ] ~docv:"FILE" ~doc)

let input =
  let doc = "The cram test to record." in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"CRAM" ~doc)

let man =
  [
    `S Manpage.s_description;
    `P
      "$(tname) records the cram test $(i,CRAM). It types each $(b,\\$) \
       command into a terminal running bash and waits for the prompt to come \
       back before the next. It ignores the expected output and exit codes.";
    `P
      "$(tname) records an $(b,.svg) output and the text outputs without \
       $(b,vhs). It runs bash on a pseudo-terminal itself and draws its \
       screens as an animated SVG image, which plays in an HTML $(i,img) \
       element, or writes their text as VHS does. $(b,vhs) records the other \
       outputs, and needs $(b,ttyd), $(b,ffmpeg) and a browser.";
    `P
      "Before the first command, and after the setup scripts, $(tname) makes \
       the last command of $(b,PROMPT_COMMAND) set $(b,PS1) to the VHS prompt \
       followed by a mark. The mark is sixteen zero-width characters, U+200B \
       and U+2060, that spell a digest of the setup scripts and the commands. \
       It tells the prompt from output, even output that ends with $(b,>) or \
       with a zero-width space, and does not show in a GIF or a video. The \
       text outputs hold it after each prompt.";
    `P
      "The prompt hooks of the setup scripts still run first, and a command \
       that replaces $(b,PROMPT_COMMAND) makes the next wait time out. So does \
       a command that leaves bash waiting for more input, such as an \
       unterminated quote or here-document, and $(tname) then names that \
       command. In a native recording, a wait that times out names the command \
       whose prompt did not come back, and a shell that exits ends the \
       recording at once.";
    `P
      "Each $(b,--env) variable is set with an $(b,Env) line of the tape, \
       before bash starts.";
    `S Manpage.s_examples;
    `Pre
      "$(cmd) --output demo.svg demo.t\n\
       $(cmd) --cols 100 --output demo.svg demo.t\n\
       $(cmd) --output demo.gif demo.t\n\
       $(cmd) --print --output demo.gif demo.t   # show the tape";
  ]

let cmd =
  let doc = "record a cram test" in
  let run print vhs setup settings env wait_timeout_s output input =
    Eio_main.run @@ fun stdenv ->
    run stdenv ~print ~vhs ~setup ~settings ~env ?wait_timeout_s ~output input
  in
  Cmd.make
    (Cmd.info "cram" ~doc ~man ~exits:Flags.exits)
    Term.(
      const run $ Flags.print $ Flags.vhs $ setup $ Flags.settings $ Flags.env
      $ Flags.wait_timeout $ output $ input)
