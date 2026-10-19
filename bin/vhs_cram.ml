let err_load input err =
  Fmt.kstr (fun e -> Error e) "%s: %a" input Eio.Exn.pp_err err

let err_no_command input = Fmt.kstr (fun e -> Error e) "%s: no command" input

(* The output and setup paths come from the command line, so the
   [Invalid_argument] of a path VHS cannot read is a user error. *)
let tape ~setup ~output input src =
  Result.bind (Vhs.Cram.parse src) @@ fun cram ->
  match Vhs.Cram.items cram with
  | [] -> err_no_command input
  | _ -> (
      match
        Vhs.output output :: Vhs.set (Shell "bash")
        :: Vhs.Cram.items ~setup cram
      with
      | tape -> Ok tape
      | exception Invalid_argument msg -> Error msg)

let record ~vhs ~setup ~output input =
  Eio_main.run @@ fun env ->
  match Eio.Path.(load (Eio.Stdenv.fs env / input)) with
  | exception Eio.Io (err, _) -> err_load input err
  | src ->
      Result.bind (tape ~setup ~output input src) @@ fun tape ->
      Vhs.render ~vhs
        ~proc:(Eio.Stdenv.process_mgr env)
        ~cwd:(Eio.Stdenv.cwd env) tape
      |> Result.map_error (Fmt.to_to_string Vhs.pp_error)

open Cmdliner

let run vhs setup output input =
  match record ~vhs ~setup ~output input with
  | Ok () -> Cmd.Exit.ok
  | Error msg ->
      Fmt.epr "vhs-cram: %s@." msg;
      Cmd.Exit.some_error

let output =
  let doc =
    "Write the recording to $(docv); VHS picks the format from its extension."
  in
  Arg.(
    required & opt (some string) None & info [ "o"; "output" ] ~docv:"FILE" ~doc)

let setup =
  let doc =
    "Source $(docv) before the first command, without recording it. Repeatable."
  in
  Arg.(value & opt_all file [] & info [ "setup" ] ~docv:"FILE" ~doc)

let vhs =
  let doc = "The $(b,vhs) executable to run." in
  Arg.(value & opt string "vhs" & info [ "vhs" ] ~docv:"PATH" ~doc)

let input =
  let doc = "The cram test to record." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"CRAM" ~doc)

let man =
  [
    `S Manpage.s_description;
    `P
      "$(tname) types each $(b,\\$) command of the cram test $(i,CRAM) into a \
       VHS terminal running bash, waits for the prompt to come back, and \
       records the session with $(b,vhs). Expected output and exit codes are \
       ignored; other settings are the VHS defaults.";
    `S Manpage.s_examples;
    `Pre
      "$(cmd) --output demo.gif demo.t\n\
       $(cmd) --vhs cat --output demo.gif demo.t   # print the tape";
  ]

let cmd =
  let doc = "record a cram test as a VHS terminal recording" in
  Cmd.make
    (Cmd.info "vhs-cram" ~doc ~man)
    Term.(const run $ vhs $ setup $ output $ input)

let () = exit (Cmd.eval' cmd)
