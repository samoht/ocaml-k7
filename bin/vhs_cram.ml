let err_load input err =
  Fmt.kstr (fun e -> Error e) "%s: %a" input Eio.Exn.pp_err err

let settings ~width =
  let open Vhs in
  [
    Shell "bash";
    Width width;
    Height 660;
    Font_size 18;
    Theme "Dracula";
    Padding 22;
    Typing_speed (Ms 24);
    Framerate 30;
  ]

(* The output and setup paths come from the command line, so the
   [Invalid_argument] of a path VHS cannot read is a user error. *)
let tape ~width ~setup_scripts ~output src =
  let settings = settings ~width in
  Result.bind (Vhs.Cram.parse src) @@ fun cram ->
  match
    (Vhs.output output :: List.map Vhs.set settings)
    @ Vhs.Cram.items ~setup:setup_scripts cram
  with
  | tape -> Ok tape
  | exception Invalid_argument msg -> Error msg

let record ~vhs ~width ~setup_scripts ~output input =
  Eio_main.run @@ fun env ->
  match Eio.Path.(load (Eio.Stdenv.fs env / input)) with
  | exception Eio.Io (err, _) -> err_load input err
  | src ->
      Result.bind (tape ~width ~setup_scripts ~output src) @@ fun tape ->
      Vhs.render ~vhs
        ~proc:(Eio.Stdenv.process_mgr env)
        ~cwd:(Eio.Stdenv.cwd env) tape
      |> Result.map_error (Fmt.to_to_string Vhs.pp_error)

let run vhs width setup_scripts output input =
  match record ~vhs ~width ~setup_scripts ~output input with
  | Ok () -> 0
  | Error msg ->
      Fmt.epr "vhs-cram: %s@." msg;
      1

open Cmdliner

let output =
  let doc =
    "Write the recording to $(docv); VHS picks the format from its extension."
  in
  Arg.(
    required & opt (some string) None & info [ "o"; "output" ] ~docv:"FILE" ~doc)

let setup_scripts =
  let doc =
    "Source $(docv) before the first command, without recording it. Repeatable."
  in
  Arg.(value & opt_all string [] & info [ "setup" ] ~docv:"FILE" ~doc)

let width =
  let doc = "Terminal width in pixels." in
  Arg.(value & opt int 1100 & info [ "width" ] ~docv:"PIXELS" ~doc)

let vhs =
  let doc = "The $(b,vhs) executable to run." in
  Arg.(value & opt string "vhs" & info [ "vhs" ] ~docv:"PATH" ~doc)

let input =
  let doc = "The cram test to record." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"CRAM" ~doc)

let cmd =
  let doc = "record a cram test as a VHS terminal recording" in
  Cmd.v (Cmd.info "vhs-cram" ~doc)
    Term.(const run $ vhs $ width $ setup_scripts $ output $ input)

let () = exit (Cmd.eval' cmd)
