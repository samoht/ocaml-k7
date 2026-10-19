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

let render ~proc ~vhs ~path tape =
  match Vhs.validate tape with
  | Error _ as e -> e
  | Ok () ->
      Vhs.render ~proc ~vhs ~path tape
      |> Result.map_error (Fmt.to_to_string Vhs.pp_error)

let record ~vhs ~width ~setup_scripts ~output input =
  match In_channel.with_open_bin input In_channel.input_all with
  | exception Sys_error msg -> Error msg
  | src ->
      let settings = settings ~width in
      let tape =
        Vhs.Cram.to_tape ~settings ~setup_scripts ~output (Vhs.Cram.parse src)
      in
      Eio_main.run @@ fun env ->
      let proc = Eio.Stdenv.process_mgr env in
      let path = Filename.temp_file "vhs-cram-" ".tape" in
      Fun.protect
        ~finally:(fun () -> try Sys.remove path with Sys_error _ -> ())
        (fun () -> render ~proc ~vhs ~path tape)

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
