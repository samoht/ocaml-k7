let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let settings ~width =
  let open Vhs in
  [
    Shell "bash";
    Width width;
    Height 660;
    Font_size 18;
    Font_family "Menlo";
    Theme "Dracula";
    Padding 22;
    Typing_speed (Ms 24);
    Framerate 30;
  ]

let render ~input ~output ~setup_scripts ~width ~vhs =
  let tape =
    Vhs.Cram.to_tape ~settings:(settings ~width) ~setup_scripts ~output
      (Vhs.Cram.parse (read_file input))
  in
  Eio_main.run @@ fun env ->
  let proc = Eio.Stdenv.process_mgr env in
  let path = Filename.temp_file "vhs-cram-" ".tape" in
  Fun.protect
    ~finally:(fun () -> try Sys.remove path with Sys_error _ -> ())
    (fun () ->
      match Vhs.render ~proc ~vhs ~path tape with
      | Ok () -> ()
      | Error error ->
          Fmt.epr "%a@." Vhs.pp_error error;
          exit 1)

let () =
  let input = ref None in
  let output = ref None in
  let setup_scripts = ref [] in
  let width = ref 1100 in
  let vhs = ref "vhs" in
  let set_input path =
    match !input with
    | None -> input := Some path
    | Some _ -> raise (Arg.Bad "only one cram transcript may be recorded")
  in
  Arg.parse
    [
      ( "--output",
        Arg.String (fun path -> output := Some path),
        "PATH output GIF (or another VHS-supported format)" );
      ( "--setup",
        Arg.String (fun path -> setup_scripts := path :: !setup_scripts),
        "PATH setup script to source before recording (repeatable)" );
      ("--width", Arg.Set_int width, "PIXELS terminal width (default: 1100)");
      ("--vhs", Arg.Set_string vhs, "PATH vhs executable (default: vhs)");
    ]
    set_input "vhs-cram [OPTION]... CRAM";
  let required name = function
    | Some value -> value
    | None -> Fmt.invalid_arg "missing %s" name
  in
  render ~input:(required "CRAM" !input)
    ~output:(required "--output" !output)
    ~setup_scripts:(List.rev !setup_scripts) ~width:!width ~vhs:!vhs
