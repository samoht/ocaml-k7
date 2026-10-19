let expect name expected got = Alcotest.(check string) name expected got

let test_top_level_constructors () =
  let tape =
    Vhs.tape
      [
        Vhs.output "demo.gif";
        Vhs.set (Typing_speed (Ms 25));
        Vhs.hide ();
        Vhs.type' "clear";
        Vhs.enter ();
        Vhs.show ();
      ]
  in
  expect "top level constructors"
    "Output \"demo.gif\"\n\
     Set TypingSpeed 25ms\n\
     Hide\n\
     Type \"clear\"\n\
     Enter\n\
     Show\n"
    (Vhs.to_string tape)

let read_file path = In_channel.with_open_bin path In_channel.input_all

let missing =
  Filename.concat (Filename.get_temp_dir_name ()) "vhs-no-such-dir/demo.tape"

let test_write_file () =
  let path = Filename.temp_file "ocaml-vhs-" ".tape" in
  Fun.protect
    ~finally:(fun () -> try Sys.remove path with Sys_error _ -> ())
    (fun () ->
      let tape = Vhs.tape [ Vhs.output "demo.gif"; Vhs.sleep (S 2.) ] in
      match Vhs.write_file ~path tape with
      | Ok () ->
          expect "file content" "Output \"demo.gif\"\nSleep 2s\n"
            (read_file path)
      | Error e -> Alcotest.failf "%a" Vhs.pp_error e)

let test_write_file_error () =
  match Vhs.write_file ~path:missing (Vhs.tape []) with
  | Error (Io _) -> ()
  | Ok () -> Alcotest.fail "wrote into a missing directory"
  | Error e -> Alcotest.failf "unexpected %a" Vhs.pp_error e

let test_render_io_error () =
  Eio_main.run @@ fun env ->
  let proc = Eio.Stdenv.process_mgr env in
  match Vhs.render ~proc ~vhs:"true" ~path:missing (Vhs.tape []) with
  | Error (Io _) -> ()
  | Ok () -> Alcotest.fail "rendered into a missing directory"
  | Error e -> Alcotest.failf "unexpected %a" Vhs.pp_error e

let test_dash_path () =
  Eio_main.run @@ fun env ->
  let proc = Eio.Stdenv.process_mgr env in
  match Vhs.publish ~proc ~vhs:"echo" "-x.gif" with
  | Ok out -> expect "argv" "publish ./-x.gif\n" out
  | Error e -> Alcotest.failf "%a" Vhs.pp_error e

let suite =
  ( "vhs",
    [
      Alcotest.test_case "top level constructors" `Quick
        test_top_level_constructors;
      Alcotest.test_case "write file" `Quick test_write_file;
      Alcotest.test_case "write file error" `Quick test_write_file_error;
      Alcotest.test_case "render io error" `Quick test_render_io_error;
      Alcotest.test_case "dash path" `Quick test_dash_path;
    ] )
