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

let test_write_file () =
  let path = Filename.temp_file "ocaml-vhs-" ".tape" in
  Fun.protect
    ~finally:(fun () -> try Sys.remove path with Sys_error _ -> ())
    (fun () ->
      let tape = Vhs.tape [ Vhs.output "demo.gif"; Vhs.sleep (S 2.) ] in
      Vhs.write_file ~path tape;
      let ic = open_in_bin path in
      let content =
        Fun.protect
          ~finally:(fun () -> close_in_noerr ic)
          (fun () -> really_input_string ic (in_channel_length ic))
      in
      expect "file content" "Output \"demo.gif\"\nSleep 2s\n" content)

let test_write_file_rejects_traversal () =
  let dir = Filename.temp_file "ocaml-vhs-" "-traversal" in
  Sys.remove dir;
  Unix.mkdir dir 0o700;
  let sub = Filename.concat dir "sub" in
  Unix.mkdir sub 0o700;
  let cwd = Unix.getcwd () in
  Fun.protect
    ~finally:(fun () ->
      (try Sys.remove (Filename.concat dir "evil.txt") with Sys_error _ -> ());
      (try Unix.rmdir sub with Unix.Unix_error _ -> ());
      (try Unix.rmdir dir with Unix.Unix_error _ -> ());
      Unix.chdir cwd)
    (fun () ->
      Unix.chdir sub;
      let tape = Vhs.tape [] in
      Alcotest.check_raises "traversal path rejected"
        (Invalid_argument
           "Vhs.write_file: path must be a relative file path with no '.' or \
            '..' segments") (fun () -> Vhs.write_file ~path:"../evil.txt" tape))

let suite =
  ( "vhs",
    [
      Alcotest.test_case "top level constructors" `Quick
        test_top_level_constructors;
      Alcotest.test_case "write file" `Quick test_write_file;
      Alcotest.test_case "write file rejects traversal" `Quick
        test_write_file_rejects_traversal;
    ] )
