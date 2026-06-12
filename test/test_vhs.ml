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
    "Output demo.gif\nSet TypingSpeed 25ms\nHide\nType \"clear\"\nEnter\nShow\n"
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
      expect "file content" "Output demo.gif\nSleep 2s\n" content)

let suite =
  ( "vhs",
    [
      Alcotest.test_case "top level constructors" `Quick
        test_top_level_constructors;
      Alcotest.test_case "write file" `Quick test_write_file;
    ] )
