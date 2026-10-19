let expect = Alcotest.(check string)
let str = Fmt.str "%a" Vhs.pp

let tape src =
  match Vhs_cram_subject.tape ~setup:[] ~output:"demo.gif" "input.t" src with
  | Ok tape -> str tape
  | Error msg -> Alcotest.fail msg

let from_flow actions =
  Eio_mock.Backend.run @@ fun () ->
  let source = Eio_mock.Flow.make "cram input" in
  Eio_mock.Flow.on_read source actions;
  Eio.Buf_read.(take_all (of_flow source ~max_size:1_000_000)) |> tape

let test_byte_reads () =
  let src = "  $ echo hello\r\n  hello\r\n" in
  let actions =
    List.init (String.length src) (fun i -> `Return (String.make 1 src.[i]))
  in
  expect "tape" (tape src) (from_flow (actions @ [ `Raise End_of_file ]))

let test_split_utf8 () =
  let b = Buffer.create 4 in
  Buffer.add_utf_8_uchar b (Uchar.of_int 0x1f642);
  let face = Buffer.contents b in
  let src = "  $ echo " ^ face in
  expect "UTF-8 and EOF without newline" (tape src)
    (from_flow
       [
         `Return ("  $ echo " ^ String.sub face 0 2);
         `Return (String.sub face 2 2);
         `Raise End_of_file;
       ])

let test_read_raises () =
  let err = Eio.Fs.err (Permission_denied Eio_mock.Simulated_failure) in
  match from_flow [ `Return "  $ echo "; `Raise err ] with
  | exception Eio.Io (code, _) -> (
      match Vhs_cram_subject.err_load "input.t" code with
      | Error msg ->
          Alcotest.(check bool)
            "input named" true
            (String.starts_with ~prefix:"input.t: " msg)
      | Ok _ -> Alcotest.fail "accepted read error")
  | _ -> Alcotest.fail "read error was lost"

let test_tape_errors () =
  let check_error ~setup ~output src =
    match Vhs_cram_subject.tape ~setup ~output "input.t" src with
    | Error msg -> Alcotest.(check bool) "diagnostic" true (msg <> "")
    | Ok _ -> Alcotest.fail "invalid tape accepted"
  in
  check_error ~setup:[] ~output:"demo.gif" "prose only";
  check_error ~setup:[] ~output:"demo.gif" "  $ echo \001\n";
  check_error ~setup:[] ~output:"no-extension" "  $ true\n";
  check_error ~setup:[ "bad\nsetup" ] ~output:"demo.gif" "  $ true\n";
  Alcotest.(check (result unit string))
    "no command diagnostic" (Error "input.t: no command")
    (Vhs_cram_subject.err_no_command "input.t")

let with_input src fn =
  let path = Filename.temp_file "vhs-cram-test" ".t" in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      let oc = open_out_bin path in
      Fun.protect
        ~finally:(fun () -> close_out oc)
        (fun () -> output_string oc src);
      fn path)

(* record owns Eio_main.run and opens a real file. The smallest real
   stand-ins are a temporary cram file and the existing true/false commands. *)
let test_record () =
  with_input "  $ echo hi\n" @@ fun input ->
  let record vhs =
    Vhs_cram_subject.record ~vhs ~setup:[] ~output:"demo.gif" input
  in
  Alcotest.(check (result unit string))
    "successful child" (Ok ()) (record "true");
  Alcotest.(check (result unit string))
    "failed child" (Error "false exited with code 1") (record "false");
  (match record "vhs-no-such-test-executable" with
  | Error msg ->
      Alcotest.(check bool)
        "missing executable" true
        (String.starts_with ~prefix:"executable not found:" msg)
  | Ok () -> Alcotest.fail "missing executable ran");
  Alcotest.(check int)
    "run success exit status" 0
    (Vhs_cram_subject.run "true" [] "demo.gif" input);
  Alcotest.(check int)
    "run failure exit status" 123
    (Vhs_cram_subject.run "false" [] "demo.gif" input)

let test_record_load_error () =
  with_input "" @@ fun input ->
  let input = input ^ ".missing" in
  match
    Vhs_cram_subject.record ~vhs:"true" ~setup:[] ~output:"demo.gif" input
  with
  | Error msg ->
      Alcotest.(check bool)
        "path named" true
        (String.starts_with ~prefix:(input ^ ": ") msg)
  | Ok () -> Alcotest.fail "missing input read"

let test_command () =
  with_input "  $ true\n" @@ fun input ->
  let err = Format.str_formatter in
  let help_buffer = Buffer.create 256 in
  let help = Format.formatter_of_buffer help_buffer in
  let eval args =
    Cmdliner.Cmd.eval_value ~err ~help
      ~argv:(Array.of_list ("vhs-cram" :: args))
      Vhs_cram_subject.cmd
  in
  (match
     eval [ "--vhs"; "true"; "--output"; "demo.gif"; "--setup"; input; input ]
   with
  | Ok (`Ok 0) -> ()
  | _ -> Alcotest.fail "valid command line failed");
  (match eval [] with
  | Error _ -> ()
  | _ -> Alcotest.fail "missing required arguments accepted");
  (match eval [ "--help=plain" ] with
  | Ok `Help -> ()
  | _ -> Alcotest.fail "help failed");
  Format.pp_print_flush help ();
  Alcotest.(check bool) "manual rendered" true (Buffer.length help_buffer > 0);
  ignore (Format.flush_str_formatter ())

let suite =
  ( "cli",
    List.map
      (fun (name, fn) -> Alcotest.test_case name `Quick fn)
      [
        ("input one byte at a time", test_byte_reads);
        ("input split UTF-8 and EOF mid-line", test_split_utf8);
        ("input read raises", test_read_raises);
        ("tape diagnostics", test_tape_errors);
        ("record and run with real child", test_record);
        ("record load error", test_record_load_error);
        ("command arguments", test_command);
      ] )
