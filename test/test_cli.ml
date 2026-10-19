(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Vhs_cram

let expect = Alcotest.(check string)

let contains ~sub s =
  let n = String.length sub in
  let rec at i =
    i + n <= String.length s && (String.sub s i n = sub || at (i + 1))
  in
  at 0

let tape ?(setup = []) ?(output = "demo.gif") src =
  Cli.tape ~setup ~output ~name:"input.t" src
  |> Result.map (Fmt.str "%a" Vhs.pp)

let test_tape () =
  expect "tape"
    "Output \"demo.gif\"\n\
     Set Shell \"bash\"\n\
     Type \"true\"\n\
     Enter\n\
     Wait\n\
     Sleep 1000ms\n"
    (Result.get_ok (tape "  $ true\n"));
  let error name expected src ~setup ~output =
    Alcotest.(check (result string string))
      name (Error expected) (tape ~setup ~output src)
  in
  error "no command" "input.t: no command" "prose only" ~setup:[]
    ~output:"x.gif";
  error "control" {|input.t: line 1: "echo \001" holds a control character|}
    "  $ echo \001\n" ~setup:[] ~output:"x.gif";
  error "output" {|Output "demo": neither an extension nor a trailing slash|}
    "  $ true\n" ~setup:[] ~output:"demo";
  match tape ~setup:[ "bad\000setup" ] "  $ true\n" with
  | Error _ -> ()
  | Ok s -> Alcotest.failf "accepted %S" s

(* Real processes: [run] owns the whole Eio environment. A shell script
   stands in for vhs. *)
let with_files files fn =
  Eio_main.run @@ fun env ->
  let dir = Filename.temp_dir "vhs-cram" "" in
  let path name = Filename.concat dir name in
  List.iter
    (fun (name, perm, contents) ->
      Eio.Path.save ~create:(`Or_truncate perm)
        Eio.Path.(Eio.Stdenv.fs env / path name)
        contents)
    files;
  fn env path

let fake_vhs status =
  Fmt.str
    "#!/bin/sh\n\
     [ \"$1\" = -q ] || exit 9\n\
     cat > /dev/null\n\
     echo 'from stdout'\n\
     echo 'from stderr' >&2\n\
     exit %d\n"
    status

let run env ?(print = false) ~vhs input =
  Cli.run env ~print ~vhs ~setup:[] ~output:"demo.gif" input

let test_run () =
  with_files
    [
      ("demo.t", 0o644, "  $ echo hi\n");
      ("ok", 0o755, fake_vhs 0);
      ("ko", 0o755, fake_vhs 3);
    ]
  @@ fun env path ->
  Alcotest.(check int) "success" 0 (run env ~vhs:(path "ok") (path "demo.t"));
  Alcotest.(check int) "failure" 123 (run env ~vhs:(path "ko") (path "demo.t"));
  Alcotest.(check int)
    "missing vhs" 123
    (run env ~vhs:"vhs-no-such-executable" (path "demo.t"));
  Alcotest.(check int)
    "missing input" 123
    (run env ~vhs:(path "ok") (path "missing.t"))

let test_render () =
  with_files
    [
      ("ko", 0o755, fake_vhs 3);
      ("pwd", 0o755, "#!/bin/sh\ncat > /dev/null\npwd -P >&2\nexit 4\n");
    ]
  @@ fun env path ->
  let proc = Eio.Stdenv.process_mgr env in
  let cwd = Eio.Stdenv.cwd env in
  let t = Result.get_ok (Vhs.tape ~outputs:[ "x.gif" ] []) in
  (match Vhs.render ~vhs:(path "ko") ~proc ~cwd t with
  | Error (Failed { status = `Exited 3; _ } as e) ->
      expect "both streams"
        (path "ko" ^ " exited with code 3\n  from stdout\n  from stderr")
        (Fmt.str "%a" Vhs.pp_error e)
  | _ -> Alcotest.fail "lost the failure");
  (match
     Vhs.render ~vhs:(path "pwd") ~proc
       ~cwd:Eio.Path.(Eio.Stdenv.fs env / "/")
       t
   with
  | Error (Failed { output = "/\n"; _ }) -> ()
  | Error e -> Alcotest.failf "not run in /: %a" Vhs.pp_error e
  | Ok () -> Alcotest.fail "pwd succeeded");
  match
    Vhs.render ~vhs:(path "ko") ~proc
      ~cwd:Eio.Path.(Eio.Stdenv.fs env / path "missing-dir")
      t
  with
  | Error (Io msg) ->
      expect "missing cwd"
        (path "missing-dir" ^ ": no such file or directory")
        msg
  | Error e -> Alcotest.failf "unexpected %a" Vhs.pp_error e
  | Ok () -> Alcotest.fail "ran in a missing directory"

let test_command () =
  let help_buffer = Buffer.create 256 in
  let help = Format.formatter_of_buffer help_buffer in
  let err = Format.formatter_of_buffer (Buffer.create 256) in
  let eval args =
    Cmdliner.Cmd.eval_value ~err ~help
      ~argv:(Array.of_list ("vhs-cram" :: args))
      Cli.cmd
  in
  (match eval [] with
  | Error _ -> ()
  | _ -> Alcotest.fail "missing required arguments accepted");
  (match eval [ "--output"; "x.gif"; Filename.get_temp_dir_name () ] with
  | Error _ -> ()
  | _ -> Alcotest.fail "directory accepted as a cram test");
  (match eval [ "--help=plain" ] with
  | Ok `Help -> ()
  | _ -> Alcotest.fail "help failed");
  Format.pp_print_flush help ();
  let man = Buffer.contents help_buffer in
  List.iter
    (fun s ->
      if not (contains ~sub:s man) then Alcotest.failf "manual lacks %S" s)
    [ "--print"; "123"; "PS1" ]

let suite =
  ( "cli",
    List.map
      (fun (name, fn) -> Alcotest.test_case name `Quick fn)
      [
        ("tape", test_tape);
        ("run", test_run);
        ("render real processes", test_render);
        ("command line", test_command);
      ] )
