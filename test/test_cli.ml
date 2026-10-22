(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open K7_cli

let expect = Alcotest.(check string)

let contains ~sub s =
  let n = String.length sub in
  let rec at i =
    i + n <= String.length s && (String.sub s i n = sub || at (i + 1))
  in
  at 0

let tape ?(setup = []) ?(output = "demo.gif") ?wait_timeout_s src =
  Cram_cmd.tape ~setup ~output ?wait_timeout_s ~name:"input.t" src
  |> Result.map K7.to_string

let test_tape () =
  expect "tape"
    {|Output "demo.gif"
Set Shell "bash"
Hide
Env K7_PROMPT `bind 'set input-meta on'; bind 'set output-meta on'; bind 'set convert-meta off'; __k7_n=; __k7_prompt() { PS1='\[\e[38;2;90;86;224m\]> \[\342\200\213\342\201\240\342\201\240\342\200\213\342\200\213\342\201\240\342\200\213\342\200\213\342\201\240\342\201\240\342\200\213\342\200\213\342\201\240\342\201\240\342\200\213\342\201\240\e[0m\]'; __k7_n=$__k7_n'\342\200\213'; PS2='> \[\342\200\213\342\201\240\342\201\240\342\200\213\342\200\213\342\201\240\342\200\213\342\200\213\342\201\240\342\201\240\342\200\213\342\200\213\342\201\240\342\201\240\342\200\213\342\201\240'$__k7_n'\]'; }; __k7=$(printf '%s\n' ${PROMPT_COMMAND+"${PROMPT_COMMAND[@]}"} __k7_prompt); unset PROMPT_COMMAND; PROMPT_COMMAND=$__k7; unset __k7 K7_PROMPT`
Type 'eval "$K7_PROMPT"; clear'
Enter
Wait+Line /\x{200B}\x{2060}\x{2060}\x{200B}\x{200B}\x{2060}\x{200B}\x{200B}\x{2060}\x{2060}\x{200B}\x{200B}\x{2060}\x{2060}\x{200B}\x{2060}$/
Show
Type "true"
Enter
Wait+Line /\x{200B}\x{2060}\x{2060}\x{200B}\x{200B}\x{2060}\x{200B}\x{200B}\x{2060}\x{2060}\x{200B}\x{200B}\x{2060}\x{2060}\x{200B}\x{2060}$/
Sleep 1000ms
|}
    (Result.get_ok (tape "  $ true\n"));
  Alcotest.(check bool)
    "wait timeout" true
    (contains ~sub:"\nSet WaitTimeout 60s\n"
       (Result.get_ok (tape ~wait_timeout_s:60 "  $ true\n")));
  let error name expected src ~setup ~output =
    Alcotest.(check (result string string))
      name (Error expected) (tape ~setup ~output src)
  in
  error "no command" {|input.t: no command: no line starts with "  $ "|}
    "prose only" ~setup:[] ~output:"x.gif";
  error "control" {|input.t: line 1: "echo \x01" holds a control character|}
    "  $ echo \001\n" ~setup:[] ~output:"x.gif";
  error "output" {|Output "demo": neither an extension nor a trailing slash|}
    "  $ true\n" ~setup:[] ~output:"demo";
  (* VHS reads extensions case-sensitively. *)
  error "upper case"
    {|Output "demo.SVG": not .gif, .mp4, .webm, .txt, .ascii, .test, .png or .svg; extensions are lower case|}
    "  $ true\n" ~setup:[] ~output:"demo.SVG";
  List.iter
    (fun path ->
      match tape ~setup:[ path ] "  $ true\n" with
      | Error msg ->
          Alcotest.(check bool)
            "names the setup path" true
            (String.starts_with ~prefix:"setup path " msg)
      | Ok s -> Alcotest.failf "accepted %S" s)
    [ "bad\000setup"; "a\tb.sh"; "a\nb.sh"; "a\xc2\x85b.sh"; "\xff.sh" ]

let with_files files fn =
  Eio_main.run @@ fun env ->
  let dir = Filename.temp_dir "k7-cli" "" in
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
  Cram_cmd.run env ~print ~vhs ~setup:[] ~output:"demo.gif" input

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

let redirect fd target fn =
  let saved = Unix.dup fd in
  (match target with Some t -> Unix.dup2 t fd | None -> Unix.close fd);
  Fun.protect
    ~finally:(fun () ->
      Unix.dup2 saved fd;
      Unix.close saved)
    fn

let captured path fn =
  let err = Unix.openfile (path "err") [ O_WRONLY; O_TRUNC; O_CREAT ] 0o644 in
  let r =
    Fun.protect ~finally:(fun () -> Unix.close err) @@ fun () ->
    redirect Unix.stderr (Some err) fn
  in
  (r, In_channel.with_open_bin (path "err") In_channel.input_all)

let test_write_errors () =
  with_files [ ("demo.t", 0o644, "  $ echo hi\n"); ("err", 0o644, "") ]
  @@ fun env path ->
  let cli ~stdout () =
    captured path @@ fun () ->
    redirect Unix.stdout stdout @@ fun () ->
    run env ~print:true ~vhs:"vhs" (path "demo.t")
  in
  let r, w = Unix.pipe ~cloexec:true () in
  Unix.close r;
  let result = cli ~stdout:(Some w) () in
  Unix.close w;
  Alcotest.(check (pair int string)) "closed pipe" (0, "") result;
  (* macOS says "Bad file descriptor", Linux "Invalid argument". *)
  let code, msg = cli ~stdout:None () in
  Alcotest.(check int) "closed standard output" 123 code;
  Native_test.check_contains "names standard output"
    ~sub:"k7: standard output: " msg;
  Alcotest.(check int)
    "closed standard error" 123
    ( redirect Unix.stderr None @@ fun () ->
      run env ~print:true ~vhs:"vhs" (path "missing.t") )

let test_error_names_input () =
  with_files [ ("demo.t", 0o644, "  $ echo hi\n"); ("err", 0o644, "") ]
  @@ fun env path ->
  Alcotest.(check (pair int string))
    "message"
    ( 123,
      Fmt.str "k7: %s: executable not found: vhs-no-such-executable\n"
        (path "demo.t") )
    ( captured path @@ fun () ->
      run env ~vhs:"vhs-no-such-executable" (path "demo.t") )

let test_render () =
  with_files
    [
      ("ko", 0o755, fake_vhs 3);
      ("pwd", 0o755, "#!/bin/sh\npwd -P >&2\nexit 4\n");
      ("early", 0o755, "#!/bin/sh\nexit 2\n");
      ( "slow",
        0o755,
        "#!/bin/sh\n\
         trap 'echo > \"$0.stopped\"; exit 7' TERM\n\
         cat > /dev/null\n\
         echo > \"$0.ready\"\n\
         while :; do sleep 0.05; done\n" );
      ( "stubborn",
        0o755,
        "#!/bin/sh\n\
         trap 'echo > \"$0.term\"' TERM\n\
         cat > /dev/null\n\
         echo > \"$0.ready\"\n\
         i=0\n\
         while [ $i -lt 400 ]; do sleep 0.05; i=$((i + 1)); done\n\
         echo > \"$0.done\"\n" );
    ]
  @@ fun env path ->
  let proc = Eio.Stdenv.process_mgr env in
  let mono = Eio.Stdenv.mono_clock env in
  let cwd = Eio.Stdenv.cwd env in
  let t = Result.get_ok (K7.tape ~outputs:[ "x.gif" ] [ K7.sleep (Ms 100) ]) in
  (match K7.render ~vhs:(path "ko") ~clock:mono ~proc ~cwd t with
  | Error (Failed { status = `Exited 3; _ } as e) ->
      expect "both streams"
        (path "ko" ^ " exited with code 3\n  from stdout\n  from stderr")
        (Fmt.str "%a" K7.pp_error e)
  | _ -> Alcotest.fail "lost the failure");
  let dir = Filename.dirname (path "pwd") in
  (match
     K7.render ~vhs:(path "pwd") ~clock:mono ~proc
       ~cwd:Eio.Path.(Eio.Stdenv.fs env / dir)
       t
   with
  | Error (Failed { output; _ }) when output = Unix.realpath dir ^ "\n" -> ()
  | Error e -> Alcotest.failf "not run in %s: %a" dir K7.pp_error e
  | Ok () -> Alcotest.fail "pwd succeeded");
  let big =
    Result.get_ok
      (K7.tape ~outputs:[ "x.gif" ]
         (List.init 20_000 (fun _ -> K7.type' "echo hello")))
  in
  (match K7.render ~vhs:(path "early") ~clock:mono ~proc ~cwd big with
  | Error (Failed { status = `Exited 2; _ }) -> ()
  | Error e -> Alcotest.failf "early exit: %a" K7.pp_error e
  | Ok () -> Alcotest.fail "early exit succeeded");
  (match
     K7.render ~vhs:(path "ko") ~clock:mono ~proc
       ~cwd:Eio.Path.(Eio.Stdenv.fs env / path "missing-dir")
       t
   with
  | Error (Io msg) ->
      expect "missing cwd"
        (path "missing-dir" ^ ": no such file or directory")
        msg
  | Error e -> Alcotest.failf "unexpected %a" K7.pp_error e
  | Ok () -> Alcotest.fail "ran in a missing directory");
  let clock = Eio.Stdenv.clock env in
  (match
     Eio.Fiber.first
       (fun () ->
         `Rendered (K7.render ~vhs:(path "slow") ~clock:mono ~proc ~cwd t))
       (fun () ->
         Eio.Time.with_timeout_exn clock 10. (fun () ->
             while not (Sys.file_exists (path "slow.ready")) do
               Eio.Time.sleep clock 0.01
             done);
         `Timeout)
   with
  | `Timeout -> ()
  | `Rendered _ -> Alcotest.fail "slow vhs finished");
  Alcotest.(check bool)
    "vhs stopped before render returned" true
    (Sys.file_exists (path "slow.stopped"));
  let grace = Eio_mock.Clock.Mono.make () in
  let rec advance () =
    if
      not
        (Sys.file_exists (path "stubborn.term")
        && Eio_mock.Clock.Mono.try_advance grace)
    then (
      Eio.Time.sleep clock 0.01;
      advance ())
  in
  (match
     Eio.Fiber.pair
       (fun () ->
         Eio.Fiber.first
           (fun () ->
             `Rendered
               (K7.render ~vhs:(path "stubborn") ~clock:grace ~proc ~cwd t))
           (fun () ->
             Eio.Time.with_timeout_exn clock 10. (fun () ->
                 while not (Sys.file_exists (path "stubborn.ready")) do
                   Eio.Time.sleep clock 0.01
                 done);
             `Timeout))
       (fun () -> Eio.Time.with_timeout_exn clock 10. advance)
   with
  | `Timeout, () -> ()
  | `Rendered _, () -> Alcotest.fail "stubborn vhs finished");
  Alcotest.(check (pair bool bool))
    "vhs sent SIGTERM, then killed" (true, false)
    ( Sys.file_exists (path "stubborn.term"),
      Sys.file_exists (path "stubborn.done") )

let test_command () =
  let help_buffer = Buffer.create 256 in
  let help = Format.formatter_of_buffer help_buffer in
  let err = Format.formatter_of_buffer (Buffer.create 256) in
  let eval args =
    Cmdliner.Cmd.eval_value ~err ~help
      ~argv:(Array.of_list ("k7" :: "cram" :: args))
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
    [ "--print"; "123"; "PS1"; "U+200B"; "cannot be written"; "--wait-timeout" ]

(* Set by the oracle alias only. *)
let oracle = Sys.getenv_opt "VHS_ORACLE"

let play ~native env t =
  let clock = Eio.Stdenv.mono_clock env and proc = Eio.Stdenv.process_mgr env in
  let cwd = Eio.Stdenv.cwd env in
  if native then K7.record ~clock ~proc ~cwd t
  else K7.render ?vhs:oracle ~clock ~proc ~cwd t

let test_output_order ~native () =
  with_files [] @@ fun env path ->
  let output = path "order.txt" in
  let src =
    "  $ printf 'decoy>'; sleep 3; echo FIRST_DONE\n  $ echo SECOND_DONE\n"
  in
  let cram = Result.get_ok (Cram.parse src) in
  let t =
    Result.get_ok
      (K7.tape
         ~settings:
           K7.
             [
               Shell Bash;
               Width 480;
               Height 240;
               Padding 0;
               Font_size 12;
               Framerate 5;
               Typing_speed (Ms 5);
             ]
         ~outputs:[ output ]
         (Cram.actions ~setup:[] cram @ [ K7.sleep (Ms 3500) ]))
  in
  let result =
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. (fun () ->
        play ~native env t)
  in
  (match result with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%a" K7.pp_error e);
  let screen = Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / output) in
  let position sub =
    let rec scan i =
      if i + String.length sub > String.length screen then None
      else if String.sub screen i (String.length sub) = sub then Some i
      else scan (i + 1)
    in
    scan 0
  in
  match (position "decoy>FIRST_DONE", position "echo SECOND_DONE") with
  | Some first, Some second ->
      Alcotest.(check bool)
        "first output precedes typing the second command" true (first < second)
  | _ -> Alcotest.failf "recording did not show both commands: %s" screen

(* VHS starts bash without history. *)
let test_history ~native () =
  with_files [ ("history.sh", 0o644, "set -o history -H\n") ] @@ fun env path ->
  let output = path "history.txt" in
  let src = "  $ echo one\n  $ echo \"x!!y\"\n" in
  let t =
    Result.get_ok
      (Cram_cmd.tape ~setup:[ path "history.sh" ] ~output ~name:"history.t" src)
  in
  let result =
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 60. (fun () ->
        play ~native env t)
  in
  (match result with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%a" K7.pp_error e);
  let screen = Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / output) in
  if not (contains ~sub:"\nx!!y" screen) then
    Alcotest.failf "history expanded: %s" screen

let record_cram ~native ?(setup = []) env path src =
  let output = path "recording.txt" in
  let cram = Result.get_ok (Cram.parse src) in
  let t =
    Result.get_ok
      (K7.tape
         ~settings:
           K7.
             [
               Shell Bash;
               Width 800;
               Height 400;
               Padding 0;
               Font_size 12;
               Framerate 5;
               Typing_speed (Ms 5);
             ]
         ~outputs:[ output ]
         ((K7.raw "Set WaitTimeout 3s" :: Cram.actions ~setup cram)
         @ [ K7.sleep (Ms 2000) ]))
  in
  (match
     Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 40. (fun () ->
         play ~native env t)
   with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%a" K7.pp_error e);
  Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / output)

let test_setup_prompt_command ~native () =
  with_files [ ("setup.sh", 0o644, "PROMPT_COMMAND='PS1=custom'\n") ]
  @@ fun env path ->
  let screen =
    record_cram ~native ~setup:[ path "setup.sh" ] env path "  $ echo READY\n"
  in
  if not (contains ~sub:"\nREADY" screen) then
    Alcotest.failf "setup prevented the command from running: %s" screen

let test_invisible_output_order ~native () =
  with_files [] @@ fun env path ->
  let src =
    "  $ printf '\\342\\200\\213'; sleep 1; printf '\\nFIRST_DONE\\n'\n\
    \  $ echo SECOND_DONE\n"
  in
  let screen = record_cram ~native env path src in
  let position sub =
    let rec scan i =
      if i + String.length sub > String.length screen then
        Alcotest.failf "recording lacks %S" sub
      else if String.sub screen i (String.length sub) = sub then i
      else scan (i + 1)
    in
    scan 0
  in
  let first = position "\nFIRST_DONE" in
  let second = position "echo SECOND_DONE" in
  Alcotest.(check bool)
    "command output precedes the next input even after U+200B" true
    (first < second)

(* One column short puts the prompt's [>] in the last column. *)
let test_wrapped_prompt ~native () =
  with_files [] @@ fun env path ->
  let screen =
    record_cram ~native env path
      "  $ printf \"%$((COLUMNS-1))s\" x\n  $ echo after\n"
  in
  if not (contains ~sub:"\nafter" screen) then
    Alcotest.failf "the prompt after a full line was missed: %s" screen

let test_unfinished ~native () =
  with_files
    [
      ("demo.t", 0o644, "  $ cat <<EOF\n  > a\n  $ echo after\n");
      ("err", 0o644, "");
    ]
  @@ fun env path ->
  let code, msg =
    captured path @@ fun () ->
    Cram_cmd.run env ~print:false
      ~vhs:(Option.value oracle ~default:"vhs")
      ~setup:[]
      ~output:(path (if native then "out.svg" else "out.gif"))
      (path "demo.t")
  in
  Alcotest.(check int) "exit code" 123 code;
  List.iter
    (fun sub ->
      if not (contains ~sub msg) then
        Alcotest.failf "error lacks %S: %s" sub msg)
    [ "command 1"; "cat <<EOF"; "waiting for more input" ]

let run_err env path ?wait_timeout_s src =
  Eio.Path.save ~create:(`Or_truncate 0o644)
    Eio.Path.(Eio.Stdenv.fs env / path "demo.t")
    src;
  captured path @@ fun () ->
  Cram_cmd.run env ~print:false ~vhs:"vhs" ~setup:[] ?wait_timeout_s
    ~output:(path "out.svg") (path "demo.t")

let check_message msg ~has ~lacks =
  List.iter
    (fun sub ->
      if not (contains ~sub msg) then
        Alcotest.failf "error lacks %S: %s" sub msg)
    has;
  List.iter
    (fun sub ->
      if contains ~sub msg then Alcotest.failf "error has %S: %s" sub msg)
    lacks

let test_native_text () =
  with_files [ ("demo.t", 0o644, "  $ echo native-text\n") ] @@ fun env path ->
  List.iter
    (fun output ->
      let code =
        Cram_cmd.run env ~print:false ~vhs:"vhs-no-such-executable" ~setup:[]
          ~output:(path output) (path "demo.t")
      in
      Alcotest.(check int) output 0 code;
      Native_test.check_contains output ~sub:"native-text"
        (In_channel.with_open_bin (path output) In_channel.input_all))
    [ "out.txt"; "out.ascii"; "out.test" ]

let test_timeout_message () =
  with_files [] @@ fun env path ->
  let code, msg =
    run_err env path ~wait_timeout_s:1 "  $ true\n  $ sleep 5\n"
  in
  Alcotest.(check int) "exit code" 123 code;
  check_message msg
    ~has:[ "command 2"; "sleep 5"; "1 second"; "--wait-timeout" ]
    ~lacks:[ "x{200B}"; "x{2060}"; "Wait+Line"; "\u{200B}" ]

let test_exit_message () =
  with_files [] @@ fun env path ->
  let start = Unix.gettimeofday () in
  let code, msg = run_err env path ~wait_timeout_s:30 "  $ exit 3\n" in
  Alcotest.(check int) "exit code" 123 code;
  Alcotest.(check bool) "at once" true (Unix.gettimeofday () -. start < 20.);
  check_message msg
    ~has:[ "the shell exited (status 3)"; "command 1"; "exit 3" ]
    ~lacks:[ "x{200B}"; "Wait+Line"; "timed out" ]

let test_non_ascii ~native () =
  with_files [] @@ fun env path ->
  let saved = Sys.getenv_opt "LC_ALL" in
  Unix.putenv "LC_ALL" "C";
  let screen =
    Fun.protect ~finally:(fun () ->
        Unix.putenv "LC_ALL" (Option.value saved ~default:""))
    @@ fun () -> record_cram ~native env path "  $ echo caf\xc3\xa9\n"
  in
  if not (contains ~sub:"\ncaf\xc3\xa9\n" screen) then
    Alcotest.failf "typed text lost: %s" screen

let readiness ~native =
  List.map
    (fun (name, fn) -> ((if native then "native " else "") ^ name, fn ~native))
    [
      ("output order", test_output_order);
      ("history", test_history);
      ("setup prompt command", test_setup_prompt_command);
      ("invisible output order", test_invisible_output_order);
      ("unfinished command", test_unfinished);
      ("wrapped prompt", test_wrapped_prompt);
      ("non-ASCII", test_non_ascii);
    ]

let test_run_exec_failure () =
  with_files [ ("not-executable", 0o644, "#!/bin/sh\nexit 0\n") ]
  @@ fun env path ->
  let code, error =
    captured path @@ fun () ->
    Run_cmd.run env ~settings:[] ~env:[] ~output:(path "failed.svg")
      [ path "not-executable" ]
  in
  Alcotest.(check int) "recording failure has documented exit code" 123 code;
  Alcotest.(check bool) "exec error is explained" true (String.trim error <> "")

let test_tape_output_option () =
  with_files [ ("t.tape", 0o644, "Type \"echo hi\"\nEnter\nSleep 1s\n") ]
  @@ fun env path ->
  let code, error =
    captured path @@ fun () ->
    Tape_cmd.run env ~print:true ~vhs:"vhs" ~settings:[] ~env:[]
      ~outputs:[ path "out.txt" ]
      (path "t.tape")
  in
  Alcotest.(check (pair int string)) "printed" (0, "") (code, error)

let test_signal_message () =
  with_files [] @@ fun env path ->
  let code, msg = run_err env path ~wait_timeout_s:30 "  $ kill -9 $$\n" in
  Alcotest.(check int) "exit code" 123 code;
  check_message msg ~has:[ "signal 9"; "command 1" ] ~lacks:[ "signal -" ]

let test_cram_dir () =
  with_files [ ("pwd.t", 0o644, "  $ pwd -P; ls -A | wc -l\n") ]
  @@ fun env path ->
  let shown ?dir () =
    let code =
      Cram_cmd.run env ~print:false ~vhs:"vhs" ~setup:[] ?dir
        ~output:(path "pwd.txt") (path "pwd.t")
    in
    Alcotest.(check int) "exit code" 0 code;
    let text = In_channel.with_open_bin (path "pwd.txt") In_channel.input_all in
    let lines = List.map String.trim (String.split_on_char '\n' text) in
    match List.filter (String.starts_with ~prefix:"/") lines with
    | dir :: _ -> (dir, List.mem "0" lines)
    | [] -> Alcotest.failf "no directory in %S" text
  in
  let fresh, empty = shown () in
  Alcotest.(check bool)
    "not the current directory" false
    (fresh = Unix.realpath (Sys.getcwd ()));
  Alcotest.(check bool) "empty" true empty;
  Alcotest.(check bool) "removed" false (Sys.file_exists fresh);
  let dir = Filename.dirname (path "pwd.t") in
  let given, _ = shown ~dir () in
  expect "--dir" (Unix.realpath dir) given

let interrupt env path ~signal ~ready f =
  let clock = Eio.Stdenv.clock env in
  Eio.Fiber.both
    (fun () -> f ())
    (fun () ->
      Eio.Time.with_timeout_exn clock 20. (fun () ->
          while not (Sys.file_exists (path ready)) do
            Eio.Time.sleep clock 0.01
          done);
      Unix.kill (Unix.getpid ()) signal)

let test_interrupt () =
  with_files
    [ ("wait.t", 0o644, "  $ echo CRAM_$((1 + 1)); touch ready; sleep 30\n") ]
  @@ fun env path ->
  let code = ref 0 in
  Unix.mkdir (path "cram") 0o755;
  interrupt env path ~signal:Sys.sigint ~ready:"ready.run" (fun () ->
      code :=
        Run_cmd.run env ~settings:[] ~env:[] ~output:(path "run.svg")
          [
            "sh";
            "-c";
            Fmt.str "echo RUN_$((1 + 1)); touch %s; sleep 30" (path "ready.run");
          ]);
  Alcotest.(check int) "k7 run, SIGINT" 130 !code;
  Alcotest.(check bool)
    "k7 run image" true
    (contains ~sub:"RUN_2"
       (In_channel.with_open_bin (path "run.svg") In_channel.input_all));
  interrupt env path ~signal:Sys.sigterm ~ready:"cram/ready" (fun () ->
      code :=
        Cram_cmd.run env ~print:false ~vhs:"vhs" ~setup:[]
          ~dir:(Filename.dirname (path "cram/x"))
          ~output:(path "cram.svg") (path "wait.t"));
  Alcotest.(check int) "k7 cram, SIGTERM" 143 !code;
  Alcotest.(check bool)
    "k7 cram output" true
    (contains ~sub:"CRAM_2"
       (In_channel.with_open_bin (path "cram.svg") In_channel.input_all))

let suite =
  ( "cli",
    List.map
      (fun (name, fn) -> Alcotest.test_case name `Quick fn)
      ([
         ("tape", test_tape);
         ("run", test_run);
         ("run reports exec failure", test_run_exec_failure);
         ("write errors", test_write_errors);
         ("errors name the input", test_error_names_input);
         ("render real processes", test_render);
         ("timeout message", test_timeout_message);
         ("native text outputs", test_native_text);
         ("shell exit message", test_exit_message);
         ("tape -o without Output", test_tape_output_option);
         ("killed shell message", test_signal_message);
         ("cram runs in a temporary directory", test_cram_dir);
         ("interrupted recordings", test_interrupt);
         ("command line", test_command);
       ]
      @ readiness ~native:true
      @ if Option.is_some oracle then readiness ~native:false else []) )
