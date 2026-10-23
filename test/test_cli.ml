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
  Play_cram.tape ~setup ~outputs:[ output ] ?wait_timeout_s ~name:"input.t" src
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
    (contains ~sub:"\nSet WaitTimeout 60000ms\n"
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

(* The commands run on a {!Native_test.with_clock} clock. *)
let with_files files fn =
  Eio_main.run @@ fun env ->
  Native_test.with_clock @@ fun clock ->
  let env = Eio_unix.Stdenv.override env ~mono_clock:clock in
  let dir = Filename.temp_dir "k7-cli" "" in
  let path name = Filename.concat dir name in
  List.iter
    (fun (name, perm, contents) ->
      if perm land 0o111 <> 0 then Native_test.executable (path name) contents
      else
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
  Play_cram.run env ~env:Native_test.no_job_control ~dry_run:false ~print ~vhs
    ~setup:[] ~outputs:[ "demo.gif" ] input

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
      Fmt.str
        "k7: %s: executable not found: vhs-no-such-executable; a GIF or a \
         video needs Charm's VHS, see \
         https://github.com/charmbracelet/vhs#installation, and an .svg output \
         does not\n"
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
  let mono = Eio_mock.Clock.Mono.make () in
  let cwd = Eio.Stdenv.cwd env in
  let t = Result.get_ok (K7.tape ~outputs:[ "x.gif" ] [ K7.sleep (Ms 100) ]) in
  (match K7.record ~engine:Vhs ~vhs:(path "ko") ~clock:mono ~proc ~cwd t with
  | Error (Vhs_failed { status = `Exited 3; _ } as e) ->
      expect "both streams"
        (path "ko" ^ " exited with code 3\n  from stdout\n  from stderr")
        (Fmt.str "%a" K7.pp_error e)
  | _ -> Alcotest.fail "lost the failure");
  let dir = Filename.dirname (path "pwd") in
  (match
     K7.record ~engine:Vhs ~vhs:(path "pwd") ~clock:mono ~proc
       ~cwd:Eio.Path.(Eio.Stdenv.fs env / dir)
       t
   with
  | Error (Vhs_failed { output; _ }) when output = Unix.realpath dir ^ "\n" ->
      ()
  | Error e -> Alcotest.failf "not run in %s: %a" dir K7.pp_error e
  | Ok () -> Alcotest.fail "pwd succeeded");
  let big =
    Result.get_ok
      (K7.tape ~outputs:[ "x.gif" ]
         (List.init 20_000 (fun _ -> K7.type' "echo hello")))
  in
  (match
     K7.record ~engine:Vhs ~vhs:(path "early") ~clock:mono ~proc ~cwd big
   with
  | Error (Vhs_failed { status = `Exited 2; _ }) -> ()
  | Error e -> Alcotest.failf "early exit: %a" K7.pp_error e
  | Ok () -> Alcotest.fail "early exit succeeded");
  (match
     K7.record ~engine:Vhs ~vhs:(path "ko") ~clock:mono ~proc
       ~cwd:Eio.Path.(Eio.Stdenv.fs env / path "missing-dir")
       t
   with
  | Error (Io msg) ->
      expect "missing cwd"
        (path "missing-dir" ^ ": no such file or directory")
        msg
  | Error e -> Alcotest.failf "unexpected %a" K7.pp_error e
  | Ok () -> Alcotest.fail "ran in a missing directory");
  (match
     Eio.Fiber.first
       (fun () ->
         `Rendered
           (K7.record ~engine:Vhs ~vhs:(path "slow") ~clock:mono ~proc ~cwd t))
       (fun () ->
         Native_test.until (fun () -> Sys.file_exists (path "slow.ready"));
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
      Eio.Fiber.yield ();
      advance ())
  in
  (match
     Eio.Fiber.pair
       (fun () ->
         Eio.Fiber.first
           (fun () ->
             `Rendered
               (K7.record ~engine:Vhs ~vhs:(path "stubborn") ~clock:grace ~proc
                  ~cwd t))
           (fun () ->
             Native_test.until (fun () ->
                 Sys.file_exists (path "stubborn.ready"));
             `Timeout))
       advance
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
      ~argv:(Array.of_list ("k7" :: "play" :: args))
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
    [
      "--print";
      "123";
      "PS1";
      "U+200B";
      "cannot be written";
      "--wait-timeout";
      ".cast";
      "--show";
    ]

(* Set by the oracle alias only. *)
let oracle = Sys.getenv_opt "VHS_ORACLE"

let play ~native env t =
  let clock = Eio.Stdenv.mono_clock env and proc = Eio.Stdenv.process_mgr env in
  let cwd = Eio.Stdenv.cwd env in
  if native then
    K7.record ~engine:Native ~env:Native_test.no_job_control ~clock ~proc ~cwd t
  else K7.record ~engine:Vhs ?vhs:oracle ~clock ~proc ~cwd t

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
         (Cram.actions ~setup:[] cram @ [ K7.sleep (Ms 500) ]))
  in
  (match play ~native env t with
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
      (Play_cram.tape
         ~setup:[ path "history.sh" ]
         ~outputs:[ output ] ~name:"history.t" src)
  in
  (match play ~native env t with
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
               Wait_timeout (Ms 3000);
             ]
         ~outputs:[ output ]
         (Cram.actions ~setup cram @ [ K7.sleep (Ms 500) ]))
  in
  (match play ~native env t with
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
    Play_cram.run env ~env:Native_test.no_job_control ~dry_run:false
      ~print:false
      ~vhs:(Option.value oracle ~default:"vhs")
      ~setup:[]
      ~outputs:[ path (if native then "out.svg" else "out.gif") ]
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
  Play_cram.run env ~env:Native_test.no_job_control ~dry_run:false ~print:false
    ~vhs:"vhs" ~setup:[] ?wait_timeout_s
    ~outputs:[ path "out.svg" ]
    (path "demo.t")

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
        Play_cram.run env ~env:Native_test.no_job_control ~dry_run:false
          ~print:false ~vhs:"vhs-no-such-executable" ~setup:[]
          ~outputs:[ path output ]
          (path "demo.t")
      in
      Alcotest.(check int) output 0 code;
      Native_test.check_contains output ~sub:"native-text"
        (In_channel.with_open_bin (path output) In_channel.input_all))
    [ "out.txt"; "out.ascii"; "out.test" ]

(* The waits stand still until the second command runs. *)
let test_timeout_message () =
  with_files [] @@ fun env path ->
  let started = path "started" in
  let code, msg =
    Native_test.with_clock ~release:(fun () -> Sys.file_exists started)
    @@ fun clock ->
    run_err
      (Eio_unix.Stdenv.override env ~mono_clock:clock)
      path ~wait_timeout_s:2
      (Fmt.str "  $ true\n  $ touch %s; sleep 5\n" (Filename.quote started))
  in
  Alcotest.(check int) "exit code" 123 code;
  check_message msg
    ~has:[ "command 2"; "sleep 5"; "2 seconds"; "--wait-timeout" ]
    ~lacks:[ "x{200B}"; "x{2060}"; "Wait+Line"; "\u{200B}" ]

let test_tape_timeout () =
  with_files [] @@ fun env path ->
  let tape = path "t.tape" in
  Out_channel.with_open_bin tape (fun oc ->
      Fmt.kstr
        (Out_channel.output_string oc)
        "Output %S\nType \"echo hi\"\nEnter\nWait@1s /never/\n" (path "t.svg"));
  let code, msg =
    captured path @@ fun () ->
    Play_tape.run env ~dry_run:false ~print:false ~vhs:"vhs" ~settings:[]
      ~env:Native_test.no_job_control ~outputs:[] tape
  in
  Alcotest.(check int) "exit code" 123 code;
  check_message msg
    ~has:[ "line 4"; "Wait@1s /never/"; "Set WaitTimeout" ]
    ~lacks:[ "line 3"; "~timeout" ];
  let code, msg =
    captured path @@ fun () ->
    Play_tape.run env ~dry_run:false ~print:false ~vhs:"vhs" ~settings:[]
      ~env:Native_test.no_job_control
      ~outputs:[ path "a.svg"; path "b.gif" ]
      tape
  in
  Alcotest.(check int) "mixed outputs" 123 code;
  check_message msg ~has:[ "a.svg"; "k7" ] ~lacks:[ "K7."; "~" ]

let test_exit_message () =
  with_files [] @@ fun env path ->
  let code, msg = run_err env path ~wait_timeout_s:30 "  $ exit 3\n" in
  Alcotest.(check int) "exit code" 123 code;
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

let test_tab ~native () =
  with_files [] @@ fun env path ->
  let screen =
    record_cram ~native env path "  $ x='a\tb'; echo \"length ${#x}\"\n"
  in
  if not (contains ~sub:"\nlength 3\n" screen) then
    Alcotest.failf "tab not typed: %s" screen

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
      ("tab", test_tab);
    ]

let test_run_exec_failure () =
  with_files [ ("not-executable", 0o644, "#!/bin/sh\nexit 0\n") ]
  @@ fun env path ->
  let code, error =
    captured path @@ fun () ->
    Rec_cmd.run env ~settings:[] ~env:[]
      ~outputs:[ path "failed.svg" ]
      [ path "not-executable" ]
  in
  Alcotest.(check int) "recording failure has documented exit code" 123 code;
  Alcotest.(check bool) "exec error is explained" true (String.trim error <> "")

let test_tape_output_option () =
  with_files [ ("t.tape", 0o644, "Type \"echo hi\"\nEnter\nSleep 1s\n") ]
  @@ fun env path ->
  let code, error =
    captured path @@ fun () ->
    Play_tape.run env ~dry_run:false ~print:true ~vhs:"vhs" ~settings:[]
      ~env:Native_test.no_job_control
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
      Play_cram.run env ~env:Native_test.no_job_control ~dry_run:false
        ~print:false ~vhs:"vhs" ~setup:[] ?dir ~settings:[ Columns 1000 ]
        ~outputs:[ path "pwd.txt" ]
        (path "pwd.t")
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

let interrupt path ~signal ~ready f =
  Eio.Fiber.both
    (fun () -> f ())
    (fun () ->
      Native_test.until (fun () -> Sys.file_exists (path ready));
      Unix.kill (Unix.getpid ()) signal)

let test_interrupt () =
  with_files
    [ ("wait.t", 0o644, "  $ echo CRAM_$((1 + 1)); touch ready; sleep 30\n") ]
  @@ fun env path ->
  let code = ref 0 in
  Unix.mkdir (path "cram") 0o755;
  interrupt path ~signal:Sys.sigint ~ready:"ready.run" (fun () ->
      code :=
        Rec_cmd.run env ~settings:[] ~env:[]
          ~outputs:[ path "run.svg" ]
          [
            "sh";
            "-c";
            Fmt.str "echo RUN_$((1 + 1)); touch %s; sleep 30" (path "ready.run");
          ]);
  Alcotest.(check int) "k7 rec, SIGINT" 130 !code;
  Alcotest.(check bool)
    "k7 rec image" true
    (contains ~sub:"RUN_2"
       (In_channel.with_open_bin (path "run.svg") In_channel.input_all));
  interrupt path ~signal:Sys.sigterm ~ready:"cram/ready" (fun () ->
      code :=
        Play_cram.run env ~env:Native_test.no_job_control ~dry_run:false
          ~print:false ~vhs:"vhs" ~setup:[]
          ~dir:(Filename.dirname (path "cram/x"))
          ~outputs:[ path "cram.svg" ]
          (path "wait.t"));
  Alcotest.(check int) "k7 play, SIGTERM" 143 !code;
  Alcotest.(check bool)
    "k7 play output" true
    (contains ~sub:"CRAM_2"
       (In_channel.with_open_bin (path "cram.svg") In_channel.input_all))

let test_dry_run_directory_output () =
  with_files [ ("input.tape", 0o644, "Sleep 100ms\n") ] @@ fun env path ->
  Unix.mkdir (path "occupied.svg") 0o755;
  let status, error =
    captured path @@ fun () ->
    Play_cmd.run env ~dry_run:true ~print:false ~vhs:"vhs" ~settings:[] ~env:[]
      ~outputs:[ path "occupied.svg" ]
      ~setup:[] ~dir:None ~wait_timeout:None
      [ path "input.tape" ]
  in
  Alcotest.(check int) "dry-run rejects a directory destination" 123 status;
  Native_test.check_contains "names the destination" ~sub:"occupied.svg" error

(* k7 play --help: -o writes "in place of the Output lines of a tape". *)
let test_output_option_replaces_late_output () =
  with_files [ ("err", 0o644, "") ] @@ fun env path ->
  let src =
    Fmt.str "Type \"echo hi\"\nEnter\nOutput %S\nSleep 100ms\n"
      (path "late.txt")
  in
  Eio.Path.save ~create:(`Or_truncate 0o644)
    Eio.Path.(Eio.Stdenv.fs env / path "late.tape")
    src;
  let play ~dry_run output =
    captured path @@ fun () ->
    Play_tape.run env ~dry_run ~print:false ~vhs:"vhs" ~settings:[]
      ~env:Native_test.no_job_control
      ~outputs:[ path output ]
      (path "late.tape")
  in
  Alcotest.(check (pair int string))
    "-o out.txt replaces the Output line" (0, "")
    (play ~dry_run:true "out.txt");
  Alcotest.(check (pair int string))
    "-o out.svg records" (0, "")
    (play ~dry_run:false "out.svg");
  Alcotest.(check bool)
    "the replaced Output line is not written" false
    (Sys.file_exists (path "late.txt"))

(* k7 play and K7.record create the missing directories of an output, as VHS
   does; k7 rec takes the same -o. *)
let test_rec_creates_output_directory () =
  with_files [ ("err", 0o644, "") ] @@ fun env path ->
  let output = path "new/dir/echo.svg" in
  let code, error =
    captured path @@ fun () ->
    Rec_cmd.run env ~settings:[] ~env:[] ~outputs:[ output ]
      [ "/bin/echo"; "hi" ]
  in
  Alcotest.(check (pair int string)) "recorded" (0, "") (code, error);
  Alcotest.(check bool) "image written" true (Sys.file_exists output)

(* A tape with a text output plays natively, which refuses pwsh; vhs
   starts it. *)
let test_engine () =
  with_files
    [
      ("pwsh.tape", 0o644, "Set Shell \"pwsh\"\nSleep 100ms\n");
      ("err", 0o644, "");
    ]
  @@ fun env path ->
  let play ?engine () =
    captured path @@ fun () ->
    Play_cmd.run env ?engine ~dry_run:true ~print:false ~vhs:"vhs" ~settings:[]
      ~env:[]
      ~outputs:[ path "out.txt" ]
      ~setup:[] ~dir:None ~wait_timeout:None
      [ path "pwsh.tape" ]
  in
  Alcotest.(check int) "native refuses pwsh" 123 (fst (play ()));
  Alcotest.(check (pair int string))
    "vhs starts pwsh" (0, "") (play ~engine:K7.Vhs ());
  Alcotest.(check int)
    "--engine native refuses a GIF" 123
    (fst
       ( captured path @@ fun () ->
         Play_cmd.run env ~engine:K7.Native ~dry_run:true ~print:false
           ~vhs:"vhs" ~settings:[] ~env:[]
           ~outputs:[ path "out.gif" ]
           ~setup:[] ~dir:None ~wait_timeout:None
           [ path "pwsh.tape" ] ))

(* k7 play draws a .cast, in any case, as K7.svg_of_casts does, byte for
   byte, and writes its last screen as K7.text_of_casts does; --dry-run writes
   nothing and the options of a shell are refused. *)
let test_cast () =
  let cast =
    "{\"version\": 2, \"width\": 20, \"height\": 3}\n\
     [0.1, \"o\", \"hello\\r\\n\"]\n\
     [0.25, \"o\", \"world\"]\n"
  in
  with_files [ ("hello.CAST", 0o644, cast); ("err", 0o644, "") ]
  @@ fun env path ->
  let play ?engine ?(dry_run = false) ?(env_vars = []) ?(setup = []) ?show
      ?delay output =
    captured path @@ fun () ->
    Play_cmd.run env ?engine ~dry_run ~print:false ~settings:[] ~env:env_vars
      ~outputs:[ path output ]
      ~setup ~dir:None ~wait_timeout:None ?show ?delay
      [ path "hello.CAST" ]
  in
  let read file = In_channel.with_open_bin (path file) In_channel.input_all in
  Alcotest.(check (pair int string)) "drawn" (0, "") (play "a.svg");
  Alcotest.(check (pair int string))
    "drawn with a command line" (0, "")
    (play ~show:[ "make" ] ~delay:(Ms 250) "b.svg");
  let c = Result.get_ok (K7.cast_of_string cast) in
  expect "as K7.svg_of_casts"
    (Result.get_ok (K7.svg_of_casts [ c ]))
    (read "a.svg");
  expect "with --show and --delay"
    (Result.get_ok (K7.svg_of_casts ~show:[ "make" ] ~delay:(Ms 250) [ c ]))
    (read "b.svg");
  Alcotest.(check (pair int string)) "the last screen" (0, "") (play "a.TXT");
  expect "as K7.text_of_casts"
    (Result.get_ok (K7.text_of_casts [ c ]))
    (read "a.TXT");
  Alcotest.(check (pair int string))
    "dry run" (0, "")
    (play ~dry_run:true "c.svg");
  Alcotest.(check bool)
    "dry run writes nothing" false
    (Sys.file_exists (path "c.svg"));
  let refused name ~sub (code, error) =
    Alcotest.(check int) name 123 code;
    Native_test.check_contains name ~sub error
  in
  refused "--engine" ~sub:"--engine applies to a tape or a cram test"
    (play ~engine:K7.Native "d.svg");
  refused "--env" ~sub:"--env applies to a tape or a cram test"
    (play ~env_vars:[ ("A", "1") ] "d.svg");
  refused "--setup" ~sub:"--setup applies to a cram test (.t)"
    (play ~setup:[ path "err" ] "d.svg");
  refused "a GIF" ~sub:"not .svg or .txt, the outputs of an asciicast recording"
    (play "d.gif");
  Alcotest.(check bool)
    "refused, nothing written" false
    (Sys.file_exists (path "d.svg"))

(* k7 rec writes each output by its extension, in any case, from one
   recording on a terminal, and the cram test from a run on pipes; the image
   is the one k7 play draws of the recording. *)
let test_rec_outputs () =
  with_files [ ("err", 0o644, "") ] @@ fun env path ->
  let code, error =
    captured path @@ fun () ->
    Rec_cmd.run env
      ~settings:K7.[ Columns 40; Rows 3 ]
      ~env:[]
      ~outputs:[ path "a.svg"; path "a.cast"; path "a.TXT"; path "a.t" ]
      [ "sh"; "-c"; "printf hi; exit 2" ]
  in
  Alcotest.(check (pair int string)) "the program's code" (2, "") (code, error);
  let read file = In_channel.with_open_bin (path file) In_channel.input_all in
  let cast = Result.get_ok (K7.cast_of_string (read "a.cast")) in
  expect "k7 play draws the same image"
    (Result.get_ok (K7.svg_of_casts [ cast ]))
    (read "a.svg");
  expect "the last screen" "$ sh -c 'printf hi; exit 2'\nhi\n" (read "a.TXT");
  expect "the cram test" "  $ sh -c 'printf hi; exit 2'\n  hi\n  [2]\n"
    (read "a.t");
  let code, error =
    captured path @@ fun () ->
    Rec_cmd.run env ~settings:[] ~env:[]
      ~outputs:[ path "b.cast"; path "b.gif" ]
      [ "true" ]
  in
  Alcotest.(check int) "refused" 123 code;
  Native_test.check_contains "the outputs of k7 rec"
    ~sub:"b.gif\": not .svg, .cast, .txt or .t, the outputs k7 rec writes" error;
  Alcotest.(check bool)
    "refused before it runs" false
    (Sys.file_exists (path "b.cast"))

(* k7 play draws several recordings one after the other in one image, as
   K7.svg_of_casts does, and refuses several files of another kind. *)
let test_casts () =
  let cast theme =
    Fmt.str
      "{\"version\": 3, \"term\": {\"cols\": 24, \"rows\": 4}, \"command\": \
       \"demo --theme %s\"}\n\
       [0.1, \"o\", \"%s\\r\\n\"]\n"
      theme theme
  in
  with_files
    [
      ("dark.cast", 0o644, cast "dark");
      ("light.cast", 0o644, cast "light");
      ("demo.t", 0o644, "  $ true\n");
    ]
  @@ fun env path ->
  let play inputs output =
    captured path @@ fun () ->
    Play_cmd.run env ~dry_run:false ~print:false ~settings:[] ~env:[]
      ~outputs:[ path output ]
      ~setup:[] ~dir:None ~wait_timeout:None (List.map path inputs)
  in
  let read file = In_channel.with_open_bin (path file) In_channel.input_all in
  Alcotest.(check (pair int string))
    "drawn" (0, "")
    (play [ "dark.cast"; "light.cast" ] "both.svg");
  let parse t = Result.get_ok (K7.cast_of_string (cast t)) in
  expect "as K7.svg_of_casts"
    (Result.get_ok (K7.svg_of_casts [ parse "dark"; parse "light" ]))
    (read "both.svg");
  let code, error = play [ "dark.cast"; "demo.t" ] "x.svg" in
  Alcotest.(check int) "refused" 123 code;
  Native_test.check_contains "several cram tests"
    ~sub:"demo.t: several files play only as asciicast recordings (.cast)" error

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
         ("tape timeout names the file's line", test_tape_timeout);
         ("native text outputs", test_native_text);
         ("shell exit message", test_exit_message);
         ("tape -o without Output", test_tape_output_option);
         ("killed shell message", test_signal_message);
         ("cram runs in a temporary directory", test_cram_dir);
         ("interrupted recordings", test_interrupt);
         ("command line", test_command);
         ("dry-run rejects directory outputs", test_dry_run_directory_output);
         ( "-o replaces a late Output line",
           test_output_option_replaces_late_output );
         ("rec creates the output directory", test_rec_creates_output_directory);
         ("rec outputs", test_rec_outputs);
         ("engine", test_engine);
         ("asciicast recordings", test_cast);
         ("several asciicast recordings", test_casts);
       ]
      @ readiness ~native:true
      @ if Option.is_some oracle then readiness ~native:false else []) )
