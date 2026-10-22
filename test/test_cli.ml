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
Env VHS_CRAM_PROMPT `bind 'set input-meta on'; bind 'set output-meta on'; bind 'set convert-meta off'; __vhs_n=; __vhs_prompt() { PS1='\[\e[38;2;90;86;224m\]> \[\342\200\213\342\201\240\342\201\240\342\200\213\342\200\213\342\201\240\342\200\213\342\200\213\342\201\240\342\201\240\342\200\213\342\200\213\342\201\240\342\201\240\342\200\213\342\201\240\e[0m\]'; __vhs_n=$__vhs_n'\342\200\213'; PS2='> \[\342\200\213\342\201\240\342\201\240\342\200\213\342\200\213\342\201\240\342\200\213\342\200\213\342\201\240\342\201\240\342\200\213\342\200\213\342\201\240\342\201\240\342\200\213\342\201\240'$__vhs_n'\]'; }; __vhs=$(printf '%s\n' ${PROMPT_COMMAND+"${PROMPT_COMMAND[@]}"} __vhs_prompt); unset PROMPT_COMMAND; PROMPT_COMMAND=$__vhs; unset __vhs VHS_CRAM_PROMPT`
Type 'eval "$VHS_CRAM_PROMPT"; clear'
Enter
Wait+Line /\x{200B}\x{2060}\x{2060}\x{200B}\x{200B}\x{2060}\x{200B}\x{200B}\x{2060}\x{2060}\x{200B}\x{200B}\x{2060}\x{2060}\x{200B}\x{2060}$/
Show
Type "true"
Enter
Wait+Line /\x{200B}\x{2060}\x{2060}\x{200B}\x{200B}\x{2060}\x{200B}\x{200B}\x{2060}\x{2060}\x{200B}\x{200B}\x{2060}\x{2060}\x{200B}\x{2060}$/
Sleep 1000ms
|}
    (Result.get_ok (tape "  $ true\n"));
  (* A wait times out after 15 seconds unless the command line says. *)
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
  (* Typed into bash, a tab completes and a newline continues the line, so
     neither reaches [source] as part of the path. *)
  List.iter
    (fun path ->
      match tape ~setup:[ path ] "  $ true\n" with
      | Error msg ->
          Alcotest.(check bool)
            "names the setup path" true
            (String.starts_with ~prefix:"setup path " msg)
      | Ok s -> Alcotest.failf "accepted %S" s)
    [ "bad\000setup"; "a\tb.sh"; "a\nb.sh"; "a\xc2\x85b.sh"; "\xff.sh" ]

(* Real processes: [run] owns the whole Eio environment. A shell script
   stands in for vhs. *)
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

(* [redirect fd target fn] runs [fn] with [fd] replaced by [target], or
   closed if [target] is [None]. *)
let redirect fd target fn =
  let saved = Unix.dup fd in
  (match target with Some t -> Unix.dup2 t fd | None -> Unix.close fd);
  Fun.protect
    ~finally:(fun () ->
      Unix.dup2 saved fd;
      Unix.close saved)
    fn

(* [captured path fn] is [fn ()] and what it wrote on standard error, which
   goes to the file [path "err"]. *)
let captured path fn =
  let err = Unix.openfile (path "err") [ O_WRONLY; O_TRUNC; O_CREAT ] 0o644 in
  let r =
    Fun.protect ~finally:(fun () -> Unix.close err) @@ fun () ->
    redirect Unix.stderr (Some err) fn
  in
  (r, In_channel.with_open_bin (path "err") In_channel.input_all)

(* Where [run] writes cannot fail with an exception: a reader that stops
   reading the tape is not an error, and another write error is reported in
   one line. *)
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
  (* The reason is the system's: "Bad file descriptor" on macOS, "Invalid
     argument" on Linux. *)
  let code, msg = cli ~stdout:None () in
  Alcotest.(check int) "closed standard output" 123 code;
  Native_test.check_contains "names standard output"
    ~sub:"k7: standard output: " msg;
  Alcotest.(check int)
    "closed standard error" 123
    ( redirect Unix.stderr None @@ fun () ->
      run env ~print:true ~vhs:"vhs" (path "missing.t") )

(* A failure that is not about a command names the cram test. *)
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
  (* A directory other than the test's, where the output can be written. *)
  let dir = Filename.dirname (path "pwd") in
  (match
     K7.render ~vhs:(path "pwd") ~clock:mono ~proc
       ~cwd:Eio.Path.(Eio.Stdenv.fs env / dir)
       t
   with
  | Error (Failed { output; _ }) when output = Unix.realpath dir ^ "\n" -> ()
  | Error e -> Alcotest.failf "not run in %s: %a" dir K7.pp_error e
  | Ok () -> Alcotest.fail "pwd succeeded");
  (* A vhs that exits without reading a tape larger than a pipe buffer is
     reported by its status, not by the broken pipe. *)
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
  (* Cancelling render stops vhs with SIGTERM and waits for it. *)
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
  (* A vhs that ignores SIGTERM is killed once the grace period is over. *)
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

(* The vhs executable of the tests that run it, from [VHS_ORACLE]: the
   oracle alias sets it, runtest does not. *)
let oracle = Sys.getenv_opt "VHS_ORACLE"

(* [play ~native env t] records [t] natively if [native], or with the vhs
   of [oracle]. *)
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
  (* The command line holds [FIRST_DONE] too; its output follows [decoy>]. *)
  match (position "decoy>FIRST_DONE", position "echo SECOND_DONE") with
  | Some first, Some second ->
      Alcotest.(check bool)
        "first output precedes typing the second command" true (first < second)
  | _ -> Alcotest.failf "recording did not show both commands: %s" screen

(* VHS starts bash without history, and a typed [!] is not expanded even
   after a setup turns history expansion back on. *)
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

(* A setup may install bash's usual dynamic prompt hook. The recorder must
   establish its own prompt after the hook has run, before waiting for it. *)
let test_setup_prompt_command ~native () =
  with_files [ ("setup.sh", 0o644, "PROMPT_COMMAND='PS1=custom'\n") ]
  @@ fun env path ->
  let screen =
    record_cram ~native ~setup:[ path "setup.sh" ] env path "  $ echo READY\n"
  in
  if not (contains ~sub:"\nREADY" screen) then
    Alcotest.failf "setup prevented the command from running: %s" screen

(* The invisible prompt marker is also ordinary output from user programs. *)
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

(* Output one column short of the width puts the prompt's [>] in the last
   column, and its space on the next line, under the cursor. *)
let test_wrapped_prompt ~native () =
  with_files [] @@ fun env path ->
  let screen =
    record_cram ~native env path
      "  $ printf \"%$((COLUMNS-1))s\" x\n  $ echo after\n"
  in
  if not (contains ~sub:"\nafter" screen) then
    Alcotest.failf "the prompt after a full line was missed: %s" screen

(* An unterminated here-document leaves bash at its continuation prompt,
   where no wait can end; the error names the command that did. *)
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

(* [run_err env path ~wait_timeout_s src] runs k7 cram natively on [src]
   and is its exit code and what it printed on standard error. *)
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

(* k7 cram records the text outputs natively too, without vhs. *)
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

(* A command that does not give the prompt back in time is named, with the
   option that waits longer, and the wait is in words, without the prompt
   mark. *)
let test_timeout_message () =
  with_files [] @@ fun env path ->
  let code, msg =
    run_err env path ~wait_timeout_s:1 "  $ true\n  $ sleep 5\n"
  in
  Alcotest.(check int) "exit code" 123 code;
  check_message msg
    ~has:[ "command 2"; "sleep 5"; "1 second"; "--wait-timeout" ]
    ~lacks:[ "x{200B}"; "x{2060}"; "Wait+Line"; "\u{200B}" ]

(* A shell that exits fails the recording at once, naming the command. *)
let test_exit_message () =
  with_files [] @@ fun env path ->
  let start = Unix.gettimeofday () in
  let code, msg = run_err env path ~wait_timeout_s:30 "  $ exit 3\n" in
  Alcotest.(check int) "exit code" 123 code;
  Alcotest.(check bool) "at once" true (Unix.gettimeofday () -. start < 20.);
  check_message msg
    ~has:[ "the shell exited (status 3)"; "command 1"; "exit 3" ]
    ~lacks:[ "x{200B}"; "Wait+Line"; "timed out" ]

(* Without a UTF-8 locale, bash's readline still takes and shows typed
   non-ASCII text. *)
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

(* The tests of the readiness of the prompt, with each recorder. *)
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

let suite =
  ( "cli",
    List.map
      (fun (name, fn) -> Alcotest.test_case name `Quick fn)
      ([
         ("tape", test_tape);
         ("run", test_run);
         ("write errors", test_write_errors);
         ("errors name the input", test_error_names_input);
         ("render real processes", test_render);
         ("timeout message", test_timeout_message);
         ("native text outputs", test_native_text);
         ("shell exit message", test_exit_message);
         ("command line", test_command);
       ]
      @ readiness ~native:true
      @ if Option.is_some oracle then readiness ~native:false else []) )
