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
    {|Output "demo.gif"
Set Shell "bash"
Hide
Env VHS_CRAM_PROMPT `__vhs_n=; __vhs_prompt() { PS1='\[\e[38;2;90;86;224m\]> \[\342\200\213\342\201\240\342\201\240\342\200\213\342\200\213\342\201\240\342\200\213\342\200\213\342\201\240\342\201\240\342\200\213\342\200\213\342\201\240\342\201\240\342\200\213\342\201\240\e[0m\]'; __vhs_n=$__vhs_n'\342\200\213'; PS2='> \[\342\200\213\342\201\240\342\201\240\342\200\213\342\200\213\342\201\240\342\200\213\342\200\213\342\201\240\342\201\240\342\200\213\342\200\213\342\201\240\342\201\240\342\200\213\342\201\240'$__vhs_n'\]'; }; __vhs=$(printf '%s\n' ${PROMPT_COMMAND+"${PROMPT_COMMAND[@]}"} __vhs_prompt); unset PROMPT_COMMAND; PROMPT_COMMAND=$__vhs; unset __vhs VHS_CRAM_PROMPT`
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
  (* Typed into bash, a tab completes and a newline continues the line, so
     neither reaches [source] as part of the path. *)
  List.iter
    (fun path ->
      match tape ~setup:[ path ] "  $ true\n" with
      | Error _ -> ()
      | Ok s -> Alcotest.failf "accepted %S" s)
    [ "bad\000setup"; "a\tb.sh"; "a\nb.sh"; "a\xc2\x85b.sh"; "\xff.sh" ]

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

(* Where [run] writes cannot fail with an exception: a reader that stops
   reading the tape is not an error, and another write error is reported in
   one line. *)
let test_write_errors () =
  with_files [ ("demo.t", 0o644, "  $ echo hi\n"); ("err", 0o644, "") ]
  @@ fun env path ->
  let stderr () = In_channel.with_open_bin (path "err") In_channel.input_all in
  let cli ~stdout () =
    let err = Unix.openfile (path "err") [ O_WRONLY; O_TRUNC ] 0 in
    Fun.protect ~finally:(fun () -> Unix.close err) @@ fun () ->
    redirect Unix.stderr (Some err) @@ fun () ->
    redirect Unix.stdout stdout @@ fun () ->
    run env ~print:true ~vhs:"vhs" (path "demo.t")
  in
  let r, w = Unix.pipe ~cloexec:true () in
  Unix.close r;
  let code = cli ~stdout:(Some w) () in
  Unix.close w;
  Alcotest.(check (pair int string)) "closed pipe" (0, "") (code, stderr ());
  let code = cli ~stdout:None () in
  Alcotest.(check (pair int string))
    "closed standard output"
    (123, "vhs-cram: standard output: Bad file descriptor\n")
    (code, stderr ());
  Alcotest.(check int)
    "closed standard error" 123
    ( redirect Unix.stderr None @@ fun () ->
      run env ~print:true ~vhs:"vhs" (path "missing.t") )

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
  let t =
    Result.get_ok (Vhs.tape ~outputs:[ "x.gif" ] [ Vhs.sleep (Ms 100) ])
  in
  (match Vhs.render ~vhs:(path "ko") ~clock:mono ~proc ~cwd t with
  | Error (Failed { status = `Exited 3; _ } as e) ->
      expect "both streams"
        (path "ko" ^ " exited with code 3\n  from stdout\n  from stderr")
        (Fmt.str "%a" Vhs.pp_error e)
  | _ -> Alcotest.fail "lost the failure");
  (match
     Vhs.render ~vhs:(path "pwd") ~clock:mono ~proc
       ~cwd:Eio.Path.(Eio.Stdenv.fs env / "/")
       t
   with
  | Error (Failed { output = "/\n"; _ }) -> ()
  | Error e -> Alcotest.failf "not run in /: %a" Vhs.pp_error e
  | Ok () -> Alcotest.fail "pwd succeeded");
  (* A vhs that exits without reading a tape larger than a pipe buffer is
     reported by its status, not by the broken pipe. *)
  let big =
    Result.get_ok
      (Vhs.tape ~outputs:[ "x.gif" ]
         (List.init 20_000 (fun _ -> Vhs.type' "echo hello")))
  in
  (match Vhs.render ~vhs:(path "early") ~clock:mono ~proc ~cwd big with
  | Error (Failed { status = `Exited 2; _ }) -> ()
  | Error e -> Alcotest.failf "early exit: %a" Vhs.pp_error e
  | Ok () -> Alcotest.fail "early exit succeeded");
  (match
     Vhs.render ~vhs:(path "ko") ~clock:mono ~proc
       ~cwd:Eio.Path.(Eio.Stdenv.fs env / path "missing-dir")
       t
   with
  | Error (Io msg) ->
      expect "missing cwd"
        (path "missing-dir" ^ ": no such file or directory")
        msg
  | Error e -> Alcotest.failf "unexpected %a" Vhs.pp_error e
  | Ok () -> Alcotest.fail "ran in a missing directory");
  (* Cancelling render stops vhs with SIGTERM and waits for it. *)
  let clock = Eio.Stdenv.clock env in
  (match
     Eio.Fiber.first
       (fun () ->
         `Rendered (Vhs.render ~vhs:(path "slow") ~clock:mono ~proc ~cwd t))
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
               (Vhs.render ~vhs:(path "stubborn") ~clock:grace ~proc ~cwd t))
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
    [ "--print"; "123"; "PS1"; "U+200B"; "cannot be written" ]

(* The vhs executable of the tests that run it, from [VHS_ORACLE]: the
   oracle alias sets it, runtest does not. *)
let oracle = Sys.getenv_opt "VHS_ORACLE"

(* [play ~native env t] records [t] natively if [native], or with the vhs
   of [oracle]. *)
let play ~native env t =
  let clock = Eio.Stdenv.mono_clock env and proc = Eio.Stdenv.process_mgr env in
  if native then
    let cwd = Eio.Path.(Eio.Stdenv.fs env / ".") in
    Result.map ignore (Vhs.record ~clock ~proc ~cwd t)
  else Vhs.render ?vhs:oracle ~clock ~proc ~cwd:(Eio.Stdenv.cwd env) t

let test_output_order ~native () =
  with_files [] @@ fun env path ->
  let output = path "order.txt" in
  let src =
    "  $ printf 'decoy>'; sleep 3; echo FIRST_DONE\n  $ echo SECOND_DONE\n"
  in
  let cram = Result.get_ok (Cram.parse src) in
  let t =
    Result.get_ok
      (Vhs.tape
         ~settings:
           Vhs.
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
         (Cram.actions ~setup:[] cram @ [ Vhs.sleep (Ms 3500) ]))
  in
  let result =
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. (fun () ->
        play ~native env t)
  in
  (match result with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%a" Vhs.pp_error e);
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
      (Cli.tape ~setup:[ path "history.sh" ] ~output ~name:"history.t" src)
  in
  let result =
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 60. (fun () ->
        play ~native env t)
  in
  (match result with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%a" Vhs.pp_error e);
  let screen = Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / output) in
  if not (contains ~sub:"\nx!!y" screen) then
    Alcotest.failf "history expanded: %s" screen

let record_cram ~native ?(setup = []) env path src =
  let output = path "recording.txt" in
  let cram = Result.get_ok (Cram.parse src) in
  let t =
    Result.get_ok
      (Vhs.tape
         ~settings:
           Vhs.
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
         ((Vhs.raw "Set WaitTimeout 3s" :: Cram.actions ~setup cram)
         @ [ Vhs.sleep (Ms 2000) ]))
  in
  (match
     Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 40. (fun () ->
         play ~native env t)
   with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%a" Vhs.pp_error e);
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
  let err = Unix.openfile (path "err") [ O_WRONLY; O_TRUNC ] 0 in
  let code =
    Fun.protect ~finally:(fun () -> Unix.close err) @@ fun () ->
    redirect Unix.stderr (Some err) @@ fun () ->
    Cli.run env ~print:false
      ~vhs:(Option.value oracle ~default:"vhs")
      ~setup:[]
      ~output:(path (if native then "out.svg" else "out.txt"))
      (path "demo.t")
  in
  let msg = In_channel.with_open_bin (path "err") In_channel.input_all in
  Alcotest.(check int) "exit code" 123 code;
  List.iter
    (fun sub ->
      if not (contains ~sub msg) then
        Alcotest.failf "error lacks %S: %s" sub msg)
    [ "command 1"; "cat <<EOF"; "waiting for more input" ]

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
    ]

let suite =
  ( "cli",
    List.map
      (fun (name, fn) -> Alcotest.test_case name `Quick fn)
      ([
         ("tape", test_tape);
         ("run", test_run);
         ("write errors", test_write_errors);
         ("render real processes", test_render);
         ("command line", test_command);
       ]
      @ readiness ~native:true
      @ if Option.is_some oracle then readiness ~native:false else []) )
