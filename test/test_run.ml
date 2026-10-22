(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Native_test

let run ?vars env cwd argv =
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 60. @@ fun () ->
  Vhs.run ~settings ?env:vars
    ~clock:(Eio.Stdenv.mono_clock env)
    ~proc:(Eio.Stdenv.process_mgr env)
    ~cwd argv

let script cwd name src =
  Eio.Path.save ~create:(`Or_truncate 0o755) Eio.Path.(cwd / name) src

(* The duration of the animation and the time each frame starts, in
   milliseconds, from its keyframes. *)
let times svg =
  let number s i =
    let j = ref i in
    while
      !j < String.length s && (s.[!j] = '.' || Char.Ascii.is_digit s.[!j])
    do
      incr j
    done;
    (float_of_string (String.sub s i (!j - i)), !j)
  in
  let rec find sub i =
    if i + String.length sub > String.length svg then None
    else if String.sub svg i (String.length sub) = sub then Some i
    else find sub (i + 1)
  in
  let duration =
    match find "{animation:s " 0 with
    | Some i -> fst (number svg (i + 13))
    | None -> Alcotest.fail "no animation"
  in
  (* The keyframes are [P%{transform:...}], the last one at [100%]. *)
  let rec keyframes i acc =
    match find "%{transform:translateX(" i with
    | None -> List.rev (List.tl acc)
    | Some j ->
        let k = ref (j - 1) in
        while svg.[!k] = '.' || Char.Ascii.is_digit svg.[!k] do
          decr k
        done;
        let p, _ = number svg (!k + 1) in
        keyframes (j + 1) ((p *. duration /. 100.) :: acc)
  in
  (duration, keyframes 0 [])

(* The program's own screens, without a prompt or a typed command: a
   coloured line, then another half a second later, held for a second after
   the program exits. *)
let test_run () =
  with_dir @@ fun env cwd ->
  script cwd "demo.sh"
    "#!/bin/sh\n\
     printf '\\033[31mred\\033[0m\\n'\n\
     sleep 0.5\n\
     printf 'done\\n'\n\
     exit 3\n";
  match run env cwd [ "./demo.sh" ] with
  | Error e -> Alcotest.failf "%a" Vhs.pp_error e
  | Ok (s, status) ->
      Alcotest.(check bool) "status" true (status = `Exited 3);
      check_contains "red" ~sub:{|class="p1">red|} s;
      let shown = frames s in
      let n = List.length shown in
      let red = screen [ "red" ] and both = screen [ "red"; "done" ] in
      Alcotest.(check (list string))
        "last frames" [ red; both ]
        (List.filteri (fun i _ -> i >= n - 2) shown);
      List.iteri
        (fun i f ->
          if i < n - 2 && f <> screen [] then Alcotest.failf "frame %d: %S" i f)
        shown;
      let duration, starts = times s in
      let last = List.nth starts (List.length starts - 1) in
      if last < 500. || last > 5000. then
        Alcotest.failf "the second line shows at %gms" last;
      if duration -. last < 1000. || duration -. last > 1040. then
        Alcotest.failf "the last screen holds %gms" (duration -. last)

(* A program that prints and exits at once still shows all it printed. *)
let test_fast () =
  with_dir @@ fun env cwd ->
  Eio.Path.save ~create:(`Or_truncate 0o644)
    Eio.Path.(cwd / "out")
    "one\ntwo\nthree\n";
  match run env cwd [ "cat"; "out" ] with
  | Error e -> Alcotest.failf "%a" Vhs.pp_error e
  | Ok (s, status) ->
      Alcotest.(check bool) "status" true (status = `Exited 0);
      Alcotest.(check string)
        "image"
        (screen [ "one"; "two"; "three" ])
        (last (frames s))

let test_env () =
  with_dir @@ fun env cwd ->
  match
    run
      ~vars:[ ("VHS_TEST", "caf\xc3\xa9 au lait") ]
      env cwd
      [ "sh"; "-c"; {|printf '%s|%s' "$VHS_TEST" "$TERM"|} ]
  with
  | Error e -> Alcotest.failf "%a" Vhs.pp_error e
  | Ok (s, status) ->
      Alcotest.(check bool) "status" true (status = `Exited 0);
      Alcotest.(check string)
        "environment"
        (screen [ "caf\xc3\xa9 au lait|xterm-256color" ])
        (last (frames s))

let test_errors () =
  with_dir @@ fun env cwd ->
  let error name ?vars argv check =
    match run ?vars env cwd argv with
    | Error e when check e -> ()
    | Error e -> Alcotest.failf "%s: %a" name Vhs.pp_error e
    | Ok _ -> Alcotest.failf "%s: ran" name
  in
  let invalid = function Vhs.Invalid _ -> true | _ -> false in
  error "no command" [] invalid;
  error "not found" [ "vhs-no-such-program" ] (function
    | Executable_not_found "vhs-no-such-program" -> true
    | _ -> false);
  error "not found relative to cwd" [ "./demo.sh" ] (function
    | Executable_not_found _ -> true
    | _ -> false);
  error "empty name" ~vars:[ ("", "x") ] [ "true" ] invalid;
  error "name with =" ~vars:[ ("A=B", "x") ] [ "true" ] invalid;
  error "NUL" ~vars:[ ("A", "x\000y") ] [ "true" ] invalid;
  match
    Vhs.run ~settings:[ Width 800; Columns 80 ]
      ~clock:(Eio.Stdenv.mono_clock env)
      ~proc:(Eio.Stdenv.process_mgr env)
      ~cwd [ "true" ]
  with
  | Error (Invalid _) -> ()
  | Error e -> Alcotest.failf "width and columns: %a" Vhs.pp_error e
  | Ok _ -> Alcotest.fail "width and columns: ran"

(* A killed program still gives its image. *)
let test_signal () =
  with_dir @@ fun env cwd ->
  match run env cwd [ "sh"; "-c"; "echo bye; kill -TERM $$" ] with
  | Error e -> Alcotest.failf "%a" Vhs.pp_error e
  | Ok (s, status) ->
      Alcotest.(check bool) "status" true (status = `Signaled Sys.sigterm);
      Alcotest.(check string) "image" (screen [ "bye" ]) (last (frames s))

(* Cancelling [run] stops the program. *)
let test_cancel () =
  with_dir @@ fun env cwd ->
  let clock = Eio.Stdenv.clock env in
  let pid_file = Eio.Path.(cwd / "run.pid") in
  let ready () =
    match String.trim (Eio.Path.load pid_file) with
    | "" -> false
    | _ -> true
    | exception Eio.Io _ -> false
  in
  let result =
    Eio.Fiber.first
      (fun () ->
        `Ran (run env cwd [ "sh"; "-c"; "echo $$ > run.pid; exec sleep 30" ]))
      (fun () ->
        while not (ready ()) do
          Eio.Time.sleep clock 0.001
        done;
        `Cancelled)
  in
  (match result with
  | `Cancelled -> ()
  | `Ran _ -> Alcotest.fail "the program ended before cancellation");
  let pid = int_of_string (String.trim (Eio.Path.load pid_file)) in
  match Unix.kill pid 0 with
  | () ->
      (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
      Alcotest.failf "process %d left alive" pid
  | exception Unix.Unix_error (ESRCH, _, _) -> ()

let suite =
  ( "run",
    List.map
      (fun (name, fn) -> Alcotest.test_case name `Quick fn)
      [
        ("run", test_run);
        ("fast exit", test_fast);
        ("environment", test_env);
        ("errors", test_errors);
        ("signal", test_signal);
        ("cancel", test_cancel);
      ] )
