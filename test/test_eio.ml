(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let expect = Alcotest.(check string)

let tape actions =
  match K7.tape ~outputs:[ "demo.gif" ] (actions @ [ K7.sleep (Ms 100) ]) with
  | Ok t -> t
  | Error e -> Alcotest.failf "%a" K7.pp_error e

let str t = Fmt.str "%a" K7.pp t

(* Eio_mock has no pipe. [""] ends the stream. *)
module Pipe = struct
  module Source = struct
    type t = { chunks : string Eio.Stream.t; mutable pending : string }

    let read_methods = []

    let single_read t dst =
      if t.pending = "" then t.pending <- Eio.Stream.take t.chunks;
      if t.pending = "" then (
        Eio.Stream.add t.chunks "";
        raise End_of_file);
      let n = min (Cstruct.length dst) (String.length t.pending) in
      Cstruct.blit_from_string t.pending 0 dst 0 n;
      t.pending <- String.sub t.pending n (String.length t.pending - n);
      n

    let close _ = ()
  end

  module Sink = struct
    type t = string Eio.Stream.t

    let single_write t bufs =
      let s = Cstruct.to_string (Cstruct.concat bufs) in
      if s <> "" then Eio.Stream.add t s;
      String.length s

    let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src
    let close t = Eio.Stream.add t ""
  end

  let v () =
    let chunks = Eio.Stream.create max_int in
    let source =
      Eio.Resource.T
        ( Source.{ chunks; pending = "" },
          Eio.Resource.handler
            (H (Eio.Resource.Close, Source.close)
            :: Eio.Resource.bindings (Eio.Flow.Pi.source (module Source))) )
    in
    let sink =
      Eio.Resource.T
        ( chunks,
          Eio.Resource.handler
            (H (Eio.Resource.Close, Sink.close)
            :: Eio.Resource.bindings (Eio.Flow.Pi.sink (module Sink))) )
    in
    (source, sink)
end

(* In the test's switch, so that it outlives a cancelled [render]. *)
module Child = struct
  type t = {
    status : Eio.Process.exit_status Eio.Promise.t;
    on_signal : int -> unit;
  }

  type tag = [ `Generic ]

  let pid _ = 1
  let await t = Eio.Promise.await t.status
  let signal t n = t.on_signal n
end

type body = {
  run :
    stdin:Eio.Flow.source_ty Eio.Resource.t ->
    output:Eio.Flow.sink_ty Eio.Resource.t ->
    Eio.Process.exit_status;
  on_signal : int -> unit;
}

module Manager = struct
  type tag = [ `Generic ]

  type t = {
    sw : Eio.Switch.t;
    inherited : string array;
    inspect_env : string array option -> unit;
    start : Eio.Fs.dir_ty Eio.Path.t option -> string list -> body;
  }

  let environment t = t.inherited

  let getenv_opt t key =
    let prefix = key ^ "=" in
    Array.find_map
      (fun entry ->
        if String.starts_with ~prefix entry then
          Some
            (String.sub entry (String.length prefix)
               (String.length entry - String.length prefix))
        else None)
      t.inherited

  let pipe _ ~sw:_ =
    let r, w = Pipe.v () in
    ( (r :> [ Eio.Resource.close_ty | Eio.Flow.source_ty ] Eio.Resource.t),
      (w :> [ Eio.Resource.close_ty | Eio.Flow.sink_ty ] Eio.Resource.t) )

  let spawn t ~sw:_ ?cwd ?stdin ?stdout ?stderr ?env ?executable args =
    t.inspect_env env;
    Alcotest.(check bool)
      "executable selected by argv" true
      (Option.is_none executable);
    let output = Option.get stdout in
    Alcotest.(check bool)
      "stdout and stderr captured together" true
      (Option.get stderr == output);
    let body = t.start cwd args in
    let status, resolve = Eio.Promise.create () in
    Eio.Fiber.fork ~sw:t.sw (fun () ->
        Eio.Promise.resolve resolve (body.run ~stdin:(Option.get stdin) ~output));
    let child = Child.{ status; on_signal = body.on_signal } in
    Eio.Resource.T (child, Eio.Process.Pi.process (module Child))
end

let manager ?(inherited = [||])
    ?(inspect_env =
      fun env ->
        Alcotest.(check bool) "environment given" true (Option.is_some env)) ~sw
    start =
  Eio.Resource.T
    ( Manager.{ sw; start; inherited; inspect_env },
      Eio.Process.Pi.mgr (module Manager) )

let cwd = (Eio.Resource.T ((), Eio.Resource.handler []), "mock-cwd")
let read_all flow = Eio.Buf_read.(take_all (of_flow flow ~max_size:1_000_000))
let clock () = Eio_mock.Clock.Mono.make ()

let run ?vhs ?(status = `Exited 3) writes t =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let proc =
    manager ~sw (fun dir args ->
        Alcotest.(check (list string))
          "argv"
          [ Option.value ~default:"vhs" vhs; "-q" ]
          args;
        expect "cwd" "mock-cwd" (snd (Option.get dir));
        let run ~stdin ~output =
          expect "stdin equals pp" (str t) (read_all stdin);
          List.iter
            (fun s ->
              Eio.Fiber.yield ();
              Eio.Flow.copy_string s output)
            writes;
          status
        in
        { run; on_signal = (fun _ -> Alcotest.fail "signalled") })
  in
  K7.record ~engine:Vhs ?vhs ~clock:(clock ()) ~proc ~cwd t

let failed expected = function
  | Error (K7.Vhs_failed { output; status = `Exited 3; _ }) ->
      expect "output" expected output
  | Error e -> Alcotest.failf "unexpected error: %a" K7.pp_error e
  | Ok () -> Alcotest.fail "expected child failure"

let test_byte_writes () =
  let text = "failure without newline" in
  failed text
    (run
       (List.init (String.length text) (fun i -> String.make 1 text.[i]))
       (tape [ K7.raw "# hello" ]))

let test_split_utf8 () =
  let euro = "\xe2\x82\xac" in
  failed
    ("cost " ^ euro ^ "\n")
    (run
       [ "cost " ^ String.sub euro 0 1; String.sub euro 1 2 ^ "\n" ]
       (tape []))

let test_long_output () =
  let line i = Fmt.str "line %07d\n" i in
  let writes = List.init 100_000 line in
  match run writes (tape []) with
  | Error (K7.Vhs_failed { output; _ }) ->
      Alcotest.(check bool)
        "at most 64 KiB kept" true
        (String.length output <= 65536);
      Alcotest.(check bool)
        "the end is kept" true
        (String.ends_with ~suffix:(line 99_999) output)
  | _ -> Alcotest.fail "expected child failure"

let test_success () =
  match
    run ~status:(`Exited 0) ~vhs:"custom-vhs" [ "progress" ]
      (tape [ K7.type' "echo hello\n" ])
  with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%a" K7.pp_error e

let test_signal () =
  match run ~status:(`Signaled 9) [ "killed" ] (tape []) with
  | Error
      (K7.Vhs_failed { vhs = "vhs"; status = `Signaled 9; output = "killed" })
    ->
      ()
  | _ -> Alcotest.fail "lost signal, executable, or output"

let test_pp_error () =
  let output =
    String.concat "\n"
      (List.init 12 (fun i -> Fmt.str "\027[31mline %d\027[0m" i))
  in
  expect "last lines, indented"
    "vhs exited with code 1\n\
    \  line 2\n\
    \  line 3\n\
    \  line 4\n\
    \  line 5\n\
    \  line 6\n\
    \  line 7\n\
    \  line 8\n\
    \  line 9\n\
    \  line 10\n\
    \  line 11"
    (Fmt.str "%a" K7.pp_error
       (Vhs_failed { vhs = "vhs"; status = `Exited 1; output = output ^ "\n\n" }))

let test_pp_error_controls () =
  let output =
    "\027]8;;https://x\027\\link\027]8;;\027\\ end\n\
     \027]0;title\007a\027Pq#0\027\\b\027(Bc\027=d\n\
     bell\007 back\bspace\ttab\n\
     \xc2\x9b31mred\xc2\x9b0m \xc2\x9d0;t\xc2\x9cu \xc2\x85v\n\
     10%\r50%\rdone\n\
     \xc2\007\x9b\xff\n\
     \027"
  in
  expect "printable"
    "vhs exited with code 1\n\
    \  link end\n\
    \  abcd\n\
    \  bell backspace tab\n\
    \  red u v\n\
    \  10%\n\
    \  50%\n\
    \  done\n\
    \  \xef\xbf\xbd"
    (Fmt.str "%a" K7.pp_error
       (Vhs_failed { vhs = "vhs"; status = `Exited 1; output }))

let test_pp_error_names () =
  let pp e = Fmt.str "%a" K7.pp_error e in
  expect "executable" "executable not found: ab"
    (pp (Executable_not_found "a\027]0;x\007b"));
  expect "failed" "ab exited with code 1"
    (pp (Vhs_failed { vhs = "a\xc2\x9b2Jb"; status = `Exited 1; output = "" }));
  expect "io" "dir x: no such file or directory"
    (pp (Io "dir\nx\027[0m: no such file or directory"));
  expect "format characters"
    "timed out waiting for line 3, Wait+Line /x/; last value was: > ab; give \
     the wait a longer ~timeout or the tape a longer Wait_timeout"
    (pp
       (Timeout
          {
            wait = "Wait+Line /x/";
            line = 3;
            last = "> a\u{200B}\u{2060}b\u{FEFF}\u{200B}";
          }));
  expect "variation selectors" "dir \u{2764}\u{FE0F} x"
    (pp (Io "\u{FE0F}dir\n\u{FE0F}\u{2764}\u{FE0F}\u{FE0F} \u{E0100}x"));
  expect "shell exit"
    "the shell exited (status 3) while waiting for line 3, Wait; last value \
     was: >"
    (pp (Exited { status = `Exited 3; wait = "Wait"; line = 3; last = ">" }));
  expect "shell killed"
    "the shell was killed by signal 9 while waiting for line 3, Wait; last \
     value was: >"
    (pp (Exited { status = `Signaled 9; wait = "Wait"; line = 3; last = ">" }));
  expect "OCaml's signal"
    "the shell was killed by signal 9 while waiting for line 3, Wait"
    (pp
       (Exited
          { status = `Signaled Sys.sigkill; wait = "Wait"; line = 3; last = "" }))

let test_spawn_errors () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  List.iter
    (fun (error, expected) ->
      let proc = manager ~sw (fun _ _ -> raise (Eio.Process.err error)) in
      match K7.record ~engine:Vhs ~clock:(clock ()) ~proc ~cwd (tape []) with
      | Error e ->
          expect "mapped spawn error" expected (Fmt.str "%a" K7.pp_error e)
      | Ok () -> Alcotest.fail "lost spawn failure")
    [
      (Executable_not_found "vhs", "executable not found: vhs");
      (Permission_denied "vhs", "vhs: permission denied");
      (Executable_format_error "vhs", "vhs: not an executable");
    ]

let test_svg_refused () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let proc = manager ~sw (fun _ _ -> Alcotest.fail "vhs ran") in
  List.iter
    (fun outputs ->
      let t =
        match K7.tape ~outputs [ K7.sleep (Ms 100) ] with
        | Ok t -> t
        | Error e -> Alcotest.failf "%a" K7.pp_error e
      in
      match K7.record ~engine:Vhs ~clock:(clock ()) ~proc ~cwd t with
      | Error (Unsupported _ as e) ->
          expect "names the output and record"
            {|not recorded by vhs: Output "demo.svg": an SVG image, which the Native engine records|}
            (Fmt.str "%a" K7.pp_error e)
      | Error e -> Alcotest.failf "%a" K7.pp_error e
      | Ok () -> Alcotest.fail "an SVG output rendered")
    [ [ "demo.svg" ]; [ "demo.gif"; "demo.svg" ]; [ "demo.svg"; "demo.txt" ] ];
  let t =
    match
      K7.tape ~outputs:[ "demo.gif" ]
        [ K7.raw {|Screenshot "shot.svg"|}; K7.sleep (Ms 100) ]
    with
    | Ok t -> t
    | Error e -> Alcotest.failf "%a" K7.pp_error e
  in
  match K7.record ~engine:Vhs ~clock:(clock ()) ~proc ~cwd t with
  | Error (Unsupported _ as e) ->
      expect "names the screenshot and record"
        {|not recorded by vhs: Screenshot "shot.svg": an SVG image, which the Native engine takes|}
        (Fmt.str "%a" K7.pp_error e)
  | Error e -> Alcotest.failf "%a" K7.pp_error e
  | Ok () -> Alcotest.fail "an SVG screenshot rendered"

let test_cancel () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let term, stop = Eio.Promise.create () in
  let exited = ref false in
  let proc =
    manager ~sw (fun _ _ ->
        let run ~stdin ~output:_ =
          ignore (read_all stdin);
          let status = Eio.Promise.await term in
          exited := true;
          status
        in
        let on_signal n =
          Alcotest.(check int) "SIGTERM" Sys.sigterm n;
          Eio.Promise.resolve stop (`Signaled n)
        in
        { run; on_signal })
  in
  (match
     Eio.Fiber.first
       (fun () ->
         `Rendered
           (K7.record ~engine:Vhs ~clock:(clock ()) ~proc ~cwd (tape [])))
       (fun () -> `Cancelled)
   with
  | `Cancelled -> ()
  | `Rendered _ -> Alcotest.fail "render finished");
  Alcotest.(check bool) "child exited before render returned" true !exited

let test_cancel_kill () =
  Eio_mock.Backend.run_full @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let killed, kill = Eio.Promise.create () in
  let signals = ref [] in
  let proc =
    manager ~sw (fun _ _ ->
        let run ~stdin ~output:_ =
          ignore (read_all stdin);
          Eio.Promise.await killed
        in
        let on_signal n =
          signals := n :: !signals;
          if Int.equal n Sys.sigkill then Eio.Promise.resolve kill (`Signaled n)
        in
        { run; on_signal })
  in
  let clock = env#mono_clock in
  (match
     Eio.Fiber.first
       (fun () -> `Rendered (K7.record ~engine:Vhs ~clock ~proc ~cwd (tape [])))
       (fun () -> `Cancelled)
   with
  | `Cancelled -> ()
  | `Rendered _ -> Alcotest.fail "render finished");
  Alcotest.(check (list int))
    "SIGTERM, then SIGKILL"
    [ Sys.sigterm; Sys.sigkill ]
    (List.rev !signals);
  Alcotest.(check int64)
    "ten seconds apart" 10_000_000_000L
    (Mtime.to_uint64_ns (Eio.Time.Mono.now clock))

let test_publish_environment () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let inherited =
    [|
      "VHS_PUBLISH=true";
      "PATH=/bin";
      "TERM=dumb";
      "rod=show";
      "PS1=$ ";
      "PROMPT=% ";
      "PROMPT_COMMAND=date";
      "VHS_NO_SANDBOX=1";
    |]
  in
  let inspect_env env =
    let effective = Option.value ~default:inherited env in
    Alcotest.(check bool)
      "recording cannot silently publish" false
      (Array.mem "VHS_PUBLISH=true" effective);
    Alcotest.(check (list string))
      "only the variables that change the recording are cleared, then env"
      [ "GREETING=hi"; "PATH=/bin"; "TERM=xterm"; "VHS_NO_SANDBOX=1" ]
      (List.sort compare (Array.to_list effective))
  in
  let proc =
    manager ~sw ~inherited ~inspect_env (fun _ _ ->
        {
          run =
            (fun ~stdin ~output:_ ->
              ignore (read_all stdin);
              `Exited 0);
          on_signal = (fun _ -> ());
        })
  in
  let env = [ ("GREETING", "hi"); ("TERM", "xterm") ] in
  (match K7.record ~engine:Vhs ~env ~clock:(clock ()) ~proc ~cwd (tape []) with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%a" K7.pp_error e);
  match
    K7.record ~env:[ ("A=B", "x") ] ~clock:(clock ()) ~proc ~cwd (tape [])
  with
  | Error (Invalid msg) ->
      expect "invalid name" {|env "A=B=x": a name with =|} msg
  | Ok () -> Alcotest.fail "recorded"
  | Error e -> Alcotest.failf "%a" K7.pp_error e

let test_vhs_path_override () =
  Native_test.with_dir @@ fun env cwd ->
  let name = "k7-r14-path-vhs" in
  let executable = Eio.Path.(cwd / name) in
  Eio.Path.save ~create:(`Or_truncate 0o755) executable
    "#!/bin/sh\n/bin/cat > /dev/null\nprintf started > launched\n";
  let vars = [ ("PATH", Eio.Path.native_exn cwd) ] in
  let record vhs =
    K7.record ~engine:Vhs ~vhs ~env:vars
      ~clock:(Eio.Stdenv.mono_clock env)
      ~proc:(Eio.Stdenv.process_mgr env)
      ~cwd (tape [])
  in
  (match record (Eio.Path.native_exn executable) with
  | Ok () -> ()
  | Error e -> Alcotest.failf "absolute path: %a" K7.pp_error e);
  Eio.Path.unlink Eio.Path.(cwd / "launched");
  (match record name with
  | Ok () -> ()
  | Error e -> Alcotest.failf "PATH override: %a" K7.pp_error e);
  expect "the executable on the supplied PATH ran" "started"
    (Eio.Path.load Eio.Path.(cwd / "launched"))

let suite =
  ( "eio",
    List.map
      (fun (name, fn) -> Alcotest.test_case name `Quick fn)
      [
        ("output one byte at a time", test_byte_writes);
        ("output split UTF-8", test_split_utf8);
        ("long output", test_long_output);
        ("success and explicit executable", test_success);
        ("signaled child", test_signal);
        ("error lines", test_pp_error);
        ("error controls", test_pp_error_controls);
        ("error names", test_pp_error_names);
        ("spawn errors", test_spawn_errors);
        ("SVG output refused", test_svg_refused);
        ("cancel", test_cancel);
        ("cancel a vhs ignoring SIGTERM", test_cancel_kill);
        ("publish environment", test_publish_environment);
        ("vhs searches overridden PATH", test_vhs_path_override);
      ] )
