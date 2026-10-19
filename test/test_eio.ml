let expect = Alcotest.(check string)
let str = Fmt.str "%a" Vhs.pp

(* Eio_mock has no process manager. This provider only connects the flows
   passed by render and returns the scripted child status. *)
module Child = struct
  type t = unit -> Eio.Process.exit_status
  type tag = [ `Generic ]

  let pid _ = 1
  let await fn = fn ()
  let signal _ _ = ()
end

module Manager = struct
  type tag = [ `Generic ]

  type t = {
    start :
      Eio.Fs.dir_ty Eio.Path.t option ->
      Eio.Flow.source_ty Eio.Resource.t option ->
      Eio.Flow.sink_ty Eio.Resource.t option ->
      string list ->
      unit ->
      Eio.Process.exit_status;
  }

  let environment _ = [||]
  let getenv_opt _ _ = None
  let pipe _ ~sw:_ = failwith "render must not request a pipe"

  let spawn t ~sw:_ ?cwd ?stdin ?stdout ?stderr ?env ?executable args =
    Alcotest.(check bool) "stdout inherited" true (Option.is_none stdout);
    Alcotest.(check bool) "environment inherited" true (Option.is_none env);
    Alcotest.(check bool)
      "executable selected by argv" true
      (Option.is_none executable);
    let child = t.start cwd stdin stderr args in
    Eio.Resource.T (child, Eio.Process.Pi.process (module Child))
end

let manager start =
  Eio.Resource.T (Manager.{ start }, Eio.Process.Pi.mgr (module Manager))

(* render only forwards this capability; no directory operations are needed. *)
let cwd = (Eio.Resource.T ((), Eio.Resource.handler []), "mock-cwd")
let read_all flow = Eio.Buf_read.(take_all (of_flow flow ~max_size:1_000_000))

let run ?vhs ?(status = `Exited 3) ?(delay = false) actions tape =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  let input = ref "" in
  let proc =
    manager (fun dir stdin stderr args ->
        Alcotest.(check (list string))
          "argv"
          [ Option.value ~default:"vhs" vhs ]
          args;
        let dir = Option.get dir in
        expect "cwd" "mock-cwd" (snd dir);
        Alcotest.(check bool)
          "cwd capability forwarded" true
          (fst dir == fst cwd);
        input := read_all (Option.get stdin);
        let source = Eio_mock.Flow.make "child stderr" in
        Eio_mock.Flow.on_read source actions;
        fun () ->
          if delay then Eio.Time.sleep clock 2.;
          Eio.Flow.copy source (Option.get stderr);
          status)
  in
  let result =
    if delay then
      Eio.Fiber.pair
        (fun () -> Vhs.render ?vhs ~proc ~cwd tape)
        (fun () ->
          Eio.Fiber.yield ();
          Eio_mock.Clock.set_time clock 2.)
      |> fst
    else Vhs.render ?vhs ~proc ~cwd tape
  in
  expect "stdin equals pp" (str tape) !input;
  result

let failed expected = function
  | Error (Vhs.Failed { stderr; status = `Exited 3; _ }) ->
      expect "stderr" expected stderr
  | Error e -> Alcotest.failf "unexpected error: %a" Vhs.pp_error e
  | Ok () -> Alcotest.fail "expected child failure"

let test_byte_reads () =
  let text = "failure without newline" in
  let actions =
    List.init (String.length text) (fun i -> `Return (String.make 1 text.[i]))
  in
  failed text (run (actions @ [ `Raise End_of_file ]) [ Vhs.comment "hello" ])

let test_split_utf8 () =
  let b = Buffer.create 4 in
  Buffer.add_utf_8_uchar b (Uchar.of_int 0x20ac);
  let euro = Buffer.contents b in
  let text = "cost " ^ euro ^ "\n" in
  failed text
    (run
       [
         `Return ("cost " ^ String.sub euro 0 1);
         `Return (String.sub euro 1 2 ^ "\n");
         `Raise End_of_file;
       ]
       [])

let test_mid_line () =
  failed "first\nunfinished"
    (run [ `Return "first\nunfin"; `Return "ished"; `Raise End_of_file ] [])

let test_read_error () =
  let err = Eio.Fs.err (Permission_denied Eio_mock.Simulated_failure) in
  match run [ `Return "partial"; `Raise err ] [] with
  | Error (Vhs.Io msg) ->
      expect "I/O diagnostic"
        (Fmt.str "%a" Eio.Exn.pp_err
           (Eio.Fs.E (Permission_denied Eio_mock.Simulated_failure)))
        msg
  | Error e -> Alcotest.failf "unexpected error: %a" Vhs.pp_error e
  | Ok () -> Alcotest.fail "lost read failure"

let test_success () =
  match
    run ~status:(`Exited 0) ~vhs:"custom-vhs"
      [ `Return "warning"; `Raise End_of_file ]
      [ Vhs.blank; Vhs.type' "echo hello"; Vhs.key Enter ]
  with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%a" Vhs.pp_error e

let test_signal () =
  match
    run ~status:(`Signaled 9) [ `Return "killed"; `Raise End_of_file ] []
  with
  | Error (Vhs.Failed { vhs = "vhs"; status = `Signaled 9; stderr = "killed" })
    ->
      ()
  | _ -> Alcotest.fail "lost signal, executable, or stderr"

let test_spawn_errors () =
  Eio_mock.Backend.run @@ fun () ->
  List.iter
    (fun (error, expected) ->
      let proc = manager (fun _ _ _ _ -> raise (Eio.Process.err error)) in
      match Vhs.render ~proc ~cwd [] with
      | Error e ->
          expect "mapped spawn error" expected (Fmt.str "%a" Vhs.pp_error e)
      | Ok () -> Alcotest.fail "lost spawn failure")
    [
      (Executable_not_found "vhs", "executable not found: vhs");
      (Permission_denied "vhs", "vhs: permission denied");
      (Executable_format_error "vhs", "vhs: not an executable");
    ]

let test_delayed_child () =
  failed "late failure"
    (run ~delay:true [ `Return "late failure"; `Raise End_of_file ] [])

let suite =
  ( "eio",
    List.map
      (fun (name, fn) -> Alcotest.test_case name `Quick fn)
      [
        ("stderr one byte at a time", test_byte_reads);
        ("stderr split UTF-8", test_split_utf8);
        ("stderr EOF mid-line", test_mid_line);
        ("stderr read raises", test_read_error);
        ("success and explicit executable", test_success);
        ("signaled child", test_signal);
        ("spawn errors", test_spawn_errors);
        ("delayed child with mock clock", test_delayed_child);
      ] )
