(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let expect = Alcotest.(check string)

let tape actions =
  match Vhs.tape ~outputs:[ "demo.gif" ] actions with
  | Ok t -> t
  | Error e -> Alcotest.fail e

let str t = Fmt.str "%a" Vhs.pp t

(* A child process whose body runs in a fiber of the test's switch. *)
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
    start : Eio.Fs.dir_ty Eio.Path.t option -> string list -> body;
  }

  let environment _ = [||]
  let getenv_opt _ _ = None
  let pipe _ ~sw:_ = failwith "render must not request a pipe"

  let spawn t ~sw:_ ?cwd ?stdin ?stdout ?stderr ?env ?executable args =
    Alcotest.(check bool) "environment inherited" true (Option.is_none env);
    Alcotest.(check bool)
      "executable selected by argv" true
      (Option.is_none executable);
    Alcotest.(check bool) "stdout inherited" true (Option.is_none stdout);
    let output = Option.get stderr in
    let body = t.start cwd args in
    let status, resolve = Eio.Promise.create () in
    Eio.Fiber.fork ~sw:t.sw (fun () ->
        Eio.Promise.resolve resolve (body.run ~stdin:(Option.get stdin) ~output));
    let child = Child.{ status; on_signal = body.on_signal } in
    Eio.Resource.T (child, Eio.Process.Pi.process (module Child))
end

let manager ~sw start =
  Eio.Resource.T (Manager.{ sw; start }, Eio.Process.Pi.mgr (module Manager))

(* render only forwards this capability; no directory operations are needed. *)
let cwd = (Eio.Resource.T ((), Eio.Resource.handler []), "mock-cwd")
let read_all flow = Eio.Buf_read.(take_all (of_flow flow ~max_size:1_000_000))

(* [run ~status writes t] renders [t] with a child that checks its arguments
   and its standard input, writes [writes] and exits with [status]. *)
let run ?vhs ?(status = `Exited 3) writes t =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let proc =
    manager ~sw (fun dir args ->
        Alcotest.(check (list string))
          "argv"
          [ Option.value ~default:"vhs" vhs ]
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
  Vhs.render ?vhs ~proc ~cwd t

let failed expected = function
  | Error (Vhs.Failed { stderr; status = `Exited 3; _ }) ->
      expect "stderr" expected stderr
  | Error e -> Alcotest.failf "unexpected error: %a" Vhs.pp_error e
  | Ok () -> Alcotest.fail "expected child failure"

let test_byte_writes () =
  let text = "failure without newline" in
  failed text
    (run
       (List.init (String.length text) (fun i -> String.make 1 text.[i]))
       (tape [ Vhs.raw "# hello" ]))

let test_split_utf8 () =
  let euro = "\xe2\x82\xac" in
  failed
    ("cost " ^ euro ^ "\n")
    (run
       [ "cost " ^ String.sub euro 0 1; String.sub euro 1 2 ^ "\n" ]
       (tape []))

let test_success () =
  match
    run ~status:(`Exited 0) ~vhs:"custom-vhs" [ "progress" ]
      (tape [ Vhs.type' "echo hello\n" ])
  with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%a" Vhs.pp_error e

let test_signal () =
  match run ~status:(`Signaled 9) [ "killed" ] (tape []) with
  | Error (Vhs.Failed { vhs = "vhs"; status = `Signaled 9; stderr = "killed" })
    ->
      ()
  | _ -> Alcotest.fail "lost signal, executable, or stderr"

let test_spawn_errors () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  List.iter
    (fun (error, expected) ->
      let proc = manager ~sw (fun _ _ -> raise (Eio.Process.err error)) in
      match Vhs.render ~proc ~cwd (tape []) with
      | Error e ->
          expect "mapped spawn error" expected (Fmt.str "%a" Vhs.pp_error e)
      | Ok () -> Alcotest.fail "lost spawn failure")
    [
      (Executable_not_found "vhs", "executable not found: vhs");
      (Permission_denied "vhs", "vhs: permission denied");
      (Executable_format_error "vhs", "vhs: not an executable");
    ]

let suite =
  ( "eio",
    List.map
      (fun (name, fn) -> Alcotest.test_case name `Quick fn)
      [
        ("stderr one byte at a time", test_byte_writes);
        ("stderr split UTF-8", test_split_utf8);
        ("success and explicit executable", test_success);
        ("signaled child", test_signal);
        ("spawn errors", test_spawn_errors);
      ] )
