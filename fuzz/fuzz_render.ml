open Alcobar

let with_dir fn =
  Eio_main.run @@ fun env ->
  let dir = Filename.temp_dir "vhs-property" "" in
  Fun.protect
    ~finally:(fun () ->
      Array.iter
        (fun name -> Sys.remove (Filename.concat dir name))
        (Sys.readdir dir);
      Unix.rmdir dir)
    (fun () -> fn env (Filename.concat dir))

let save env path contents =
  Eio.Path.save ~create:(`Or_truncate 0o755)
    Eio.Path.(Eio.Stdenv.fs env / path)
    contents

let tape () = Result.get_ok (Vhs.tape ~outputs:[ "demo.gif" ] [])

(* Supply a manager environment without mutating the test runner's process. *)
module Inherited = struct
  type tag = [ `Generic ]

  type t = {
    base : [ `Generic ] Eio.Process.mgr_ty Eio.Resource.t;
    inherited : string array;
  }

  let environment t = t.inherited

  let getenv_opt t name =
    let prefix = name ^ "=" in
    Array.find_map
      (fun entry ->
        if String.starts_with ~prefix entry then
          Some
            (String.sub entry (String.length prefix)
               (String.length entry - String.length prefix))
        else None)
      t.inherited

  let pipe t ~sw = Eio.Process.pipe ~sw t.base

  let spawn t ~sw ?cwd ?stdin ?stdout ?stderr ?env ?executable args =
    let env = Option.value ~default:t.inherited env in
    Eio.Process.spawn ~sw ?cwd ?stdin ?stdout ?stderr ~env ?executable t.base
      args
end

let test_no_publication term =
  with_dir @@ fun env path ->
  let executable = path "recorder" in
  save env executable
    "#!/bin/sh\ncat > /dev/null\n[ \"$VHS_PUBLISH\" != true ]\n";
  let base =
    (Eio.Stdenv.process_mgr env
      :> [ `Generic ] Eio.Process.mgr_ty Eio.Resource.t)
  in
  let inherited =
    Eio.Process.environment base
    |> Array.to_list
    |> List.filter (fun s ->
        not
          (String.starts_with ~prefix:"VHS_PUBLISH=" s
          || String.starts_with ~prefix:"TERM=" s))
  in
  let inherited =
    Array.of_list (("TERM=" ^ term) :: "VHS_PUBLISH=true" :: inherited)
  in
  let proc =
    Eio.Resource.T
      (Inherited.{ base; inherited }, Eio.Process.Pi.mgr (module Inherited))
  in
  match
    Vhs.render ~vhs:executable ~proc ~cwd:(Eio.Stdenv.cwd env) (tape ())
  with
  | Ok () -> ()
  | Error e -> failf "ambient environment enables upload: %a" Vhs.pp_error e

let test_cancel_reaps delay =
  with_dir @@ fun env path ->
  let executable = path "recorder" in
  save env executable
    "#!/bin/sh\n\
     trap 'exit 0' TERM\n\
     cat > /dev/null\n\
     echo $$ > \"$0.pid\"\n\
     while :; do sleep 0.01; done\n";
  let clock = Eio.Stdenv.clock env in
  let result =
    Eio.Time.with_timeout_exn clock 5. (fun () ->
        Eio.Fiber.first
          (fun () ->
            `Rendered
              (Vhs.render ~vhs:executable
                 ~proc:(Eio.Stdenv.process_mgr env)
                 ~cwd:(Eio.Stdenv.cwd env) (tape ())))
          (fun () ->
            while not (Sys.file_exists (executable ^ ".pid")) do
              Eio.Time.sleep clock 0.001
            done;
            Eio.Time.sleep clock (float_of_int (delay land 15) /. 1000.);
            `Cancelled))
  in
  check (result = `Cancelled);
  let pid =
    Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / (executable ^ ".pid"))
    |> String.trim |> int_of_string
  in
  let alive =
    try
      Unix.kill pid 0;
      true
    with Unix.Unix_error (Unix.ESRCH, _, _) -> false
  in
  if alive then failf "cancelled render left process %d alive" pid

let test_exit_codes n =
  with_dir @@ fun env path ->
  let executable = path "recorder" in
  let status = n land 255 in
  let signaled = n land 256 <> 0 in
  save env executable
    ("#!/bin/sh\ncat > /dev/null\n"
    ^ if signaled then "kill -TERM $$\n" else Fmt.str "exit %d\n" status);
  save env (path "input.t") "  $ echo hello\n";
  save env (path "empty.t") "prose only\n";
  save env (path "bad.t") "  $ echo \001\n";
  let cli =
    Option.value ~default:"vhs-cram" (Sys.getenv_opt "VHS_CRAM_TEST_EXE")
  in
  let eval expected args =
    let buffer = Buffer.create 256 in
    let sink = Eio.Flow.buffer_sink buffer in
    let received =
      Eio.Switch.run @@ fun sw ->
      let child =
        Eio.Process.spawn ~sw ~stdout:sink ~stderr:sink
          (Eio.Stdenv.process_mgr env)
          (cli :: args)
      in
      Eio.Process.await child
    in
    if received <> `Exited expected then
      failf "%s: expected %d, got %a: %s" (String.concat " " args) expected
        Eio.Process.pp_status received (Buffer.contents buffer)
  in
  let flags = [ "--output"; path "demo.gif"; "--vhs"; executable ] in
  eval
    (if status = 0 && not signaled then 0 else 123)
    (flags @ [ path "input.t" ]);
  eval 123 (flags @ [ path "empty.t" ]);
  eval 123 (flags @ [ path "bad.t" ]);
  eval 124 (flags @ [ path "missing.t" ]);
  eval 124 (("--setup" :: path "missing.sh" :: flags) @ [ path "input.t" ]);
  eval 0
    ("--print" :: "--vhs" :: path "missing-vhs"
    :: [ "--output"; path "demo.gif"; path "input.t" ]);
  eval 123
    [ "--vhs"; path "missing-vhs"; "--output"; path "demo.gif"; path "input.t" ];
  eval 123 [ "--print"; "--output"; "no-extension"; path "input.t" ];
  eval 0 [ "--help=plain" ];
  eval 124 [ "--unknown-adversarial-flag" ]

let suite =
  ( "render",
    [
      test_case "documented executable exit codes" [ int16 ] test_exit_codes;
      test_case "cancelled render reaps the child" [ int8 ] test_cancel_reaps;
      test_case "never publish implicitly"
        [ choose [ const "dumb"; const "xterm-256color"; const "" ] ]
        test_no_publication;
    ] )
