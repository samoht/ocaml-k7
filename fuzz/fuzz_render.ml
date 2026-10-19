open Alcobar

let with_dir fn =
  Eio_main.run @@ fun env ->
  let dir = Filename.temp_dir "vhs-property" "" in
  Fun.protect
    ~finally:(fun () ->
      Array.iter (fun name -> Sys.remove (Filename.concat dir name)) (Sys.readdir dir);
      Unix.rmdir dir)
    (fun () -> fn env (Filename.concat dir))

let save env path contents =
  Eio.Path.save ~create:(`Or_truncate 0o755)
    Eio.Path.(Eio.Stdenv.fs env / path) contents

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
          Some (String.sub entry (String.length prefix)
                  (String.length entry - String.length prefix))
        else None)
      t.inherited

  let pipe t ~sw = Eio.Process.pipe ~sw t.base
  let spawn t ~sw ?cwd ?stdin ?stdout ?stderr ?env ?executable args =
    let env = Option.value ~default:t.inherited env in
    Eio.Process.spawn ~sw ?cwd ?stdin ?stdout ?stderr ~env ?executable t.base args
end

let test_no_publication term =
  with_dir @@ fun env path ->
  let executable = path "recorder" in
  save env executable
    "#!/bin/sh\ncat > /dev/null\n[ \"$VHS_PUBLISH\" != true ]\n";
  let base =
    (Eio.Stdenv.process_mgr env :> [ `Generic ] Eio.Process.mgr_ty Eio.Resource.t)
  in
  let inherited =
    Eio.Process.environment base |> Array.to_list
    |> List.filter (fun s ->
        not (String.starts_with ~prefix:"VHS_PUBLISH=" s
             || String.starts_with ~prefix:"TERM=" s))
  in
  let inherited = Array.of_list (("TERM=" ^ term) :: "VHS_PUBLISH=true" :: inherited) in
  let proc = Eio.Resource.T (Inherited.{ base; inherited },
                            Eio.Process.Pi.mgr (module Inherited)) in
  match Vhs.render ~vhs:executable ~proc ~cwd:(Eio.Stdenv.cwd env) (tape ()) with
  | Ok () -> ()
  | Error e -> failf "ambient environment enables upload: %a" Vhs.pp_error e

let suite =
  ("render", [test_case "never publish implicitly"
                 [choose [const "dumb"; const "xterm-256color"; const ""]]
                 test_no_publication])
