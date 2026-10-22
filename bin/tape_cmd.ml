(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let ( let* ) = Result.bind

(* As VHS's [Env]: the shell inherits the recorder's environment. *)
let set_env env =
  List.fold_left
    (fun acc (k, v) ->
      let* () = acc in
      match Unix.putenv k v with
      | () -> Ok ()
      | exception (Unix.Unix_error _ | Invalid_argument _) ->
          Fmt.error "--env %a: not a variable to set" Quote.pp (k ^ "=" ^ v))
    (Ok ()) env

let record ?stop stdenv ~print ~vhs ~settings ~env ~outputs input =
  let* src = Flags.load stdenv input in
  let error = function
    | K7.Invalid msg when String.starts_with ~prefix:"no output" msg ->
        Fmt.str "%s: %s; give it an Output line or -o FILE" input msg
    | e -> Fmt.str "%s: %a" input (Flags.pp_play_error ~vhs) e
  in
  let* t = Result.map_error error (K7.of_string src) in
  let* t =
    if settings = [] then Ok t
    else Result.map_error error (K7.with_settings settings t)
  in
  let* t =
    if outputs = [] then Ok t
    else Result.map_error error (K7.with_outputs outputs t)
  in
  if print then Flags.print_tape stdenv t
  else
    let* () = set_env env in
    K7.play ?stop ~vhs
      ~clock:(Eio.Stdenv.mono_clock stdenv)
      ~proc:(Eio.Stdenv.process_mgr stdenv)
      ~cwd:(Eio.Stdenv.cwd stdenv) t
    |> Result.map_error error

let run stdenv ~print ~vhs ~settings ~env ~outputs input =
  Flags.interruptible (fun stop ->
      record ~stop stdenv ~print ~vhs ~settings ~env ~outputs input)
  |> Flags.exit stdenv ~ok:(fun () -> 0)

open Cmdliner

let outputs =
  let doc =
    "Write the recording to $(docv), in the format of its extension, in place \
     of the tape's $(b,Output) lines before its first action. Repeatable."
  in
  Arg.(value & opt_all string [] & info [ "o"; "output" ] ~docv:"FILE" ~doc)

let input =
  let doc = "The tape to record, in VHS's $(b,.tape) syntax." in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"TAPE" ~doc)

let man =
  [
    `S Manpage.s_description;
    `P
      "$(cmd) reads the tape $(i,TAPE) and records it. Its $(b,Set) lines \
       before the first action are its settings, which $(b,--cols), \
       $(b,--rows), $(b,--font-size) and $(b,--theme) replace.";
    `P
      "When every output is an $(b,.svg) image or a text output ($(b,.txt), \
       $(b,.ascii) or $(b,.test)), $(cmd) plays the tape itself, in bash, zsh \
       or fish on a pseudo-terminal. Otherwise $(b,vhs), which needs \
       $(b,ttyd), $(b,ffmpeg) and a browser, records it. A tape cannot ask for \
       both an $(b,.svg) image and an output only $(b,vhs) writes.";
    `P
      "Each $(b,--env) variable is set in the environment of $(cmd), which the \
       shell inherits, as an $(b,Env) line does; $(b,--print) leaves it out of \
       the tape.";
    `S Manpage.s_examples;
    `Pre
      "$(cmd) demo.tape\n\
       $(cmd) --cols 100 -o demo.svg demo.tape\n\
       $(cmd) --print demo.tape";
  ]

let cmd =
  let doc = "record a VHS tape" in
  let run print vhs settings env outputs input =
    Eio_main.run @@ fun stdenv ->
    run stdenv ~print ~vhs ~settings ~env ~outputs input
  in
  Cmd.make
    (Cmd.info "tape" ~doc ~man ~exits:Flags.exits)
    Term.(
      const run $ Flags.print $ Flags.vhs $ Flags.settings $ Flags.env $ outputs
      $ input)
