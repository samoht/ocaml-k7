(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let ( let* ) = Result.bind

let record ?stop ?engine stdenv ~dry_run ~print ~vhs ~settings ~env ~outputs
    input =
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
  if dry_run then
    K7.check ?engine ~env ~cwd:(Eio.Stdenv.cwd stdenv) t
    |> Result.map_error error
  else if print then Flags.print_tape stdenv t
  else
    K7.record ?engine ?stop ~vhs ~env
      ~clock:(Eio.Stdenv.mono_clock stdenv)
      ~proc:(Eio.Stdenv.process_mgr stdenv)
      ~cwd:(Eio.Stdenv.cwd stdenv) t
    |> Result.map_error error

let run stdenv ?engine ~dry_run ~print ~vhs ~settings ~env ~outputs input =
  Flags.interruptible (fun stop ->
      record ~stop ?engine stdenv ~dry_run ~print ~vhs ~settings ~env ~outputs
        input)
  |> Flags.exit stdenv ~ok:(fun () -> 0)
