(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let ( let* ) = Result.bind

let record ?stop ?engine ?idle_time_limit stdenv ~dry_run ~print ~vhs ~settings
    ~env ~outputs input =
  let* src = Flags.load stdenv input in
  let error e = Fmt.str "%s: %a" input (Flags.pp_play_error ~vhs) e in
  let* t = Result.map_error error (K7.of_string src) in
  let* t =
    if settings = [] then Ok t
    else Result.map_error error (K7.with_settings settings t)
  in
  let* t =
    if outputs = [] then Ok t
    else Result.map_error error (K7.with_outputs outputs t)
  in
  if K7.outputs t = [] && not print then
    Fmt.error "%s: no output; give it an Output line or -o FILE" input
  else if dry_run then
    K7.check ?engine ~env ~cwd:(Eio.Stdenv.cwd stdenv) t
    |> Result.map_error error
  else if print then Flags.print_tape stdenv t
  else
    K7.record ?engine ?stop ?idle_time_limit ~vhs ~env
      ~clock:(Eio.Stdenv.mono_clock stdenv)
      ~proc:(Eio.Stdenv.process_mgr stdenv)
      ~cwd:(Eio.Stdenv.cwd stdenv) t
    |> Result.map_error error

let run stdenv ?engine ?idle_time_limit ~dry_run ~print ~vhs ~settings ~env
    ~outputs input =
  Flags.interruptible (fun stop ->
      record ~stop ?engine ?idle_time_limit stdenv ~dry_run ~print ~vhs
        ~settings ~env ~outputs input)
  |> Flags.exit stdenv ~ok:(fun () -> 0)
