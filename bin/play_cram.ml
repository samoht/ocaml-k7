(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let ( let* ) = Result.bind

let tape ~setup ?(settings = []) ~outputs ?wait_timeout_s ~name src =
  let* cram =
    Cram.parse src |> Result.map_error (fun e -> Fmt.str "%s: %s" name e)
  in
  match
    (Cram.is_empty cram, List.find_opt (Fun.negate Cram.is_typeable) setup)
  with
  | true, _ -> Fmt.error "%s: no command: no line starts with \"  $ \"" name
  | false, _ when outputs = [] ->
      Fmt.error "%s: no output; give it -o FILE" name
  | false, Some path ->
      Fmt.error "setup path %a: holds invalid UTF-8 or a control character"
        Quote.pp path
  | false, None ->
      let timeout =
        Option.map (fun s -> K7.Wait_timeout (Ms (s * 1000))) wait_timeout_s
      in
      K7.tape
        ~settings:((K7.Shell Bash :: settings) @ Option.to_list timeout)
        ~outputs (Cram.actions ~setup cram)
      |> Result.map_error (fun e -> Fmt.str "%a" Flags.pp_error e)

let shown = Flags.shown
let last_value = function "" -> "" | last -> "; last value was: " ^ shown last

(* A cram tape waits after its setup, then after each command. *)
let after cram tape line =
  let waits =
    String.split_on_char '\n' (K7.to_string tape)
    |> List.filteri (fun i l -> i < line && String.starts_with ~prefix:"Wait" l)
    |> List.length
  in
  match Cram.command cram (waits - 1) with
  | Some first ->
      Fmt.str "the prompt after command %d, %a" (waits - 1) Quote.pp first
  | None -> "the prompt after the setup scripts"

let rest_after sub l =
  let n = String.length sub in
  let rec go i =
    match String.index_from_opt l i sub.[0] with
    | Some j when j + n <= String.length l ->
        if String.sub l j n = sub then
          Some (String.sub l (j + n) (String.length l - j - n))
        else go (j + 1)
    | _ -> None
  in
  go 0

let vhs_timeout output =
  List.find_map
    (fun l ->
      Option.map
        (fun rest ->
          Option.value (rest_after "last value was: " rest) ~default:"")
        (rest_after "timeout waiting for" l))
    (String.split_on_char '\n' output)

let seconds_of n = Fmt.str "%d second%s" n (if n = 1 then "" else "s")

(* A command left at bash's continuation prompt times the next wait out. *)
let render_error ~vhs ~setup ~wait ~name ~tape src e =
  let cram = Result.to_option (Cram.parse src) in
  let unfinished output =
    Option.bind cram (fun t -> Cram.unfinished ~setup t output)
  in
  let longer = "; --wait-timeout gives it longer" in
  let output =
    match e with
    | K7.Vhs_failed { output; _ } -> Some output
    | Timeout { last; _ } | Exited { last; _ } ->
        Some ("last value was: " ^ last)
    | _ -> None
  in
  match (Option.bind output unfinished, e, cram) with
  | Some (n, line), _, _ ->
      Fmt.str
        "%s: command %d, %a, left bash waiting for more input, such as the end \
         of a quote or of a here-document"
        name n Quote.pp line
  | None, Timeout { line; last; _ }, Some cram ->
      Fmt.str "%s: timed out after %s waiting for %s%s%s" name (seconds_of wait)
        (after cram tape line) (last_value last) longer
  | None, Exited { status; line; last; _ }, Some cram ->
      let how =
        match status with
        | `Exited n -> Fmt.str "exited (status %d)" n
        | `Signaled n -> Fmt.str "was killed by signal %d" (Sys.signal_to_int n)
      in
      Fmt.str "%s: the shell %s while waiting for %s%s" name how
        (after cram tape line) (last_value last)
  | None, Vhs_failed { output; _ }, _ when Option.is_some (vhs_timeout output)
    ->
      Fmt.str
        "%s: vhs timed out after %s waiting for the prompt after a command%s%s"
        name (seconds_of wait)
        (last_value (Option.get (vhs_timeout output)))
        longer
  | None, _, _ -> Fmt.str "%s: %a" name (Flags.pp_play_error ~vhs) e

let absolute path =
  if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
  else path

let in_dir env dir f =
  let fs = Eio.Stdenv.fs env in
  match dir with
  | Some dir -> f Eio.Path.(fs / absolute dir)
  | None -> (
      match Filename.temp_dir "k7-cram-" "" with
      | exception Sys_error msg -> Error msg
      | dir ->
          let dir = Eio.Path.(fs / dir) in
          Fun.protect
            ~finally:(fun () ->
              try Eio.Path.rmtree ~missing_ok:true dir with Eio.Io _ -> ())
            (fun () -> f dir))

let record ?stop env ~print ~vhs ~setup ?dir ?settings ?env:vars ?wait_timeout_s
    ~outputs input =
  let* src = Flags.load env input in
  let tape ~setup ~outputs =
    tape ~setup ?settings ~outputs ?wait_timeout_s ~name:input src
  in
  let* given = tape ~setup ~outputs in
  if print then Flags.print_tape env given
  else
    let* tape =
      tape ~setup:(List.map absolute setup) ~outputs:(List.map absolute outputs)
    in
    let run_vhs = if String.contains vhs '/' then absolute vhs else vhs in
    let named = function
      | K7.Vhs_failed f -> K7.Vhs_failed { f with vhs }
      | K7.Executable_not_found exe when exe = run_vhs ->
          K7.Executable_not_found vhs
      | e -> e
    in
    in_dir env dir @@ fun cwd ->
    K7.record ?stop ~vhs:run_vhs ?env:vars
      ~clock:(Eio.Stdenv.mono_clock env)
      ~proc:(Eio.Stdenv.process_mgr env)
      ~cwd tape
    |> Result.map_error (fun e ->
        render_error ~vhs ~setup
          ~wait:(Option.value wait_timeout_s ~default:15)
          ~name:input ~tape:given src (named e))

let run env ~print ~vhs ~setup ?dir ?settings ?env:vars ?wait_timeout_s ~outputs
    input =
  Flags.interruptible (fun stop ->
      record ~stop env ~print ~vhs ~setup ?dir ?settings ?env:vars
        ?wait_timeout_s ~outputs input)
  |> Flags.exit env ~ok:(fun () -> 0)
