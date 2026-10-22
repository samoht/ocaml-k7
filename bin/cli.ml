(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let ( let* ) = Result.bind

let tape ~setup ~output ?wait_timeout_s ~name src =
  let* cram = Cram.parse src |> Result.map_error (Fmt.str "%s: %s" name) in
  match
    (Cram.is_empty cram, List.find_opt (Fun.negate Cram.is_typeable) setup)
  with
  | true, _ -> Fmt.error "%s: no command: no line starts with \"  $ \"" name
  | false, Some path ->
      Fmt.error "setup path %s: holds invalid UTF-8 or a control character"
        (Cram.quote path)
  | false, None ->
      let timeout =
        match wait_timeout_s with
        | None -> []
        | Some s -> [ Fmt.kstr Vhs.raw "Set WaitTimeout %ds" s ]
      in
      Vhs.tape ~settings:[ Shell Bash ] ~outputs:[ output ]
        (timeout @ Cram.actions ~setup cram)
      |> Result.map_error (Fmt.str "%a" Vhs.pp_error)

let load_error name = function
  | Eio.Fs.E (Not_found _) -> Fmt.str "%s: no such file" name
  | Eio.Fs.E (Permission_denied _) -> Fmt.str "%s: permission denied" name
  | err -> Fmt.str "%s: %a" name Eio.Exn.pp_err err

(* [write flow s] writes [s] to [flow]. It is [Error None] if the reader
   closed the pipe, and [Error (Some msg)] on another failure. *)
let write flow s =
  match Eio.Flow.copy_string s flow with
  | () -> Ok ()
  | exception
      Eio.Io
        (Eio.Net.E (Connection_reset (Eio_unix.Unix_error (EPIPE, _, _))), _) ->
      Error None
  | exception Eio.Io (Eio.Exn.X (Eio_unix.Unix_error (code, _, _)), _) ->
      Error (Some (Unix.error_message code))
  | exception Eio.Io (err, _) ->
      Error (Fmt.kstr Option.some "%a" Eio.Exn.pp_err err)

(* [shown s] is [s] as [Vhs.pp_error] prints it: without escape sequences,
   controls and the invisible characters of the prompt mark. *)
let shown s = Fmt.str "%a" Vhs.pp_error (Io s)
let last_value = function "" -> "" | last -> "; last value was: " ^ shown last

(* [after cram tape line] is the prompt that the wait on line [line] of
   [tape] waits for, in words. The waits of a cram tape are the setup's,
   then one after each command. *)
let after cram tape line =
  let waits =
    String.split_on_char '\n' (Vhs.to_string tape)
    |> List.filteri (fun i l -> i < line && String.starts_with ~prefix:"Wait" l)
    |> List.length
  in
  match Cram.command cram (waits - 1) with
  | Some first ->
      Fmt.str "the prompt after command %d, %s" (waits - 1) (Cram.quote first)
  | None -> "the prompt after the setup scripts"

(* [rest_after sub l] is what follows the first [sub] in [l], if any. *)
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

(* What [vhs] last read in a wait that timed out, from the line of its
   message, if a wait timed out. *)
let vhs_timeout output =
  List.find_map
    (fun l ->
      Option.map
        (fun rest ->
          Option.value (rest_after "last value was: " rest) ~default:"")
        (rest_after "timeout waiting for" l))
    (String.split_on_char '\n' output)

let seconds_of n = Fmt.str "%d second%s" n (if n = 1 then "" else "s")

(* A command that leaves bash at its continuation prompt makes the next
   wait time out; the error then names it. A timeout otherwise names the
   prompt it waited for, in words, without the prompt mark of the tape's
   pattern, and the option that waits longer. *)
let render_error ~setup ~wait ~name ~tape src e =
  let cram = Result.to_option (Cram.parse src) in
  let unfinished output =
    Option.bind cram (fun t -> Cram.unfinished ~setup t output)
  in
  let longer = "; --wait-timeout gives it longer" in
  let output =
    match e with
    | Vhs.Failed { output; _ } -> Some output
    | Timeout { last; _ } | Exited { last; _ } ->
        Some ("last value was: " ^ last)
    | _ -> None
  in
  match (Option.bind output unfinished, e, cram) with
  | Some (n, line), _, _ ->
      Fmt.str
        "%s: command %d, %s, left bash waiting for more input, such as the end \
         of a quote or of a here-document"
        name n (Cram.quote line)
  | None, Timeout { line; last; _ }, Some cram ->
      Fmt.str "%s: timed out after %s waiting for %s%s%s" name (seconds_of wait)
        (after cram tape line) (last_value last) longer
  | None, Exited { status; line; last; _ }, Some cram ->
      let how =
        match status with
        | `Exited n -> Fmt.str "exited (status %d)" n
        | `Signaled n -> Fmt.str "was killed by signal %d" n
      in
      Fmt.str "%s: the shell %s while waiting for %s%s" name how
        (after cram tape line) (last_value last)
  | None, Failed { output; _ }, _ when Option.is_some (vhs_timeout output) ->
      Fmt.str
        "%s: vhs timed out after %s waiting for the prompt after a command%s%s"
        name (seconds_of wait)
        (last_value (Option.get (vhs_timeout output)))
        longer
  | None, _, _ -> Fmt.str "%s: %a" name Vhs.pp_error e

(* The outputs [Vhs.record] writes, with no browser. *)
let native = [ ".svg"; ".txt"; ".ascii"; ".test" ]

let record env ~print ~vhs ~setup ?wait_timeout_s ~output input =
  let* src =
    match Eio.Path.(load (Eio.Stdenv.fs env / input)) with
    | src -> Ok src
    | exception Eio.Io (err, _) -> Error (load_error input err)
  in
  let* tape = tape ~setup ~output ?wait_timeout_s ~name:input src in
  if print then
    (* Like a process killed by SIGPIPE, which Eio ignores, [--print] stops
       quietly when the reader goes away, as in [vhs-cram --print | head]. *)
    match write (Eio.Stdenv.stdout env) (Vhs.to_string tape) with
    | Ok () | Error None -> Ok ()
    | Error (Some msg) -> Fmt.error "standard output: %s" msg
  else
    let clock = Eio.Stdenv.mono_clock env
    and proc = Eio.Stdenv.process_mgr env
    and cwd = Eio.Stdenv.cwd env in
    (if List.mem (Filename.extension output) native then
       Vhs.record ~clock ~proc ~cwd tape
     else Vhs.render ~vhs ~clock ~proc ~cwd tape)
    |> Result.map_error
         (render_error ~setup
            ~wait:(Option.value wait_timeout_s ~default:15)
            ~name:input ~tape src)

let run env ~print ~vhs ~setup ?wait_timeout_s ~output input =
  match record env ~print ~vhs ~setup ?wait_timeout_s ~output input with
  | Ok () -> 0
  | Error msg ->
      (* A closed standard error leaves the exit code to report. *)
      ignore (write (Eio.Stdenv.stderr env) (Fmt.str "vhs-cram: %s\n" msg));
      Cmdliner.Cmd.Exit.some_error

open Cmdliner

(* The seconds of Go's longest duration. *)
let max_seconds = 9_223_372_036

let seconds s =
  let too_large () =
    Fmt.error "%s is too large: a wait lasts at most %d seconds" (Cram.quote s)
      max_seconds
  in
  match int_of_string_opt s with
  | Some n when n > 0 && n <= max_seconds -> Ok n
  | Some n when n > 0 -> too_large ()
  (* A number of digits [int_of_string] cannot read is beyond [max_int]. *)
  | None when s <> "" && String.for_all Char.Ascii.is_digit s -> too_large ()
  | _ -> Fmt.error "%s is not a positive number of seconds" (Cram.quote s)

let output =
  let doc =
    "Write the recording to $(docv), in the format of its extension, in lower \
     case: an animated SVG image for $(b,.svg), and the text VHS writes for \
     $(b,.txt), $(b,.ascii) and $(b,.test), which $(tname) records itself, and \
     what VHS writes for the others."
  in
  Arg.(
    required & opt (some string) None & info [ "o"; "output" ] ~docv:"FILE" ~doc)

let setup =
  let doc =
    "Source $(docv) before the first command, without recording it. Repeatable."
  in
  Arg.(value & opt_all non_dir_file [] & info [ "setup" ] ~docv:"FILE" ~doc)

let vhs =
  let doc = "The $(b,vhs) executable to run." in
  Arg.(value & opt string "vhs" & info [ "vhs" ] ~docv:"PATH" ~doc)

let wait_timeout_s =
  let doc =
    "Fail the recording when a command has not given the prompt back after \
     $(docv) seconds (defaults to VHS's 15)."
  in
  Arg.(
    value
    & opt
        (some (Arg.Conv.make ~docv:"SECONDS" ~parser:seconds ~pp:Fmt.int ()))
        None
    & info [ "wait-timeout" ] ~docv:"SECONDS" ~doc)

let print =
  let doc = "Print the tape on standard output instead of running $(b,vhs)." in
  Arg.(value & flag & info [ "print" ] ~doc)

let input =
  let doc = "The cram test to record." in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"CRAM" ~doc)

let man =
  [
    `S Manpage.s_description;
    `P
      "$(tname) types each $(b,\\$) command of the cram test $(i,CRAM) into a \
       VHS terminal running bash, waits for the prompt to come back, and \
       records the session. Expected output and exit codes are ignored; other \
       settings are the VHS defaults.";
    `P
      "An $(b,.svg) output and the text outputs are recorded without $(b,vhs): \
       $(tname) runs bash on a pseudo-terminal itself and draws its screens as \
       an animated SVG image, which plays in an HTML $(i,img) element, or \
       writes their text as VHS does. The other outputs are recorded by \
       $(b,vhs), which needs $(b,ttyd), $(b,ffmpeg) and a browser.";
    `P
      "Before the first command, and after the setup scripts, $(tname) makes \
       the last command of $(b,PROMPT_COMMAND) set $(b,PS1) to the VHS prompt \
       followed by a mark: sixteen zero-width characters, U+200B and U+2060, \
       that spell a digest of the setup scripts and the commands. The mark \
       tells the prompt from output, even output that ends with $(b,>) or with \
       a zero-width space. It does not show in a GIF or a video, but the text \
       outputs ($(b,.txt), $(b,.ascii) and $(b,.test)) hold it after each \
       prompt. The prompt hooks of the setup scripts still run first; a \
       command that replaces $(b,PROMPT_COMMAND) makes the next wait time out. \
       A command that leaves bash waiting for more input, such as an \
       unterminated quote or here-document, makes the wait after it time out \
       too; $(tname) then names that command. A wait that times out names the \
       command whose prompt did not come back, when the recording is native, \
       and a shell that exits ends a native recording at once.";
    `S Manpage.s_examples;
    `Pre
      "$(cmd) --output demo.svg demo.t\n\
       $(cmd) --output demo.gif demo.t\n\
       $(cmd) --print --output demo.gif demo.t   # show the tape";
  ]

let exits =
  Cmd.Exit.
    [
      info ok
        ~doc:
          "on success, also when the reader of $(b,--print) closes the pipe \
           early.";
      info some_error
        ~doc:
          "on a cram test that cannot be recorded, when $(b,vhs) cannot run or \
           fails, or when the tape of $(b,--print) cannot be written; the \
           reason is on standard error.";
      info cli_error ~doc:"on command line parsing errors.";
      info internal_error ~doc:"on unexpected internal errors (bugs).";
    ]

let cmd =
  let doc = "record a cram test as a VHS terminal recording" in
  let run print vhs setup wait_timeout_s output input =
    Eio_main.run @@ fun env ->
    run env ~print ~vhs ~setup ?wait_timeout_s ~output input
  in
  Cmd.make
    (Cmd.info "vhs-cram" ~doc ~man ~exits)
    Term.(const run $ print $ vhs $ setup $ wait_timeout_s $ output $ input)
