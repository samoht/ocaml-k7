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
  | true, _ -> Fmt.error "%s: no command" name
  | false, Some path ->
      Fmt.error "setup %S: holds invalid UTF-8 or a control character" path
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

(* A command that leaves bash at its continuation prompt makes the next
   wait time out; the error then names it. A native timeout carries the line
   that [vhs] prints after "last value was: ". *)
let render_error ~setup ~name src e =
  let output =
    match e with
    | Vhs.Failed { output; _ } -> Some output
    | Timeout { last; _ } -> Some ("last value was: " ^ last)
    | _ -> None
  in
  match output with
  | Some output -> (
      match
        Option.bind
          (Result.to_option (Cram.parse src))
          (fun t -> Cram.unfinished ~setup t output)
      with
      | Some (n, line) ->
          Fmt.str
            "%s: command %d, %S, left bash waiting for more input, such as the \
             end of a quote or of a here-document"
            name n line
      | None -> Fmt.str "%s: %a" name Vhs.pp_error e)
  | None -> Fmt.str "%s: %a" name Vhs.pp_error e

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
    (if Filename.extension output = ".svg" then
       (* [vhs] writes absolute paths too. *)
       let cwd = Eio.Path.(Eio.Stdenv.fs env / ".") in
       Result.map ignore (Vhs.record ~clock ~proc ~cwd tape)
     else Vhs.render ~vhs ~clock ~proc ~cwd tape)
    |> Result.map_error (render_error ~setup ~name:input src)

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
    Fmt.error "%S is too large: a wait lasts at most %d seconds" s max_seconds
  in
  match int_of_string_opt s with
  | Some n when n > 0 && n <= max_seconds -> Ok n
  | Some n when n > 0 -> too_large ()
  (* A number of digits [int_of_string] cannot read is beyond [max_int]. *)
  | None when s <> "" && String.for_all Char.Ascii.is_digit s -> too_large ()
  | _ -> Fmt.error "%S is not a positive number of seconds" s

let output =
  let doc =
    "Write the recording to $(docv), in the format of its extension: an \
     animated SVG image for $(b,.svg), which $(tname) records itself, and what \
     VHS writes for the others."
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
      "An $(b,.svg) output is recorded without $(b,vhs): $(tname) runs bash on \
       a pseudo-terminal itself and draws its screens as an animated SVG \
       image, which plays in an HTML $(i,img) element. The other outputs are \
       recorded by $(b,vhs), which needs $(b,ttyd), $(b,ffmpeg) and a browser.";
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
       too; $(tname) then names that command.";
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
