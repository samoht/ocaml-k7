(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let ( let* ) = Result.bind

type kind = Svg | Cast | Txt | Cram

(* Ignoring case, as k7 play does. *)
let kind output =
  match String.lowercase_ascii (Filename.extension output) with
  | ".svg" -> Ok Svg
  | ".cast" -> Ok Cast
  | ".txt" -> Ok Txt
  | ".t" -> Ok Cram
  | _ ->
      Fmt.error "%a: not .svg, .cast, .txt or .t, the outputs k7 rec writes"
        Quote.pp output

let kinds outputs =
  List.fold_left
    (fun acc o ->
      let* acc = acc in
      let* k = kind o in
      Ok ((o, k) :: acc))
    (Ok []) outputs
  |> Result.map List.rev

let exit_code = function
  | `Exited n -> n
  | `Signaled s -> 128 + Sys.signal_to_int s

external window_size : Unix.file_descr -> (int * int) option = "k7_window_size"

let sized fd settings =
  match window_size fd with
  | None -> settings
  | Some (columns, rows) ->
      let lacks p = not (List.exists p settings) in
      settings
      @ (if lacks (function K7.Columns _ -> true | _ -> false) then
           [ K7.Columns columns ]
         else [])
      @
      if lacks (function K7.Rows _ -> true | _ -> false) then [ K7.Rows rows ]
      else []

let raw saved =
  Unix.
    {
      saved with
      c_brkint = false;
      c_icrnl = false;
      c_inpck = false;
      c_istrip = false;
      c_ixon = false;
      c_opost = false;
      c_csize = 8;
      c_parenb = false;
      c_echo = false;
      c_echonl = false;
      c_icanon = false;
      c_isig = false;
      c_vmin = 1;
      c_vtime = 0;
    }

(* The terminal that runs k7 shows the program and sends it each key as typed,
   Ctrl-C included, until it exits. *)
let attached tty f =
  if tty then (
    let saved = Unix.tcgetattr Unix.stdin in
    Unix.tcsetattr Unix.stdin TCSAFLUSH (raw saved);
    Fun.protect ~finally:(fun () -> Unix.tcsetattr Unix.stdin TCSADRAIN saved) f)
  else f ()

let record ?show ?delay ?stop stdenv ~settings ~env ~outputs argv =
  let* outputs = kinds outputs in
  let error r = Result.map_error (Fmt.to_to_string Flags.pp_error) r in
  let proc = Eio.Stdenv.process_mgr stdenv and cwd = Eio.Stdenv.cwd stdenv in
  let clock = Eio.Stdenv.mono_clock stdenv in
  let tty = Unix.isatty Unix.stdin && Unix.isatty Unix.stdout in
  let settings = if tty then sized Unix.stdout settings else settings in
  let input, echo =
    if tty then (Some (Eio.Stdenv.stdin stdenv), Some (Eio.Stdenv.stdout stdenv))
    else (None, None)
  in
  let cast =
    lazy
      ( attached tty @@ fun () ->
        K7.capture ~settings ~env ?show ?stop ?input ?echo ~clock ~proc ~cwd
          argv
        |> error )
  in
  let cram = lazy (K7.cram ~env ?stop ~clock ~proc ~cwd argv |> error) in
  let draw = function
    | Svg ->
        let* c, _ = Lazy.force cast in
        K7.svg_of_casts ~settings ?delay [ c ] |> error
    | Cast ->
        let* c, _ = Lazy.force cast in
        Ok (K7.string_of_cast c)
    | Txt ->
        let* c, _ = Lazy.force cast in
        K7.text_of_casts ~settings [ c ] |> error
    | Cram -> Result.map fst (Lazy.force cram)
  in
  let* drawn =
    List.fold_left
      (fun acc (o, k) ->
        let* acc = acc in
        let* s = draw k in
        Ok ((o, s) :: acc))
      (Ok []) outputs
  in
  (* Once all are ready, so that the program does not see the directories. *)
  let* () =
    List.fold_left
      (fun acc (o, s) ->
        let* () = acc in
        Flags.save stdenv o s)
      (Ok ()) (List.rev drawn)
  in
  if Lazy.is_val cast then Result.map snd (Lazy.force cast)
  else Result.map snd (Lazy.force cram)

let run ?show ?delay stdenv ~settings ~env ~outputs argv =
  Flags.interruptible (fun stop ->
      record ?show ?delay ~stop stdenv ~settings ~env ~outputs argv)
  |> Flags.exit stdenv ~ok:exit_code

open Cmdliner

let outputs =
  let doc =
    "Write the recording to $(docv), in the format of its extension, creating \
     its missing directories: $(b,.svg) for an animated SVG image, $(b,.cast) \
     for an asciicast v3 recording, $(b,.txt) for the text of the last screen, \
     or $(b,.t) for a cram test. Repeatable."
  in
  Arg.(non_empty & opt_all string [] & info [ "o"; "output" ] ~docv:"FILE" ~doc)

let show =
  let doc =
    "Show $(docv) after the $(b,\\$) of the image's first line instead of \
     $(i,COMMAND), and write it as the command of an asciicast recording; an \
     empty $(docv) shows no line. A cram test holds $(i,COMMAND) itself."
  in
  Arg.(value & opt (some string) None & info [ "show" ] ~docv:"TEXT" ~doc)

let delay =
  let doc =
    "Show the command line alone for $(docv) milliseconds, as if typed, before \
     the program's output in an image; 0 shows the output at once."
  in
  Arg.(value & opt Flags.milliseconds 500 & info [ "delay" ] ~docv:"MS" ~doc)

let command =
  let doc = "The program to record, then its arguments, after $(b,--)." in
  Arg.(non_empty & pos_all string [] & info [] ~docv:"COMMAND" ~doc)

let man =
  [
    `S Manpage.s_description;
    `P
      "$(cmd) runs $(i,COMMAND) on a pseudo-terminal, with no shell, and \
       writes what the terminal showed to each output, in the format of its \
       extension, in any case. An $(b,.svg) output is an animated SVG image, \
       which plays in an HTML $(i,img) element: each screen shows at the time \
       it arrived since the program started, and the last one holds for a \
       second after the program exits. The first line of the image shows \
       $(b,\\$) and $(i,COMMAND), each argument shell-quoted where needed, and \
       the program's output starts on the next line. Output beyond the last \
       row of the terminal scrolls its first rows away.";
    `P
      "When $(cmd) runs in a terminal, the program shows there as it runs, and \
       each key typed goes to it as typed, Ctrl-C included, so a program that \
       asks questions can be answered. That terminal answers the program's \
       queries, such as its colours, and the image shows what it would show. \
       Otherwise the program gets nothing typed.";
    `P
      "A $(b,.cast) output is the recording itself, in asciinema's asciicast \
       v3 format: the size of the terminal, the command, what the program \
       printed at the millisecond it arrived, and its exit. $(b,k7 play) \
       $(b,-o) $(i,FILE.svg) $(i,FILE.cast) draws the same image later, with \
       other options if need be, and asciinema plays it. With $(b,--theme), \
       its header holds the theme, and its cursor colours in $(i,cursor) and \
       $(i,cursor_accent), two members that k7 adds to asciicast v3. A \
       $(b,.txt) output is the text of the last screen.";
    `P
      "A $(b,.t) output is a cram test of $(i,COMMAND), as dune runs it: \
       $(cmd) runs the command line with $(b,sh -c) on pipes, with an empty \
       standard input and standard error on standard output, not on the \
       terminal, and writes what it printed and its exit code as dune does, so \
       that dune finds no difference. With other outputs, $(i,COMMAND) runs \
       twice: once on the terminal, whose exit code $(cmd) exits with, and \
       once on pipes.";
    `P
      "Without $(b,--columns) and $(b,--rows), the terminal has the size of \
       the terminal $(cmd) runs in. When it runs in none, the terminal has as \
       many columns and rows as a frame of 1200 by 600 pixels holds, as in \
       VHS, with characters $(b,--font-size) pixels high: 75 columns and 16 \
       rows at the default 22 pixels, and 150 columns and 33 rows at 11. In \
       the other cases, the image is as wide and as high as the terminal needs \
       at that font size.";
    `P
      "The program is looked up in the $(b,PATH) it gets unless its name holds \
       a slash. It gets the environment of $(cmd) without $(b,COLUMNS) and \
       $(b,LINES), so that it reads the size of the terminal, with \
       $(b,TERM=xterm-256color), then the $(b,--env) variables. Its standard \
       input is the terminal, where what is typed in the terminal $(cmd) runs \
       in arrives. When it runs in none, nothing is typed, so a program that \
       waits for input waits until $(cmd) is interrupted.";
    `P
      "On SIGINT or SIGTERM, $(cmd) sends the program and its descendants \
       SIGTERM, then SIGKILL a second later, writes the image of what the \
       terminal showed until the program exited, and exits 130 or 143.";
    `S Manpage.s_examples;
    `Pre
      "$(cmd) -o ls.svg -- ls -l --color=always\n\
       $(cmd) -o columns.svg --columns 100 -- dune exec -- ./main.exe\n\
       $(cmd) -o demo.cast -- ./demo.sh && k7 play -o demo.svg demo.cast\n\
       $(cmd) -o version.t -- k7 --version";
  ]

let exits =
  Cmd.Exit.
    [
      info 0 ~max:255
        ~doc:
          "the exit code of $(i,COMMAND), once the outputs are written, or 128 \
           plus the number of the signal that killed it.";
      info some_error
        ~doc:
          "also when k7 itself fails, such as when the program cannot start or \
           an output cannot be written, after printing $(b,k7:) and the reason \
           on standard error. A program can exit with 123, 124 or 125 too; \
           only k7's own errors print that line.";
      info 130 ~doc:"also when $(cmd) is interrupted by SIGINT.";
      info 143 ~doc:"also when $(cmd) is interrupted by SIGTERM.";
      info cli_error ~doc:"on command line parsing errors.";
      info internal_error ~doc:"on unexpected internal errors (bugs).";
    ]

let cmd =
  let doc = "run a program on a pseudo-terminal and record its screens" in
  let run settings env show delay outputs argv =
    Eio_main.run @@ fun stdenv ->
    run ?show ~delay:(Ms delay) stdenv ~settings ~env ~outputs argv
  in
  Cmd.make
    (Cmd.info "rec" ~doc ~man ~exits)
    Term.(
      const run $ Flags.settings $ Flags.env $ show $ delay $ outputs $ command)
