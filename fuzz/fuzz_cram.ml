(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Alcobar
open K7_cli

let is_control u =
  Uchar.to_int u < 0x20 || (Uchar.to_int u >= 0x7f && Uchar.to_int u <= 0x9f)

let has_control s =
  let rec go i =
    i < String.length s
    &&
    let d = String.get_utf_8_uchar s i in
    (Uchar.utf_decode_is_valid d && is_control (Uchar.utf_decode_uchar d))
    || go (i + Uchar.utf_decode_length d)
  in
  go 0

(* VHS records no frame without a command: the sleep gives it one. *)
let printed ?(setup = []) cram =
  match
    K7.tape ~outputs:[ "x.gif" ]
      (Cram.actions ~setup cram @ [ K7.sleep (Ms 100) ])
  with
  | Ok t -> Fmt.str "%a" K7.pp t
  | Error e -> failf "rejected actions: %a" K7.pp_error e

let submitted tape =
  let decode line =
    let n = String.length line in
    String.sub line 6 (n - 7)
  in
  let rec go pending acc = function
    | [] -> List.rev acc
    | "Enter" :: rest -> go "" (pending :: acc) rest
    | line :: rest when String.starts_with ~prefix:"Type " line ->
        go (pending ^ decode line) acc rest
    | _ :: rest -> go pending acc rest
  in
  go "" [] (String.split_on_char '\n' tape)

let typeable s =
  let b = Buffer.create (String.length s) in
  let rec go i =
    if i < String.length s then (
      let d = String.get_utf_8_uchar s i in
      let u = Uchar.utf_decode_uchar d in
      if is_control u then Buffer.add_char b ' ' else Buffer.add_utf_8_uchar b u;
      go (i + Uchar.utf_decode_length d))
  in
  go 0;
  Buffer.contents b

let test_parse src =
  let command_lines =
    String.split_on_char '\n' src
    |> List.map (fun l ->
        if String.ends_with ~suffix:"\r" l then
          String.sub l 0 (String.length l - 1)
        else l)
    |> List.filter (String.starts_with ~prefix:"  $ ")
    |> List.map (fun l -> String.sub l 4 (String.length l - 4))
  in
  let untabbed l = String.map (fun c -> if c = '\t' then ' ' else c) l in
  let bad l = (not (String.is_valid_utf_8 l)) || has_control (untabbed l) in
  match Cram.parse src with
  | Ok cram ->
      if List.exists bad command_lines then failf "accepted %S" src;
      ignore (printed cram)
  | Error _ -> ()

let test_commands commands eol =
  let commands = List.map typeable commands in
  let src =
    "prose" ^ eol ^ "  > orphan" ^ eol
    ^ String.concat ""
        (List.map
           (fun c ->
             "  $ " ^ c ^ eol ^ "  > " ^ c ^ eol ^ "  >" ^ eol ^ "  output"
             ^ eol ^ "  > not a continuation" ^ eol)
           commands)
  in
  match Cram.parse src with
  | Error e -> failf "typeable commands rejected: %s" e
  | Ok cram ->
      let got =
        match submitted (printed cram) with
        | prelude :: got when String.ends_with ~suffix:"; clear" prelude -> got
        | _ -> fail "no hidden prelude"
      in
      let expected = List.concat_map (fun c -> [ c; c; "" ]) commands in
      if got <> expected then
        failf "%S submitted %d lines, not %d" src (List.length got)
          (List.length expected);
      check (Cram.is_empty cram = (commands = []))

let test_setup scripts command =
  let scripts = List.map typeable scripts and command = typeable command in
  let cram = Result.get_ok (Cram.parse ("  $ " ^ command)) in
  let tape = printed ~setup:scripts cram in
  let lines = String.split_on_char '\n' tape in
  let count l = List.length (List.filter (String.equal l) lines) in
  check (count "Hide" = 1 && count "Show" = 1);
  match submitted tape with
  | [ prelude; c ] ->
      check (String.equal c command);
      (* bash's [source] does not look ./ up in the PATH. *)
      let path s = if Filename.is_relative s then "./" ^ s else s in
      let sources =
        String.concat ""
          (List.map
             (fun s -> "source " ^ Filename.quote (path s) ^ "; ")
             scripts)
      in
      check (String.starts_with ~prefix:(sources ^ "eval ") prelude);
      check (String.ends_with ~suffix:"; clear" prelude)
  | l -> failf "submitted %d lines" (List.length l)

(* Output must not add a command or a continuation to a generated test. *)
let test_recorded_output_is_data n continuation =
  Native_test.with_dir @@ fun env cwd ->
  let prefix = if continuation then "> " else "$ " in
  let output = prefix ^ "echo " ^ String.make (1 + (n mod 32)) 'a' in
  let source, _ =
    match
      K7.cram
        ~clock:(Eio_mock.Clock.Mono.make ())
        ~proc:(Eio.Stdenv.process_mgr env)
        ~cwd
        [ "printf"; "%s\n"; output ]
    with
    | Ok result -> result
    | Error e -> failf "%a" K7.pp_error e
  in
  let commands src =
    match Cram.parse src with
    | Error e -> failf "%s" e
    | Ok t ->
        printed t |> String.split_on_char '\n'
        |> List.filter (String.starts_with ~prefix:"Type ")
  in
  let header = List.hd (String.split_on_char '\n' source) ^ "\n" in
  check_eq ~pp:Fmt.(Dump.list string) (commands header) (commands source)

(* The readiness file is written after installing the trap. No clock moves:
   the child's SIGTERM output must drain before cram returns. *)
let test_cleanup_output n =
  Native_test.with_dir @@ fun env cwd ->
  let token = Fmt.str "TERM_SEEN_%d" n in
  let child =
    "trap 'printf " ^ token ^ "; exit 0' TERM; : > ready; while :; do :; done"
  in
  let command =
    "/bin/sh -c " ^ Filename.quote child
    ^ " & while [ ! -e ready ]; do :; done; exit 0"
  in
  match
    K7.cram
      ~clock:(Eio_mock.Clock.Mono.make ())
      ~proc:(Eio.Stdenv.process_mgr env)
      ~cwd
      [ "/bin/sh"; "-c"; command ]
  with
  | Error e -> failf "%a" K7.pp_error e
  | Ok (source, status) -> (
      check (status = `Exited 0);
      match String.split_on_char '\n' source with
      | _command :: output ->
          check_eq ~pp:Fmt.(Dump.list string) [ "  " ^ token; "" ] output
      | [] -> fail "no recorded command")

let test_cram_nul_argument prefix suffix =
  Native_test.with_dir @@ fun env cwd ->
  let argument = prefix ^ "\000" ^ suffix in
  let result =
    K7.cram
      ~clock:(Eio_mock.Clock.Mono.make ())
      ~proc:(Eio.Stdenv.process_mgr env)
      ~cwd
      [ "/bin/sh"; "-c"; ": > launched"; "k7-argv"; argument ]
  in
  check (not (Eio.Path.is_file Eio.Path.(cwd / "launched")));
  match result with
  | Error (Invalid _ | Io _) -> ()
  | Error e -> failf "unexpected error: %a" K7.pp_error e
  | Ok _ -> failf "executed an argv containing NUL: %S" argument

let alphabet =
  [|
    0x1;
    0x9;
    0xa;
    0xd;
    0x1b;
    0x7f;
    0x85;
    0xe9;
    0x2028;
    0x4e2d;
    0x1f44d;
    0x27;
    0x22;
    0x5c;
    0x24;
    0x60;
    0x20;
    0x61;
  |]

let test_cram_argument_bytes scalars =
  Native_test.with_dir @@ fun env cwd ->
  let buffer = Buffer.create 128 in
  List.iter
    (fun n ->
      Buffer.add_utf_8_uchar buffer
        (Uchar.of_int alphabet.(n mod Array.length alphabet)))
    scalars;
  let argument = Buffer.contents buffer in
  let proc = Eio.Stdenv.process_mgr env in
  let received = Eio.Path.(cwd / "received") in
  match
    K7.cram
      ~clock:(Eio_mock.Clock.Mono.make ())
      ~proc ~cwd
      [ "/bin/sh"; "-c"; "printf '%s' \"$1\" > received"; "k7-argv"; argument ]
  with
  | Error (Invalid _) when String.ends_with ~suffix:"\n" argument ->
      check (not (Eio.Path.is_file received))
  | Error e -> failf "%a" K7.pp_error e
  | Ok (source, status) ->
      check (status = `Exited 0);
      check_eq ~pp:Fmt.Dump.string argument (Eio.Path.load received);
      let line = List.hd (String.split_on_char '\n' source) in
      let line = String.sub line 4 (String.length line - 4) in
      List.iter
        (fun shell ->
          Eio.Path.unlink received;
          Eio.Process.run proc ~cwd [ shell; "-c"; line ];
          check_eq ~pp:Fmt.Dump.string argument (Eio.Path.load received))
        (List.filter Sys.file_exists [ "/bin/sh"; "/bin/dash"; "/bin/bash" ])

let suite =
  ( "cram",
    [
      test_case "cram preserves non-NUL Unicode and control arguments"
        [ list uint8 ]
        test_cram_argument_bytes;
      test_case "cram rejects NUL anywhere in an argument before execution"
        [ bytes; bytes ] test_cram_nul_argument;
      test_case "cram drains output from descendants during group cleanup"
        [ uint8 ] test_cleanup_output;
      test_case "recorded output stays out of cram commands" [ uint8; bool ]
        test_recorded_output_is_data;
      test_case "parse"
        [ choose [ bytes; map [ bytes ] (fun s -> "  $ " ^ s) ] ]
        test_parse;
      test_case "commands"
        [ list bytes; choose [ const "\n"; const "\r\n" ] ]
        test_commands;
      test_case "setup" [ list bytes; bytes ] test_setup;
    ] )
