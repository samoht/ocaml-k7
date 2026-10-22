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

let suite =
  ( "cram",
    [
      test_case "parse"
        [ choose [ bytes; map [ bytes ] (fun s -> "  $ " ^ s) ] ]
        test_parse;
      test_case "commands"
        [ list bytes; choose [ const "\n"; const "\r\n" ] ]
        test_commands;
      test_case "setup" [ list bytes; bytes ] test_setup;
    ] )
