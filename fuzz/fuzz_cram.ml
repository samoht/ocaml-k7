(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Alcobar
open K7_cli

(* C0, DEL and C1. *)
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

(* A cram test without commands records no frame; the final sleep keeps
   its tape one VHS can record. *)
let printed ?(setup = []) cram =
  match
    Vhs.tape ~outputs:[ "x.gif" ]
      (Cram.actions ~setup cram @ [ Vhs.sleep (Ms 100) ])
  with
  | Ok t -> Fmt.str "%a" Vhs.pp t
  | Error e -> failf "rejected actions: %a" Vhs.pp_error e

(* The text the printed tape types, one string per [Enter]. *)
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

(* Make arbitrary bytes a typeable command: valid UTF-8, no control
   characters. *)
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

(* [parse] is total, and fails iff a command line holds a control character
   or invalid UTF-8. *)
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
  let bad l = (not (String.is_valid_utf_8 l)) || has_control l in
  match Cram.parse src with
  | Ok cram ->
      if List.exists bad command_lines then failf "accepted %S" src;
      ignore (printed cram)
  | Error _ ->
      (* A rejected line may also be a continuation. *)
      ()

(* Commands and continuations are typed in order, each line submitted, a
   bare [  >] as an empty line; prose, output and orphan continuations are
   not; CRLF reads as LF. *)
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

(* A setup is sourced hidden, in order, then the prompt is set and the
   screen cleared, before the first command; without a setup, only the
   prompt and the clear are. *)
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
      (* A relative path starts with ./, which bash's source does not
         search for in the PATH. *)
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
