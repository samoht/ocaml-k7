(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Vhs_cram

let expect = Alcotest.(check string)
let ready = {|Wait+Line /\x{200B}$/|} ^ "\n"
let ps1 = {|PS1='\[\e[38;2;90;86;224m\]>\[\342\200\213\] \[\e[0m\]'|}

(* What every recording starts with, hidden. *)
let prelude ?(sources = "") () =
  "Hide\nType \"" ^ sources ^ ps1 ^ "; clear\"\nEnter\n" ^ ready ^ "Show\n"

let cram ?(setup = []) src =
  match Cram.parse src with
  | Error msg -> Alcotest.fail msg
  | Ok t -> (
      match Vhs.tape ~outputs:[ "x.gif" ] (Cram.actions ~setup t) with
      | Ok tape ->
          let s = Fmt.str "%a" Vhs.pp tape in
          let header = "Output \"x.gif\"\n" in
          String.sub s (String.length header)
            (String.length s - String.length header)
      | Error msg -> Alcotest.fail msg)

let test_minimal () =
  expect "minimal"
    (prelude () ^ "Type \"./main.exe\"\nEnter\n" ^ ready ^ "Sleep 1000ms\n")
    (cram "  $ ./main.exe\n  +hello, world\n")

let test_parse () =
  let src =
    "prose is ignored\n\n\
    \  $ echo hi\n\
    \  hi\n\
    \  $ printf a\\\n\
    \  > b\n\
    \  ab\n\
    \  [1]\n\
    \  > not a continuation\n"
  in
  expect "cram"
    (prelude () ^ "Type \"echo hi\"\nEnter\n" ^ ready
   ^ "Sleep 1000ms\nType \"printf a\\\"\nEnter\nType \"b\"\nEnter\n" ^ ready
   ^ "Sleep 1000ms\n")
    (cram src)

let test_crlf () =
  expect "crlf"
    (prelude () ^ "Type \"echo hi\"\nEnter\n" ^ ready ^ "Sleep 1000ms\n")
    (cram "  $ echo hi\r\n  hi\r\n")

let test_parse_errors () =
  let error name expected src =
    Alcotest.(check (result unit string))
      name (Error expected)
      (Result.map ignore (Cram.parse src))
  in
  error "control" {|line 2: "echo \001" holds a control character|}
    "prose\n  $ echo \x01\n";
  (* A tab would reach bash's completion, not the command. *)
  error "tab" {|line 1: "a\tb" holds a control character|} "  $ a\tb\n";
  error "continuation" {|line 2: "\255" is not valid UTF-8|} "  $ a\n  > \xff\n";
  Alcotest.(check bool)
    "empty" true
    (Cram.is_empty (Result.get_ok (Cram.parse "prose\n  > orphan\n")))

(* The setup and the clear finish while the recording is hidden, and the
   prompt [ready] relies on survives a setup that changes [PS1]. *)
let test_setup () =
  expect "setup"
    (prelude ~sources:{|source 'slow.sh'; source 'it'\''s.sh'; |} ()
    ^ "Type \"echo ready\"\nEnter\n" ^ ready ^ "Sleep 1000ms\n")
    (cram ~setup:[ "slow.sh"; "it's.sh" ] "  $ echo ready\n")

let suite =
  ( "cram",
    List.map
      (fun (name, fn) -> Alcotest.test_case name `Quick fn)
      [
        ("minimal", test_minimal);
        ("parse", test_parse);
        ("crlf", test_crlf);
        ("parse errors", test_parse_errors);
        ("setup", test_setup);
      ] )
