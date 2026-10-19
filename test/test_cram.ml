(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Vhs_cram

let expect = Alcotest.(check string)

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
  expect "minimal" "Type \"./main.exe\"\nEnter\nWait\nSleep 1000ms\n"
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
    "Type \"echo hi\"\n\
     Enter\n\
     Wait\n\
     Sleep 1000ms\n\
     Type \"printf a\\\"\n\
     Enter\n\
     Type \"b\"\n\
     Enter\n\
     Wait\n\
     Sleep 1000ms\n"
    (cram src)

let test_crlf () =
  expect "crlf" "Type \"echo hi\"\nEnter\nWait\nSleep 1000ms\n"
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

let test_setup () =
  expect "setup"
    "Hide\n\
     Type \"source 'slow.sh'\"\n\
     Enter\n\
     Type \"source 'it'\\''s.sh'\"\n\
     Enter\n\
     Type \"clear\"\n\
     Enter\n\
     Show\n\
     Type \"echo ready\"\n\
     Enter\n\
     Wait\n\
     Sleep 1000ms\n"
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
