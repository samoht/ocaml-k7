(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Vhs_cram

let expect = Alcotest.(check string)

(* [replace ~sub ~by s] is [s] with each [sub] replaced by [by]. *)
let replace ~sub ~by s =
  let n = String.length sub in
  let b = Buffer.create (String.length s) in
  let rec go i =
    if i > String.length s - n then
      Buffer.add_substring b s i (String.length s - i)
    else if String.sub s i n = sub then (
      Buffer.add_string b by;
      go (i + n))
    else (
      Buffer.add_char b s.[i];
      go (i + 1))
  in
  go 0;
  Buffer.contents b

(* [unmarked s] is [s] with each character of the prompt mark written as a
   zero-width space, so that tapes compare whatever the mark. *)
let unmarked s =
  s
  |> replace ~sub:{|\x{2060}|} ~by:{|\x{200B}|}
  |> replace ~sub:{|\342\201\240|} ~by:{|\342\200\213|}

let repeat s = String.concat "" (List.init 16 (fun _ -> s))
let ready = "Wait+Line /" ^ repeat {|\x{200B}|} ^ "$/\n"

let env =
  {|Env VHS_CRAM_PROMPT `__vhs=$(printf '%s\n' ${PROMPT_COMMAND+"${PROMPT_COMMAND[@]}"} "PS1='\[\e[38;2;90;86;224m\]>\[|}
  ^ repeat {|\342\200\213|}
  ^ {|\] \[\e[0m\]'"); unset PROMPT_COMMAND; PROMPT_COMMAND=$__vhs; unset __vhs VHS_CRAM_PROMPT`|}

(* What every recording starts with, hidden. *)
let prelude ?(typed = {|Type 'eval "$VHS_CRAM_PROMPT"; clear'|}) () =
  "Hide\n" ^ env ^ "\n" ^ typed ^ "\nEnter\n" ^ ready ^ "Show\n"

let printed ?(setup = []) src =
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

let cram ?setup src = unmarked (printed ?setup src)

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
  (* A terminal reads U+009B as CSI. *)
  error "C1" {|line 1: "a\194\155b" holds a control character|}
    "  $ a\xc2\x9bb\n";
  error "continuation" {|line 2: "\255" is not valid UTF-8|} "  $ a\n  > \xff\n";
  Alcotest.(check bool)
    "empty" true
    (Cram.is_empty (Result.get_ok (Cram.parse "prose\n  > orphan\n")))

(* The setup and the clear finish while the recording is hidden, and the
   prompt [ready] relies on survives a setup that changes [PS1]. *)
let test_setup () =
  expect "setup"
    (prelude
       ~typed:
         {|Type `source 'slow.sh'; source 'it'\''s.sh'; eval "$VHS_CRAM_PROMPT"; clear`|}
       ()
    ^ "Type \"echo ready\"\nEnter\n" ^ ready ^ "Sleep 1000ms\n")
    (cram ~setup:[ "slow.sh"; "it's.sh" ] "  $ echo ready\n")

(* The mark is the same for the same test and differs between tests, so
   that a recording is not ready on the prompt of another one. *)
let test_mark () =
  let wait src =
    String.split_on_char '\n' (printed src)
    |> List.find (String.starts_with ~prefix:"Wait+Line")
  in
  Alcotest.(check string) "stable" (wait "  $ true\n") (wait "  $ true\n");
  Alcotest.(check bool) "distinct" false (wait "  $ true\n" = wait "  $ false\n");
  Alcotest.(check bool)
    "sixteen characters" true
    (unmarked (wait "  $ false\n") = String.trim ready)

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
        ("mark", test_mark);
      ] )
