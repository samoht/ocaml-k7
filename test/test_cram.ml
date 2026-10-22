(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open K7_cli

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
  let z = {|\342\200\213|} in
  String.concat ""
    [
      {|Env K7_PROMPT `bind 'set input-meta on'; bind 'set output-meta on'; bind 'set convert-meta off'; __k7_n=; __k7_prompt() { PS1='\[\e[38;2;90;86;224m\]> \[|};
      repeat z;
      {|\e[0m\]'; __k7_n=$__k7_n'|};
      z;
      {|'; PS2='> \[|};
      repeat z;
      {|'$__k7_n'\]'; }; __k7=$(printf '%s\n' ${PROMPT_COMMAND+"${PROMPT_COMMAND[@]}"} __k7_prompt); unset PROMPT_COMMAND; PROMPT_COMMAND=$__k7; unset __k7 K7_PROMPT`|};
    ]

(* What every recording starts with, hidden. *)
let prelude ?(typed = {|Type 'eval "$K7_PROMPT"; clear'|}) () =
  "Hide\n" ^ env ^ "\n" ^ typed ^ "\nEnter\n" ^ ready ^ "Show\n"

let printed ?(setup = []) src =
  match Cram.parse src with
  | Error msg -> Alcotest.fail msg
  | Ok t -> (
      match K7.tape ~outputs:[ "x.gif" ] (Cram.actions ~setup t) with
      | Ok tape ->
          let s = Fmt.str "%a" K7.pp tape in
          let header = "Output \"x.gif\"\n" in
          String.sub s (String.length header)
            (String.length s - String.length header)
      | Error msg -> Alcotest.failf "%a" K7.pp_error msg)

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

(* As in dune, a bare [  >] continues the command with an empty line, so a
   heredoc keeps its blank lines; a CRLF line ending reads as LF. *)
let test_empty_continuation () =
  let heredoc =
    "Type \"cat <<EOF\"\n\
     Enter\n\
     Type \"a\"\n\
     Enter\n\
     Enter\n\
     Type \"b\"\n\
     Enter\n\
     Type \"EOF\"\n\
     Enter\n"
  in
  let expected = prelude () ^ heredoc ^ ready ^ "Sleep 1000ms\n" in
  expect "LF" expected (cram "  $ cat <<EOF\n  > a\n  >\n  > b\n  > EOF\n");
  expect "CRLF" expected
    (cram "  $ cat <<EOF\r\n  > a\r\n  >\r\n  > b\r\n  > EOF\r\n");
  expect "end of input"
    (prelude () ^ "Type \"cat <<EOF\"\nEnter\nEnter\n" ^ ready
   ^ "Sleep 1000ms\n")
    (cram "  $ cat <<EOF\n  >")

let test_parse_errors () =
  let error name expected src =
    Alcotest.(check (result unit string))
      name (Error expected)
      (Result.map ignore (Cram.parse src))
  in
  error "control" {|line 2: "echo \x01" holds a control character|}
    "prose\n  $ echo \x01\n";
  (* A tab would reach bash's completion, not the command. *)
  error "tab" {|line 1: "a\tb" holds a control character|} "  $ a\tb\n";
  (* A terminal reads U+009B as CSI. *)
  error "C1" {|line 1: "a\u{9B}b" holds a control character|} "  $ a\xc2\x9bb\n";
  (* The message keeps valid UTF-8 as it is and replaces an invalid byte. *)
  error "continuation" "line 2: \"caf\u{e9} \u{FFFD}\" is not valid UTF-8"
    "  $ a\n  > caf\xc3\xa9 \xff\n";
  error "UTF-8"
    "line 1: \"\\\"caf\u{e9}\\\" \\\\ \\x01\" holds a control character"
    "  $ \"caf\xc3\xa9\" \\ \x01\n";
  Alcotest.(check bool)
    "empty" true
    (Cram.is_empty (Result.get_ok (Cram.parse "prose\n  > orphan\n")))

(* The setup and the clear finish while the recording is hidden, and the
   prompt [ready] relies on survives a setup that changes [PS1]. *)
let test_setup () =
  expect "setup"
    (prelude
       ~typed:
         {|Type `source './slow.sh'; source './it'\''s.sh'; eval "$K7_PROMPT"; clear`|}
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

(* After a timeout, vhs prints the line under the cursor; at bash's
   continuation prompt, it is the mark followed by one more character per
   prompt shown, which counts the commands. *)
let test_unfinished () =
  let t = Result.get_ok (Cram.parse "  $ cat <<EOF\n  $ echo b\n") in
  let wait =
    String.split_on_char '\n' (printed "  $ cat <<EOF\n  $ echo b\n")
    |> List.find (String.starts_with ~prefix:"Wait+Line")
  in
  let zwsp = "\xe2\x80\x8b" and wj = "\xe2\x81\xa0" in
  let mark =
    wait
    |> replace ~sub:{|\x{200B}|} ~by:zwsp
    |> replace ~sub:{|\x{2060}|} ~by:wj
    |> replace ~sub:"Wait+Line /" ~by:""
    |> replace ~sub:"$/" ~by:""
  in
  let tally = if String.ends_with ~suffix:zwsp mark then wj else zwsp in
  let output k =
    "failed to execute command: timeout waiting for ...; last value was: > "
    ^ mark
    ^ String.concat "" (List.init k (fun _ -> tally))
    ^ "\n"
  in
  let check name expected out =
    Alcotest.(check (option (pair int string)))
      name expected
      (Cram.unfinished ~setup:[] t out)
  in
  check "first" (Some (1, "cat <<EOF")) (output 1);
  check "second" (Some (2, "echo b")) (output 2);
  check "primary prompt" None (output 0);
  check "no such command" None (output 3);
  check "other output" None "last value was: >\n";
  check "mark before the last value" None
    ("\x20" ^ mark ^ tally ^ "\nlast value was: >\n")

let suite =
  ( "cram",
    List.map
      (fun (name, fn) -> Alcotest.test_case name `Quick fn)
      [
        ("minimal", test_minimal);
        ("parse", test_parse);
        ("crlf", test_crlf);
        ("empty continuation", test_empty_continuation);
        ("parse errors", test_parse_errors);
        ("setup", test_setup);
        ("mark", test_mark);
        ("unfinished", test_unfinished);
      ] )
