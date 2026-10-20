(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The matcher is internal: test it through the waits of [Vhs.record]. *)

open Native_test

(* The command prints "hello Wörld 42 x>y end". *)
let format = {|hello W\303\266rld 42 x>y end\n|}

let record_waits waits =
  with_dir @@ fun env cwd ->
  record env cwd
    (tape
       ([ Vhs.type' ("printf '" ^ format ^ "'\n"); Vhs.wait Prompt ] @ waits))

(* Each pattern holds on the screen, so no wait times out. *)
let test_matching () =
  let patterns =
    [
      "hello";
      {|W\x{f6}rld|};
      "(?i)HELLO";
      {|\d+ x>y|};
      {|\bend\b|};
      "[[:digit:]]{2}";
      "[^a-z ]rld";
      {|\Qx>y\E|};
      {|^> printf|};
      "(?m)^hello";
      "(?s)end.>";
      "a|42";
      {|(?m)^>$|};
      "W.rld";
      "[ö]";
      {|4\d|};
    ]
  in
  match
    record_waits
      (List.map (fun p -> Vhs.wait ~timeout:(Ms 3000) (Screen p)) patterns)
  with
  | Ok _ -> ()
  | Error e -> Alcotest.failf "%a" Vhs.pp_error e

(* A pattern that does not hold times out, even one that matches a prefix
   of the text, a byte of a multi-byte character or across lines without
   [(?s)]. *)
let test_not_matching () =
  List.iter
    (fun p ->
      match record_waits [ Vhs.wait ~timeout:(Ms 200) (Screen p) ] with
      | Error (Timeout _) -> ()
      | Ok _ -> Alcotest.failf "%S matched" p
      | Error e -> Alcotest.failf "%S: %a" p Vhs.pp_error e)
    [ "helloo"; "^hello"; "W[^\\x{f6}]rld"; "end.>"; {|\bell|}; "x{3}" ]

let test_unicode_class () =
  match record_waits [ Vhs.wait (Screen {|\p{Greek}|}) ] with
  | Error (Unsupported why) ->
      check_contains "says why" ~sub:"Unicode class" why
  | Ok _ -> Alcotest.fail "a Unicode class played natively"
  | Error e -> Alcotest.failf "%a" Vhs.pp_error e

let suite =
  ( "regexp_match",
    List.map
      (fun (name, fn) -> Alcotest.test_case name `Quick fn)
      [
        ("matching", test_matching);
        ("not matching", test_not_matching);
        ("Unicode class", test_unicode_class);
      ] )
