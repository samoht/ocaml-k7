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

(* Go folds case with Unicode's simple folding, counts ASCII word
   characters only at a word boundary, keeps a branch's flags in the branches
   after it and repeats the item before a flag group. *)
let test_go_semantics () =
  List.iter
    (fun (p, t, expected) ->
      match Regexp_match.compile p with
      | Ok re ->
          Alcotest.(check bool)
            (Fmt.str "%S on %S" p t) expected
            (Regexp_match.matches re t)
      | Error e -> Alcotest.failf "%S: %s" p e)
    [
      ("(?i)k", "\xe2\x84\xaa", true);
      ("(?i)\xc3\xa9", "\xc3\x89", true);
      ("(?i)\\x{3c3}", "\xcf\x82", true);
      ("(?i)[a-z]", "\xc5\xbf", true);
      ("(?i)\\W", "\xc5\xbf", false);
      ("(?i)[^\\w]", "\xe2\x84\xaa", false);
      ("\\bcaf\\b", "caf\xc3\xa9", true);
      ("\\b\xc3\xa9", " \xc3\xa9", false);
      ("x\\B", "x\xc3\xa9", false);
      ("a(?i)|B", "b", true);
      ("(a(?i))B", "aB", true);
      ("(a(?i))b", "aB", false);
      ("a(?s){2}", "aa", true);
    ]

(* Patterns and texts Go's MatchString judged, written by match_oracle.go.
   The cases are too many to wait on each one, so they reach the matcher
   directly. *)
let test_go_oracle () =
  let unhex h =
    String.init
      (String.length h / 2)
      (fun i -> Char.chr (int_of_string ("0x" ^ String.sub h (2 * i) 2)))
  in
  In_channel.with_open_bin "match_cases.txt" In_channel.input_all
  |> String.split_on_char '\n'
  |> List.iter (fun line ->
      match String.split_on_char '\t' line with
      | [ v; p; t ] -> (
          let p = unhex p and t = unhex t in
          match Regexp_match.compile p with
          | Ok re ->
              Alcotest.(check bool)
                (Fmt.str "%S on %S" p t) (v = "1")
                (Regexp_match.matches re t)
          | Error e -> Alcotest.failf "%S: %s" p e)
      | [ "" ] -> ()
      | _ -> Alcotest.failf "bad case %S" line)

(* VHS and the native recorder agree on Unicode case folding and ASCII word
   boundaries: the waits that hold end both recordings, and one that does not
   hold makes both time out. *)
let test_vhs_oracle () =
  with_dir @@ fun env cwd ->
  let clock = Eio.Stdenv.mono_clock env and proc = Eio.Stdenv.process_mgr env in
  let text = {|\307\204 \305\277tra\303\237e K caf\303\251|} in
  let tape waits =
    match
      Vhs.tape ~outputs:[ "out.txt" ]
        (Vhs.type' ("printf '" ^ text ^ "\\n'\n") :: Vhs.wait Prompt :: waits)
    with
    | Ok t -> t
    | Error e -> Alcotest.fail e
  in
  let both t =
    (Vhs.render ?vhs:oracle ~clock ~proc ~cwd t, Native_test.record env cwd t)
  in
  let holds =
    List.map
      (fun re -> Vhs.wait ~timeout:(Ms 2000) (Screen re))
      [
        "(?i)\xc7\x86 ";
        "(?i)STRA\xe1\xba\x9eE";
        "(?i)\xe2\x84\xaa ";
        {|\bcaf\b|};
        {|(?i)[^\W]af|};
      ]
  in
  (match both (tape holds) with
  | Ok (), Ok _ -> ()
  | Error e, _ | _, Error e -> Alcotest.failf "%a" Vhs.pp_error e);
  match both (tape [ Vhs.wait ~timeout:(Ms 1000) (Screen {|caf\B|}) ]) with
  | Error (Failed _), Error (Timeout _) -> ()
  | _ -> Alcotest.fail "a wait that does not hold ended a recording"

let suite =
  ( "regexp_match",
    List.map
      (fun (name, fn) -> Alcotest.test_case name `Quick fn)
      ([
         ("matching", test_matching);
         ("not matching", test_not_matching);
         ("Unicode class", test_unicode_class);
         ("Go semantics", test_go_semantics);
         ("Go oracle", test_go_oracle);
       ]
      @
      if Option.is_some oracle then [ ("vhs oracle", test_vhs_oracle) ] else []
      ) )
