(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let line re =
  match K7.tape ~outputs:[ "x.gif" ] [ K7.wait (Screen re) ] with
  | Error e -> Error (Native_test.invalid e)
  | Ok t -> (
      match String.split_on_char '\n' (K7.to_string t) with
      | [ _output; line; "" ] -> Ok line
      | _ -> Alcotest.failf "%S: not one line" re)

let written name expected re =
  Alcotest.(check (result string string))
    name
    (Ok ("Wait+Screen " ^ expected))
    (line re)

let valid name ok re =
  Alcotest.(check bool) (Fmt.str "%s %S" name re) ok (Result.is_ok (line re))

let test_slashes () =
  written "slashes" {|/a\x2fb\x2fc\\\x2f[\x2f]/|} {|a/b\/c\\/[/]|};
  (* Inside \Q...\E a hex escape is literal, so the quote ends around the
     slash. *)
  written "quoted slash" {|/\Q\E\x2f\Q\E/|} {|\Q/\E|};
  written "quoted backslash" {|/\Qa\\E\x2f\Qb\E/|} {|\Qa\/b\E|};
  written "unterminated quote" {|/\Qa\\E/|} {|\Qa\|}

let test_syntax () =
  List.iter
    (fun (ok, re) -> valid "Go" ok re)
    [
      (true, {|(?P<n>a)(?<m>b)|});
      (true, {|\p{greek}\p{Old_Italic}\pL\P{^Lu}\p{Letter}|});
      (false, {|\p{Grek}|});
      (true, {|[[:^alpha:]\d-]|});
      (false, {|[[:foo:]]|});
      (false, {|[z-a]|});
      (true, {|(x{2}){500}|});
      (false, {|(x{2}){501}|});
      (true, {|x{01}|});
      (false, {|a**|});
      (true, {|a*?|});
      (false, {|a*??|});
      (true, {|a(?i)*|});
      (false, {|(?i)*|});
      (false, {|(?i-)a|});
      (true, {|(?)|});
      (false, {|\1|});
      (true, {|\12|});
      (false, {|\Z|});
      (false, {|\C|});
      (true, {|\x{10FFFF}|});
      (false, {|\x{110000}|});
      (false, "\xff");
    ]

(* The check is linear: milliseconds, not minutes. *)
let test_linear () =
  let n = 200_000 in
  let re = "[" ^ String.concat "" (List.init n (fun _ -> "[:")) ^ "a]" in
  valid "many [:" true re;
  let re = String.concat "" (List.init n (fun _ -> "[[:a]")) in
  valid "many classes" true re

(* Go's regexp.Compile, which VHS runs, written by regexp_oracle.go. *)
let test_oracle () =
  let cases =
    In_channel.with_open_bin "regexp_cases.txt" In_channel.input_all
  in
  String.split_on_char '\n' cases
  |> List.iter (fun l ->
      if l <> "" then
        valid "Go" (l.[0] = '1') (String.sub l 2 (String.length l - 2)))

let suite =
  ( "regexp_syntax",
    List.map
      (fun (name, fn) -> Alcotest.test_case name `Quick fn)
      [
        ("slashes", test_slashes);
        ("syntax", test_syntax);
        ("Go oracle", test_oracle);
        ("linear", test_linear);
      ] )
