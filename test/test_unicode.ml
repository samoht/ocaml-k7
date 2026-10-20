(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let mem c ranges = List.exists (fun (lo, hi) -> lo <= c && c <= hi) ranges

let table name =
  match Unicode.find name with
  | Some t -> t
  | None -> Alcotest.failf "no class %S" name

(* Each table is sorted and disjoint, holds what Go's does, and leaves out
   the rest. *)
let test_tables () =
  let check name ~has ~lacks =
    let ranges, negated = table name in
    Alcotest.(check bool) (name ^ " not negated") false negated;
    let rec sorted = function
      | (a, b) :: ((c, _) :: _ as rest) -> a <= b && b + 1 < c && sorted rest
      | [ (a, b) ] -> a <= b
      | [] -> true
    in
    Alcotest.(check bool) (name ^ " sorted") true (sorted ranges);
    List.iter
      (fun c ->
        Alcotest.(check bool) (Fmt.str "%s has %x" name c) true (mem c ranges))
      has;
    List.iter
      (fun c ->
        Alcotest.(check bool)
          (Fmt.str "%s lacks %x" name c)
          false (mem c ranges))
      lacks
  in
  check "Lu" ~has:[ 0x41; 0x391; 0x1e9e ] ~lacks:[ 0x61; 0x31 ];
  check "L" ~has:[ 0x41; 0x61; 0x4e2d ] ~lacks:[ 0x31; 0x20 ];
  check "Nd" ~has:[ 0x30; 0x663 ] ~lacks:[ 0x41 ];
  check "Greek" ~has:[ 0x3b1; 0x3a9 ] ~lacks:[ 0x61 ];
  check "Any" ~has:[ 0; 0x10ffff ] ~lacks:[];
  check "Ascii" ~has:[ 0; 0x7f ] ~lacks:[ 0x80; 0x17f ];
  check "Lc" ~has:[ 0x41; 0x61 ] ~lacks:[ 0x2b0 ]

(* Go reads a name ignoring underscores, hyphens, spaces and the case of
   its letters, reads long category names and negates Cn for Assigned. *)
let test_names () =
  let same a b =
    Alcotest.(check bool) (a ^ " is " ^ b) true (table a = table b)
  in
  same "old_italic" "Old_Italic";
  same "OLD-ITALIC" "Olditalic";
  same "Letter" "L";
  same " L " "L";
  same "lowercase letter" "Ll";
  same "Cased_Letter" "LC";
  Alcotest.(check bool) "Assigned" true (snd (table "Assigned"));
  Alcotest.(check bool)
    "Assigned is Cn" true
    (fst (table "Assigned") = fst (table "Cn"));
  List.iter
    (fun n ->
      Alcotest.(check bool) (n ^ " unknown") true (Unicode.find n = None))
    [ ""; "Klingon"; "Lx"; "^L"; "Ll\xc3\xa9" ]

let suite =
  ( "unicode",
    List.map
      (fun (name, fn) -> Alcotest.test_case name `Quick fn)
      [ ("tables", test_tables); ("names", test_names) ] )
