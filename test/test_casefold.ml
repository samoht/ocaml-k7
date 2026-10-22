(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let mem c ranges = List.exists (fun (lo, hi) -> lo <= c && c <= hi) ranges

let check name c ~folds ~not_folds =
  let r = Casefold.fold [ (c, c) ] in
  List.iter
    (fun f -> if not (mem f r) then Alcotest.failf "%s: %x lacks %x" name c f)
    (c :: folds);
  List.iter
    (fun f -> if mem f r then Alcotest.failf "%s: %x has %x" name c f)
    not_folds

let test_orbits () =
  check "k" 0x6b ~folds:[ 0x4b; 0x212a ] ~not_folds:[ 0x6c ];
  check "Kelvin" 0x212a ~folds:[ 0x4b; 0x6b ] ~not_folds:[];
  check "long s" 0x17f ~folds:[ 0x53; 0x73 ] ~not_folds:[];
  check "sigma" 0x3c2 ~folds:[ 0x3a3; 0x3c3 ] ~not_folds:[];
  check "dz" 0x1c5 ~folds:[ 0x1c4; 0x1c6 ] ~not_folds:[];
  check "e acute" 0xe9 ~folds:[ 0xc9 ] ~not_folds:[ 0x65 ];
  check "no case" 0x4e2d ~folds:[] ~not_folds:[ 0x4e2c; 0x4e2e ]

let test_ranges () =
  let r = Casefold.fold [ (0x61, 0x63); (0x3b1, 0x3b1) ] in
  List.iter
    (fun c -> if not (mem c r) then Alcotest.failf "lacks %x" c)
    [ 0x41; 0x42; 0x43; 0x61; 0x63; 0x391 ];
  if mem 0x44 r then Alcotest.fail "has D"

let suite =
  ( "casefold",
    List.map
      (fun (name, fn) -> Alcotest.test_case name `Quick fn)
      [ ("orbits", test_orbits); ("ranges", test_ranges) ] )
