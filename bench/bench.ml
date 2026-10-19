(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let measure name ~runs f =
  ignore (Sys.opaque_identity (f ()));
  Gc.full_major ();
  let bytes = Gc.allocated_bytes () in
  let t0 = Unix.gettimeofday () in
  for _ = 1 to runs do
    ignore (Sys.opaque_identity (f ()))
  done;
  let t1 = Unix.gettimeofday () in
  let bytes' = Gc.allocated_bytes () in
  let runs' = float runs in
  Printf.printf "%-28s %12.0f ns/op %12.0f bytes/op\n%!" name
    ((t1 -. t0) *. 1e9 /. runs')
    ((bytes' -. bytes) /. runs')

let items n =
  List.init n (fun i ->
      match i mod 4 with
      | 0 -> Vhs.type' (Printf.sprintf "echo %d" i)
      | 1 -> Vhs.key Enter
      | 2 -> Vhs.wait Prompt
      | _ -> Vhs.sleep (Ms 100))

let cram n =
  String.concat ""
    (List.init n (fun i -> Printf.sprintf "  $ echo %d\n  %d\n" i i))

let long_text =
  String.concat "" (List.init 2_000 (fun i -> Printf.sprintf "a\"b'c`d %d " i))

let () =
  let tape = items 10_000 in
  measure "print 10k-item tape" ~runs:50 (fun () -> Fmt.str "%a" Vhs.pp tape);
  let src = cram 10_000 in
  measure "parse 10k-command cram" ~runs:50 (fun () -> Vhs.Cram.parse src);
  measure "type 3-quote text" ~runs:200 (fun () ->
      Fmt.str "%a" Vhs.pp [ Vhs.type' long_text ])
