(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Timings are noisy: report the fastest of five batches. *)
let measure name ~runs f =
  ignore (Sys.opaque_identity (f ()));
  Gc.full_major ();
  let batch () =
    let t0 = Unix.gettimeofday () in
    for _ = 1 to runs do
      ignore (Sys.opaque_identity (f ()))
    done;
    (Unix.gettimeofday () -. t0) *. 1e9 /. float runs
  in
  let bytes = Gc.allocated_bytes () in
  let first = batch () in
  let bytes' = Gc.allocated_bytes () in
  let best = List.fold_left min first (List.init 4 (fun _ -> batch ())) in
  Fmt.pr "%-28s %12.0f ns/op %12.0f bytes/op@." name best
    ((bytes' -. bytes) /. float runs)

let actions n =
  List.init n (fun i ->
      match i mod 4 with
      | 0 -> Fmt.kstr K7.type' "echo %d" i
      | 1 -> K7.key Enter
      | 2 -> K7.wait Prompt
      | _ -> K7.sleep (Ms 100))

let tape actions = Result.get_ok (K7.tape ~outputs:[ "demo.gif" ] actions)

let cram n =
  String.concat "" (List.init n (fun i -> Fmt.str "  $ echo %d\n  %d\n" i i))

let long_text =
  String.concat "" (List.init 2_000 (fun i -> Fmt.str "a\"b'c`d %d " i))

let () =
  let t = tape (actions 10_000) in
  measure "print 10k-item tape" ~runs:50 (fun () -> K7.to_string t);
  let src = cram 10_000 in
  measure "parse 10k-command cram" ~runs:50 (fun () -> K7_cli.Cram.parse src);
  let typed = [ K7.type' long_text ] in
  measure "check 3-quote text" ~runs:200 (fun () -> tape typed);
  let t = tape typed in
  measure "print 3-quote text" ~runs:200 (fun () -> K7.to_string t)
