(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Alcobar
open Vhs_cram

(* [parse] and [actions] are total. *)
let test_parse src =
  match Cram.parse src with
  | Error _ -> ()
  | Ok cram ->
      ignore (Vhs.tape ~outputs:[ "x.gif" ] (Cram.actions ~setup:[] cram))

let suite =
  ( "cram",
    [
      test_case "parse"
        [ choose [ bytes; map [ bytes ] (fun s -> "  $ " ^ s) ] ]
        test_parse;
    ] )
