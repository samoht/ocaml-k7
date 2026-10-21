(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The theme list is internal: test it through [Vhs.Theme]. *)

let expect = Alcotest.(check string)

let tape settings =
  Vhs.tape ~settings ~outputs:[ "demo.gif" ] [ Vhs.sleep (Ms 100) ]
  |> Result.map Vhs.to_string
  |> Result.map_error Native_test.invalid

let body settings =
  match tape settings with
  | Ok s -> s
  | Error e -> Alcotest.failf "rejected: %s" e

let error name expected settings =
  Alcotest.(check (result string string)) name (Error expected) (tape settings)

let rejects name settings =
  match tape settings with
  | Ok s -> Alcotest.failf "%s: accepted %S" name s
  | Error _ -> ()

(* VHS finds a theme by its exact name, reads a value starting with [{] as
   JSON, which Go must read, and a blank one as its default theme. *)
let test_names () =
  let open Vhs in
  expect "name" "Output \"demo.gif\"\nSet Theme \"Dracula\"\nSleep 100ms\n"
    (body [ Theme "Dracula" ]);
  expect "blank" "Output \"demo.gif\"\nSet Theme \" \xc2\xa0\"\nSleep 100ms\n"
    (body [ Theme " \xc2\xa0" ]);
  error "unknown" {|Set Theme "Draculaa": not a VHS theme|} [ Theme "Draculaa" ];
  rejects "case" [ Theme "dracula" ];
  expect "JSON" "Output \"demo.gif\"\nSet Theme \"{}\"\nSleep 100ms\n"
    (body [ Theme "{}" ]);
  rejects "invalid JSON" [ Theme "{x" ];
  rejects "space before JSON" [ Theme " {}" ];
  rejects "invalid UTF-8" [ Theme " \xff" ];
  rejects "line break" [ Theme "Dracula\n" ]

(* Every theme [vhs themes] lists is a theme. *)
let test_vhs () =
  match Sys.getenv_opt "VHS_ORACLE" with
  | None -> ()
  | Some vhs ->
      let ic = Unix.open_process_args_in vhs [| vhs; "themes" |] in
      let names = In_channel.input_lines ic in
      (match Unix.close_process_in ic with
      | WEXITED 0 -> ()
      | _ -> Alcotest.fail "vhs themes failed");
      Alcotest.(check (list string))
        "rejected themes" []
        (List.filter (fun name -> Result.is_error (tape [ Theme name ])) names)

let suite =
  ( "themes",
    List.map
      (fun (name, fn) -> Alcotest.test_case name `Quick fn)
      [ ("names", test_names); ("vhs themes", test_vhs) ] )
