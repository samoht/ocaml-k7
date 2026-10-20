(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A 46 x 9 terminal of 14 x 29 pixel cells. *)
let settings = Vhs.[ Width 800; Height 400; Font_size 22 ]
let cell_width = 14
let cell_height = 29
let rows = 9

let svg ?(settings = settings) output =
  match Vhs.svg ~settings output with
  | Ok s -> s
  | Error e -> Alcotest.failf "%a" Vhs.pp_error e

let frames svg =
  match Svg_check.frames ~cell_width ~cell_height ~rows svg with
  | Ok f -> f
  | Error e -> Alcotest.failf "not well-formed XML: %s\n%s" e svg

let contains ~sub s =
  let n = String.length sub in
  let rec go i =
    i + n <= String.length s && (String.sub s i n = sub || go (i + 1))
  in
  go 0

let count ~sub s =
  let n = String.length sub in
  let rec go i acc =
    if i + n > String.length s then acc
    else if String.sub s i n = sub then go (i + n) (acc + 1)
    else go (i + 1) acc
  in
  go 0 0

let check_contains what ~sub s =
  if not (contains ~sub s) then Alcotest.failf "%s: %S not in\n%s" what sub s

let screen lines =
  String.concat "\n" (lines @ List.init (rows - List.length lines) (fun _ -> ""))

let with_dir f =
  Eio_main.run @@ fun env ->
  let tmp = Filename.temp_dir "vhs-native" "" in
  Fun.protect ~finally:(fun () ->
      ignore (Sys.command ("rm -rf " ^ Filename.quote tmp)))
  @@ fun () -> f env Eio.Path.(Eio.Stdenv.fs env / tmp)

let record env cwd t =
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 60. @@ fun () ->
  Vhs.record
    ~clock:(Eio.Stdenv.mono_clock env)
    ~proc:(Eio.Stdenv.process_mgr env)
    ~cwd t

let tape actions =
  match
    Vhs.tape
      ~settings:(Vhs.Typing_speed (Ms 5) :: settings)
      ~outputs:[ "demo.svg" ] actions
  with
  | Ok t -> t
  | Error e -> Alcotest.fail e

let last l = List.nth l (List.length l - 1)

(* The vhs executable of the tests that run it, from [VHS_ORACLE]: the
   oracle alias sets it, runtest does not. *)
let oracle = Sys.getenv_opt "VHS_ORACLE"
let separator = String.concat "" (List.init 80 (fun _ -> "\xe2\x94\x80"))

let screens txt =
  String.split_on_char '\n' txt
  |> List.fold_left
       (fun (cur, acc) l ->
         if l = separator then ([], String.concat "\n" (List.rev cur) :: acc)
         else (l :: cur, acc))
       ([], [])
  |> snd |> List.rev

let last_screen txt =
  match List.rev (screens txt) with
  | s :: _ -> s
  | [] -> Alcotest.fail "no screen"
