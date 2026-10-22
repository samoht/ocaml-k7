(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let settings = K7.[ Width 800; Height 400; Font_size 22 ]
let cell_width = 14
let cell_height = 29
let rows = 9

let replace ~sub ~by s =
  let n = String.length sub in
  let b = Buffer.create (String.length s) in
  let rec go i =
    if i + n > String.length s then
      Buffer.add_substring b s i (String.length s - i)
    else if String.sub s i n = sub then (
      Buffer.add_string b by;
      go (i + n))
    else (
      Buffer.add_char b s.[i];
      go (i + 1))
  in
  go 0;
  Buffer.contents b

let prefix svg =
  let key = "<svg xmlns=\"http://www.w3.org/2000/svg\" id=\"" in
  let rec find i =
    if i + String.length key > String.length svg then None
    else if String.sub svg i (String.length key) = key then
      let j = i + String.length key in
      Some (String.sub svg j (String.index_from svg j '"' - j))
    else find (i + 1)
  in
  find 0

let unprefixed svg =
  match prefix svg with
  | None -> svg
  | Some p -> replace ~sub:p ~by:"" (replace ~sub:("#" ^ p ^ " ") ~by:"" svg)

let svg ?(settings = settings) output =
  match K7.svg ~settings output with
  | Ok s -> unprefixed s
  | Error e -> Alcotest.failf "%a" K7.pp_error e

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
  let tmp = Filename.temp_dir "k7-native" "" in
  Fun.protect ~finally:(fun () ->
      ignore (Sys.command ("rm -rf " ^ Filename.quote tmp)))
  @@ fun () -> f env Eio.Path.(Eio.Stdenv.fs env / tmp)

let play env cwd t =
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 60. @@ fun () ->
  K7.record ~engine:Native
    ~clock:(Eio.Stdenv.mono_clock env)
    ~proc:(Eio.Stdenv.process_mgr env)
    ~cwd t

let record ?(image = "demo.svg") env cwd t =
  (try Eio.Path.unlink Eio.Path.(cwd / image) with Eio.Io _ -> ());
  Result.map (fun () -> Eio.Path.load Eio.Path.(cwd / image)) (play env cwd t)

let tape actions =
  match
    K7.tape
      ~settings:(K7.Typing_speed (Ms 5) :: settings)
      ~outputs:[ "demo.svg" ] actions
  with
  | Ok t -> t
  | Error e -> Alcotest.failf "%a" K7.pp_error e

let last l = List.nth l (List.length l - 1)

let invalid = function
  | K7.Invalid msg -> msg
  | e -> Alcotest.failf "not Invalid: %a" K7.pp_error e

(* Set by the oracle alias only. *)
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
