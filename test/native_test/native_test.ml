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
  let key = "<title id=\"" in
  let rec find i =
    if i + String.length key > String.length svg then None
    else if String.sub svg i (String.length key) = key then
      let j = i + String.length key in
      Some (String.sub svg j (String.index_from svg j '-' + 1 - j))
    else find (i + 1)
  in
  find 0

let unprefixed svg =
  match prefix svg with None -> svg | Some p -> replace ~sub:p ~by:"" svg

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

let executable path contents =
  Out_channel.with_open_gen [ Open_wronly; Open_creat; Open_trunc; Open_binary ]
    0o755 path (fun oc -> Out_channel.output_string oc contents)

(* Under load, bash 3.2 on macOS can print a failed setpgid of a job. *)
let no_job_control = [ ("PROMPT_COMMAND", "set +m") ]

type clock = Eio.Time.Mono.ty Eio.Resource.t

module Held = struct
  type t = {
    clock : Eio_mock.Clock.Mono.t;
    hold : float;
    released : unit Eio.Promise.t;
  }

  type time = Mtime.t

  let now t = Eio.Time.Mono.now t.clock

  let sleep_until t time =
    let ns = Int64.sub (Mtime.to_uint64_ns time) (Mtime.to_uint64_ns (now t)) in
    if Int64.to_float ns /. 1e9 > t.hold then Eio.Promise.await t.released;
    Eio.Time.Mono.sleep_until t.clock time
end

let silent = { Eio.Debug.traceln = (fun ?__POS__:_ -> Fmt.kstr ignore) }

(* The clock moves until [f] returns, so that a recording that is cancelled
   still stops its program. It moves silently, as a command's standard error
   may be what a test checks. *)
let with_clock ?(hold = 1.) ?(release = fun () -> false) f =
  let clock = Eio_mock.Clock.Mono.make () in
  let released, set = Eio.Promise.create () in
  let finished = ref false in
  Eio.Switch.run @@ fun sw ->
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Cancel.protect @@ fun () ->
      Eio.Fiber.with_binding Eio.Private.Debug.v#traceln silent @@ fun () ->
      while not !finished do
        if (not (Eio.Promise.is_resolved released)) && release () then
          Eio.Promise.resolve set ();
        if not (Eio_mock.Clock.Mono.try_advance clock) then Eio.Fiber.yield ()
      done);
  Fun.protect ~finally:(fun () -> finished := true) @@ fun () ->
  f
    (Eio.Resource.T
       ({ Held.clock; hold; released }, Eio.Time.Pi.clock (module Held)))

let rec until p =
  if not (p ()) then (
    Eio.Fiber.yield ();
    until p)

let play env cwd t =
  with_clock @@ fun clock ->
  K7.record ~engine:Native ~env:no_job_control ~clock
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
