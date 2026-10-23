(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = { columns : int; rows : int; output : (int * string) list }

let ( let* ) = Result.bind

let pp ppf t =
  Fmt.pf ppf "@[<v>%dx%d%a@]" t.columns t.rows
    Fmt.(
      list ~sep:nop (cut ++ pair ~sep:(any " ") (int ++ any "ms") Dump.string))
    t.output

type value = Number of float | String of string

let value =
  Jsont.any ~kind:"number or string"
    ~dec_number:(Jsont.map ~dec:(fun x -> Number x) Jsont.number)
    ~dec_string:(Jsont.map ~dec:(fun s -> String s) Jsont.string)
    ()

let version =
  Jsont.Object.map ~kind:"asciicast header" Fun.id
  |> Jsont.Object.mem "version" value ~enc:Fun.id
  |> Jsont.Object.finish

let header =
  Jsont.Object.map ~kind:"asciicast header" (fun w h -> (w, h))
  |> Jsont.Object.mem "width" value ~enc:fst
  |> Jsont.Object.mem "height" value ~enc:snd
  |> Jsont.Object.finish

let event = Jsont.t3 ~kind:"asciicast event" ~dec:(fun t c d -> (t, c, d)) value

let decode codec line =
  match Jsont_bytesrw.decode_string' ~locs:true codec line with
  | Ok v -> Ok v
  | Error (_, meta, kind) ->
      let at = Jsont.Textloc.first_byte (Jsont.Meta.textloc meta) in
      Fmt.error "%s at byte %d"
        (Palette.plain (Jsont.Error.kind_to_string kind))
        at

let max_ms = 9_223_372_036_854

let pp_number ppf x =
  if Float.is_integer x && Float.abs x < 1e15 then Fmt.pf ppf "%.0f" x
  else Fmt.pf ppf "%.17g" x

let size name = function
  | Number x when Float.is_integer x && x >= 1. && x <= float max_int ->
      Ok (Float.to_int x)
  | Number x -> Fmt.error "%s %a is not a positive integer" name pp_number x
  | String s -> Fmt.error "%s %S is not a number" name s

let read_header line =
  let* v = decode version line in
  let* () =
    match v with
    | Number 2. -> Ok ()
    | Number x -> Fmt.error "asciicast version %a, not 2" pp_number x
    | String s -> Fmt.error "asciicast version %S, not 2" s
  in
  let* w, h = decode header line in
  let* columns = size "width" w in
  let* rows = size "height" h in
  Ok (columns, rows)

let ms seconds =
  let us = Float.round (seconds *. 1e6) in
  if us < 0x1p62 then (Float.to_int us + 500) / 1000
  else Float.to_int (Float.round (seconds *. 1e3))

let read_event ~prev line =
  let* time, code, data = decode event line in
  match (time, code, data) with
  | Number t, String code, String data ->
      if t < 0. then Fmt.error "time %as is negative" pp_number t
      else if t < prev then
        Fmt.error "time %as is before %as" pp_number t pp_number prev
      else if t *. 1e3 > float max_ms || ms t > max_ms then
        Fmt.error "time %as is beyond %dms" pp_number t max_ms
      else Ok (t, if code = "o" then Some (ms t, data) else None)
  | String _, _, _ -> Error "the time is not a number"
  | _, Number _, _ -> Error "the code is not a string"
  | _, _, Number _ -> Error "the data is not a string"

let is_blank s =
  String.for_all (function ' ' | '\t' | '\r' -> true | _ -> false) s

let of_string s =
  let at n r = Result.map_error (Fmt.str "line %d: %s" n) r in
  let rec events n ~prev acc = function
    | [] -> Ok (List.rev acc)
    | line :: rest when is_blank line -> events (n + 1) ~prev acc rest
    | line :: rest -> (
        let* t, out = at n (read_event ~prev line) in
        match out with
        | Some o -> events (n + 1) ~prev:t (o :: acc) rest
        | None -> events (n + 1) ~prev:t acc rest)
  in
  let first, rest =
    match String.index_opt s '\n' with
    | Some i ->
        (String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1))
    | None -> (s, "")
  in
  if is_blank first then Error "line 1: no asciicast header"
  else
    let* columns, rows = at 1 (read_header first) in
    let* output = events 2 ~prev:0. [] (String.split_on_char '\n' rest) in
    Ok { columns; rows; output }
