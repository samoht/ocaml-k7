(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let ( let* ) = Result.bind

type kind = Svg | Txt

let kind o =
  match String.lowercase_ascii (Filename.extension o) with
  | ".svg" -> Some Svg
  | ".txt" -> Some Txt
  | _ -> None

let check_outputs input = function
  | [] -> Fmt.error "%s: no output; give it -o FILE.svg" input
  | outputs ->
      List.fold_left
        (fun acc o ->
          let* acc = acc in
          match kind o with
          | Some k -> Ok ((o, k) :: acc)
          | None ->
              Fmt.error
                "%s: %a: not .svg or .txt, the outputs of an asciicast \
                 recording"
                input Quote.pp o)
        (Ok []) outputs
      |> Result.map List.rev

let draw stdenv ~dry_run ~settings ?show ?delay ~outputs input =
  let* outputs = check_outputs input outputs in
  let* src = Flags.load stdenv input in
  let error e = Fmt.str "%s: %a" input K7.pp_error e in
  let* cast = K7.cast_of_string src |> Result.map_error error in
  let render = function
    | Svg -> K7.svg_of_casts ~settings ?show ?delay [ cast ]
    | Txt -> K7.text_of_casts ~settings ?show [ cast ]
  in
  let* drawn =
    List.fold_left
      (fun acc (_, k) ->
        let* acc = acc in
        if List.mem_assoc k acc then Ok acc
        else
          let* r = render k |> Result.map_error error in
          Ok ((k, r) :: acc))
      (Ok []) outputs
  in
  if dry_run then Ok ()
  else
    List.fold_left
      (fun acc (output, k) ->
        let* () = acc in
        Flags.save stdenv output (List.assoc k drawn))
      (Ok ()) outputs

let run stdenv ~dry_run ~settings ?show ?delay ~outputs input =
  match draw stdenv ~dry_run ~settings ?show ?delay ~outputs input with
  | Ok () -> 0
  | Error msg -> Flags.fail stdenv msg
