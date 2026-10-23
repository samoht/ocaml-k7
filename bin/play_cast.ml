(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let ( let* ) = Result.bind

let check_outputs input = function
  | [] -> Fmt.error "%s: no output; give it -o FILE.svg" input
  | outputs -> (
      match List.find_opt (fun o -> Filename.extension o <> ".svg") outputs with
      | Some o ->
          Fmt.error
            "%s: %a: not .svg, the only output of an asciicast recording" input
            Quote.pp o
      | None -> Ok ())

let draw stdenv ~dry_run ~settings ?show ?delay ~outputs input =
  let* () = check_outputs input outputs in
  let* src = Flags.load stdenv input in
  let error e = Fmt.str "%s: %a" input K7.pp_error e in
  let* svg =
    K7.svg_of_cast ~settings ?show ?delay src |> Result.map_error error
  in
  if dry_run then Ok ()
  else
    List.fold_left
      (fun acc output ->
        let* () = acc in
        Flags.save stdenv output svg)
      (Ok ()) outputs

let run stdenv ~dry_run ~settings ?show ?delay ~outputs input =
  match draw stdenv ~dry_run ~settings ?show ?delay ~outputs input with
  | Ok () -> 0
  | Error msg -> Flags.fail stdenv msg
