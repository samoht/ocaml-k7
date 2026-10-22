(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let pp ppf s =
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b '"';
  let rec go i =
    if i < String.length s then (
      let d = String.get_utf_8_uchar s i in
      let u = Uchar.utf_decode_uchar d in
      (match Uchar.to_int u with
      | 0x22 -> Buffer.add_string b {|\"|}
      | 0x5c -> Buffer.add_string b {|\\|}
      | 0x09 -> Buffer.add_string b {|\t|}
      | 0x0a -> Buffer.add_string b {|\n|}
      | 0x0d -> Buffer.add_string b {|\r|}
      | c when c < 0x20 || c = 0x7f ->
          Fmt.kstr (Buffer.add_string b) {|\x%02x|} c
      | c when c >= 0x80 && c <= 0x9f ->
          Fmt.kstr (Buffer.add_string b) {|\u{%X}|} c
      (* An invalid byte decodes as U+FFFD. *)
      | _ -> Buffer.add_utf_8_uchar b u);
      go (i + Uchar.utf_decode_length d))
  in
  go 0;
  Buffer.add_char b '"';
  Fmt.string ppf (Buffer.contents b)
