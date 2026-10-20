(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Alcobar

(* A 50 x 13 terminal of 7 x 13 pixel cells. *)
let settings = Vhs.[ Width 400; Height 200; Font_size 10; Padding 10 ]
let cols = 50
let rows = 13

(* The characters XML cannot hold show as U+FFFD. *)
let xml_text s =
  let b = Buffer.create (String.length s) in
  let rec go i =
    if i < String.length s then (
      let d = String.get_utf_8_uchar s i in
      (match Uchar.to_int (Uchar.utf_decode_uchar d) with
      | 0xfffe | 0xffff -> Buffer.add_string b "\xef\xbf\xbd"
      | _ -> Buffer.add_substring b s i (Uchar.utf_decode_length d));
      go (i + Uchar.utf_decode_length d))
  in
  go 0;
  Buffer.contents b

(* The terminal erases the character a backspace moves over, so the
   recorder sends it a cursor move instead outside escape sequences. This
   is the same rewrite, which the expected screens go through too. *)
type state = Ground | Escape | Csi | String | String_escape

let escape = function
  | '[' -> Csi
  | ']' | 'P' | 'X' | '^' | '_' -> String
  | ' ' .. '/' -> Escape
  | _ -> Ground

let backspaces state s =
  let b = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      if !state = Ground && c = '\b' then Buffer.add_string b "\027[D"
      else Buffer.add_char b c;
      state :=
        match (!state, c) with
        | Ground, '\027' -> Escape
        | Ground, _ -> Ground
        | Escape, c -> escape c
        | String_escape, '\\' -> Ground
        | String_escape, c -> escape c
        | Csi, '@' .. '~' -> Ground
        | Csi, _ -> Csi
        | String, '\007' -> Ground
        | String, '\027' -> String_escape
        | String, _ -> String)
    s;
  Buffer.contents b

let rec collapse = function
  | a :: (b :: _ as rest) when a = b -> collapse rest
  | a :: rest -> a :: collapse rest
  | [] -> []

(* Whatever a program prints, the image is well-formed XML and each of its
   screens has the text the terminal shows, in order. *)
let test_frames chunks =
  let output = List.mapi (fun i s -> (Vhs.Ms (i * 20), s)) chunks in
  match Vhs.svg ~settings output with
  | Error e -> failf "%a" Vhs.pp_error e
  | Ok svg -> (
      match Svg_check.frames ~cell_width:7 ~cell_height:13 ~rows svg with
      | Error e -> failf "not well-formed: %s" e
      | Ok frames ->
          let vte = Vte.create ~rows ~cols () in
          let state = ref Ground in
          let expected =
            List.map
              (fun s ->
                Vte.feed_string vte (backspaces state s);
                xml_text (Svg_check.trim_rows (Vte.to_string vte)))
              chunks
          in
          let expected =
            if chunks = [] then [ String.make (rows - 1) '\n' ] else expected
          in
          check_eq
            ~pp:Fmt.(Dump.list string)
            (collapse expected) (collapse frames))

let chunk =
  choose
    [
      bytes;
      const "\027[31mred\027[0m";
      const "\xe4\xb8\xad\xe6\x96\x87";
      const "\r\n";
      const "]]><&\"";
      const "\027[2J\027[H";
      const "\027[?1049h";
      const "e\xcc\x81";
      const "abc\b\bX";
      const "\027]0;t\bitle\007";
    ]

let suite = ("svg", [ test_case "frames" [ list chunk ] test_frames ])
