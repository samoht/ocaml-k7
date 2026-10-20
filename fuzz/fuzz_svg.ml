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
   recorder sends it a cursor move instead outside escape sequences, where it
   also replaces invalid UTF-8 with U+FFFD, as [String.get_utf_8_uchar] reads
   it, the characters of a sequence split across chunks included. This is
   the same rewrite, which the expected screens go through too. *)
type state = Ground | Escape | Csi | String | String_escape

let escape = function
  | '[' -> Csi
  | ']' | 'P' | 'X' | '^' | '_' -> String
  | ' ' .. '/' -> Escape
  | _ -> Ground

let is_lead c = c >= '\xc2' && c <= '\xf4'

let rewrite (state, pending) s =
  let b = Buffer.create (String.length s) in
  let rec byte c =
    if Buffer.length pending > 0 then
      let s = Buffer.contents pending ^ String.make 1 c in
      let d = String.get_utf_8_uchar s 0 in
      if Uchar.utf_decode_is_valid d then (
        Buffer.clear pending;
        Buffer.add_string b s)
      else if Uchar.utf_decode_length d = String.length s then
        Buffer.add_char pending c
      else (
        Buffer.clear pending;
        Buffer.add_string b "\xef\xbf\xbd";
        byte c)
    else if !state = Ground && c >= '\x80' then
      if is_lead c then Buffer.add_char pending c
      else Buffer.add_string b "\xef\xbf\xbd"
    else (
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
  in
  String.iter byte s;
  Buffer.contents b

(* The zero-width characters, which the terminal drops but the recorder
   keeps on the cell before them. *)
let is_zero_width s =
  let u = Uchar.to_int (Uchar.utf_decode_uchar (String.get_utf_8_uchar s 0)) in
  u >= 0xa0 && Glyph.String.measure ~width_method:`Unicode ~tab_width:2 s = 0

let strip_zero_width s =
  let b = Buffer.create (String.length s) in
  let rec go i =
    if i < String.length s then (
      let n = Uchar.utf_decode_length (String.get_utf_8_uchar s i) in
      let c = String.sub s i n in
      if not (is_zero_width c) then Buffer.add_string b c;
      go (i + n))
  in
  go 0;
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
          let state = (ref Ground, Buffer.create 4) in
          let expected =
            List.map
              (fun s ->
                Vte.feed_string vte (rewrite state s);
                strip_zero_width
                  (xml_text (Svg_check.trim_rows (Vte.to_string vte))))
              chunks
          in
          let expected =
            if chunks = [] then [ String.make (rows - 1) '\n' ] else expected
          in
          check_eq
            ~pp:Fmt.(Dump.list string)
            (collapse expected)
            (collapse (List.map strip_zero_width frames)))

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

let bases = [| "a"; " "; "\xc3\xa9"; "\xe4\xb8\xad"; "\xf0\x9f\x91\xa8" |]

let marks =
  [|
    "\xcc\x81"; "\xe2\x80\x8d"; "\xe2\x80\x8b"; "\xe2\x81\xa0"; "\xef\xb8\x8f";
  |]

(* Each character followed by a zero-width one shows both, whether they
   arrive together or apart: the first row holds them all, in order. *)
let test_zero_width items =
  let items = List.filteri (fun i _ -> i < 20) items in
  let pieces =
    List.map
      (fun (b, m, apart) ->
        let b = bases.(b mod Array.length bases)
        and m = marks.(m mod Array.length marks) in
        if apart then [ b; m ] else [ b ^ m ])
      items
  in
  let output =
    List.mapi (fun i s -> (Vhs.Ms (i * 20), s)) (List.concat pieces)
  in
  match Vhs.svg ~settings output with
  | Error e -> failf "%a" Vhs.pp_error e
  | Ok svg -> (
      match Svg_check.frames ~cell_width:7 ~cell_height:13 ~rows svg with
      | Error e -> failf "not well-formed: %s" e
      | Ok frames ->
          let last = List.nth frames (List.length frames - 1) in
          let first = List.hd (String.split_on_char '\n' last) in
          let expected =
            Svg_check.trim_rows (String.concat "" (List.concat pieces))
          in
          check_eq ~pp:pp_string expected first)

let suite =
  ( "svg",
    [
      test_case "frames" [ list chunk ] test_frames;
      test_case "zero width"
        [ list (map [ uint8; uint8; bool ] (fun b m a -> (b, m, a))) ]
        test_zero_width;
    ] )
