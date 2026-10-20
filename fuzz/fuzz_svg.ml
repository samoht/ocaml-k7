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

(* The text of each row of a screen as the image draws it: a hidden cell
   is blank, and trailing spaces do not show. *)
let drawn screen =
  Array.to_list (Vhs.Screen.rows screen)
  |> List.map (fun r ->
      Array.to_list (Vhs.Screen.cells r)
      |> List.map (fun (c : Vhs.Screen.cell) ->
          if c.width = 0 then ""
          else if Ansi.Attr.mem Hidden c.attrs then " "
          else c.text)
      |> String.concat "")
  |> String.concat "\n" |> Svg_check.trim_rows |> xml_text

let rec collapse = function
  | a :: (b :: _ as rest) when a = b -> collapse rest
  | a :: rest -> a :: collapse rest
  | [] -> []

(* Whatever a program prints, the image is well-formed XML and each of its
   screens has the text the screen shows, in order. *)
let test_frames chunks =
  let output = List.mapi (fun i s -> (Vhs.Ms (i * 20), s)) chunks in
  match Vhs.svg ~settings output with
  | Error e -> failf "%a" Vhs.pp_error e
  | Ok svg -> (
      match Svg_check.frames ~cell_width:7 ~cell_height:13 ~rows svg with
      | Error e -> failf "not well-formed: %s" e
      | Ok frames ->
          let term = Vhs.Screen.terminal ~rows ~cols in
          let expected =
            List.map
              (fun s ->
                Vhs.Screen.feed term s;
                drawn (Vhs.Screen.of_terminal term))
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
      const "\027[2;5r";
      const "\027M\027D\027E";
      const "\027(0lqk\027(B";
      const "\027[?7l";
      const "\027[?6h\027[H";
      const "\027[3b\027[2X\027[L\027[M";
      const "\027[S\027[T";
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
