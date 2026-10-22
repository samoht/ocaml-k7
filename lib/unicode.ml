(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The tables of Unicode 17.0.0, which Go 1.27 and the VHS it builds use, in
   unicode.txt, which test/unicode_table.go writes: a line per table, its
   name and its ranges in hexadecimal, then a line per name that resolves
   to another table, [name=table], or [name=^table] for its complement. *)
let tables =
  lazy
    (let tables = Hashtbl.create 256 and names = Hashtbl.create 256 in
     let range r =
       match String.index_opt r '-' with
       | None ->
           let c = int_of_string ("0x" ^ r) in
           (c, c)
       | Some i ->
           ( int_of_string ("0x" ^ String.sub r 0 i),
             int_of_string
               ("0x" ^ String.sub r (i + 1) (String.length r - i - 1)) )
     in
     String.split_on_char '\n' Unicode_data.table
     |> List.iter (fun line ->
         match String.split_on_char ' ' line with
         | [ "" ] -> ()
         | [ alias ] -> (
             match String.split_on_char '=' alias with
             | [ name; table ] when String.starts_with ~prefix:"^" table ->
                 Hashtbl.replace names name
                   (String.sub table 1 (String.length table - 1), true)
             | [ name; table ] -> Hashtbl.replace names name (table, false)
             | _ -> Hashtbl.replace tables alias [])
         | name :: ranges -> Hashtbl.replace tables name (List.map range ranges)
         | [] -> ());
     (tables, names))

(* Go's [canonicalName]: drop underscores, hyphens and spaces, then capitalise
   the first letter and lower the others. *)
let canonical name =
  let b = Buffer.create (String.length name) in
  let add first = function
    | '_' | '-' | ' ' -> first
    | c ->
        Buffer.add_char b
          (if first then Char.uppercase_ascii c else Char.lowercase_ascii c);
        false
  in
  ignore (String.fold_left add true name);
  Buffer.contents b

let find name =
  let tables, names = Lazy.force tables in
  let name = canonical name in
  let table, negated =
    Option.value (Hashtbl.find_opt names name) ~default:(name, false)
  in
  Option.map (fun r -> (r, negated)) (Hashtbl.find_opt tables table)

let format = lazy (match find "Cf" with Some (r, false) -> r | _ -> [])

(* The prepended concatenation marks, format characters that show, over the
   digits after them. *)
let is_concatenation_mark u =
  (u >= 0x600 && u <= 0x605)
  || u = 0x6dd || u = 0x70f || u = 0x890 || u = 0x891 || u = 0x8e2
  || u = 0x110bd || u = 0x110cd

let is_format u =
  u <> 0x200c && u <> 0x200d
  && (not (is_concatenation_mark u))
  && List.exists (fun (lo, hi) -> lo <= u && u <= hi) (Lazy.force format)

(* The length of the emoji tag sequence at [i] of [s], which picks a
   subdivision flag: U+1F3F4, then tag characters, then U+E007F CANCEL TAG;
   [0] if there is none. *)
let tag_sequence s i =
  let decode j =
    let d = String.get_utf_8_uchar s j in
    (Uchar.to_int (Uchar.utf_decode_uchar d), Uchar.utf_decode_length d)
  in
  let rec tags j n =
    if j >= String.length s then 0
    else
      match decode j with
      | 0xe007f, l when n > 0 -> j + l - i
      | u, l when u >= 0xe0020 && u <= 0xe007e -> tags (j + l) (n + 1)
      | _ -> 0
  in
  match decode i with 0x1f3f4, l -> tags (i + l) 0 | _ -> 0

let strip_format s =
  let b = Buffer.create (String.length s) in
  let rec go i =
    if i < String.length s then
      match tag_sequence s i with
      | 0 ->
          let d = String.get_utf_8_uchar s i in
          let n = Uchar.utf_decode_length d in
          if not (is_format (Uchar.to_int (Uchar.utf_decode_uchar d))) then
            Buffer.add_substring b s i n;
          go (i + n)
      | n ->
          Buffer.add_substring b s i n;
          go (i + n)
  in
  go 0;
  Buffer.contents b
