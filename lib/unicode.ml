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

let is_selector u =
  (u >= 0xfe00 && u <= 0xfe0f) || (u >= 0xe0100 && u <= 0xe01ef)

(* A character a variation selector may follow: one that shows, other than
   a selector or a joiner. *)
let is_base u =
  u > 0x20
  && (not (u >= 0x7f && u <= 0x9f))
  && (not (is_selector u))
  && u <> 0x200c && u <> 0x200d

(* UTS #51 ED-14a: an emoji tag sequence is a base, tag characters U+E0020
   to U+E007E, then U+E007F CANCEL TAG. Its only base in
   emoji-sequences.txt is U+1F3F4, which picks a subdivision flag.
   [tags s j n] is where the sequence whose [n] tags so far end at [j]
   ends, if it does. *)
let rec tags s j n =
  if j >= String.length s then None
  else
    let d = String.get_utf_8_uchar s j in
    let next = j + Uchar.utf_decode_length d in
    match Uchar.to_int (Uchar.utf_decode_uchar d) with
    | _ when not (Uchar.utf_decode_is_valid d) -> None
    | u when u >= 0xe0020 && u <= 0xe007e -> tags s next (n + 1)
    | 0xe007f when n > 0 -> Some next
    | _ -> None

let strip_format s =
  let b = Buffer.create (String.length s) in
  let rec go i ~base =
    if i < String.length s then
      let d = String.get_utf_8_uchar s i in
      let next = i + Uchar.utf_decode_length d in
      let u = Uchar.to_int (Uchar.utf_decode_uchar d) in
      let keep stop ~base =
        Buffer.add_substring b s i (stop - i);
        go stop ~base
      in
      if not (Uchar.utf_decode_is_valid d) then keep next ~base:true
      else
        match if u = 0x1f3f4 then tags s next 0 else None with
        | Some stop -> keep stop ~base:true
        | None when is_format u || (is_selector u && not base) -> go next ~base
        | None -> keep next ~base:(is_base u)
  in
  go 0 ~base:false;
  Buffer.contents b
