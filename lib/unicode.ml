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
