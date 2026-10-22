(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The orbits of Go's [unicode.SimpleFold] (Unicode 17.0.0), from
   casefold.txt, which test/casefold_table.go writes. *)
let index =
  lazy
    (let orbits =
       String.split_on_char '\n' Casefold_data.table
       |> List.filter (( <> ) "")
       |> List.map (fun o ->
           Array.of_list
             (List.map
                (fun h -> int_of_string ("0x" ^ h))
                (String.split_on_char ' ' o)))
     in
     let entries =
       List.concat_map
         (fun o -> Array.to_list (Array.map (fun c -> (c, o)) o))
         orbits
     in
     Array.of_list (List.sort (fun (a, _) (b, _) -> compare a b) entries))

let first index c =
  let rec go lo hi =
    if lo >= hi then lo
    else
      let mid = (lo + hi) / 2 in
      if fst index.(mid) < c then go (mid + 1) hi else go lo mid
  in
  go 0 (Array.length index)

let fold ranges =
  let index = Lazy.force index in
  List.concat_map
    (fun (lo, hi) ->
      let rec go i acc =
        if i < Array.length index && fst index.(i) <= hi then
          go (i + 1)
            (Array.fold_left (fun acc c -> (c, c) :: acc) acc (snd index.(i)))
        else acc
      in
      (lo, hi) :: go (first index lo) [])
    ranges
