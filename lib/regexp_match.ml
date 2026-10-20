(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Go's syntax, as in [regexp/syntax/parse.go] (BSD-3-Clause), translated to
   an ocaml-re expression on the UTF-8 bytes of the text. *)

type t = Re.re

exception Unsupported of string

(* Code point sets are sorted lists of disjoint inclusive ranges. *)

let max_cp = 0x10ffff

let normalise ranges =
  let rec merge = function
    | (a, b) :: (c, d) :: rest when c <= b + 1 -> merge ((a, max b d) :: rest)
    | r :: rest -> r :: merge rest
    | [] -> []
  in
  merge (List.sort compare ranges)

let negate ranges =
  let rec go lo = function
    | [] -> if lo <= max_cp then [ (lo, max_cp) ] else []
    | (a, b) :: rest ->
        if a > lo then (lo, a - 1) :: go (b + 1) rest else go (b + 1) rest
  in
  go 0 (normalise ranges)

(* The other case of each ASCII letter of [ranges]. *)
let fold_case ranges =
  let shift lo hi base delta =
    let lo = max lo base and hi = min hi (base + 25) in
    if lo <= hi then [ (lo + delta, hi + delta) ] else []
  in
  ranges
  @ List.concat_map
      (fun (lo, hi) ->
        shift lo hi (Char.code 'a') (-32) @ shift lo hi (Char.code 'A') 32)
      ranges

let utf_8 cp =
  let b = Buffer.create 4 in
  Buffer.add_utf_8_uchar b (Uchar.of_int cp);
  Buffer.contents b

let length cp =
  if cp < 0x80 then 1
  else if cp < 0x800 then 2
  else if cp < 0x10000 then 3
  else 4

(* The byte sequences of the code points [lo..hi], split so that each is a
   sequence of byte ranges, as Go's [utf8] range compiler does. *)
let rec utf_8_ranges lo hi =
  let split at = utf_8_ranges lo at @ utf_8_ranges (at + 1) hi in
  if lo > hi then []
  else if lo < 0xd800 && hi > 0xdfff then
    utf_8_ranges lo 0xd7ff @ utf_8_ranges 0xe000 hi
  else if lo >= 0xd800 && lo <= 0xdfff then utf_8_ranges 0xe000 hi
  else if hi >= 0xd800 && hi <= 0xdfff then utf_8_ranges lo 0xd7ff
  else if length lo <> length hi then
    split (match length lo with 1 -> 0x7f | 2 -> 0x7ff | _ -> 0xffff)
  else
    let n = length lo in
    let rec cut i =
      if i >= n then
        let a = utf_8 lo and b = utf_8 hi in
        [ Re.seq (List.init n (fun k -> Re.rg a.[k] b.[k])) ]
      else
        let m = (1 lsl (6 * i)) - 1 in
        if lo land lnot m = hi land lnot m then cut (i + 1)
        else if lo land m <> 0 then split (lo lor m)
        else if hi land m <> m then split ((hi land lnot m) - 1)
        else cut (i + 1)
    in
    cut 1

let of_ranges ranges =
  Re.alt
    (List.concat_map (fun (lo, hi) -> utf_8_ranges lo hi) (normalise ranges))

(* Parsing *)

type flags = { fold : bool; dot_nl : bool; multi_line : bool }

let range a b = (Char.code a, Char.code b)
let digit = [ range '0' '9' ]
let space = [ (9, 10); (12, 13); (32, 32) ]
let word = [ range '0' '9'; range 'A' 'Z'; range 'a' 'z'; range '_' '_' ]

let posix =
  [
    ("alnum", [ range '0' '9'; range 'A' 'Z'; range 'a' 'z' ]);
    ("alpha", [ range 'A' 'Z'; range 'a' 'z' ]);
    ("ascii", [ (0, 0x7f) ]);
    ("blank", [ (9, 9); (32, 32) ]);
    ("cntrl", [ (0, 0x1f); (0x7f, 0x7f) ]);
    ("digit", digit);
    ("graph", [ range '!' '~' ]);
    ("lower", [ range 'a' 'z' ]);
    ("print", [ range ' ' '~' ]);
    ("punct", [ range '!' '/'; range ':' '@'; range '[' '`'; range '{' '~' ]);
    ("space", [ (9, 13); (32, 32) ]);
    ("upper", [ range 'A' 'Z' ]);
    ("word", word);
    ("xdigit", [ range '0' '9'; range 'A' 'F'; range 'a' 'f' ]);
  ]

type parser = { cps : int array; mutable pos : int }

let peek p = if p.pos < Array.length p.cps then p.cps.(p.pos) else -1
let next p = p.pos <- p.pos + 1

let looking p s =
  let n = String.length s in
  p.pos + n <= Array.length p.cps
  && List.for_all
       (fun k -> p.cps.(p.pos + k) = Char.code s.[k])
       (List.init n Fun.id)

(* Code points outside Latin-1, and the end, are no ASCII syntax character. *)
let chr c = if c < 0 || c > 0xff then '\xff' else Char.chr c

let take p =
  let c = peek p in
  next p;
  c

let is_hex c =
  (c >= Char.code '0' && c <= Char.code '9')
  || (c >= Char.code 'a' && c <= Char.code 'f')
  || (c >= Char.code 'A' && c <= Char.code 'F')

let hex_value c =
  if c <= Char.code '9' then c - Char.code '0'
  else (c lor 0x20) - Char.code 'a' + 10

let rec hex_digits p acc n =
  if n > 0 && is_hex (peek p) then
    hex_digits p ((acc * 16) + hex_value (take p)) (n - 1)
  else acc

let is_octal c = c >= Char.code '0' && c <= Char.code '7'

(* A Perl class escape, after its backslash, as ranges. *)
let perl_class c =
  match chr c with
  | 'd' -> Some digit
  | 'D' -> Some (negate digit)
  | 's' -> Some space
  | 'S' -> Some (negate space)
  | 'w' -> Some word
  | 'W' -> Some (negate word)
  | _ -> None

(* The code point of a character escape, after its backslash. *)
let escaped_char p c =
  match chr c with
  | 'a' -> 7
  | 'f' -> 12
  | 't' -> 9
  | 'n' -> 10
  | 'r' -> 13
  | 'v' -> 11
  | 'x' when peek p = Char.code '{' ->
      next p;
      let v = hex_digits p 0 max_int in
      next p;
      v
  | 'x' -> hex_digits p 0 2
  | '0' .. '7' ->
      let rec octal acc n =
        if n > 0 && is_octal (peek p) then
          octal ((acc * 8) + take p - 48) (n - 1)
        else acc
      in
      octal (c - 48) 2
  | 'p' | 'P' -> raise (Unsupported "a Unicode class (\\p or \\P)")
  | _ -> c

let fold flags ranges = if flags.fold then fold_case ranges else ranges

(* The ranges of a class item, after [\[]: an escape, a POSIX class, a
   character or a range. *)
let class_item p =
  let close =
    let rec find i =
      if i + 1 >= Array.length p.cps then None
      else if p.cps.(i) = Char.code ':' && p.cps.(i + 1) = Char.code ']' then
        Some i
      else find (i + 1)
    in
    if looking p "[:" then find (p.pos + 2) else None
  in
  let name i j =
    String.init (j - i) (fun k -> Char.chr (p.cps.(i + k) land 0x7f))
  in
  let posix_class =
    Option.bind close (fun j ->
        let neg = p.cps.(p.pos + 2) = Char.code '^' in
        let first = p.pos + if neg then 3 else 2 in
        Option.map
          (fun r -> (j, if neg then negate r else r))
          (List.assoc_opt (name first j) posix))
  in
  match posix_class with
  | Some (j, r) ->
      p.pos <- j + 2;
      r
  | None -> (
      let single () =
        let c = take p in
        if c = Char.code '\\' then
          let e = take p in
          match perl_class e with
          | Some r -> Error r
          | None -> Ok (escaped_char p e)
        else Ok c
      in
      match single () with
      | Error r -> r
      | Ok lo ->
          let dash =
            peek p = Char.code '-'
            && p.pos + 1 < Array.length p.cps
            && p.cps.(p.pos + 1) <> Char.code ']'
          in
          if not dash then [ (lo, lo) ]
          else (
            next p;
            match single () with
            | Ok hi -> [ (lo, hi) ]
            | Error r -> (lo, lo) :: (45, 45) :: r))

let char_class flags p =
  let neg = peek p = Char.code '^' in
  if neg then next p;
  let rec items acc first =
    if peek p = Char.code ']' && not first then (
      next p;
      acc)
    else if peek p < 0 then acc
    else items (class_item p @ acc) false
  in
  let r = fold flags (items [] true) in
  of_ranges (if neg then negate r else r)

let literal flags c = of_ranges (fold flags [ (c, c) ])

let flag_group p flags =
  let rec go flags on =
    match chr (take p) with
    | 'i' -> go { flags with fold = on } on
    | 's' -> go { flags with dot_nl = on } on
    | 'm' -> go { flags with multi_line = on } on
    | 'U' -> go flags on
    | '-' -> go flags false
    | ':' -> (flags, true)
    | _ -> (flags, false)
  in
  go flags true

let escape p flags =
  let c = take p in
  match chr c with
  | 'A' -> Re.bos
  | 'z' -> Re.eos
  | 'b' -> Re.alt [ Re.bow; Re.eow ]
  | 'B' -> Re.not_boundary
  | 'Q' ->
      let rec quoted acc =
        if peek p < 0 then acc
        else if looking p "\\E" then (
          p.pos <- p.pos + 2;
          acc)
        else quoted (literal flags (take p) :: acc)
      in
      Re.seq (List.rev (quoted []))
  | 'C' -> of_ranges [ (0, max_cp) ]
  | _ -> (
      match perl_class c with
      | Some r -> of_ranges (fold flags r)
      | None -> literal flags (escaped_char p c))

(* [{n}], [{n,}] or [{n,m}] at the cursor, which is otherwise a literal. *)
let bounds p =
  let start = p.pos in
  let number () =
    let rec go acc seen =
      let c = peek p in
      if c >= Char.code '0' && c <= Char.code '9' then (
        next p;
        go ((acc * 10) + c - 48) true)
      else if seen then Some acc
      else None
    in
    go 0 false
  in
  next p;
  let result =
    match number () with
    | None -> None
    | Some n -> (
        match take p with
        | 125 -> Some (n, Some n)
        | 44 -> (
            if peek p = 125 then (
              next p;
              Some (n, None))
            else
              match number () with
              | Some m when take p = 125 -> Some (n, Some m)
              | _ -> None)
        | _ -> None)
  in
  if result = None then p.pos <- start;
  result

let rec repeat p r =
  let quantified r' =
    if peek p = Char.code '?' then next p;
    repeat p r'
  in
  match peek p with
  | 42 ->
      next p;
      quantified (Re.rep r)
  | 43 ->
      next p;
      quantified (Re.rep1 r)
  | 63 ->
      next p;
      quantified (Re.opt r)
  | 123 -> (
      match bounds p with
      | Some (n, m) -> quantified (Re.repn r n m)
      | None -> r)
  | _ -> r

let rec alternation p flags =
  let rec branches acc =
    let b = sequence p flags in
    if peek p = Char.code '|' then (
      next p;
      branches (b :: acc))
    else List.rev (b :: acc)
  in
  match branches [] with [ b ] -> b | bs -> Re.alt bs

and sequence p flags =
  let rec go flags acc =
    let c = peek p in
    if c < 0 || c = Char.code '|' || c = Char.code ')' then
      Re.seq (List.rev acc)
    else
      match atom p flags with
      | `Flags flags -> go flags acc
      | `Re r -> go flags (repeat p r :: acc)
  in
  go flags []

and atom p flags =
  let c = take p in
  match chr c with
  | '(' when looking p "?P<" || looking p "?<" ->
      while
        let c = take p in
        c >= 0 && c <> Char.code '>'
      do
        ()
      done;
      `Re (group p flags)
  | '(' when peek p = Char.code '?' ->
      next p;
      let flags', scoped = flag_group p flags in
      if scoped then `Re (group p flags') else `Flags flags'
  | '(' -> `Re (group p flags)
  | '[' -> `Re (char_class flags p)
  | '.' ->
      `Re
        (of_ranges
           (if flags.dot_nl then [ (0, max_cp) ] else negate [ (10, 10) ]))
  | '^' -> `Re (if flags.multi_line then Re.bol else Re.bos)
  | '$' -> `Re (if flags.multi_line then Re.eol else Re.eos)
  | '\\' -> `Re (escape p flags)
  | _ -> `Re (literal flags c)

and group p flags =
  let r = alternation p flags in
  next p;
  r

let code_points s =
  let rec go acc i =
    if i >= String.length s then Array.of_list (List.rev acc)
    else
      let d = String.get_utf_8_uchar s i in
      go
        (Uchar.to_int (Uchar.utf_decode_uchar d) :: acc)
        (i + Uchar.utf_decode_length d)
  in
  go [] 0

let compile re =
  let p = { cps = code_points re; pos = 0 } in
  match alternation p { fold = false; dot_nl = false; multi_line = false } with
  | r -> Ok (Re.compile r)
  | exception Unsupported what ->
      Fmt.kstr Result.error "%s is not supported natively" what

let matches t s = Re.execp t s
