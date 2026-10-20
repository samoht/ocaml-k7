(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Go's syntax, as in [regexp/syntax/parse.go] (BSD-3-Clause), parsed to a
   tree on code points and matched by a Pike VM, as Go's [regexp] matches:
   in linear time, with ASCII word boundaries and Unicode case folding. *)

exception Unsupported of string

type assertion = Bot | Eot | Bol | Eol | Boundary | Not_boundary

type node =
  | Set of (int * int) list
  | Seq of node list
  | Alt of node list
  | Repeat of node * int * int option
  | Assert of assertion

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

let of_ranges ranges = Set (normalise ranges)

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
let fold flags ranges = if flags.fold then Casefold.fold ranges else ranges

(* A negated class is the complement of the folded one, as in Go's
   [appendGroup]. *)
let negated flags ranges = negate (fold flags ranges)

(* A Perl class escape, after its backslash, as ranges. *)
let perl_class flags c =
  match chr c with
  | 'd' -> Some digit
  | 'D' -> Some (negated flags digit)
  | 's' -> Some space
  | 'S' -> Some (negated flags space)
  | 'w' -> Some word
  | 'W' -> Some (negated flags word)
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

(* The ranges of a class item, after [\[]: an escape, a POSIX class, a
   character or a range. *)
let class_item flags p =
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
          (fun r -> (j, if neg then negated flags r else r))
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
          match perl_class flags e with
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
    else items (class_item flags p @ acc) false
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
  | 'A' -> Assert Bot
  | 'z' -> Assert Eot
  | 'b' -> Assert Boundary
  | 'B' -> Assert Not_boundary
  | 'Q' ->
      let rec quoted acc =
        if peek p < 0 then acc
        else if looking p "\\E" then (
          p.pos <- p.pos + 2;
          acc)
        else quoted (literal flags (take p) :: acc)
      in
      Seq (List.rev (quoted []))
  | 'C' -> of_ranges [ (0, max_cp) ]
  | _ -> (
      match perl_class flags c with
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
      quantified (Repeat (r, 0, None))
  | 43 ->
      next p;
      quantified (Repeat (r, 1, None))
  | 63 ->
      next p;
      quantified (Repeat (r, 0, Some 1))
  | 123 -> (
      match bounds p with
      | Some (n, m) -> quantified (Repeat (r, n, m))
      | None -> r)
  | _ -> r

(* The flags a branch sets hold in the branches after it, up to the end of
   the group, as in Go. *)
let rec alternation p flags =
  let rec branches flags acc =
    let b, flags = sequence p flags in
    if peek p = Char.code '|' then (
      next p;
      branches flags (b :: acc))
    else List.rev (b :: acc)
  in
  match branches flags [] with [ b ] -> b | bs -> Alt bs

and sequence p flags =
  let rec go flags acc =
    let c = peek p in
    if c < 0 || c = Char.code '|' || c = Char.code ')' then
      (Seq (List.rev acc), flags)
    else
      match atom p flags with
      | `Flags flags -> (
          (* A flag group is no item: a repetition after it repeats the item
             before it, as in Go. *)
          match acc with
          | r :: rest -> go flags (repeat p r :: rest)
          | [] -> go flags [])
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
  | '^' -> `Re (Assert (if flags.multi_line then Bol else Bot))
  | '$' -> `Re (Assert (if flags.multi_line then Eol else Eot))
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

(* Programs *)

type inst =
  | Char of (int * int) array
  | Split of int * int
  | Jmp of int
  | Check of assertion
  | Match

type t = inst array

let emit code i =
  Dynarray.add_last code i;
  Dynarray.length code - 1

let set code pc i = Dynarray.set code pc i

let rec gen code = function
  | Set r -> ignore (emit code (Char (Array.of_list r)))
  | Seq ns -> List.iter (gen code) ns
  | Assert a -> ignore (emit code (Check a))
  | Alt [] -> ()
  | Alt [ n ] -> gen code n
  | Alt (n :: ns) ->
      let split = emit code (Jmp 0) in
      gen code n;
      let jmp = emit code (Jmp 0) in
      set code split (Split (split + 1, Dynarray.length code));
      gen code (Alt ns);
      set code jmp (Jmp (Dynarray.length code))
  | Repeat (n, lo, hi) -> (
      for _ = 1 to lo do
        gen code n
      done;
      match hi with
      | None ->
          let split = emit code (Jmp 0) in
          gen code n;
          ignore (emit code (Jmp split));
          set code split (Split (split + 1, Dynarray.length code))
      | Some hi ->
          let splits =
            List.init (hi - lo) (fun _ ->
                let split = emit code (Jmp 0) in
                gen code n;
                split)
          in
          List.iter
            (fun split ->
              set code split (Split (split + 1, Dynarray.length code)))
            splits)

(* Go's [isWordChar]: ASCII letters, digits and the underscore. *)
let is_word c =
  (c >= 0x30 && c <= 0x39)
  || (c >= 0x41 && c <= 0x5a)
  || (c >= 0x61 && c <= 0x7a)
  || c = 0x5f

let holds a cps i =
  let n = Array.length cps in
  let before = if i > 0 then cps.(i - 1) else -1
  and after = if i < n then cps.(i) else -1 in
  match a with
  | Bot -> i = 0
  | Eot -> i = n
  | Bol -> i = 0 || before = 10
  | Eol -> i = n || after = 10
  | Boundary -> is_word before <> is_word after
  | Not_boundary -> is_word before = is_word after

let mem ranges c =
  let rec go lo hi =
    lo < hi
    &&
    let mid = (lo + hi) / 2 in
    let a, b = ranges.(mid) in
    if c < a then go lo mid else if c > b then go (mid + 1) hi else true
  in
  go 0 (Array.length ranges)

exception Matched

(* [add prog cps i marks list pc] adds the thread at [pc] and those its
   jumps and true assertions lead to at position [i], each once. *)
let add prog cps i marks list pc =
  let stack = ref [ pc ] in
  while !stack <> [] do
    let pc = List.hd !stack in
    stack := List.tl !stack;
    if marks.(pc) <> i then (
      marks.(pc) <- i;
      match prog.(pc) with
      | Jmp x -> stack := x :: !stack
      | Split (x, y) -> stack := x :: y :: !stack
      | Check a -> if holds a cps i then stack := (pc + 1) :: !stack
      | Match -> raise_notrace Matched
      | Char _ -> Dynarray.add_last list pc)
  done

(* [marks.(pc)] is the last position whose list holds [pc]. *)
let matches prog s =
  let cps = code_points s in
  let n = Array.length cps in
  let marks = Array.make (Array.length prog) (-1) in
  let current = Dynarray.create () and following = Dynarray.create () in
  match
    for i = 0 to n do
      add prog cps i marks current 0;
      Dynarray.iter
        (fun pc ->
          match prog.(pc) with
          | Char r when i < n && mem r cps.(i) ->
              add prog cps (i + 1) marks following (pc + 1)
          | _ -> ())
        current;
      Dynarray.clear current;
      Dynarray.append current following;
      Dynarray.clear following
    done
  with
  | () -> false
  | exception Matched -> true

let compile re =
  let p = { cps = code_points re; pos = 0 } in
  match alternation p { fold = false; dot_nl = false; multi_line = false } with
  | node ->
      let code = Dynarray.create () in
      gen code node;
      ignore (emit code Match);
      Ok (Dynarray.to_array code)
  | exception Unsupported what ->
      Fmt.kstr Result.error "%s is not supported natively" what
