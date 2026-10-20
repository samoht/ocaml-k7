(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The checker follows [parse] in Go's [regexp/syntax/parse.go] (BSD-3-Clause)
   with the [Perl] flags, case by case, but builds no tree: each group frame
   keeps only what decides validity. *)

exception Bad of string

let fail fmt = Fmt.kstr (fun msg -> raise_notrace (Bad msg)) fmt

(* The [\p] class names Go 1.27 accepts, in the canonical form below: the
   special cases, then the canonical names of [unicode.Categories],
   [unicode.CategoryAliases] and [unicode.Scripts]. *)
module Names = Set.Make (String)

let unicode_names =
  Names.of_list
    [
      "Adlam"; "Ahom"; "Anatolianhieroglyphs"; "Any"; "Arabic"; "Armenian";
      "Ascii"; "Assigned"; "Avestan"; "Balinese"; "Bamum"; "Bassavah"; "Batak";
      "Bengali"; "Beriaerfe"; "Bhaiksuki"; "Bopomofo"; "Brahmi"; "Braille";
      "Buginese"; "Buhid"; "C"; "Canadianaboriginal"; "Carian"; "Casedletter";
      "Caucasianalbanian"; "Cc"; "Cf"; "Chakma"; "Cham"; "Cherokee";
      "Chorasmian"; "Closepunctuation"; "Cn"; "Cntrl"; "Co"; "Combiningmark";
      "Common"; "Connectorpunctuation"; "Control"; "Coptic"; "Cs"; "Cuneiform";
      "Currencysymbol"; "Cypriot"; "Cyprominoan"; "Cyrillic"; "Dashpunctuation";
      "Decimalnumber"; "Deseret"; "Devanagari"; "Digit"; "Divesakuru"; "Dogra";
      "Duployan"; "Egyptianhieroglyphs"; "Elbasan"; "Elymaic"; "Enclosingmark";
      "Ethiopic"; "Finalpunctuation"; "Format"; "Garay"; "Georgian";
      "Glagolitic"; "Gothic"; "Grantha"; "Greek"; "Gujarati"; "Gunjalagondi";
      "Gurmukhi"; "Gurungkhema"; "Han"; "Hangul"; "Hanifirohingya"; "Hanunoo";
      "Hatran"; "Hebrew"; "Hiragana"; "Imperialaramaic"; "Inherited";
      "Initialpunctuation"; "Inscriptionalpahlavi"; "Inscriptionalparthian";
      "Javanese"; "Kaithi"; "Kannada"; "Katakana"; "Kawi"; "Kayahli";
      "Kharoshthi"; "Khitansmallscript"; "Khmer"; "Khojki"; "Khudawadi";
      "Kiratrai"; "L"; "Lao"; "Latin"; "Lc"; "Lepcha"; "Letter"; "Letternumber";
      "Limbu"; "Lineara"; "Linearb"; "Lineseparator"; "Lisu"; "Ll"; "Lm"; "Lo";
      "Lowercaseletter"; "Lt"; "Lu"; "Lycian"; "Lydian"; "M"; "Mahajani";
      "Makasar"; "Malayalam"; "Mandaic"; "Manichaean"; "Marchen"; "Mark";
      "Masaramgondi"; "Mathsymbol"; "Mc"; "Me"; "Medefaidrin"; "Meeteimayek";
      "Mendekikakui"; "Meroiticcursive"; "Meroitichieroglyphs"; "Miao"; "Mn";
      "Modi"; "Modifierletter"; "Modifiersymbol"; "Mongolian"; "Mro"; "Multani";
      "Myanmar"; "N"; "Nabataean"; "Nagmundari"; "Nandinagari"; "Nd"; "Newa";
      "Newtailue"; "Nko"; "Nl"; "No"; "Nonspacingmark"; "Number"; "Nushu";
      "Nyiakengpuachuehmong"; "Ogham"; "Olchiki"; "Oldhungarian"; "Olditalic";
      "Oldnortharabian"; "Oldpermic"; "Oldpersian"; "Oldsogdian";
      "Oldsoutharabian"; "Oldturkic"; "Olduyghur"; "Olonal"; "Openpunctuation";
      "Oriya"; "Osage"; "Osmanya"; "Other"; "Otherletter"; "Othernumber";
      "Otherpunctuation"; "Othersymbol"; "P"; "Pahawhhmong"; "Palmyrene";
      "Paragraphseparator"; "Paucinhau"; "Pc"; "Pd"; "Pe"; "Pf"; "Phagspa";
      "Phoenician"; "Pi"; "Po"; "Privateuse"; "Ps"; "Psalterpahlavi"; "Punct";
      "Punctuation"; "Rejang"; "Runic"; "S"; "Samaritan"; "Saurashtra"; "Sc";
      "Separator"; "Sharada"; "Shavian"; "Siddham"; "Sidetic"; "Signwriting";
      "Sinhala"; "Sk"; "Sm"; "So"; "Sogdian"; "Sorasompeng"; "Soyombo";
      "Spaceseparator"; "Spacingmark"; "Sundanese"; "Sunuwar"; "Surrogate";
      "Sylotinagri"; "Symbol"; "Syriac"; "Tagalog"; "Tagbanwa"; "Taile";
      "Taitham"; "Taiviet"; "Taiyo"; "Takri"; "Tamil"; "Tangsa"; "Tangut";
      "Telugu"; "Thaana"; "Thai"; "Tibetan"; "Tifinagh"; "Tirhuta";
      "Titlecaseletter"; "Todhri"; "Tolongsiki"; "Toto"; "Tulutigalari";
      "Ugaritic"; "Unassigned"; "Uppercaseletter"; "Vai"; "Vithkuqi"; "Wancho";
      "Warangciti"; "Yezidi"; "Yi"; "Z"; "Zanabazarsquare"; "Zl"; "Zp"; "Zs";
    ]
[@@ocamlformat "disable"]

let posix_names =
  Names.of_list
    (List.concat_map
       (fun n -> [ "[:" ^ n ^ ":]"; "[:^" ^ n ^ ":]" ])
       [
         "alnum"; "alpha"; "ascii"; "blank"; "cntrl"; "digit"; "graph";
         "lower"; "print"; "punct"; "space"; "upper"; "word"; "xdigit";
       ])
[@@ocamlformat "disable"]

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

let is_digit = function '0' .. '9' -> true | _ -> false
let is_letter = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
let is_octal c = c >= '0' && c <= '7'
let is_alnum c = is_digit c || is_letter c
let is_perl_class c = String.contains "dDsSwW" c

let unhex = function
  | '0' .. '9' as c -> Char.code c - 48
  | 'a' .. 'f' as c -> Char.code c - 87
  | 'A' .. 'F' as c -> Char.code c - 55
  | _ -> -1

(* A group being parsed: [operand] when its last item can take a repetition,
   [top] the repetition product of that item, [widest] the largest product
   among its earlier items. *)
type frame = { operand : bool; top : int; widest : int }

let empty = { operand = false; top = 1; widest = 1 }
let push w f = { operand = true; top = w; widest = max f.widest f.top }
let bar f = { f with operand = false; widest = max f.widest f.top }
let weight f = max f.widest f.top

let index_of s ~from sub =
  let n = String.length s and m = String.length sub in
  let rec matches i k = k = m || (s.[i + k] = sub.[k] && matches i (k + 1)) in
  let rec at i =
    if i + m > n then None else if matches i 0 then Some i else at (i + 1)
  in
  at from

let slice re i j = String.sub re i (j - i)
let byte_at re i = if i < String.length re then Some re.[i] else None
let is_byte p re i = Option.fold ~none:false ~some:p (byte_at re i)

(* The code point at [i] and the index after it. *)
let rune re i =
  let d = String.get_utf_8_uchar re i in
  (Uchar.to_int (Uchar.utf_decode_uchar d), i + Uchar.utf_decode_length d)

(* [\x{h...}] from the [h] at [j]. *)
let rec hex_braces re ~invalid j nhex r =
  if j >= String.length re then invalid j
  else
    let c, j' = rune re j in
    if c = Char.code '}' then if nhex = 0 then invalid j' else (r, j')
    else
      let v = if c < 0x80 then unhex (Char.chr c) else -1 in
      let r = (r * 16) + v in
      if v < 0 || r > 0x10FFFF then invalid j'
      else hex_braces re ~invalid j' (nhex + 1) r

let rec octal re r j k =
  match byte_at re j with
  | Some d when k < 3 && is_octal d ->
      octal re ((r * 8) + unhex d) (j + 1) (k + 1)
  | _ -> (r, j)

(* [parseEscape]: the code point of the escape at [i] and the index after
   it. *)
let escape re i =
  let n = String.length re in
  let invalid j = fail "invalid escape sequence: `%s`" (slice re i j) in
  if i + 1 >= n then fail "trailing backslash at end of expression";
  let c, j = rune re (i + 1) in
  if c >= 0x80 then invalid j
  else
    match Char.chr c with
    | c when not (is_alnum c) -> (Char.code c, j)
    | '1' .. '7' when not (is_byte is_octal re j) -> invalid j
    | '0' .. '7' as d -> octal re (unhex d) j 1
    | 'x' when j >= n -> invalid j
    | 'x' when re.[j] = '{' -> hex_braces re ~invalid (j + 1) 0 0
    | 'x' ->
        let x = unhex re.[j] in
        let y, k = if j + 1 < n then (unhex re.[j + 1], j + 2) else (-1, n) in
        if x < 0 || y < 0 then invalid k else ((x * 16) + y, k)
    | 'a' -> (7, j)
    | 'f' -> (12, j)
    | 'n' -> (10, j)
    | 'r' -> (13, j)
    | 't' -> (9, j)
    | 'v' -> (11, j)
    | _ -> invalid j

(* [parseUnicodeClass] for the [\p] or [\P] at [i]. *)
let unicode_class re i =
  let n = String.length re and k = i + 2 in
  let name, j =
    if k >= n then ("", n)
    else if re.[k] <> '{' then
      let _, j = rune re k in
      (slice re k j, j)
    else
      match String.index_from_opt re i '}' with
      | None -> fail "invalid character class range: `%s`" (slice re i n)
      | Some e -> (slice re (i + 3) e, e + 1)
  in
  let name =
    if name <> "" && name.[0] = '^' then
      String.sub name 1 (String.length name - 1)
    else name
  in
  if not (Names.mem (canonical name) unicode_names) then
    fail "invalid character class range: `%s`" (slice re i j);
  j

(* The last search for [":]"]: the pattern, where it started and what it
   found. A search from [from] reuses it while the answer cannot differ, so
   the searches of one pattern scan each byte once, however many classes
   open with [[:]. *)
let last_close = Domain.DLS.new_key (fun () -> ("", 0, None))

let close_of re from =
  match Domain.DLS.get last_close with
  | re', start, found
    when re' == re && from >= start
         && Option.fold ~none:true ~some:(fun e -> from <= e) found ->
      found
  | _ ->
      let found = index_of re ~from ":]" in
      Domain.DLS.set last_close (re, from, found);
      found

(* [parseNamedClass] for a [[:name:]] at [k], if any. *)
let named_class re k =
  let named = String.length re - k > 2 && re.[k] = '[' && re.[k + 1] = ':' in
  match if named then close_of re (k + 2) else None with
  | None -> None
  | Some e ->
      let name = slice re k (e + 2) in
      if not (Names.mem name posix_names) then
        fail "invalid character class range: `%s`" name;
      Some (e + 2)

(* A character of a class at [k], or a range from it: the index after. *)
let class_range re ~missing k =
  let n = String.length re in
  let class_char k =
    if k >= n then missing ()
    else if re.[k] = '\\' then escape re k
    else rune re k
  in
  let lo, j = class_char k in
  if j + 1 < n && re.[j] = '-' && re.[j + 1] <> ']' then (
    let hi, j' = class_char (j + 1) in
    if hi < lo then fail "invalid character class range: `%s`" (slice re k j');
    j')
  else j

(* [parseClass] for the [\[] at [i]: the index after its closing bracket. *)
let char_class re i =
  let n = String.length re in
  let missing () = fail "missing closing ]: `%s`" (slice re i n) in
  let rec items k first =
    if k < n && re.[k] = ']' && not first then k + 1
    else if k >= n then missing ()
    else
      let next =
        match (named_class re k, re.[k], byte_at re (k + 1)) with
        | Some k, _, _ -> k
        | None, '\\', Some ('p' | 'P') -> unicode_class re k
        | None, '\\', Some c when is_perl_class c -> k + 2
        | None, _, _ -> class_range re ~missing k
      in
      items next false
  in
  let k = i + 1 in
  items (if byte_at re k = Some '^' then k + 1 else k) true

(* Go's [parseInt]: [Some (v, j)] for the digits at [k], without leading
   zero, [v] being [-1] past [10^8]. *)
let int_at re k =
  let rec scan k v =
    match byte_at re k with
    | Some c when is_digit c ->
        scan (k + 1)
          (if v < 0 || v >= 100_000_000 then -1 else (v * 10) + unhex c)
    | _ -> Some (v, k)
  in
  match byte_at re k with
  | Some '0' when is_byte is_digit re (k + 1) -> None
  | Some c when is_digit c -> scan k 0
  | _ -> None

(* [parseRepeat] for the [{] at [i]: [Some (min, max, j)] when it reads as a
   counted repetition, [min] being [-1] for too large a number. *)
let counted re i =
  let close min max k =
    if byte_at re k = Some '}' then Some (min, max, k + 1) else None
  in
  let upper min k =
    match (byte_at re k, int_at re k) with
    | Some '}', _ -> Some (min, -1, k + 1)
    | _, Some (max, k) -> close (if max < 0 then -1 else min) max k
    | _, None -> None
  in
  match int_at re (i + 1) with
  | None -> None
  | Some (min, k) when byte_at re k = Some ',' -> upper min (k + 1)
  | Some (min, k) -> close min min k

let is_capture_name name =
  name <> "" && String.for_all (fun c -> c = '_' || is_alnum c) name

(* [parsePerlFlags] for the [(?] at [i]. *)
let perl_flags re i =
  let n = String.length re in
  let named_p = n - i > 4 && re.[i + 2] = 'P' && re.[i + 3] = '<' in
  let named = n - i > 3 && re.[i + 2] = '<' in
  let invalid k =
    fail "invalid or unsupported Perl syntax: `%s`" (slice re i k)
  in
  let rec flags k negated saw =
    if k >= n then invalid k
    else
      let c, k' = rune re k in
      match if c < 0x80 then Char.chr c else '\000' with
      | 'i' | 'm' | 's' | 'U' -> flags k' negated true
      | '-' when not negated -> flags k' true false
      | (':' | ')') when negated && not saw -> invalid k'
      | ':' -> `Group k'
      | ')' -> `Flags k'
      | _ -> invalid k'
  in
  if named_p || named then (
    match String.index_from_opt re i '>' with
    | None -> fail "invalid named capture: `%s`" (slice re i n)
    | Some e ->
        if not (is_capture_name (slice re (if named_p then i + 4 else i + 3) e))
        then fail "invalid named capture: `%s`" (slice re i (e + 1));
        `Group (e + 1))
  else flags (i + 2) false false

(* The repetition product of the item a counted repetition [{min,max}]
   applies to, whose product is [top]. *)
let repeated ~min ~max top =
  let m = if max = -1 then min else max in
  let top = if m = 0 then 1 else m * top in
  if (min >= 2 || max >= 2) && top > 1000 then None else Some top

(* The index after [\Q...\E] at [i] and whether it quotes anything. *)
let quote re i =
  match index_of re ~from:(i + 2) "\\E" with
  | None -> (String.length re, String.length re > i + 2)
  | Some e -> (e + 2, e > i + 2)

(* The escape at [i] in the main parser: [`Item j] when it is an item ending
   before [j], [`Nothing j] for an empty [\\Q\\E]. *)
let backslash_item re i =
  match byte_at re (i + 1) with
  | Some ('A' | 'b' | 'B' | 'z') -> `Item (i + 2)
  | Some 'C' -> fail "invalid escape sequence: `\\C`"
  | Some 'Q' ->
      let next, quotes = quote re i in
      if quotes then `Item next else `Nothing next
  | Some ('p' | 'P') -> `Item (unicode_class re i)
  | Some c when is_perl_class c -> `Item (i + 2)
  | _ -> `Item (snd (escape re i))

let rec parse re i f outer last_repeat =
  let n = String.length re in
  let atom j = push_atom re j f outer in
  let repeat j ~count =
    if last_repeat then
      fail "invalid nested repetition operator: `%s`" (slice re i j);
    if not f.operand then
      fail "missing argument to repetition operator: `%s`" (slice re i j);
    let top =
      match count with
      | None -> f.top
      | Some (min, max) -> (
          match repeated ~min ~max f.top with
          | Some top -> top
          | None -> fail "invalid repeat count: `%s`" (slice re i j))
    in
    let j = if byte_at re j = Some '?' then j + 1 else j in
    parse re j { f with top } outer true
  in
  if i >= n then (if outer <> [] then fail "missing closing ): `%s`" re)
  else
    match re.[i] with
    | '(' when byte_at re (i + 1) = Some '?' -> (
        match perl_flags re i with
        | `Group j -> parse re j empty (f :: outer) false
        | `Flags j -> parse re j f outer false)
    | '(' -> parse re (i + 1) empty (f :: outer) false
    | '|' -> parse re (i + 1) (bar f) outer false
    | ')' -> (
        match outer with
        | [] -> fail "unexpected ): `%s`" re
        | p :: outer -> parse re (i + 1) (push (weight f) p) outer false)
    | '^' | '$' | '.' -> atom (i + 1)
    | '[' -> atom (char_class re i)
    | '*' | '+' | '?' -> repeat (i + 1) ~count:None
    | '{' -> (
        match counted re i with
        | None -> atom (i + 1)
        | Some (min, max, j) ->
            if min < 0 || min > 1000 || max > 1000 || (max >= 0 && min > max)
            then fail "invalid repeat count: `%s`" (slice re i j);
            repeat j ~count:(Some (min, max)))
    | '\\' -> backslash re i f outer
    | _ -> atom (snd (rune re i))

(* An item without repetition, then the rest from [j]. *)
and push_atom re j f outer = parse re j (push 1 f) outer false

and backslash re i f outer =
  match backslash_item re i with
  | `Item j -> push_atom re j f outer
  | `Nothing j -> parse re j f outer false

let check re =
  if not (String.is_valid_utf_8 re) then Error "invalid UTF-8"
  else
    match parse re 0 empty [] false with
    | () -> Ok ()
    | exception Bad msg -> Error msg

let write b re =
  let n = String.length re in
  let rec plain i =
    if i < n then
      match re.[i] with
      | '/' ->
          Buffer.add_string b {|\x2f|};
          plain (i + 1)
      | '\\' when i + 1 < n && re.[i + 1] = '/' ->
          Buffer.add_string b {|\x2f|};
          plain (i + 2)
      | '\\' when i + 1 < n && re.[i + 1] = 'Q' ->
          Buffer.add_string b {|\Q|};
          quoted (i + 2)
      | '\\' when i + 1 < n ->
          Buffer.add_char b '\\';
          Buffer.add_char b re.[i + 1];
          plain (i + 2)
      | c ->
          Buffer.add_char b c;
          plain (i + 1)
  and quoted i =
    if i >= n then Buffer.add_string b {|\E|}
    else if re.[i] = '\\' && i + 1 < n && re.[i + 1] = 'E' then (
      Buffer.add_string b {|\E|};
      plain (i + 2))
    else if re.[i] = '/' then (
      Buffer.add_string b {|\E\x2f\Q|};
      quoted (i + 1))
    else (
      Buffer.add_char b re.[i];
      quoted (i + 1))
  in
  Buffer.add_char b '/';
  plain 0;
  Buffer.add_char b '/'
