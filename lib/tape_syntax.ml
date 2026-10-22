(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* This follows VHS's lexer/lexer.go, token/token.go and parser/parser.go
   (MIT, Charm) at v0.12.1, and Go's time.ParseDuration. *)

type command =
  | Blank
  | Hide
  | Show
  | Require
  | Output of string
  | Set of string * string
  | Screenshot of string
  | Other

(* Lexing *)

type kind =
  | Punct of char
  | Word  (** A keyword; its literal says which. *)
  | String
  | Regex
  | Json
  | Number
  | Comment
  | Illegal
  | Eof

type token = { kind : kind; lit : string }

let keywords =
  [ "em"; "px"; "ms"; "s"; "m"; "Set"; "Sleep"; "Type"; "Enter"; "Space";
    "Backspace"; "Delete"; "Insert"; "Ctrl"; "Alt"; "Shift"; "Down"; "Left";
    "Right"; "Up"; "PageUp"; "PageDown"; "ScrollUp"; "ScrollDown"; "Tab";
    "Escape"; "End"; "Hide"; "Require"; "Show"; "Output"; "Shell";
    "FontFamily"; "MarginFill"; "Margin"; "WindowBar"; "WindowBarSize";
    "BorderRadius"; "FontSize"; "Framerate"; "Height"; "LetterSpacing";
    "LineHeight"; "PlaybackSpeed"; "TypingSpeed"; "Padding"; "Theme"; "Width";
    "Rows"; "Columns"; "LoopOffset"; "WaitTimeout"; "WaitPattern"; "Wait";
    "Source"; "CursorBlink"; "true"; "false"; "Screenshot"; "Copy"; "Paste";
    "Env" ]
[@@ocamlformat "disable"]

let settings =
  [ "Shell"; "FontFamily"; "FontSize"; "LetterSpacing"; "LineHeight";
    "Framerate"; "TypingSpeed"; "Theme"; "PlaybackSpeed"; "Height"; "Width";
    "Rows"; "Columns"; "Padding"; "LoopOffset"; "MarginFill"; "Margin";
    "WindowBar"; "WindowBarSize"; "BorderRadius"; "CursorBlink"; "WaitTimeout";
    "WaitPattern" ]
[@@ocamlformat "disable"]

let keys =
  [ "Space"; "Backspace"; "Delete"; "Insert"; "Enter"; "Escape"; "Tab"; "Down";
    "Left"; "Right"; "Up"; "PageUp"; "PageDown"; "ScrollUp"; "ScrollDown" ]
[@@ocamlformat "disable"]

let is_ident = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '.' | '-' | '_' | '/' | '%' -> true
  | _ -> false

exception Error of string

let fail fmt = Fmt.kstr (fun s -> raise (Error s)) fmt

let rec span line p i =
  if i < String.length line && p line.[i] then span line p (i + 1) else i

(* [delimited line q i] is the index of the [q] closing what starts at [i]. *)
let delimited line q i =
  Option.value (String.index_from_opt line i q) ~default:(String.length line)

(* The index of the slash closing the regular expression that starts at
   [i]: a run of backslashes escapes the slash after it iff it is odd. *)
let rec regex line i =
  let n = String.length line in
  if i >= n then i
  else if line.[i] = '\\' then
    let j = span line (( = ) '\\') i in
    if j < n && line.[j] = '/' && (j - i) mod 2 = 0 then j
    else regex line (j + 1)
  else if line.[i] = '/' then i
  else regex line (i + 1)

let closed line i what =
  if i >= String.length line then fail "unterminated %s" what

let tok kind lit = { kind; lit }

(* The token at [i], not a comment, and the index after it. *)
let token line i =
  let n = String.length line in
  let sub i j = String.sub line i (j - i) in
  let enclosed kind what j =
    closed line j what;
    (tok kind (sub (i + 1) j), j + 1)
  in
  match line.[i] with
  | ('@' | '=' | ']' | '[' | '-' | '%' | '^' | '\\' | '+') as c ->
      (tok (Punct c) (String.make 1 c), i + 1)
  | '{' ->
      let j = delimited line '}' (i + 1) in
      closed line j "JSON value";
      (tok Json (sub i (j + 1)), j + 1)
  | ('"' | '\'' | '`') as q ->
      enclosed String "string" (delimited line q (i + 1))
  | '/' -> enclosed Regex "regular expression" (regex line (i + 1))
  | c
    when Char.Ascii.is_digit c
         || (c = '.' && i + 1 < n && Char.Ascii.is_digit line.[i + 1]) ->
      let j = span line (fun c -> Char.Ascii.is_digit c || c = '.') i in
      (tok Number (sub i j), j)
  | c when Char.Ascii.is_letter c || c = '.' ->
      let j = span line is_ident i in
      let lit = sub i j in
      (tok (if List.mem lit keywords then Word else String) lit, j)
  | c -> (tok Illegal (String.make 1 c), i + 1)

(* [lex line] are the tokens of [line], ending with [Eof]. A string, regular
   expression or JSON value that [line] does not close would take the next
   lines of a tape, so it is an error here. *)
let lex line =
  let n = String.length line in
  let rec go acc i =
    let i = span line (fun c -> c = ' ' || c = '\t') i in
    if i >= n then List.rev (tok Eof "" :: acc)
    else if line.[i] = '#' then
      List.rev
        (tok Eof "" :: tok Comment (String.sub line (i + 1) (n - i - 1)) :: acc)
    else
      let t, j = token line i in
      go (t :: acc) j
  in
  Array.of_list (go [] 0)

(* Durations: Go's time.ParseDuration, in unsigned 64-bit arithmetic. *)

let ( >! ) a b = Int64.unsigned_compare a b > 0
let top = Int64.min_int (* 1 lsl 63, unsigned *)

let units =
  [ ("ns", 1L); ("us", 1_000L); ("\xc2\xb5s", 1_000L); ("\xce\xbcs", 1_000L);
    ("ms", 1_000_000L); ("s", 1_000_000_000L); ("m", 60_000_000_000L);
    ("h", 3_600_000_000_000L) ]
[@@ocamlformat "disable"]

let digit s i = Int64.of_int (Char.code s.[i] - 48)

let unsigned_float u =
  if Int64.compare u 0L < 0 then Int64.to_float u +. 0x1p64
  else Int64.to_float u

(* [leadingInt]: the integer at [i] and the index after it. *)
let rec leading_int s v i =
  if i < String.length s && Char.Ascii.is_digit s.[i] then
    let v' = Int64.(add (mul v 10L) (digit s i)) in
    if v >! Int64.unsigned_div top 10L || v' >! top then None
    else leading_int s v' (i + 1)
  else Some (v, i)

(* [leadingFraction]: the digits at [i] as [f / scale], those past the
   precision of [f] dropped, and the index after them. *)
let rec fraction s f scale i =
  if i < String.length s && Char.Ascii.is_digit s.[i] then
    let y = Int64.(add (mul f 10L) (digit s i)) in
    if f >! Int64.unsigned_div Int64.max_int 10L || y >! top then
      (f, scale, span s Char.Ascii.is_digit i)
    else fraction s y (scale *. 10.) (i + 1)
  else (f, scale, i)

(* [v + f / scale] in [unit]s, as nanoseconds. *)
let scaled v f scale unit =
  if v >! Int64.unsigned_div top unit then None
  else
    let v = Int64.mul v unit in
    let v =
      if f = 0L then v
      else
        Int64.add v
          (Int64.of_float (unsigned_float f *. (Int64.to_float unit /. scale)))
    in
    if v >! top then None else Some v

(* One [number unit] term at [i]: its nanoseconds and the index after it. *)
let term s i =
  let is_num c = c = '.' || Char.Ascii.is_digit c in
  match leading_int s 0L i with
  | None -> None
  | Some (v, j) -> (
      let f, scale, k =
        if j < String.length s && s.[j] = '.' then fraction s 0L 1. (j + 1)
        else (0L, 1., j)
      in
      let u = span s (fun c -> not (is_num c)) k in
      match List.assoc_opt (String.sub s k (u - k)) units with
      | Some unit when j > i || k > j + 1 ->
          Option.map (fun d -> (d, u)) (scaled v f scale unit)
      | _ -> None)

let duration s =
  let rec terms d i =
    if i >= String.length s then if d >! Int64.max_int then None else Some d
    else if not (s.[i] = '.' || Char.Ascii.is_digit s.[i]) then None
    else
      match term s i with
      | None -> None
      | Some (v, u) ->
          let d = Int64.add d v in
          if d >! top then None else terms d u
  in
  if s = "0" then Some 0L else if s = "" then None else terms 0L 0

(* Numbers: Go's strconv.Atoi, on 64 bits, and strconv.ParseFloat (s, 64). *)

let without_sign s =
  if s <> "" && (s.[0] = '+' || s.[0] = '-') then
    String.sub s 1 (String.length s - 1)
  else s

let atoi s =
  let digits = without_sign s in
  if digits = "" || not (String.for_all Char.Ascii.is_digit digits) then None
  else Int64.of_string_opt (if s.[0] = '+' then digits else s)

(* Go's [special]: an infinity, with a sign or not, or a NaN, without. *)
let is_special s =
  let l = String.lowercase_ascii s in
  let body = without_sign l in
  body = "inf" || body = "infinity" || l = "nan"

(* Go's [underscoreOK]: each [_] follows a digit or the base prefix, and
   precedes a digit. *)
let underscore_ok s =
  let s = without_sign s in
  let n = String.length s in
  let base =
    if n >= 2 && s.[0] = '0' then Char.lowercase_ascii s.[1] else ' '
  in
  let prefix = String.contains "box" base in
  let rec go saw i =
    if i = n then saw <> '_'
    else
      match s.[i] with
      | '0' .. '9' -> go '0' (i + 1)
      | ('a' .. 'f' | 'A' .. 'F') when base = 'x' -> go '0' (i + 1)
      | '_' -> saw = '0' && go '_' (i + 1)
      | _ -> saw <> '_' && go '!' (i + 1)
  in
  go (if prefix then '0' else '^') (if prefix then 2 else 0)

(* Go's [readFloat] reads all of [s]: digits with at most one dot, then an
   exponent, which a hexadecimal mantissa must have. *)
let is_float_syntax s =
  let n = String.length s in
  let lower i = Char.lowercase_ascii s.[i] in
  let i = if n > 0 && (s.[0] = '+' || s.[0] = '-') then 1 else 0 in
  let hex = i + 2 < n && s.[i] = '0' && lower (i + 1) = 'x' in
  let is_mantissa c =
    Char.Ascii.is_digit c
    || hex
       && match Char.lowercase_ascii c with 'a' .. 'f' -> true | _ -> false
  in
  let rec mantissa i ~dot ~digits =
    if i < n && s.[i] = '_' then mantissa (i + 1) ~dot ~digits
    else if i < n && s.[i] = '.' && not dot then
      mantissa (i + 1) ~dot:true ~digits
    else if i < n && is_mantissa s.[i] then mantissa (i + 1) ~dot ~digits:true
    else if digits then Some i
    else None
  in
  let exponent i =
    if i < n && lower i = if hex then 'p' else 'e' then
      let i =
        if i + 1 < n && (s.[i + 1] = '+' || s.[i + 1] = '-') then i + 2
        else i + 1
      in
      if i < n && Char.Ascii.is_digit s.[i] then
        Some (span s (fun c -> Char.Ascii.is_digit c || c = '_') i)
      else None
    else if hex then None
    else Some i
  in
  match
    Option.bind
      (mantissa (if hex then i + 2 else i) ~dot:false ~digits:false)
      exponent
  with
  | Some i -> i = n && ((not (String.contains s '_')) || underscore_ok s)
  | None -> false

(* A well-formed value fails only if it rounds beyond the largest float. *)
let is_float s =
  is_special s
  || is_float_syntax s
     &&
     match
       float_of_string_opt (String.concat "" (String.split_on_char '_' s))
     with
     | Some f -> Float.is_finite f
     | None -> true

(* Parsing *)

type parser = { tokens : token array; mutable pos : int }

let cur p = p.tokens.(p.pos)
let peek p = p.tokens.(min (p.pos + 1) (Array.length p.tokens - 1))
let next p = p.pos <- min (p.pos + 1) (Array.length p.tokens - 1)
let is_word w t = t.kind = Word && t.lit = w

(* [take p] moves to the argument after the current token, which VHS takes
   whatever it is: at the end of the line, the next line's first token. *)
let take p =
  if (peek p).kind = Eof then fail "%s expects an argument" (cur p).lit;
  next p;
  (cur p).lit

let check_duration what s =
  if Option.is_none (duration s) then fail "%s %s: not a Go duration" what s

(* parseTime: a number and an optional unit, seconds by default. *)
let time p =
  if (peek p).kind <> Number then fail "Expected time after %s" (cur p).lit;
  next p;
  let t = (cur p).lit in
  let t =
    if List.exists (fun u -> is_word u (peek p)) [ "ms"; "s"; "m" ] then (
      next p;
      t ^ (cur p).lit)
    else t ^ "s"
  in
  check_duration "time" t;
  t

let speed p =
  if (peek p).kind = Punct '@' then (
    next p;
    Some (time p))
  else None

let strings p =
  if (peek p).kind <> String then fail "%s expects string" (cur p).lit;
  while (peek p).kind = String do
    next p
  done

(* Go's filepath.Ext. *)
let extension path =
  let rec go i =
    if i < 0 || path.[i] = '/' then ""
    else if path.[i] = '.' then String.sub path i (String.length path - i)
    else go (i - 1)
  in
  go (String.length path - 1)

let no_extension = "neither an extension nor a trailing slash"

let path p ~what =
  if (peek p).kind <> String then fail "Expected path after %s" what;
  next p;
  (cur p).lit

let set p =
  let name = (peek p).lit in
  if not ((peek p).kind = Word && List.mem name settings) then
    fail "Unknown setting: %s" name;
  next p;
  let arg = (peek p).lit in
  (match name with
  | "WaitTimeout" -> ignore (time p)
  | "WaitPattern" ->
      let re = take p in
      Result.iter_error
        (fun _ -> fail "Invalid regexp pattern: %s" re)
        (Regexp_syntax.check re)
  | "LoopOffset" ->
      let v = take p in
      if (peek p).kind = Punct '%' then next p;
      let n = String.length v in
      let rec last i = if i > 0 && v.[i - 1] = '%' then last (i - 1) else i in
      if not (is_float (String.sub v 0 (last n))) then
        fail "LoopOffset %s: not a Go float" v
  | "LetterSpacing" | "LineHeight" | "PlaybackSpeed" ->
      let v = take p in
      if not (is_float v) then fail "%s %s: not a Go float" name v
  | "Margin" | "WindowBarSize" | "BorderRadius" ->
      let v = take p in
      if Option.is_none (atoi v) then fail "%s %s: not a Go integer" name v
  | "Rows" | "Columns" -> (
      let v = take p in
      match atoi v with
      | None -> fail "%s %s: not a Go integer" name v
      | Some n when n <= 0L ->
          fail "%s must be a positive integer" (String.lowercase_ascii name)
      | Some _ -> ())
  | "TypingSpeed" ->
      let t = take p in
      let t =
        if is_word "ms" (peek p) || is_word "s" (peek p) then (
          next p;
          t ^ (cur p).lit)
        else t ^ "s"
      in
      check_duration "TypingSpeed" t
  | "WindowBar" ->
      let bar = take p in
      if
        not
          (List.mem bar
             [ ""; "Colorful"; "ColorfulRight"; "Rings"; "RingsRight" ])
      then fail "%s is not a valid bar style." bar
  | "MarginFill" ->
      let fill = take p in
      if
        String.starts_with ~prefix:"#" fill
        && not
             (String.length fill = 7
             && String.for_all Char.Ascii.is_hex_digit (String.sub fill 1 6))
      then fail "%a is not a valid color." Quote.pp fill
  | "CursorBlink" ->
      ignore (take p);
      if not (is_word "true" (cur p) || is_word "false" (cur p)) then
        fail "expected boolean value."
  | _ -> ignore (take p));
  Set (name, arg)

let wait p =
  if (peek p).kind = Punct '+' then (
    next p;
    let scope = peek p in
    if not (scope.kind = String && (scope.lit = "Line" || scope.lit = "Screen"))
    then fail "Wait+ expects Line or Screen";
    next p);
  (match speed p with
  | Some t when Option.fold ~none:true ~some:(fun d -> d <= 0L) (duration t) ->
      fail "Wait expects positive duration"
  | _ -> ());
  if (peek p).kind = Regex then (
    next p;
    let re = (cur p).lit in
    Result.iter_error
      (fun e -> fail "Invalid regular expression '%s': %s" re e)
      (Regexp_syntax.check re))

let modifier t = t.lit = "Alt" || t.lit = "Shift"

let ctrl_key t =
  match t.kind with
  | Word ->
      List.mem t.lit
        [ "Enter"; "Space"; "Backspace"; "Left"; "Right"; "Up"; "Down" ]
  | Punct c -> String.contains "-@[]^\\" c
  | String -> String.length t.lit = 1
  | _ -> false

(* parseCtrl: modifiers, then keys, each after a [+]. *)
let ctrl p =
  let rec go ~chain args =
    if (peek p).kind <> Punct '+' then args
    else (
      next p;
      let k = peek p in
      if modifier k then (
        if not chain then fail "Modifiers must come before other characters";
        next p;
        go ~chain (k.lit :: args))
      else (
        if not (ctrl_key k) then fail "Invalid control argument: %s" k.lit;
        next p;
        go ~chain:false (k.lit :: args)))
  in
  if go ~chain:true [] = [] then
    fail "Expected control character with args, got %s" (cur p).lit

let chord p name =
  let ok t =
    match t.kind with
    | String -> true
    | Word -> t.lit = "Enter" || t.lit = "Tab"
    | Punct ('[' | ']') -> true
    | _ -> false
  in
  if (peek p).kind = Punct '+' then next p;
  if (cur p).kind = Punct '+' && ok (peek p) then next p
  else fail "Expected %s character, got %s" name (cur p).lit

let command p =
  let t = cur p in
  match t.kind with
  | Word when List.mem t.lit keys ->
      ignore (speed p);
      if (peek p).kind = Number then next p;
      Other
  | Word -> (
      match t.lit with
      | "Set" -> set p
      | "Output" ->
          let out = path p ~what:"output" in
          if extension out = "" && not (String.ends_with ~suffix:"/" out) then
            fail "%s" no_extension;
          Output out
      | "Sleep" ->
          ignore (time p);
          Other
      | "Type" ->
          ignore (speed p);
          strings p;
          Other
      | "Copy" ->
          strings p;
          Other
      | "Ctrl" ->
          ctrl p;
          Other
      | "Alt" ->
          chord p "alt";
          Other
      | "Shift" ->
          chord p "shift";
          Other
      | "Hide" -> Hide
      | "Show" -> Show
      | "Require" ->
          if (peek p).kind <> String then fail "Require expects one string";
          next p;
          Require
      | "Wait" ->
          wait p;
          Other
      | "Source" ->
          if extension (path p ~what:"Source") <> ".tape" then
            fail "Expected file with .tape extension";
          Other
      | "Screenshot" -> (
          (* VHS takes a PNG image; Vhs.record also takes an SVG one. *)
          let shot = path p ~what:"Screenshot" in
          match extension shot with
          | ".png" | ".svg" -> Screenshot shot
          | _ -> fail "Expected file with .png extension")
      | "Paste" -> Other
      | "Env" ->
          ignore (take p);
          if (peek p).kind <> String then fail "%s expects string" (cur p).lit;
          next p;
          Other
      | lit -> fail "Invalid command: %s" lit)
  | _ -> fail "Invalid command: %s" t.lit

let check line =
  match
    let p = { tokens = lex line; pos = 0 } in
    let skip_comments () =
      while (cur p).kind = Comment do
        next p
      done
    in
    skip_comments ();
    if (cur p).kind = Eof then Blank
    else
      let c = command p in
      next p;
      skip_comments ();
      if (cur p).kind <> Eof then fail "more than one command";
      c
  with
  | c -> Ok c
  | exception Error msg -> Error msg

(* Tokens *)

type lexeme =
  | Keyword of string
  | Str of string
  | Pattern of string
  | Json_value of string
  | Num of string
  | Sym of char

let tokens line =
  Array.to_list (lex line)
  |> List.filter_map (fun t ->
      match t.kind with
      | Word -> Some (Keyword t.lit)
      | String -> Some (Str t.lit)
      | Regex -> Some (Pattern t.lit)
      | Json -> Some (Json_value t.lit)
      | Number -> Some (Num t.lit)
      | Punct c -> Some (Sym c)
      | Comment | Eof | Illegal -> None)

let has_control ?(except = "") s =
  let rec go i =
    i < String.length s
    &&
    let d = String.get_utf_8_uchar s i in
    match Uchar.to_int (Uchar.utf_decode_uchar d) with
    | c when c < 0x20 -> (not (String.contains except (Char.chr c))) || next d i
    | c when c >= 0x7f && c <= 0x9f -> true
    | _ -> next d i
  and next d i = go (i + Uchar.utf_decode_length d) in
  go 0
