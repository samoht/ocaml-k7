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
  | Set of string
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

let is_digit = function '0' .. '9' -> true | _ -> false
let is_letter = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false

let is_ident = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '.' | '-' | '_' | '/' | '%' -> true
  | _ -> false

exception Error of string

let fail fmt = Fmt.kstr (fun s -> raise (Error s)) fmt

(* [lex line] are the tokens of [line], ending with [Eof]. A string, regular
   expression or JSON value that [line] does not close would take the next
   lines of a tape, so it is an error here. *)
let lex line =
  let n = String.length line in
  let at i = if i < n then line.[i] else '\000' in
  let closed i what = if i >= n then fail "unterminated %s" what in
  (* [delimited q i] is the index of the [q] closing what starts at [i]. *)
  let rec delimited q i =
    if i >= n || line.[i] = q then i else delimited q (i + 1)
  in
  (* A run of backslashes escapes the slash after it iff it is odd. *)
  let rec regex i =
    if i >= n then i
    else if line.[i] = '\\' then (
      let j = ref i in
      while !j < n && line.[!j] = '\\' do
        incr j
      done;
      if !j < n && line.[!j] = '/' && (!j - i) mod 2 = 0 then !j
      else regex (!j + 1))
    else if line.[i] = '/' then i
    else regex (i + 1)
  in
  let rec span p i = if i < n && p line.[i] then span p (i + 1) else i in
  let rec go acc i =
    let i = span (fun c -> c = ' ' || c = '\t') i in
    let tok kind lit = { kind; lit } in
    if i >= n then List.rev (tok Eof "" :: acc)
    else
      match line.[i] with
      | ('@' | '=' | ']' | '[' | '-' | '%' | '^' | '\\' | '+') as c ->
          go (tok (Punct c) (String.make 1 c) :: acc) (i + 1)
      | '#' ->
          List.rev
            (tok Eof ""
            :: tok Comment (String.sub line (i + 1) (n - i - 1))
            :: acc)
      | '{' ->
          let j = delimited '}' (i + 1) in
          closed j "JSON value";
          go (tok Json (String.sub line i (j - i + 1)) :: acc) (j + 1)
      | ('"' | '\'' | '`') as q ->
          let j = delimited q (i + 1) in
          closed j "string";
          go (tok String (String.sub line (i + 1) (j - i - 1)) :: acc) (j + 1)
      | '/' ->
          let j = regex (i + 1) in
          closed j "regular expression";
          go (tok Regex (String.sub line (i + 1) (j - i - 1)) :: acc) (j + 1)
      | c when is_digit c || (c = '.' && is_digit (at (i + 1))) ->
          let j = span (fun c -> is_digit c || c = '.') i in
          go (tok Number (String.sub line i (j - i)) :: acc) j
      | c when is_letter c || c = '.' ->
          let j = span is_ident i in
          let lit = String.sub line i (j - i) in
          go (tok (if List.mem lit keywords then Word else String) lit :: acc) j
      | c -> go (tok Illegal (String.make 1 c) :: acc) (i + 1)
  in
  Array.of_list (go [] 0)

(* Durations *)

(* Go's time.ParseDuration, in unsigned 64-bit arithmetic. *)
let duration s =
  let ( >! ) a b = Int64.unsigned_compare a b > 0 in
  let top =
    Int64.min_int
    (* 1 lsl 63 *)
  in
  let n = String.length s in
  let units =
    [ ("ns", 1L); ("us", 1_000L); ("\xc2\xb5s", 1_000L); ("\xce\xbcs", 1_000L);
      ("ms", 1_000_000L); ("s", 1_000_000_000L); ("m", 60_000_000_000L);
      ("h", 3_600_000_000_000L) ]
  [@@ocamlformat "disable"]
  in
  let digit i = Int64.of_int (Char.code s.[i] - 48) in
  let to_float u =
    if Int64.compare u 0L < 0 then Int64.to_float u +. 0x1p64
    else Int64.to_float u
  in
  let rec leading_int v i =
    if i < n && is_digit s.[i] then
      if v >! Int64.unsigned_div top 10L then None
      else
        let v = Int64.(add (mul v 10L) (digit i)) in
        if v >! top then None else leading_int v (i + 1)
    else Some (v, i)
  in
  let rec fraction f scale overflow i =
    if i < n && is_digit s.[i] then
      if overflow || f >! Int64.unsigned_div Int64.max_int 10L then
        fraction f scale true (i + 1)
      else
        let y = Int64.(add (mul f 10L) (digit i)) in
        if y >! top then fraction f scale true (i + 1)
        else fraction y (scale *. 10.) overflow (i + 1)
    else (f, scale, i)
  in
  let rec terms d i =
    if i >= n then if d >! Int64.max_int then None else Some d
    else if not (s.[i] = '.' || is_digit s.[i]) then None
    else
      match leading_int 0L i with
      | None -> None
      | Some (v, j) -> (
          let f, scale, k =
            if j < n && s.[j] = '.' then fraction 0L 1. false (j + 1)
            else (0L, 1., j)
          in
          let has_fraction = k > j + 1 in
          let u = span_unit k in
          if j = i && not has_fraction then None
          else
            match List.assoc_opt (String.sub s k (u - k)) units with
            | None -> None
            | Some unit ->
                if v >! Int64.unsigned_div top unit then None
                else
                  let v = Int64.mul v unit in
                  let v =
                    if f = 0L then v
                    else
                      Int64.add v
                        (Int64.of_float
                           (to_float f *. (Int64.to_float unit /. scale)))
                  in
                  let d = Int64.add d v in
                  if v >! top || d >! top then None else terms d u)
  and span_unit i =
    if i < n && not (s.[i] = '.' || is_digit s.[i]) then span_unit (i + 1)
    else i
  in
  if s = "0" then Some 0L else if s = "" then None else terms 0L 0

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

let path p ~what =
  if (peek p).kind <> String then fail "Expected path after %s" what;
  next p;
  (cur p).lit

let is_hex = function
  | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
  | _ -> false

let set p =
  let name = (peek p).lit in
  if not ((peek p).kind = Word && List.mem name settings) then
    fail "Unknown setting: %s" name;
  next p;
  (match name with
  | "WaitTimeout" -> ignore (time p)
  | "WaitPattern" ->
      let re = take p in
      Result.iter_error
        (fun _ -> fail "Invalid regexp pattern: %s" re)
        (Regexp_syntax.check re)
  | "LoopOffset" ->
      ignore (take p);
      if (peek p).kind = Punct '%' then next p
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
             && String.for_all is_hex (String.sub fill 1 6))
      then fail "%S is not a valid color." fill
  | "CursorBlink" ->
      ignore (take p);
      if not (is_word "true" (cur p) || is_word "false" (cur p)) then
        fail "expected boolean value."
  | _ -> ignore (take p));
  Set name

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
            fail "Expected folder with trailing slash";
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
      | "Screenshot" ->
          if extension (path p ~what:"Screenshot") <> ".png" then
            fail "Expected file with .png extension";
          Other
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
