(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = string list list

(* [Vhs.tape] decides what a terminal can type, given something to type; a
   tab would reach bash's completion and a newline would end the line. *)
let is_typeable s =
  s = ""
  || (not (String.exists (fun c -> c = '\t' || c = '\n') s))
     && Result.is_ok (Vhs.tape ~outputs:[ "x.gif" ] [ Vhs.type' s ])

let check_line n line =
  if not (String.is_valid_utf_8 line) then
    Fmt.error "line %d: %a is not valid UTF-8" n Quote.pp line
  else if not (is_typeable line) then
    Fmt.error "line %d: %a holds a control character" n Quote.pp line
  else Ok line

(* [parse] scans [src] once, line by line, without splitting it. As in
   dune, a line [  > ] continues the command and a bare [  >] continues it
   with an empty line. *)
let parse src =
  let n = String.length src in
  (* [lead i last c] is [true] iff the line [src.[i..last-1]] starts with
     [  c], then a space or nothing. *)
  let lead i last c =
    last - i >= 3
    && src.[i] = ' '
    && src.[i + 1] = ' '
    && src.[i + 2] = c
    && (last - i = 3 || src.[i + 3] = ' ')
  in
  let flush cmds = function [] -> cmds | cmd -> List.rev cmd :: cmds in
  (* [cmds] are the finished commands and [cmd] the lines of the current one,
     both in reverse order. *)
  let rec line no i cmds cmd =
    if i > n then Ok (List.rev (flush cmds cmd))
    else
      let stop = Option.value (String.index_from_opt src i '\n') ~default:n in
      let last = if stop > i && src.[stop - 1] = '\r' then stop - 1 else stop in
      let start = last - i >= 4 && lead i last '$' in
      if start || (cmd <> [] && lead i last '>') then
        let text =
          if last - i = 3 then "" else String.sub src (i + 4) (last - i - 4)
        in
        match check_line no text with
        | Ok l when start -> line (no + 1) (stop + 1) (flush cmds cmd) [ l ]
        | Ok l -> line (no + 1) (stop + 1) cmds (l :: cmd)
        | Error _ as e -> e
      else line (no + 1) (stop + 1) (flush cmds cmd) []
  in
  line 1 0 [] []

let is_empty t = t = []

let command t n =
  if n < 1 then None
  else match List.nth_opt t (n - 1) with Some (l :: _) -> Some l | _ -> None

let sh_quote s =
  "'" ^ String.concat {|'\''|} (String.split_on_char '\'' s) ^ "'"

(* [mark ~setup t] is the sixteen zero-width characters that end the prompt,
   after its space, one per bit of a digest of [setup] and [t]: U+200B for
   a 0, U+2060 for a 1. The recording looks the same, but a line holds the
   mark only on this prompt, not when a command prints a zero-width space or
   the prompt of another recording. *)
let mark ~setup t =
  let d = Digest.string (String.concat "\n" (setup @ List.concat t)) in
  let bits = (Char.code d.[0] lsl 8) lor Char.code d.[1] in
  List.init 16 (fun i -> (bits lsr i) land 1 = 1)

let concat_map f l = String.concat "" (List.map f l)

(* A bit of the mark as UTF-8, as a prompt escape and as a Go regexp. *)
let utf_8 b = if b then "\xe2\x81\xa0" else "\xe2\x80\x8b"
let octal b = if b then {|\342\201\240|} else {|\342\200\213|}
let regexp b = if b then {|\x{2060}|} else {|\x{200B}|}

(* The character that counts the prompts after the mark in [PS2]: it is not
   the last of [mark], so that a [PS2] line never ends with [mark]. *)
let tally mark = not (List.nth mark (List.length mark - 1))
let ready mark = Vhs.wait (Line (concat_map regexp mark ^ "$"))

(* The variable that hands [init] to bash, through VHS's [Env], which sets it
   before the shell starts; [init] unsets it. *)
let var = "VHS_CRAM_PROMPT"

(* [init mark] makes [__vhs_prompt] the last command of [PROMPT_COMMAND],
   after the hooks a setup installs, which may rewrite [PS1] before each
   prompt; the hooks of an array, which bash 5.1 and later run in order, are
   joined into one string, run by every bash. [__vhs_prompt] sets [PS1] to
   the prompt VHS gives bash followed by [mark], and [PS2] to bash's [> ]
   followed by [mark] and one [tally mark] per prompt shown so far. The mark
   follows the space so that it stays on the line under the cursor when the
   [>] fills the last column. [init] also makes readline take, show and
   keep 8-bit input, which it drops without a UTF-8 locale. *)
let init mark =
  String.concat ""
    [
      {|bind 'set input-meta on'; bind 'set output-meta on'; bind 'set convert-meta off'; __vhs_n=; __vhs_prompt() { PS1='\[\e[38;2;90;86;224m\]> \[|};
      concat_map octal mark;
      {|\e[0m\]'; __vhs_n=$__vhs_n'|};
      octal (tally mark);
      {|'; PS2='> \[|};
      concat_map octal mark;
      {|'$__vhs_n'\]'; }; __vhs=$(printf '%s\n' ${PROMPT_COMMAND+"${PROMPT_COMMAND[@]}"} __vhs_prompt); unset PROMPT_COMMAND; PROMPT_COMMAND=$__vhs; unset __vhs |};
      var;
    ]

(* [string s] is [s] as a tape string, between a quote it does not hold. *)
let string s =
  let q =
    Option.value ~default:'"'
      (List.find_opt (fun q -> not (String.contains s q)) [ '"'; '\''; '`' ])
  in
  Fmt.str "%c%s%c" q s q

let prepare ~env mark setup =
  let line =
    List.map
      (fun s ->
        (* bash's [source] looks a name without a slash up in the PATH
           first. *)
        let s = if Filename.is_relative s then "./" ^ s else s in
        "source " ^ sh_quote s)
      setup
    @ [ Fmt.str {|eval "$%s"|} var; "clear" ]
  in
  Vhs.hidden
    (List.map
       (fun (k, v) -> Fmt.kstr Vhs.raw "Env %s %s" (string k) (string v))
       env
    @ [
        Fmt.kstr Vhs.raw "Env %s `%s`" var (init mark);
        Vhs.type' (String.concat "; " line ^ "\n");
        ready mark;
      ])

let typed mark lines =
  [
    Vhs.type' (String.concat "\n" lines ^ "\n"); ready mark; Vhs.sleep (Ms 1000);
  ]

let actions ?(env = []) ~setup t =
  let mark = mark ~setup t in
  prepare ~env mark setup :: List.concat_map (typed mark) t

(* [find sub s ~from] is the index of the first [sub] in [s] from [from]. *)
let find sub s ~from =
  let n = String.length sub in
  let rec go i =
    if i + n > String.length s then None
    else if String.sub s i n = sub then Some i
    else go (i + 1)
  in
  go from

let unfinished ~setup t output =
  let mark = mark ~setup t in
  let ps2 = " " ^ concat_map utf_8 mark in
  let tally = utf_8 (tally mark) in
  let n = String.length output in
  let rec count k i =
    if i + 3 <= n && String.sub output i 3 = tally then count (k + 1) (i + 3)
    else (k, i)
  in
  match
    Option.bind (find "last value was: " output ~from:0) (fun from ->
        find ps2 output ~from)
  with
  | None -> None
  | Some i -> (
      let k, j = count 0 (i + String.length ps2) in
      let ends = j = n || output.[j] = '\n' || output.[j] = '\r' in
      match if k >= 1 && ends then List.nth_opt t (k - 1) else None with
      | Some (line :: _) -> Some (k, line)
      | _ -> None)
