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
    Fmt.error "line %d: %S is not valid UTF-8" n line
  else if not (is_typeable line) then
    Fmt.error "line %d: %S holds a control character" n line
  else Ok line

(* [parse] scans [src] once, line by line, without splitting it. *)
let parse src =
  let n = String.length src in
  let marker i last c =
    last - i >= 4
    && src.[i] = ' '
    && src.[i + 1] = ' '
    && src.[i + 2] = c
    && src.[i + 3] = ' '
  in
  let flush cmds = function [] -> cmds | cmd -> List.rev cmd :: cmds in
  (* [cmds] are the finished commands and [cmd] the lines of the current one,
     both in reverse order. *)
  let rec line no i cmds cmd =
    if i > n then Ok (List.rev (flush cmds cmd))
    else
      let stop = Option.value (String.index_from_opt src i '\n') ~default:n in
      let last = if stop > i && src.[stop - 1] = '\r' then stop - 1 else stop in
      let start = marker i last '$' in
      if start || (cmd <> [] && marker i last '>') then
        match check_line no (String.sub src (i + 4) (last - i - 4)) with
        | Ok l when start -> line (no + 1) (stop + 1) (flush cmds cmd) [ l ]
        | Ok l -> line (no + 1) (stop + 1) cmds (l :: cmd)
        | Error _ as e -> e
      else line (no + 1) (stop + 1) (flush cmds cmd) []
  in
  line 1 0 [] []

let is_empty t = t = []

let sh_quote s =
  "'" ^ String.concat {|'\''|} (String.split_on_char '\'' s) ^ "'"

(* [mark ~setup t] is the sixteen zero-width characters that follow the
   [>] of the prompt, one per bit of a digest of [setup] and [t]: U+200B for
   a 0, U+2060 for a 1. The recording looks the same, but a line holds the
   mark only on this prompt, not when a command prints a zero-width space or
   the prompt of another recording. *)
let mark ~setup t =
  let d = Digest.string (String.concat "\n" (setup @ List.concat t)) in
  let bits = (Char.code d.[0] lsl 8) lor Char.code d.[1] in
  List.init 16 (fun i -> (bits lsr i) land 1 = 1)

let concat_map f l = String.concat "" (List.map f l)

(* The prompt VHS gives bash, with [mark] after the [>]. *)
let ps1 mark =
  {|PS1='\[\e[38;2;90;86;224m\]>\[|}
  ^ concat_map (fun b -> if b then {|\342\201\240|} else {|\342\200\213|}) mark
  ^ {|\] \[\e[0m\]'|}

let ready mark =
  Vhs.wait
    (Line
       (concat_map (fun b -> if b then {|\x{2060}|} else {|\x{200B}|}) mark
       ^ "$"))

(* The variable that hands [init] to bash, through VHS's [Env], which sets it
   before the shell starts; [init] unsets it. *)
let var = "VHS_CRAM_PROMPT"

(* [init mark] makes [ps1 mark] the last command of [PROMPT_COMMAND], after
   the hooks a setup installs, which may rewrite [PS1] before each prompt.
   The hooks of an array, which bash 5.1 and later run in order, are joined
   into one string, run by every bash. *)
let init mark =
  String.concat ""
    [
      {|__vhs=$(printf '%s\n' ${PROMPT_COMMAND+"${PROMPT_COMMAND[@]}"} "|};
      ps1 mark;
      {|"); unset PROMPT_COMMAND; PROMPT_COMMAND=$__vhs; unset __vhs |};
      var;
    ]

let prepare mark setup =
  let line =
    List.map (fun s -> "source " ^ sh_quote s) setup
    @ [ Fmt.str {|eval "$%s"|} var; "clear" ]
  in
  Vhs.hidden
    [
      Fmt.kstr Vhs.raw "Env %s `%s`" var (init mark);
      Vhs.type' (String.concat "; " line ^ "\n");
      ready mark;
    ]

let command mark lines =
  [
    Vhs.type' (String.concat "\n" lines ^ "\n"); ready mark; Vhs.sleep (Ms 1000);
  ]

let actions ~setup t =
  let mark = mark ~setup t in
  prepare mark setup :: List.concat_map (command mark) t
