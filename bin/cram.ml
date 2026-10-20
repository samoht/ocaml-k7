(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = string list list

(* [Vhs.tape] decides what a terminal can type; a tab would reach bash's
   completion and a newline would end the line. *)
let is_typeable s =
  (not (String.exists (fun c -> c = '\t' || c = '\n') s))
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

(* The prompt VHS gives bash with a zero-width space after the [>]: the
   recording looks the same, but [ready] holds only on this prompt, not on
   output that ends with [>] while its command still runs. *)
let ps1 = {|PS1='\[\e[38;2;90;86;224m\]>\[\342\200\213\] \[\e[0m\]'|}
let ready = Vhs.wait (Line {|\x{200B}$|})

let prepare setup =
  let line =
    List.map (fun s -> "source " ^ sh_quote s) setup @ [ ps1; "clear" ]
  in
  Vhs.hidden [ Vhs.type' (String.concat "; " line ^ "\n"); ready ]

let command lines =
  [ Vhs.type' (String.concat "\n" lines ^ "\n"); ready; Vhs.sleep (Ms 1000) ]

let actions ~setup t = prepare setup :: List.concat_map command t
