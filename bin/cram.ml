(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = string list list

let check_line n line =
  if not (String.is_valid_utf_8 line) then
    Fmt.error "line %d: %S is not valid UTF-8" n line
  else if not (String.for_all (fun c -> c >= ' ' && c <> '\x7f') line) then
    Fmt.error "line %d: %S holds a control character" n line
  else Ok line

let drop n s = String.sub s n (String.length s - n)

let chomp l =
  if String.ends_with ~suffix:"\r" l then String.sub l 0 (String.length l - 1)
  else l

(* [cmds] are the finished commands and [cmd] the lines of the current one,
   both in reverse order. *)
let rec parse_lines n cmds cmd = function
  | [] -> Ok (List.rev (if cmd = [] then cmds else List.rev cmd :: cmds))
  | line :: rest ->
      let line = chomp line in
      let flush () = if cmd = [] then cmds else List.rev cmd :: cmds in
      let next = parse_lines (n + 1) in
      if String.starts_with ~prefix:"  $ " line then
        match check_line n (drop 4 line) with
        | Ok l -> next (flush ()) [ l ] rest
        | Error _ as e -> e
      else if cmd <> [] && String.starts_with ~prefix:"  > " line then
        match check_line n (drop 4 line) with
        | Ok l -> next cmds (l :: cmd) rest
        | Error _ as e -> e
      else next (flush ()) [] rest

let parse src = parse_lines 1 [] [] (String.split_on_char '\n' src)
let is_empty t = t = []

let sh_quote s =
  "'" ^ String.concat {|'\''|} (String.split_on_char '\'' s) ^ "'"

(* The prompt VHS gives bash, which [Prompt] waits for. *)
let ps1 = {|PS1='\[\e[38;2;90;86;224m\]> \[\e[0m\]'|}

let setup_actions = function
  | [] -> []
  | scripts ->
      let line = List.map (fun s -> "source " ^ sh_quote s) scripts @ [ ps1 ] in
      [
        Vhs.hidden
          [
            Vhs.type' (String.concat "; " line ^ "\n");
            Vhs.wait Prompt;
            Vhs.type' "clear\n";
            Vhs.wait Prompt;
          ];
      ]

let command lines =
  [
    Vhs.type' (String.concat "\n" lines ^ "\n");
    Vhs.wait Prompt;
    Vhs.sleep (Ms 1000);
  ]

let actions ~setup t = setup_actions setup @ List.concat_map command t
