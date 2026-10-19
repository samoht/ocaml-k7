(*---------------------------------------------------------------------------
  Copyright (c) 2024 The jsont programmers. All rights reserved.
  SPDX-License-Identifier: ISC

  Extracted from jsont (jsont.ml / jsont_base.ml) with JSON-specific pieces
  removed.
  ---------------------------------------------------------------------------*)

(* Bold-styled code formatter, matching jsont's Fmt.code. Styling respects the
   formatter's renderer (opam fmt): on ppf with [Fmt.set_style_renderer ppf
   `Ansi_tty] the output is bold ANSI, on a default formatter (including
   asprintf / str_formatter) it is plain. *)
let pp_code = Fmt.styled `Bold Fmt.string

type fpath = string

let file_none = "-"
let pp_path = Format.pp_print_string

type byte_pos = int

let byte_pos_none = -1
let compare_byte_pos : byte_pos -> byte_pos -> int = Int.compare

type line_num = int

let line_num_none = -1
let compare_line_num : line_num -> line_num -> int = Int.compare

(* Text locations.

   A line position is a (line number, byte offset of line start) pair.
   Subtracting the line start from an absolute byte gives a column approximation
   that is correct on US-ASCII data; UTF-8 multibyte runs shift it. *)

type t = {
  file : fpath;
  first_byte : byte_pos;
  last_byte : byte_pos;
  first_line_num : line_num;
  first_line_byte : byte_pos;
  last_line_num : line_num;
  last_line_byte : byte_pos;
}

let v ~file ~first_byte ~last_byte ~first_line_num ~first_line_byte
    ~last_line_num ~last_line_byte =
  {
    file;
    first_byte;
    last_byte;
    first_line_num;
    first_line_byte;
    last_line_num;
    last_line_byte;
  }

let file l = l.file
let set_file l file = { l with file }
let first_byte l = l.first_byte
let last_byte l = l.last_byte
let first_line_num l = l.first_line_num
let first_line_byte l = l.first_line_byte
let last_line_num l = l.last_line_num
let last_line_byte l = l.last_line_byte

let none =
  v ~file:file_none ~first_byte:byte_pos_none ~last_byte:byte_pos_none
    ~first_line_num:line_num_none ~first_line_byte:byte_pos_none
    ~last_line_num:line_num_none ~last_line_byte:byte_pos_none

let is_none l = l.first_byte < 0
let is_empty l = l.first_byte > l.last_byte

let equal l0 l1 =
  String.equal l0.file l1.file
  && Int.equal l0.first_byte l1.first_byte
  && Int.equal l0.last_byte l1.last_byte

let compare l0 l1 =
  let c = String.compare l0.file l1.file in
  if c <> 0 then c
  else
    let c = Int.compare l0.first_byte l1.first_byte in
    if c <> 0 then c else Int.compare l0.last_byte l1.last_byte

let set_first l ~first_byte ~first_line_num ~first_line_byte =
  { l with first_byte; first_line_num; first_line_byte }

let to_first l =
  v ~file:l.file ~first_byte:l.first_byte ~last_byte:l.first_byte
    ~first_line_num:l.first_line_num ~first_line_byte:l.first_line_byte
    ~last_line_num:l.first_line_num ~last_line_byte:l.first_line_byte

let to_last l =
  v ~file:l.file ~first_byte:l.last_byte ~last_byte:l.last_byte
    ~first_line_num:l.last_line_num ~first_line_byte:l.last_line_byte
    ~last_line_num:l.last_line_num ~last_line_byte:l.last_line_byte

let before l =
  v ~file:l.file ~first_byte:l.first_byte ~last_byte:byte_pos_none
    ~first_line_num:l.first_line_num ~first_line_byte:l.first_line_byte
    ~last_line_num:line_num_none ~last_line_byte:byte_pos_none

let after l =
  v ~file:l.file ~first_byte:(l.last_byte + 1) ~last_byte:byte_pos_none
    ~first_line_num:l.last_line_num ~first_line_byte:l.last_line_byte
    ~last_line_num:line_num_none ~last_line_byte:byte_pos_none

let span l0 l1 =
  let first_byte, first_line_num, first_line_byte =
    if l0.first_byte < l1.first_byte then
      (l0.first_byte, l0.first_line_num, l0.first_line_byte)
    else (l1.first_byte, l1.first_line_num, l1.first_line_byte)
  in
  let last_byte, last_line_num, last_line_byte, file =
    if l0.last_byte < l1.last_byte then
      (l1.last_byte, l1.last_line_num, l1.last_line_byte, l1.file)
    else (l0.last_byte, l0.last_line_num, l0.last_line_byte, l0.file)
  in
  v ~file ~first_byte ~first_line_num ~first_line_byte ~last_byte ~last_line_num
    ~last_line_byte

let pf = Fmt.pf

let pp_ocaml ppf l =
  if is_none l then pf ppf "File \"%a\"" pp_path l.file
  else
    let pp_lines ppf l =
      if l.first_line_num = l.last_line_num then
        pf ppf "line %d" l.first_line_num
      else pf ppf "lines %d-%d" l.first_line_num l.last_line_num
    in
    let pos_s = l.first_byte - l.first_line_byte in
    let pos_e = l.last_byte - l.last_line_byte + 1 in
    if pos_s = 0 && pos_e = 0 then
      pf ppf "File \"%a\", %a" pp_path l.file pp_lines l
    else
      pf ppf "File \"%a\", %a, characters %d-%d" pp_path l.file pp_lines l pos_s
        pos_e

let pp_gnu ppf l =
  if is_none l then pf ppf "%a:" pp_path l.file
  else
    let pp_lines ppf l =
      let col_s = l.first_byte - l.first_line_byte + 1 in
      let col_e = l.last_byte - l.last_line_byte + 1 in
      if l.first_line_num = l.last_line_num then
        pf ppf "%d.%d-%d" l.first_line_num col_s col_e
      else pf ppf "%d.%d-%d.%d" l.first_line_num col_s l.last_line_num col_e
    in
    pf ppf "%a:%a" pp_path l.file pp_lines l

let pp = pp_ocaml

(* String.sub by byte range, tolerant of out-of-bounds. *)

let string_subrange ?(first = 0) ?last s =
  let max = String.length s - 1 in
  let last =
    match last with None -> max | Some l when l > max -> max | Some l -> l
  in
  let first = if first < 0 then 0 else first in
  if first > last then "" else String.sub s first (last - first + 1)

module Meta = struct
  type location = t

  type t = {
    loc : location;
    ws_before : string;
    ws_after : string;
    text : string option;
  }

  let v ?(ws_before = "") ?(ws_after = "") ?text loc =
    { loc; ws_before; ws_after; text }

  let none = { loc = none; ws_before = ""; ws_after = ""; text = None }
  let is_none m = none == m
  let loc m = m.loc
  let ws_before m = m.ws_before
  let ws_after m = m.ws_after
  let text m = m.text
  let with_loc m loc = { m with loc }
  let with_text m text = { m with text = Some text }
  let clear_ws m = { m with ws_before = ""; ws_after = "" }
  let clear_loc m = { m with loc = none.loc }

  let clear_text m =
    match m.text with None -> m | Some _ -> { m with text = None }

  let copy_ws src ~dst =
    { dst with ws_before = src.ws_before; ws_after = src.ws_after }
end

type 'a node = 'a * Meta.t

module Path = struct
  type step = ..
  type step += Mem of string node | Nth of int node

  let pp_name = pp_code
  let pp_step_num ppf n = pp_code ppf (Int.to_string n)

  let step_printers : (step -> (Format.formatter -> unit) option) list ref =
    ref []

  let register_step_printer p = step_printers := p :: !step_printers

  let default_pp_step ppf = function
    | Mem (n, _) -> pp_name ppf n
    | Nth (n, _) -> Fmt.pf ppf "[%a]" pp_step_num n
    | _ -> Format.pp_print_string ppf "<unknown path step>"

  let pp_step ppf s =
    let rec find = function
      | [] -> default_pp_step ppf s
      | p :: ps -> ( match p s with Some f -> f ppf | None -> find ps)
    in
    find !step_printers

  let default_pp_step_trace ppf = function
    | Mem (n, meta) ->
        Fmt.pf ppf "%a: in member %a" pp (Meta.loc meta) pp_name n
    | Nth (n, meta) ->
        Fmt.pf ppf "%a: at index %a" pp (Meta.loc meta) pp_step_num n
    | s -> pp_step ppf s

  let pp_step_trace = default_pp_step_trace

  type t = step list
  (* Internally leaf-to-root: [push s p] conses [s] at the head. Public
     accessors normalize. *)

  let root = []
  let is_root = function [] -> true | _ -> false
  let push s p = s :: p
  let nth ?(meta = Meta.none) n p = Nth (n, meta) :: p
  let mem ?(meta = Meta.none) n p = Mem (n, meta) :: p
  let steps p = List.rev p
  let rev_steps p = p

  let pp ppf steps =
    let pp_sep ppf () = Fmt.char ppf '.' in
    Fmt.list ~sep:pp_sep pp_step ppf (List.rev steps)

  let err i fmt = Fmt.failwith ("%d: " ^^ fmt) i
  let err_unexp_eoi i = err i "Unexpected end of input"
  let err_unexp_char i s = err i "Unexpected character: %C" s.[i]
  let err_illegal_char i s = err i "Illegal character here: %C" s.[i]

  (* An index step is [-?[0-9]+]. [int_of_string] reads more than that -- it
     takes [0x10] for 16, [1_0] for 10 and [+3] for 3 -- and a member is free to
     be named any of those. *)
  let is_index s =
    let len = String.length s in
    let first = if len > 0 && s.[0] = '-' then 1 else 0 in
    let rec digits i =
      i >= len || match s.[i] with '0' .. '9' -> digits (i + 1) | _ -> false
    in
    first < len && digits first

  let parse_step p s i max =
    let first, stop = match s.[i] with '[' -> (i + 1, ']') | _ -> (i, '.') in
    let last, next =
      let rec loop stop s i max =
        match i > max with
        | true -> if stop = ']' then err_unexp_eoi i else (i - 1, i)
        | false ->
            let illegal = s.[i] = '[' || (s.[i] = ']' && stop = '.') in
            if illegal then err_illegal_char i s
            else if s.[i] <> stop then loop stop s (i + 1) max
            else (i - 1, if stop = ']' then i + 1 else i)
      in
      loop stop s first max
    in
    let step = string_subrange ~first ~last s in
    if step = "" then err first "illegal empty index"
    else if is_index step then
      match int_of_string_opt step with
      | Some n -> (next, Nth (n, Meta.none) :: p)
      | None -> err first "Index out of range: %s" step
    else if stop = ']' then err first "Not an index: %S" step
    else (next, Mem (step, Meta.none) :: p)

  let of_string s =
    let rec loop p s i max =
      if i > max then p
      else
        let next, p = parse_step p s i max in
        if next > max then p
        else if s.[next] <> '.' then err_unexp_char next s
        else if next + 1 <= max then loop p s (next + 1) max
        else err_unexp_eoi next
    in
    try
      if s = "" then Ok []
      else
        let start = if s.[0] = '.' then 1 else 0 in
        Ok (loop [] s start (String.length s - 1))
    with Failure e -> Error e
end

module Context = struct
  type frame = { sort : string node; step : Path.step }
  type t = frame list
  (* ROOT-to-LEAF: the head is the outermost frame. A decoder meets the
     enclosing frames innermost first -- an error unwinds through the inner
     catcher before the outer one -- so each frame it adds encloses everything
     already stored and is consed at the head, leaving the outermost there.
     Path's raw storage is the mirror, LEAF-to-ROOT (cons at head during
     descent); the accessors below bridge the two. *)

  let empty = []
  let is_empty ctx = ctx = []
  let push ~sort step ctx = { sort; step } :: ctx
  let push_nth sort n ctx = push ~sort (Path.Nth n) ctx
  let push_mem sort n ctx = push ~sort (Path.Mem n) ctx

  (* The spelling used by a decoder that builds the context only when an error
     propagates up. The frame it adds is the enclosing one, which is what [push]
     adds too, so the two agree on storage. *)
  let snoc ~sort step ctx = push ~sort step ctx

  let last_step ctx =
    match List.rev ctx with [] -> None | f :: _ -> Some f.step

  let last_sort ctx =
    match List.rev ctx with [] -> None | f :: _ -> Some f.sort

  let frames ctx = List.map (fun f -> (f.sort, f.step)) ctx
  let path ctx = List.rev_map (fun f -> f.step) ctx
  let pp_name = pp_code
  let pp_int ppf i = pp_code ppf (Int.to_string i)

  let pp ppf ctx =
    let pp_meta ppf meta =
      let loc = Meta.loc meta in
      if is_none loc then () else Fmt.pf ppf "%a: " pp loc
    in
    let pp_el ppf { sort; step } =
      match step with
      | Path.Nth (n, meta) ->
          Fmt.pf ppf "@[<v>%aat index %a of@,%a%a@]" pp_meta meta pp_int n
            pp_meta (snd sort) pp_name (fst sort)
      | Path.Mem (name, meta) ->
          Fmt.pf ppf "@[<v>%ain member %a of@,%a%a@]" pp_meta meta pp_name name
            pp_meta (snd sort) pp_name (fst sort)
      | step ->
          Fmt.pf ppf "@[<v>%a of@,%a%a@]" Path.pp_step_trace step pp_meta
            (snd sort) pp_name (fst sort)
    in
    if ctx = [] then ()
    else Fmt.pf ppf "@,@[<v>%a@]" (Fmt.list pp_el) (List.rev ctx)
end

type error_kind = ..
type error_kind += Msg of string
type error = { ctx : Context.t; meta : Meta.t; kind : error_kind }

exception Error of error

module Error = struct
  type kind = error_kind = ..
  type kind += Msg = Msg
  type nonrec t = error = { ctx : Context.t; meta : Meta.t; kind : kind }

  (* Registry of printers contributed by codec libraries. The first printer that
     returns [Some f] wins; [Msg] falls through to the default. *)
  let kind_printers : (error_kind -> (Format.formatter -> unit) option) list ref
      =
    ref []

  let register_kind_printer p = kind_printers := p :: !kind_printers

  let default_pp_kind ppf = function
    | Msg s -> Fmt.lines ppf s
    | _ -> Format.pp_print_string ppf "<unknown error kind>"

  let pp_kind ppf k =
    let rec find = function
      | [] -> default_pp_kind ppf k
      | p :: ps -> ( match p k with Some f -> f ppf | None -> find ps)
    in
    find !kind_printers

  let string_of_kind k = Fmt.str "%a" pp_kind k
  let pp_code = pp_code
  let v ~ctx ~meta kind = { ctx; meta; kind }
  let ctx e = e.ctx
  let meta e = e.meta
  let kind e = e.kind
  let msg ~ctx ~meta s = { ctx; meta; kind = Msg s }
  let raise ~ctx ~meta kind = raise_notrace (Error { ctx; meta; kind })

  let fail meta s =
    raise_notrace (Error { ctx = Context.empty; meta; kind = Msg s })

  let failf meta fmt = Fmt.kstr (fun s -> fail meta s) fmt

  let push_array sort n e =
    raise_notrace (Error { e with ctx = Context.push_nth sort n e.ctx })

  let push_object sort n e =
    raise_notrace (Error { e with ctx = Context.push_mem sort n e.ctx })

  let adjust_context ~first_byte ~first_line_num ~first_line_byte e =
    match e.ctx with
    | [] -> raise_notrace (Error e)
    | { sort = name, smeta; step } :: is ->
        let loc = Meta.loc smeta in
        let loc =
          if is_none loc then loc
          else set_first loc ~first_byte ~first_line_num ~first_line_byte
        in
        let smeta = Meta.with_loc smeta loc in
        let ctx = { Context.sort = (name, smeta); step } :: is in
        raise_notrace (Error { e with ctx })

  (* The line a diagnostic opens with, and the one decision about it both
     renderings below read: a location leads when it names anything. Positions
     are one way it does; a file alone is the other, which is the location an
     error about a whole document carries and what the compiler prints for one.
     [Meta.none] names neither, and an error carrying it opens on its message. *)
  let pp_loc_line ppf m =
    let loc = Meta.loc m in
    if not (is_none loc && file loc = file_none) then Fmt.pf ppf "%a:@," pp loc

  (* Location first, then the message, then the context: the layout every OCaml
     tool emits, and the only one editors, CI annotators and grep-based tooling
     can match, since they all key on a leading [file:line:col:]. The trailing
     colon belongs to the location line, which introduces the message below
     it. With no location the message stands alone -- no colon, no blank
     line. Context frames carry their own [location: ] prefix (Context.pp). *)
  let pp ppf e =
    Fmt.pf ppf "@[<v>%a%a%a@]" pp_loc_line e.meta pp_kind e.kind Context.pp
      e.ctx

  let to_string e = Fmt.str "%a" pp e

  let pp_label ppf () =
    Fmt.styled (`Fg `Red) (Fmt.styled `Bold Fmt.string) ppf "Error";
    Fmt.char ppf ':'

  (* The whole diagnostic, label included, so no caller assembles one. The
     label goes between the location and the message: ahead of the location it
     would break the [file:line:col:] prefix the layout exists to create. *)
  let pp_labelled ppf e =
    Fmt.pf ppf "@[<v>%a%a %a%a@]" pp_loc_line e.meta pp_label () pp_kind e.kind
      Context.pp e.ctx

  let expected meta exp ~fnd =
    failf meta "Expected %a but found %a" pp_code exp pp_code fnd
end
