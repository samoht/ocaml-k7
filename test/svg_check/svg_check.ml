(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type node = El of string * (string * string) list * node list | Text of string

exception Bad of string

let tree svg =
  let fail_report (line, col) e =
    Fmt.kstr
      (fun msg -> raise (Bad msg))
      "%d:%d: %s" line col (Markup.Error.to_string e)
  in
  Markup.string svg
  |> Markup.parse_xml ~report:fail_report ~context:`Document
  |> Markup.signals
  |> Markup.tree
       ~text:(fun ss -> Text (String.concat "" ss))
       ~element:(fun (_, name) attrs children ->
         El (name, List.map (fun ((_, k), v) -> (k, v)) attrs, children))

let rec text = function
  | Text s -> s
  | El (_, _, children) -> String.concat "" (List.map text children)

let attr k attrs = Option.value (List.assoc_opt k attrs) ~default:""

let rec find_all p = function
  | Text _ -> []
  | El (_, _, children) as n ->
      (if p n then [ n ] else []) @ List.concat_map (find_all p) children

let graphemes s =
  let rec go acc i =
    if i >= String.length s then List.rev acc
    else
      let n = Uchar.utf_decode_length (String.get_utf_8_uchar s i) in
      go (String.sub s i n :: acc) (i + n)
  in
  go [] 0

let measure = Matrix_text.measure ~width_method:`Unicode ~tab_width:2

(* The cells a grapheme takes: those of its characters, without the
   zero-width ones that join a cell, such as a variation selector, which
   [measure] counts as making an emoji wide. *)
let width g =
  let b = Buffer.create (String.length g) in
  List.iter
    (fun c ->
      let u = Uchar.utf_decode_uchar (String.get_utf_8_uchar c 0) in
      if Uchar.to_int u < 0xa0 || measure c > 0 then Buffer.add_string b c)
    (graphemes g);
  max 1 (measure (Buffer.contents b))

let trim_end s =
  let rec go n = if n > 0 && s.[n - 1] = ' ' then go (n - 1) else n in
  String.sub s 0 (go (String.length s))

let trim_rows s =
  String.concat "\n" (List.map trim_end (String.split_on_char '\n' s))

(* The cells of a row definition: those of the rows it uses, then each code
   point of its tspans at its [x], or a whole tspan at its only [x]. *)
let rec row_cells ~cell_width defs def =
  let cells = Hashtbl.create 80 in
  let add = function
    | El ("use", attrs, _) -> (
        match Hashtbl.find_opt defs (attr "href" attrs) with
        | Some d ->
            Hashtbl.iter (Hashtbl.replace cells) (row_cells ~cell_width defs d)
        | None -> raise (Bad ("no definition " ^ attr "href" attrs)))
    | El ("text", _, _) as t ->
        List.iter
          (function
            | El ("tspan", attrs, _) as t ->
                let xs =
                  List.map
                    (fun x -> int_of_string x / cell_width)
                    (String.split_on_char ' ' (attr "x" attrs))
                in
                let s = text t in
                let cps = graphemes s in
                if List.length xs = List.length cps then
                  List.iter2 (fun x g -> Hashtbl.replace cells x g) xs cps
                else Hashtbl.replace cells (List.hd xs) s
            | _ -> ())
          (find_all (function El ("tspan", _, _) -> true | _ -> false) t)
    | _ -> ()
  in
  (match def with
  | El (_, _, children) -> List.iter add children
  | Text _ -> ());
  cells

let row_text cells =
  let last = Hashtbl.fold (fun k _ m -> max k m) cells (-1) in
  let b = Buffer.create 80 in
  let rec go x =
    if x <= last then
      match Hashtbl.find_opt cells x with
      | Some g ->
          Buffer.add_string b g;
          go (x + width g)
      | None ->
          Buffer.add_char b ' ';
          go (x + 1)
  in
  go 0;
  trim_end (Buffer.contents b)

let frames ~cell_width ~cell_height ~rows svg =
  match tree svg with
  | None -> Error "no document"
  | exception Bad msg -> Error msg
  | Some root -> (
      let defs = Hashtbl.create 64 in
      List.iter
        (function
          | El ("g", attrs, _) as g when attr "id" attrs <> "" ->
              Hashtbl.replace defs ("#" ^ attr "id" attrs) g
          | _ -> ())
        (find_all (function El ("g", _, _) -> true | _ -> false) root);
      let row href =
        row_text (row_cells ~cell_width defs (Hashtbl.find defs href))
      in
      let strip =
        find_all
          (function El ("g", a, _) -> attr "class" a = "s" | _ -> false)
          root
      in
      let frame = function
        | El (_, _, uses) ->
            let lines = Array.make rows "" in
            List.iter
              (function
                | El ("use", a, _) when Hashtbl.mem defs (attr "href" a) ->
                    lines.(int_of_string (attr "y" a) / cell_height) <-
                      row (attr "href" a)
                | _ -> ())
              uses;
            String.concat "\n" (Array.to_list lines)
        | Text _ -> ""
      in
      match
        List.concat_map
          (function El (_, _, frames) -> List.map frame frames | Text _ -> [])
          strip
      with
      | frames -> Ok frames
      | exception Bad msg -> Error msg)
