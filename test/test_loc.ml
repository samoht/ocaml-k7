module Loc = Vhs.Loc

let contains_substring needle haystack =
  Re.execp Re.(compile (str needle)) haystack

let index_of needle haystack =
  match Re.exec_opt Re.(compile (str needle)) haystack with
  | Some g -> Re.Group.start g 0
  | None -> Alcotest.failf "%S not found in %S" needle haystack

let sample =
  Loc.v ~file:"foo.ml" ~first_byte:10 ~last_byte:14 ~first_line_num:2
    ~first_line_byte:5 ~last_line_num:2 ~last_line_byte:5

let and_accessors () =
  Alcotest.(check string) "file" "foo.ml" (Loc.file sample);
  Alcotest.(check int) "first_byte" 10 (Loc.first_byte sample);
  Alcotest.(check int) "last_byte" 14 (Loc.last_byte sample);
  Alcotest.(check int) "first_line_num" 2 (Loc.first_line_num sample);
  Alcotest.(check int) "first_line_byte" 5 (Loc.first_line_byte sample);
  Alcotest.(check int) "last_line_num" 2 (Loc.last_line_num sample);
  Alcotest.(check int) "last_line_byte" 5 (Loc.last_line_byte sample)

let set_file () =
  let l = Loc.set_file sample "other.ml" in
  Alcotest.(check string) "updated" "other.ml" (Loc.file l)

let none_is_none () =
  Alcotest.(check bool) "none.is_none" true (Loc.is_none Loc.none);
  Alcotest.(check bool) "sample.is_none" false (Loc.is_none sample)

let empty_detection () =
  let empty =
    Loc.v ~file:"-" ~first_byte:5 ~last_byte:3 ~first_line_num:1
      ~first_line_byte:0 ~last_line_num:1 ~last_line_byte:0
  in
  Alcotest.(check bool) "empty" true (Loc.is_empty empty);
  Alcotest.(check bool) "sample not empty" false (Loc.is_empty sample)

let equal_ignores_line_info () =
  let a =
    Loc.v ~file:"f" ~first_byte:3 ~last_byte:7 ~first_line_num:1
      ~first_line_byte:0 ~last_line_num:1 ~last_line_byte:0
  in
  let b =
    Loc.v ~file:"f" ~first_byte:3 ~last_byte:7 ~first_line_num:2
      ~first_line_byte:10 ~last_line_num:3 ~last_line_byte:20
  in
  Alcotest.(check bool) "equal despite different line info" true (Loc.equal a b)

let compare_orders_by_file_byte () =
  let a =
    Loc.v ~file:"a" ~first_byte:0 ~last_byte:0 ~first_line_num:1
      ~first_line_byte:0 ~last_line_num:1 ~last_line_byte:0
  in
  let b =
    Loc.v ~file:"b" ~first_byte:0 ~last_byte:0 ~first_line_num:1
      ~first_line_byte:0 ~last_line_num:1 ~last_line_byte:0
  in
  Alcotest.(check bool) "a < b (by file)" true (Loc.compare a b < 0);
  let b2 =
    Loc.v ~file:"a" ~first_byte:5 ~last_byte:10 ~first_line_num:1
      ~first_line_byte:0 ~last_line_num:1 ~last_line_byte:0
  in
  Alcotest.(check bool) "a < b2 (by first_byte)" true (Loc.compare a b2 < 0)

let to_first_and_to_last () =
  let l =
    Loc.v ~file:"-" ~first_byte:5 ~last_byte:10 ~first_line_num:2
      ~first_line_byte:2 ~last_line_num:3 ~last_line_byte:8
  in
  let fst = Loc.to_first l in
  Alcotest.(check int) "to_first first_byte" 5 (Loc.first_byte fst);
  Alcotest.(check int) "to_first last_byte" 5 (Loc.last_byte fst);
  let lst = Loc.to_last l in
  Alcotest.(check int) "to_last first_byte" 10 (Loc.first_byte lst);
  Alcotest.(check int) "to_last last_byte" 10 (Loc.last_byte lst)

let span_unions_ranges () =
  let a =
    Loc.v ~file:"-" ~first_byte:3 ~last_byte:7 ~first_line_num:1
      ~first_line_byte:0 ~last_line_num:1 ~last_line_byte:0
  in
  let b =
    Loc.v ~file:"-" ~first_byte:10 ~last_byte:20 ~first_line_num:1
      ~first_line_byte:0 ~last_line_num:1 ~last_line_byte:0
  in
  let s = Loc.span a b in
  Alcotest.(check int) "span first_byte" 3 (Loc.first_byte s);
  Alcotest.(check int) "span last_byte" 20 (Loc.last_byte s)

(* [before] and [after] bracket a range. Each is an empty location carrying the
   file, and each takes its byte position and its line position from the SAME
   end: [before] from the first position, [after] from the last one, at
   [last_byte + 1]. *)
let before_and_after () =
  let l =
    Loc.v ~file:"f" ~first_byte:5 ~last_byte:10 ~first_line_num:2
      ~first_line_byte:2 ~last_line_num:3 ~last_line_byte:8
  in
  let b = Loc.before l in
  Alcotest.(check string) "before file" "f" (Loc.file b);
  Alcotest.(check bool) "before is empty" true (Loc.is_empty b);
  Alcotest.(check int) "before first_byte" 5 (Loc.first_byte b);
  Alcotest.(check int) "before line num" 2 (Loc.first_line_num b);
  Alcotest.(check int) "before line byte" 2 (Loc.first_line_byte b);
  let a = Loc.after l in
  Alcotest.(check string) "after file" "f" (Loc.file a);
  Alcotest.(check bool) "after is empty" true (Loc.is_empty a);
  Alcotest.(check int) "after first_byte" 11 (Loc.first_byte a);
  Alcotest.(check int) "after line num" 3 (Loc.first_line_num a);
  Alcotest.(check int) "after line byte" 8 (Loc.first_line_byte a)

let pp_ocaml_single_line () =
  let l =
    Loc.v ~file:"foo.ml" ~first_byte:10 ~last_byte:14 ~first_line_num:2
      ~first_line_byte:5 ~last_line_num:2 ~last_line_byte:5
  in
  let out = Fmt.str "%a" Loc.pp_ocaml l in
  Alcotest.(check string)
    "ocaml format" "File \"foo.ml\", line 2, characters 5-10" out

let pp_gnu_format () =
  let l =
    Loc.v ~file:"foo.ml" ~first_byte:10 ~last_byte:14 ~first_line_num:2
      ~first_line_byte:5 ~last_line_num:2 ~last_line_byte:5
  in
  let out = Fmt.str "%a" Loc.pp_gnu l in
  Alcotest.(check string) "gnu format" "foo.ml:2.6-10" out

let pp_none () =
  let out = Fmt.str "%a" Loc.pp_ocaml Loc.none in
  Alcotest.(check string) "none ocaml" "File \"-\"" out

let meta_basic () =
  let m = Loc.Meta.v ~ws_before:"  " ~ws_after:"\n" sample in
  Alcotest.(check bool) "is_none false" false (Loc.Meta.is_none m);
  Alcotest.(check string) "ws_before" "  " (Loc.Meta.ws_before m);
  Alcotest.(check string) "ws_after" "\n" (Loc.Meta.ws_after m);
  Alcotest.(check bool) "loc eq" true (Loc.equal sample (Loc.Meta.loc m))

let meta_none () =
  Alcotest.(check bool) "none is_none" true (Loc.Meta.is_none Loc.Meta.none);
  Alcotest.(check string) "none ws_before" "" (Loc.Meta.ws_before Loc.Meta.none)

let meta_with_loc () =
  let m = Loc.Meta.v ~ws_before:"a" sample in
  let m' = Loc.Meta.with_loc m Loc.none in
  Alcotest.(check bool) "loc replaced" true (Loc.is_none (Loc.Meta.loc m'));
  Alcotest.(check string) "ws preserved" "a" (Loc.Meta.ws_before m')

let meta_clear () =
  let m = Loc.Meta.v ~ws_before:"a" ~ws_after:"b" sample in
  let m1 = Loc.Meta.clear_ws m in
  Alcotest.(check string) "ws_before cleared" "" (Loc.Meta.ws_before m1);
  Alcotest.(check string) "ws_after cleared" "" (Loc.Meta.ws_after m1);
  let m2 = Loc.Meta.clear_loc m in
  Alcotest.(check bool) "loc cleared" true (Loc.is_none (Loc.Meta.loc m2))

let meta_copy_ws () =
  let src = Loc.Meta.v ~ws_before:"x" ~ws_after:"y" sample in
  let dst = Loc.Meta.v ~ws_before:"a" ~ws_after:"b" sample in
  let out = Loc.Meta.copy_ws src ~dst in
  Alcotest.(check string) "ws_before copied" "x" (Loc.Meta.ws_before out);
  Alcotest.(check string) "ws_after copied" "y" (Loc.Meta.ws_after out)

let path_root () =
  Alcotest.(check bool) "root is_root" true (Loc.Path.is_root Loc.Path.root)

let path_build () =
  let p = Loc.Path.mem "a" Loc.Path.root in
  let p = Loc.Path.mem "b" p in
  let p = Loc.Path.nth 3 p in
  match Loc.Path.rev_steps p with
  | [ Loc.Path.Nth (3, _); Loc.Path.Mem ("b", _); Loc.Path.Mem ("a", _) ] -> ()
  | _ -> Alcotest.fail "unexpected path shape"

let path_of_string_roundtrip () =
  match Loc.Path.of_string "ocaml.libs.[0]" with
  | Error e -> Alcotest.failf "parse error: %s" e
  | Ok p -> (
      match Loc.Path.rev_steps p with
      | [
       Loc.Path.Nth (0, _); Loc.Path.Mem ("libs", _); Loc.Path.Mem ("ocaml", _);
      ] ->
          ()
      | _ -> Alcotest.fail "unexpected parsed path")

let path_pp () =
  let p = Loc.Path.mem "ocaml" Loc.Path.root in
  let p = Loc.Path.mem "libs" p in
  let p = Loc.Path.nth 0 p in
  let out = Fmt.str "%a" Loc.Path.pp p in
  Alcotest.(check string) "pp" "ocaml.libs.[0]" out

let path_of_string_err () =
  match Loc.Path.of_string "a.[]" with
  | Ok _ -> Alcotest.fail "expected parse error"
  | Error _ -> ()

(* An index step is [-?[0-9]+] and nothing else. [0x10], [1_0] and [+3] are the
   names of members, not the integers OCaml's [int_of_string] reads out of them.
   The bracketed form addresses an index by construction, so a body that is not
   one is a parse error rather than a member whose name has a space in it. *)
let path_of_string_step_grammar () =
  let steps s =
    match Loc.Path.of_string s with
    | Error e -> Alcotest.failf "%s: unexpected parse error: %s" s e
    | Ok p -> Loc.Path.steps p
  in
  let is_mem n = function
    | Loc.Path.Mem (m, _) -> String.equal m n
    | _ -> false
  in
  let is_nth n = function Loc.Path.Nth (i, _) -> Int.equal i n | _ -> false in
  let check what s pred =
    match steps s with
    | [ _; step ] when pred step -> ()
    | _ -> Alcotest.failf "%s: %s" s what
  in
  check "0x10 is a member name" "a.0x10" (is_mem "0x10");
  check "1_0 is a member name" "a.1_0" (is_mem "1_0");
  check "+3 is a member name" "a.+3" (is_mem "+3");
  check "digits are an index" "a.2" (is_nth 2);
  check "a leading minus is an index" "a.-3" (is_nth (-3));
  check "bracketed digits are an index" "a.[2]" (is_nth 2);
  let rejects s =
    match Loc.Path.of_string s with
    | Ok _ -> Alcotest.failf "%s: expected a parse error" s
    | Error _ -> ()
  in
  rejects "a.[ 2]";
  rejects "a.[x]";
  rejects "a.[0x10]";
  rejects "a.99999999999999999999999"

let array_sort = ("array", Loc.Meta.none)
let nth_step n = Loc.Path.Nth (n, Loc.Meta.none)

(* A decoder that adds the enclosing frames as an error unwinds meets them
   innermost first. Whether it spells that [push] or [snoc], one storage order
   comes out and every accessor reads it the same way: the context of an error
   at [1][2] has [1] outermost. *)
let context_snoc_agrees_with_push () =
  let pushed =
    Loc.Context.push ~sort:array_sort (nth_step 1)
      (Loc.Context.push ~sort:array_sort (nth_step 2) Loc.Context.empty)
  in
  let snocced =
    Loc.Context.snoc ~sort:array_sort (nth_step 1)
      (Loc.Context.snoc ~sort:array_sort (nth_step 2) Loc.Context.empty)
  in
  Alcotest.(check string)
    "same trace"
    (Fmt.str "%a" Loc.Context.pp pushed)
    (Fmt.str "%a" Loc.Context.pp snocced);
  Alcotest.(check string)
    "path is root-to-leaf" "[1].[2]"
    (Fmt.str "%a" Loc.Path.pp (Loc.Context.path snocced));
  (match Loc.Context.last_step snocced with
  | Some (Loc.Path.Nth (2, _)) -> ()
  | _ -> Alcotest.fail "last_step is the innermost frame");
  match Loc.Context.frames snocced with
  | [ (_, Loc.Path.Nth (1, _)); (_, Loc.Path.Nth (2, _)) ] -> ()
  | _ -> Alcotest.fail "frames are root-to-leaf"

(* The trace reads outwards from the error: innermost frame first. *)
let context_trace_is_innermost_first () =
  let ctx =
    Loc.Context.snoc ~sort:array_sort (nth_step 1)
      (Loc.Context.snoc ~sort:array_sort (nth_step 2) Loc.Context.empty)
  in
  let s = Loc.Error.to_string (Loc.Error.msg ~ctx ~meta:Loc.Meta.none "boom") in
  Alcotest.(check bool)
    "index 2 before index 1" true
    (index_of "index 2" s < index_of "index 1" s)

(* Whether to print a location is a question about the LOCATION. [Meta.is_none]
   answers a different one -- it is physical equality on the metadata -- so a
   meta built around [Loc.none] must not print the [File "-"] that formatting
   {!Loc.none} yields. *)
let context_pp_skips_meta_without_loc () =
  let m = Loc.Meta.v Loc.none in
  let ctx =
    Loc.Context.push ~sort:("array", m) (Loc.Path.Nth (0, m)) Loc.Context.empty
  in
  let s = Fmt.str "%a" Loc.Context.pp ctx in
  Alcotest.(check bool) "frame kept" true (contains_substring "index 0" s);
  Alcotest.(check bool) "sort kept" true (contains_substring "array" s);
  Alcotest.(check bool)
    "no empty location" false
    (contains_substring "File \"-\"" s)

let meta_of_sample = Loc.Meta.v sample

(* The location leads and the message follows it, as OCaml writes it: the
   first line has to be matchable as [file:line:col:] by an editor or a CI
   annotator. A substring check would pass on either order, so this one is
   exact. *)
let error_msg () =
  let e = Loc.Error.msg ~ctx:Loc.Context.empty ~meta:meta_of_sample "boom" in
  Alcotest.(check string)
    "location first, message second"
    "File \"foo.ml\", line 2, characters 5-10:\nboom" (Loc.Error.to_string e)

(* Same order once a context is attached: location, message, then the frames,
   each carrying its own location prefix. *)
let error_pp_with_context () =
  let m = Loc.Meta.v sample in
  let ctx =
    Loc.Context.push ~sort:("config object", m)
      (Loc.Path.Mem ("port", m))
      Loc.Context.empty
  in
  let e = Loc.Error.msg ~ctx ~meta:meta_of_sample "boom" in
  Alcotest.(check string)
    "location, message, context"
    "File \"foo.ml\", line 2, characters 5-10:\n\
     boom\n\
     File \"foo.ml\", line 2, characters 5-10: in member port of\n\
     File \"foo.ml\", line 2, characters 5-10: config object"
    (Loc.Error.to_string e)

(* Same guard, on the error's own metadata. *)
let error_pp_skips_meta_without_loc () =
  let m = Loc.Meta.v Loc.none in
  Alcotest.(check bool) "not physically Meta.none" false (Loc.Meta.is_none m);
  let e = Loc.Error.msg ~ctx:Loc.Context.empty ~meta:m "boom" in
  Alcotest.(check string) "message only" "boom" (Loc.Error.to_string e)

(* A location that names a file and no position is what an error about a whole
   document carries, and it still leads: the alternative is a diagnostic that
   drops the only thing it knew about where the failure was. *)
let error_pp_names_a_file_without_a_position () =
  let loc = Loc.set_file Loc.none "a.json" in
  Alcotest.(check bool) "no position" true (Loc.is_none loc);
  let e = Loc.Error.msg ~ctx:Loc.Context.empty ~meta:(Loc.Meta.v loc) "boom" in
  Alcotest.(check string)
    "file, then message" "File \"a.json\":\nboom" (Loc.Error.to_string e)

let labelled e = Fmt.str "%a" Loc.Error.pp_labelled e

(* The compiler's own diagnostic: the label sits between the location and the
   message, so the first line keeps the [file:line:col:] prefix. *)
let error_pp_labelled () =
  let m = Loc.Meta.v sample in
  let ctx =
    Loc.Context.push ~sort:("config object", m)
      (Loc.Path.Mem ("port", m))
      Loc.Context.empty
  in
  let e = Loc.Error.msg ~ctx ~meta:meta_of_sample "boom" in
  Alcotest.(check string)
    "location, label, message, context"
    "File \"foo.ml\", line 2, characters 5-10:\n\
     Error: boom\n\
     File \"foo.ml\", line 2, characters 5-10: in member port of\n\
     File \"foo.ml\", line 2, characters 5-10: config object"
    (labelled e)

(* With nothing to lead with the label opens the line, which is where OCaml
   puts it for an error that names no place. *)
let error_pp_labelled_without_loc () =
  let e = Loc.Error.msg ~ctx:Loc.Context.empty ~meta:Loc.Meta.none "boom" in
  Alcotest.(check string) "label, message" "Error: boom" (labelled e)

let error_msg_raises () =
  try
    let _ : int = Loc.Error.fail meta_of_sample "bad" in
    Alcotest.fail "expected Error"
  with Loc.Error e ->
    Alcotest.(check bool)
      "ctx empty" true
      (Loc.Context.is_empty (Loc.Error.ctx e));
    Alcotest.(check string)
      "kind" "bad"
      (Loc.Error.string_of_kind (Loc.Error.kind e))

let error_expected_raises () =
  try
    let _ : int = Loc.Error.expected meta_of_sample "int" ~fnd:"string" in
    Alcotest.fail "expected Error"
  with Loc.Error e ->
    let s = Loc.Error.string_of_kind (Loc.Error.kind e) in
    Alcotest.(check bool)
      "mentions expected" true
      (contains_substring "Expected int" s);
    Alcotest.(check bool)
      "mentions found" true
      (contains_substring "found string" s)

let error_push_context () =
  let e = Loc.Error.msg ~ctx:Loc.Context.empty ~meta:meta_of_sample "bad" in
  let kinded = ("array", Loc.Meta.none) in
  let n = (3, Loc.Meta.none) in
  try
    let _ : int = Loc.Error.push_array kinded n e in
    Alcotest.fail "expected Error"
  with Loc.Error e ->
    let ctx = Loc.Error.ctx e in
    Alcotest.(check bool) "ctx non-empty" false (Loc.Context.is_empty ctx);
    Alcotest.(check int)
      "one layer" 1
      (List.length (Loc.Path.rev_steps (Loc.Context.path ctx)))

let error_pp_mentions_path () =
  let kinded = ("array", Loc.Meta.none) in
  let n = (3, Loc.Meta.none) in
  let ctx = Loc.Context.push_nth kinded n Loc.Context.empty in
  let e = Loc.Error.msg ~ctx ~meta:meta_of_sample "bad" in
  let s = Loc.Error.to_string e in
  Alcotest.(check bool) "msg in output" true (contains_substring "bad" s);
  Alcotest.(check bool) "index in output" true (contains_substring "index 3" s);
  Alcotest.(check bool) "kind in output" true (contains_substring "array" s)

(* Extensible kind: a codec library defines its own typed kind and registers a
   printer. The kind is pattern-matchable by consumers. *)
type Loc.Error.kind += Demo_sort_mismatch of { exp : string; fnd : string }

let () =
  Loc.Error.register_kind_printer (function
    | Demo_sort_mismatch { exp; fnd } ->
        Some (fun ppf -> Fmt.pf ppf "expected %s, got %s" exp fnd)
    | _ -> None)

let error_kind_extension () =
  let k = Demo_sort_mismatch { exp = "integer"; fnd = "string" } in
  Alcotest.(check string)
    "registered printer runs" "expected integer, got string"
    (Loc.Error.string_of_kind k);
  let e = Loc.Error.v ~ctx:Loc.Context.empty ~meta:meta_of_sample k in
  Alcotest.(check bool)
    "pattern-matches through Error.t" true
    (match Loc.Error.kind e with
    | Demo_sort_mismatch { exp = "integer"; _ } -> true
    | _ -> false);
  Alcotest.(check string)
    "Msg fallback still works" "plain text"
    (Loc.Error.string_of_kind (Loc.Error.Msg "plain text"))

let suite =
  ( "loc",
    [
      Alcotest.test_case "make + accessors" `Quick and_accessors;
      Alcotest.test_case "set_file" `Quick set_file;
      Alcotest.test_case "none is_none" `Quick none_is_none;
      Alcotest.test_case "is_empty detection" `Quick empty_detection;
      Alcotest.test_case "equal ignores line info" `Quick
        equal_ignores_line_info;
      Alcotest.test_case "compare orders by file then byte" `Quick
        compare_orders_by_file_byte;
      Alcotest.test_case "to_first / to_last" `Quick to_first_and_to_last;
      Alcotest.test_case "before / after" `Quick before_and_after;
      Alcotest.test_case "span unions ranges" `Quick span_unions_ranges;
      Alcotest.test_case "pp_ocaml single line" `Quick pp_ocaml_single_line;
      Alcotest.test_case "pp_gnu format" `Quick pp_gnu_format;
      Alcotest.test_case "pp_ocaml none" `Quick pp_none;
      Alcotest.test_case "Meta.make/accessors" `Quick meta_basic;
      Alcotest.test_case "Meta.none is_none" `Quick meta_none;
      Alcotest.test_case "Meta.with_loc" `Quick meta_with_loc;
      Alcotest.test_case "Meta.clear_ws / clear_loc" `Quick meta_clear;
      Alcotest.test_case "Meta.copy_ws" `Quick meta_copy_ws;
      Alcotest.test_case "Path.root" `Quick path_root;
      Alcotest.test_case "Path.mem/nth/rev_indices" `Quick path_build;
      Alcotest.test_case "Path.of_string round-trip" `Quick
        path_of_string_roundtrip;
      Alcotest.test_case "Path.pp" `Quick path_pp;
      Alcotest.test_case "Path.of_string error" `Quick path_of_string_err;
      Alcotest.test_case "Path.of_string step grammar" `Quick
        path_of_string_step_grammar;
      Alcotest.test_case "Context.snoc agrees with push" `Quick
        context_snoc_agrees_with_push;
      Alcotest.test_case "Context trace is innermost first" `Quick
        context_trace_is_innermost_first;
      Alcotest.test_case "Context.pp skips meta without loc" `Quick
        context_pp_skips_meta_without_loc;
      Alcotest.test_case "Error.msg / to_string" `Quick error_msg;
      Alcotest.test_case "Error.pp with context" `Quick error_pp_with_context;
      Alcotest.test_case "Error.pp skips meta without loc" `Quick
        error_pp_skips_meta_without_loc;
      Alcotest.test_case "Error.pp names a file without a position" `Quick
        error_pp_names_a_file_without_a_position;
      Alcotest.test_case "Error.pp_labelled" `Quick error_pp_labelled;
      Alcotest.test_case "Error.pp_labelled without loc" `Quick
        error_pp_labelled_without_loc;
      Alcotest.test_case "Error.msg raises" `Quick error_msg_raises;
      Alcotest.test_case "Error.fail_expected raises" `Quick
        error_expected_raises;
      Alcotest.test_case "Error.fail_push_array" `Quick error_push_context;
      Alcotest.test_case "Error.pp mentions path" `Quick error_pp_mentions_path;
      Alcotest.test_case "Error.kind extension + printer" `Quick
        error_kind_extension;
    ] )
