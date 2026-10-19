(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Alcobar

let outputs = [ "demo.gif" ]
let max_ms = 9_223_372_036_854

let tape ?settings ?(outputs = outputs) actions =
  Vhs.tape ?settings ~outputs actions |> Result.map (Fmt.str "%a" Vhs.pp)

(* The printed lines of the actions, without the [Output] line. *)
let lines ?settings actions =
  match tape ?settings actions with
  | Error _ -> None
  | Ok s -> (
      match String.split_on_char '\n' s with
      | _output :: rest -> Some (List.filter (( <> ) "") rest)
      | [] -> fail "empty tape")

let accepted ?settings actions = Option.is_some (lines ?settings actions)
let is_control c = Char.code c < 0x20 || c = '\x7f'

(* An independent reader for the [Type] subset of VHS: no escapes, the
   matching quote ends the string. *)
let decode_type line =
  let n = String.length line in
  if n < 7 || not (String.starts_with ~prefix:"Type " line) then
    failf "not a Type line: %S" line;
  let q = line.[5] in
  if not (List.mem q [ '"'; '\''; '`' ] && line.[n - 1] = q) then
    failf "badly quoted: %S" line;
  let payload = String.sub line 6 (n - 7) in
  if String.contains payload q then failf "quote inside: %S" line;
  payload

(* What the printed lines type, [Tab] and [Enter] included. *)
let typed lines =
  String.concat ""
    (List.map
       (function "Tab" -> "\t" | "Enter" -> "\n" | line -> decode_type line)
       lines)

(* [type' s] is accepted iff [s] is UTF-8 without control characters other
   than tab and newline, and then types exactly [s]. *)
let test_type s =
  let typeable =
    String.is_valid_utf_8 s
    && not (String.exists (fun c -> is_control c && c <> '\t' && c <> '\n') s)
  in
  match lines [ Vhs.type' s ] with
  | None -> if typeable then failf "rejected typeable %S" s
  | Some lines ->
      if not typeable then failf "accepted %S" s;
      let got = typed lines in
      if got <> s then failf "%S typed as %S" s got

(* Every builder and setting is total: [tape] returns, it never raises, and
   an accepted tape prints lines without control characters. *)
let test_total s t n =
  let c = Char.chr (abs n mod 256) in
  let actions =
    Vhs.
      [
        type' s;
        key (Ctrl c);
        key (Alt c);
        sleep (Ms n);
        wait ~timeout:(Ms n) (Screen s);
        wait (Screen t);
        hidden [ raw s; type' t ];
        raw t;
      ]
  in
  let settings =
    Vhs.
      [
        Font_size n;
        Font_family s;
        Width n;
        Height n;
        Theme t;
        Padding n;
        Typing_speed (Ms n);
        Framerate n;
      ]
  in
  List.iter
    (fun (settings, outputs, actions) ->
      match tape ~settings ~outputs actions with
      | Error _ -> ()
      | Ok printed ->
          if String.exists (fun c -> is_control c && c <> '\n') printed then
            failf "control character in %S" printed)
    [
      ([], [ s ], actions);
      (settings, outputs, []);
      ([], outputs, actions);
      ([], [ t ^ ".gif" ], List.map (fun a -> Vhs.hidden [ a ]) actions);
    ]

(* A sleep accepts [0] to [max_ms], a wait timeout [1] to [max_ms], and both
   print the value. *)
let test_durations n =
  let in_range lo = n >= lo && n <= max_ms in
  (match lines [ Vhs.sleep (Ms n) ] with
  | Some [ l ] -> check (in_range 0 && l = Fmt.str "Sleep %dms" n)
  | Some _ -> fail "sleep printed several lines"
  | None -> check (not (in_range 0)));
  match lines [ Vhs.wait ~timeout:(Ms n) Prompt ] with
  | Some [ l ] -> check (in_range 1 && l = Fmt.str "Wait@%dms" n)
  | Some _ -> fail "wait printed several lines"
  | None -> check (not (in_range 1))

(* An accepted pattern is one VHS regexp token: no slash between the
   delimiters, no line break, no odd run of backslashes before the closing
   slash; and the token's own text is an accepted pattern. *)
let test_screen re =
  match lines [ Vhs.wait (Screen re) ] with
  | None -> ()
  | Some [ l ] ->
      let prefix = "Wait+Screen /" in
      if not (String.starts_with ~prefix l && String.ends_with ~suffix:"/" l)
      then failf "not a regexp token: %S" l;
      let body =
        String.sub l (String.length prefix)
          (String.length l - String.length prefix - 1)
      in
      if String.contains body '/' then failf "slash in %S" l;
      let rec trailing i =
        if i > 0 && body.[i - 1] = '\\' then 1 + trailing (i - 1) else 0
      in
      if trailing (String.length body) mod 2 = 1 then
        failf "odd backslashes end %S" l;
      if not (accepted [ Vhs.wait (Screen body) ]) then
        failf "%S written as the invalid %S" re body
  | Some _ -> fail "wait printed several lines"

(* A raw line is printed as is; a [Set] other than [TypingSpeed] or a
   [Require] is refused after the first other action. *)
let test_raw line =
  (match lines [ Vhs.raw line ] with
  | Some [ l ] -> check (l = line)
  | Some [] -> check (line = "")
  | Some _ -> fail "raw printed several lines"
  | None -> ());
  (match lines [ Vhs.raw line ] with
  | Some _ when List.mem (String.trim line) [ "Hide"; "Show" ] ->
      failf "accepted %S" line
  | _ -> ());
  let words =
    String.split_on_char ' ' (String.trim line) |> List.filter (( <> ) "")
  in
  let late =
    match words with
    | "Set" :: "TypingSpeed" :: _ -> false
    | ("Set" | "Require") :: _ -> true
    | _ -> false
  in
  if late && accepted [ Vhs.key Enter; Vhs.raw line ] then
    failf "accepted %S after an action" line

(* [hidden] prints one [Hide] ... [Show] pair around the actions it hides,
   however deep the nesting, and nothing for no action. *)
let test_hidden texts depth =
  let actions = List.map Vhs.type' texts in
  let rec nest d a = if d <= 0 then a else nest (d - 1) [ Vhs.hidden a ] in
  match (lines actions, lines (nest (1 + (abs depth mod 4)) actions)) with
  | Some plain, Some nested ->
      if plain = [] then check (nested = [])
      else check (nested = ("Hide" :: plain) @ [ "Show" ])
  | None, None -> ()
  | _ -> fail "hidden changed acceptance"

(* Integer settings are accepted in their documented ranges, the frame size
   being at least twice the padding, defaults included. *)
let test_sizes w h p =
  let p' = if p < 0 then 60 else p in
  let ok = p >= 0 && w >= 2 * p' && h >= 2 * p' in
  let settings = Vhs.[ Padding p; Width w; Height h ] in
  match lines ~settings [] with
  | Some l ->
      check ok;
      check
        (List.equal String.equal l
           [
             Fmt.str "Set Padding %d" p;
             Fmt.str "Set Width %d" w;
             Fmt.str "Set Height %d" h;
           ])
  | None -> check (not ok)

(* An output is accepted iff it ends with a slash or one of the documented
   extensions in its last segment, does not start with a dash, and holds no
   control character nor all three quotes. *)
let test_output path suffix =
  let path = path ^ suffix in
  let ext =
    List.exists
      (fun e -> String.ends_with ~suffix:e path)
      [ ".gif"; ".mp4"; ".webm"; ".txt"; ".ascii"; ".test"; ".png" ]
  in
  let quotes =
    List.for_all (fun q -> String.contains path q) [ '"'; '\''; '`' ]
  in
  let ok =
    (ext || String.ends_with ~suffix:"/" path)
    && (not (String.starts_with ~prefix:"-" path))
    && (not (String.exists is_control path))
    && not quotes
  in
  check (Result.is_ok (tape ~outputs:[ path ] []) = ok)

(* [pp_error] never raises; [Io] prints its message and [Failed] at most ten
   lines of output under one header line. *)
let test_pp_error s n =
  let pp e = Fmt.str "%a" Vhs.pp_error e in
  check (pp (Vhs.Io s) = s);
  ignore (pp (Vhs.Executable_not_found s));
  List.iter
    (fun status ->
      let printed = pp (Failed { vhs = "vhs"; status; output = s }) in
      let lines = String.split_on_char '\n' printed in
      check (List.length lines <= 11))
    [ `Exited n; `Signaled n ]

(* [Ctrl c] is accepted iff [c] is an ASCII letter or one of [@[\\]^-.];
   [Alt c] iff [c] is a letter other than [m] and [s], [.] or a bracket. *)
let test_keys c =
  let letter = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') in
  let ctrl = letter || String.contains {|@[\]^-.|} c in
  let alt = (letter && c <> 'm' && c <> 's') || String.contains "[]." c in
  check (accepted [ Vhs.key (Ctrl c) ] = ctrl);
  check (accepted [ Vhs.key (Alt c) ] = alt);
  match lines [ Vhs.key (Alt c) ] with
  | Some [ l ] -> check (String.equal l ("Alt+" ^ String.make 1 c))
  | Some _ -> fail "Alt printed several lines"
  | None -> ()

(* Scalar settings are accepted in their documented ranges, and a font list
   iff it holds no single quote, backslash or control character. *)
let test_settings n font =
  check (accepted ~settings:[ Font_size n ] [] = (n > 0));
  check (accepted ~settings:[ Framerate n ] [] = (n > 0));
  let quotes =
    List.for_all (fun q -> String.contains font q) [ '"'; '\''; '`' ]
  in
  let ok =
    not
      (quotes
      || String.exists (fun c -> c = '\'' || c = '\\' || is_control c) font)
  in
  check (accepted ~settings:[ Font_family font ] [] = ok)

(* [to_string] prints the outputs, then the settings, then the actions; [pp]
   prints the same text. *)
let test_order path text =
  match
    Vhs.tape ~settings:[ Width 1000 ]
      ~outputs:[ path ^ ".gif"; "x.txt" ]
      [ Vhs.type' text ]
  with
  | Error _ -> ()
  | Ok t -> (
      let s = Vhs.to_string t in
      check (String.equal s (Fmt.str "%a" Vhs.pp t));
      match String.split_on_char '\n' s with
      | o1 :: o2 :: set :: _ ->
          check (String.starts_with ~prefix:"Output " o1);
          check (String.equal o2 {|Output "x.txt"|});
          check (String.equal set "Set Width 1000")
      | _ -> fail "missing lines")

(* Check against the real parser, rather than a second tape lexer. *)
let test_validates text n =
  let open Vhs in
  let candidates =
    [
      ([], [ type' text ]);
      ([], [ wait (Screen text) ]);
      ([], [ raw text ]);
      ([], [ raw (Fmt.str "Sleep -%dms" (1 + (n land 255))) ]);
      ([ Theme text ], []);
      ([ Font_family text ], []);
      ([ Padding n; Width 1200; Height 600 ], []);
      ([], [ hidden [ type' text ]; key Enter; sleep (Ms n) ]);
    ]
  in
  List.iter
    (fun (settings, actions) ->
      match Vhs.tape ~settings ~outputs:[ "demo.gif" ] actions with
      | Error _ -> ()
      | Ok t ->
          let path = Filename.temp_file "vhs-property" ".tape" in
          Fun.protect
            ~finally:(fun () -> Sys.remove path)
            (fun () ->
              let oc = open_out_bin path in
              output_string oc (Vhs.to_string t);
              close_out oc;
              if
                Sys.command
                  ("vhs validate " ^ Filename.quote path ^ " > /dev/null 2>&1")
                <> 0
              then failf "accepted tape rejected by VHS: %s" (Vhs.to_string t)))
    candidates

let test_padding_bound padding =
  let settings = Vhs.[ Padding padding; Width 1200; Height 600 ] in
  let expected = padding >= 0 && padding <= 300 in
  check (Result.is_ok (Vhs.tape ~settings ~outputs []) = expected)

let suite =
  ( "vhs",
    [
      test_case "padding fits the frame"
        [ choose [ int; const max_int; const ((max_int / 2) + 1); const 300 ] ]
        test_padding_bound;
      test_case "accepted tapes validate" [ bytes; int ] test_validates;
      test_case "type is exact"
        [ choose [ bytes; const "\xff"; const {|a"b'c`d|} ] ]
        test_type;
      test_case "tape is total" [ bytes; bytes; int ] test_total;
      test_case "durations"
        [ choose [ int; const max_ms; const (max_ms + 1); const 0 ] ]
        test_durations;
      test_case "screen tokens"
        [ choose [ bytes; const {|\Q/\E|}; const {|\Qa\|} ] ]
        test_screen;
      test_case "raw lines"
        [
          choose
            [ bytes; const "Set Width 300"; const "Require x"; const " Hide" ];
        ]
        test_raw;
      test_case "hidden" [ list bytes; int ] test_hidden;
      test_case "sizes" [ int16; int16; int8 ] test_sizes;
      test_case "outputs"
        [
          bytes;
          choose
            [ const ""; const ".gif"; const ".png"; const "/"; const ".gfi" ];
        ]
        test_output;
      test_case "pp_error" [ bytes; int ] test_pp_error;
      test_case "keys" [ char ] test_keys;
      test_case "settings" [ int; bytes ] test_settings;
      test_case "order" [ bytes; bytes ] test_order;
    ] )
