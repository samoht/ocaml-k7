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
        wait (Line s);
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
  (* [2 * p] overflows for large [p]; compare in a wider type. *)
  let fits size = Int64.(compare (of_int size) (mul 2L (of_int p))) >= 0 in
  let ok = p >= 0 && fits w && fits h in
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

(* The vhs executable of the properties that run it, from [VHS_ORACLE]:
   the oracle alias sets it, runtest does not. *)
let oracle = Sys.getenv_opt "VHS_ORACLE"

let vhs_validates t =
  let path = Filename.temp_file "vhs-property" ".tape" in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      Out_channel.with_open_bin path (fun oc ->
          output_string oc (Vhs.to_string t));
      Sys.command
        (Filename.quote_command (Option.get oracle) [ "validate"; path ]
           ~stdout:"/dev/null" ~stderr:"/dev/null")
      = 0)

(* Check against the real parser, rather than a second tape lexer. *)
let test_validates text n =
  let open Vhs in
  let candidates =
    [
      ([], [ type' text ]);
      ([], [ wait (Screen text) ]);
      ([], [ wait ~timeout:(Ms (1 + (n land 1023))) (Line text) ]);
      ([], [ raw text ]);
      ([], [ Fmt.kstr raw "Sleep -%dms" (1 + (n land 255)) ]);
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
          if not (vhs_validates t) then
            failf "accepted tape rejected by VHS: %s" (Vhs.to_string t))
    candidates

(* Raw lines from VHS's own vocabulary reach the parser far more often than
   arbitrary bytes do. *)
let fragments =
  [ "Type"; "Sleep"; "Wait"; "Wait+Line"; "Wait+Screen"; "Set"; "Output";
    "Require"; "Env"; "Screenshot"; "Copy"; "Paste"; "Ctrl"; "Alt"; "Shift";
    "Enter"; "Tab"; "Hide"; "#"; "\"a\""; "'b c'"; "`d`"; "x.gif"; "out/";
    "s.png"; "Line"; "Screen"; "/re/"; "/[/"; {|/a\/b/|}; {|/a\\/|};
    {|{"k": 1}|}; "{"; "1"; "0"; "1.5"; "1.2.3"; ".5"; "ms"; "s"; "m"; "@";
    "+"; "%"; "-"; "^"; "["; "]"; "true"; "Margin"; "MarginFill"; "#ff00ff";
    "WindowBar"; "Rings"; "CursorBlink"; "TypingSpeed"; "WaitTimeout";
    "WaitPattern"; "LoopOffset"; "LetterSpacing"; "A"; "x" ]
[@@ocamlformat "disable"]

let test_raw_validates words =
  let line =
    List.map
      (fun (i, glue) ->
        List.nth fragments (i mod List.length fragments)
        ^ if glue then "" else " ")
      words
    |> String.concat ""
  in
  match Vhs.tape ~outputs:[ "demo.gif" ] [ Vhs.raw line ] with
  | Error _ -> ()
  | Ok t ->
      if not (vhs_validates t) then
        failf "accepted raw line rejected by VHS: %S" line

let test_padding_bound padding =
  let settings = Vhs.[ Padding padding; Width 1200; Height 600 ] in
  let expected = padding >= 0 && padding <= 300 in
  check (Result.is_ok (Vhs.tape ~settings ~outputs []) = expected)

let test_no_injection text =
  let hostile = text ^ "\"'`\\\nOutput injected.gif\nHide\nSet Shell bash\n" in
  (* Decoding must recover only typed text, tabs and Enter, never a new
     setting, output, visibility change or executable tape command. *)
  test_type hostile

let test_error_item n =
  let bad = Fmt.str "item-%d\001" (n land 65535) in
  let check_error prefix result =
    match result with
    | Ok _ -> failf "accepted invalid item %S" bad
    | Error message ->
        if not (String.starts_with ~prefix message) then
          failf "error does not identify %S: %s" bad message
  in
  check_error
    (Fmt.str "Output %S:" (bad ^ ".gif"))
    (Vhs.tape ~outputs:[ "good.gif"; bad ^ ".gif" ] []);
  check_error (Fmt.str "Type %S:" bad)
    (Vhs.tape ~outputs [ Vhs.type' "good"; Vhs.hidden [ Vhs.type' bad ] ]);
  check_error
    (Fmt.str "Set FontFamily %S:" bad)
    (Vhs.tape ~settings:[ Vhs.Font_family bad ] ~outputs []);
  check_error
    (Fmt.str "Wait+Screen %S:" bad)
    (Vhs.tape ~outputs [ Vhs.wait (Screen bad) ])

let with_dir fn =
  Eio_main.run @@ fun env ->
  let dir = Filename.temp_dir "vhs-property" "" in
  Fun.protect
    ~finally:(fun () ->
      Array.iter
        (fun name -> Sys.remove (Filename.concat dir name))
        (Sys.readdir dir);
      Unix.rmdir dir)
    (fun () -> fn env (Filename.concat dir))

let save env path contents =
  Eio.Path.save ~create:(`Or_truncate 0o755)
    Eio.Path.(Eio.Stdenv.fs env / path)
    contents

let render_tape () = Result.get_ok (Vhs.tape ~outputs:[ "demo.gif" ] [])

(* Supply a manager environment without mutating the test runner's process. *)
module Inherited = struct
  type tag = [ `Generic ]

  type t = {
    base : [ `Generic ] Eio.Process.mgr_ty Eio.Resource.t;
    inherited : string array;
  }

  let environment t = t.inherited

  let getenv_opt t name =
    let prefix = name ^ "=" in
    Array.find_map
      (fun entry ->
        if String.starts_with ~prefix entry then
          Some
            (String.sub entry (String.length prefix)
               (String.length entry - String.length prefix))
        else None)
      t.inherited

  let pipe t ~sw = Eio.Process.pipe ~sw t.base

  let spawn t ~sw ?cwd ?stdin ?stdout ?stderr ?env ?executable args =
    let env = Option.value ~default:t.inherited env in
    Eio.Process.spawn ~sw ?cwd ?stdin ?stdout ?stderr ~env ?executable t.base
      args
end

let test_no_publication term =
  with_dir @@ fun env path ->
  let executable = path "recorder" in
  save env executable
    "#!/bin/sh\ncat > /dev/null\n[ \"$VHS_PUBLISH\" != true ]\n";
  let base =
    (Eio.Stdenv.process_mgr env
      :> [ `Generic ] Eio.Process.mgr_ty Eio.Resource.t)
  in
  let inherited =
    Eio.Process.environment base
    |> Array.to_list
    |> List.filter (fun s ->
        not
          (String.starts_with ~prefix:"VHS_PUBLISH=" s
          || String.starts_with ~prefix:"TERM=" s))
  in
  let inherited =
    Array.of_list (("TERM=" ^ term) :: "VHS_PUBLISH=true" :: inherited)
  in
  let proc =
    Eio.Resource.T
      (Inherited.{ base; inherited }, Eio.Process.Pi.mgr (module Inherited))
  in
  match
    Vhs.render ~vhs:executable ~proc ~cwd:(Eio.Stdenv.cwd env) (render_tape ())
  with
  | Ok () -> ()
  | Error e -> failf "ambient environment enables upload: %a" Vhs.pp_error e

let test_cancel_reaps delay =
  with_dir @@ fun env path ->
  let executable = path "recorder" in
  save env executable
    "#!/bin/sh\n\
     trap 'exit 0' TERM\n\
     cat > /dev/null\n\
     echo $$ > \"$0.pid\"\n\
     while :; do sleep 0.01; done\n";
  let clock = Eio.Stdenv.clock env in
  let result =
    Eio.Time.with_timeout_exn clock 5. (fun () ->
        Eio.Fiber.first
          (fun () ->
            `Rendered
              (Vhs.render ~vhs:executable
                 ~proc:(Eio.Stdenv.process_mgr env)
                 ~cwd:(Eio.Stdenv.cwd env) (render_tape ())))
          (fun () ->
            while not (Sys.file_exists (executable ^ ".pid")) do
              Eio.Time.sleep clock 0.001
            done;
            Eio.Time.sleep clock (float_of_int (delay land 15) /. 1000.);
            `Cancelled))
  in
  check (result = `Cancelled);
  let pid =
    Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / (executable ^ ".pid"))
    |> String.trim |> int_of_string
  in
  let alive =
    try
      Unix.kill pid 0;
      true
    with Unix.Unix_error (Unix.ESRCH, _, _) -> false
  in
  if alive then failf "cancelled render left process %d alive" pid

let test_exit_codes n =
  with_dir @@ fun env path ->
  let executable = path "recorder" in
  let status = n land 255 in
  let signaled = n land 256 <> 0 in
  save env executable
    ("#!/bin/sh\ncat > /dev/null\n"
    ^ if signaled then "kill -TERM $$\n" else Fmt.str "exit %d\n" status);
  save env (path "input.t") "  $ echo hello\n";
  save env (path "empty.t") "prose only\n";
  save env (path "bad.t") "  $ echo \001\n";
  let cli =
    Option.value ~default:"vhs-cram" (Sys.getenv_opt "VHS_CRAM_TEST_EXE")
  in
  let eval expected args =
    let buffer = Buffer.create 256 in
    let sink = Eio.Flow.buffer_sink buffer in
    let received =
      Eio.Switch.run @@ fun sw ->
      let child =
        Eio.Process.spawn ~sw ~stdout:sink ~stderr:sink
          (Eio.Stdenv.process_mgr env)
          (cli :: args)
      in
      Eio.Process.await child
    in
    if
      not
        (match received with `Exited n -> n = expected | `Signaled _ -> false)
    then
      failf "%s: expected %d, got %a: %s" (String.concat " " args) expected
        Eio.Process.pp_status received (Buffer.contents buffer)
  in
  let flags = [ "--output"; path "demo.gif"; "--vhs"; executable ] in
  eval
    (if status = 0 && not signaled then 0 else 123)
    (flags @ [ path "input.t" ]);
  eval 123 (flags @ [ path "empty.t" ]);
  eval 123 (flags @ [ path "bad.t" ]);
  eval 124 (flags @ [ path "missing.t" ]);
  eval 124 (("--setup" :: path "missing.sh" :: flags) @ [ path "input.t" ]);
  eval 0
    ("--print" :: "--vhs" :: path "missing-vhs"
    :: [ "--output"; path "demo.gif"; path "input.t" ]);
  eval 123
    [ "--vhs"; path "missing-vhs"; "--output"; path "demo.gif"; path "input.t" ];
  eval 123 [ "--print"; "--output"; "no-extension"; path "input.t" ];
  eval 0 [ "--help=plain" ];
  eval 124 [ "--unknown-adversarial-flag" ]

let test_output_order n =
  with_dir @@ fun env path ->
  let output = path "order.txt" in
  let decoy = Fmt.str "progress-%d>" (n land 255) in
  let src =
    Fmt.str "  $ printf '%s'; sleep 3; echo FIRST_DONE\n  $ echo SECOND_DONE\n"
      decoy
  in
  let cram = Result.get_ok (Vhs_cram.Cram.parse src) in
  let t =
    Result.get_ok
      (Vhs.tape
         ~settings:
           Vhs.
             [
               Shell Bash;
               Width 480;
               Height 240;
               Padding 0;
               Font_size 12;
               Framerate 5;
               Typing_speed (Ms 5);
             ]
         ~outputs:[ output ]
         (Vhs_cram.Cram.actions ~setup:[] cram @ [ Vhs.sleep (Ms 3500) ]))
  in
  let result =
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 40. (fun () ->
        Vhs.render ?vhs:oracle
          ~proc:(Eio.Stdenv.process_mgr env)
          ~cwd:(Eio.Stdenv.cwd env) t)
  in
  (match result with Ok () -> () | Error e -> failf "%a" Vhs.pp_error e);
  let screen = Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / output) in
  let position sub =
    let rec scan i =
      if i + String.length sub > String.length screen then None
      else if String.sub screen i (String.length sub) = sub then Some i
      else scan (i + 1)
    in
    scan 0
  in
  (* The command line holds [FIRST_DONE] too; its output follows [decoy]. *)
  match (position (decoy ^ "FIRST_DONE"), position "echo SECOND_DONE") with
  | Some first, Some second when first < second -> ()
  | _ -> failf "next command was typed before the first output for %S" src

(* Properties that run [vhs] itself, so need [oracle]. *)
let with_vhs =
  [
    test_case "command output precedes next input" [ int8 ] test_output_order;
    test_case "accepted tapes validate" [ bytes; int ] test_validates;
    test_case "accepted raw lines validate"
      [ list1 (pair uint8 bool) ]
      test_raw_validates;
  ]

let suite =
  ( "vhs",
    (if Option.is_some oracle then with_vhs else [])
    @ [
        test_case "documented executable exit codes" [ int16 ] test_exit_codes;
        test_case "cancelled render reaps the child" [ int8 ] test_cancel_reaps;
        test_case "never publish implicitly"
          [ choose [ const "dumb"; const "xterm-256color"; const "" ] ]
          test_no_publication;
        test_case "errors identify the invalid item" [ int ] test_error_item;
        test_case "typed text cannot inject commands"
          [
            choose
              [ bytes; const ""; const "λ中文👩🏽‍💻é"; const "\000"; const "\xff" ];
          ]
          test_no_injection;
        test_case "padding fits the frame"
          [
            choose [ int; const max_int; const ((max_int / 2) + 1); const 300 ];
          ]
          test_padding_bound;
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
