(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Alcobar

let outputs = [ "demo.gif" ]
let max_ms = 9_223_372_036_854

(* VHS records no frame without a visible action, so each tape ends with
   [last], which [tape] does not print. *)
let last = Vhs.sleep (Ms 100)
let last_line = "Sleep 100ms\n"

let tape ?settings ?(outputs = outputs) actions =
  Vhs.tape ?settings ~outputs (actions @ [ last ])
  |> Result.map (fun t ->
      let s = Vhs.to_string t in
      String.sub s 0 (String.length s - String.length last_line))

(* The printed lines of the actions, without the [Output] line. *)
let lines ?settings actions =
  match tape ?settings actions with
  | Error _ -> None
  | Ok s -> (
      match String.split_on_char '\n' s with
      | _output :: rest -> Some (List.filter (( <> ) "") rest)
      | [] -> fail "empty tape")

let accepted ?settings actions = Option.is_some (lines ?settings actions)

(* C0 other than [except], DEL and C1, decoded from UTF-8 where valid. *)
let has_control ?(except = "") s =
  let control d =
    Uchar.utf_decode_is_valid d
    &&
    let u = Uchar.to_int (Uchar.utf_decode_uchar d) in
    (u < 0x20 && not (String.contains except (Char.chr u)))
    || (u >= 0x7f && u <= 0x9f)
  in
  let rec go i =
    i < String.length s
    &&
    let d = String.get_utf_8_uchar s i in
    control d || go (i + Uchar.utf_decode_length d)
  in
  go 0

(* [decode s] is [s] as the stdlib decodes it: each character, U+FFFD for a
   maximal invalid subpart, with its bytes. *)
let decode s =
  let rec go i acc =
    if i >= String.length s then List.rev acc
    else
      let d = String.get_utf_8_uchar s i in
      let n = Uchar.utf_decode_length d in
      go (i + n)
        ((Uchar.to_int (Uchar.utf_decode_uchar d), String.sub s i n) :: acc)
  in
  go 0 []

(* [Tape_syntax.has_control] finds a C0 control but [except], DEL or a C1
   control where the stdlib decodes one. *)
let test_control_decoding s except =
  let control (u, _) =
    (u < 0x20 && not (String.contains except (Char.chr u)))
    || (u >= 0x7f && u <= 0x9f)
  in
  check_eq ~pp:Fmt.bool
    (List.exists control (decode s))
    (Tape_syntax.has_control ~except s)

(* [Screen.trim_end] drops the JavaScript white space that ends the
   characters the stdlib decodes. *)
let test_trim_end_decoding s =
  let js_space u =
    List.mem u
      [
        0x09;
        0x0a;
        0x0b;
        0x0c;
        0x0d;
        0x20;
        0xa0;
        0x1680;
        0x2028;
        0x2029;
        0x202f;
        0x205f;
        0x3000;
        0xfeff;
      ]
    || (u >= 0x2000 && u <= 0x200a)
  in
  let rec drop = function
    | (u, _) :: rest when js_space u -> drop rest
    | l -> l
  in
  let expected =
    String.concat "" (List.rev_map snd (drop (List.rev (decode s))))
  in
  check_eq ~pp:Fmt.Dump.string expected (Screen.trim_end s)

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
    String.is_valid_utf_8 s && not (has_control ~except:"\t\n" s)
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
          if has_control ~except:"\n" printed then
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
  (* The GIF output's encoder takes at most 65535 pixels a side, and ffmpeg
     a frame below (8 w + 1024) (h + 128) = 2{^31}. *)
  let encodes =
    w <= 65535 && h <= 65535
    && Int64.(
         compare (mul (of_int ((8 * w) + 1024)) (of_int (h + 128))) 2147483647L)
       < 0
  in
  let ok = p >= 0 && fits w && fits h && encodes in
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
    && (not (has_control path))
    && not quotes
  in
  check (Result.is_ok (tape ~outputs:[ path ] []) = ok)

(* [pp_error] never raises nor prints a control character but newlines;
   [Io] prints a printable message without its format characters, and [Failed] at most ten lines of
   output under one header line. *)
let test_pp_error s n =
  let pp e = Fmt.str "%a" Vhs.pp_error e in
  List.iter
    (fun e ->
      let printed = pp e in
      if has_control printed then failf "control character in %S" printed)
    [ Vhs.Io s; Executable_not_found s ];
  if String.is_valid_utf_8 s && not (has_control s) then
    check (pp (Vhs.Io s) = Unicode.strip_format s);
  List.iter
    (fun status ->
      let printed = pp (Failed { vhs = "vhs"; status; output = s }) in
      if has_control ~except:"\n" printed then
        failf "control character in %S" printed;
      let lines = String.split_on_char '\n' printed in
      check (List.length lines <= 11))
    [ `Exited n; `Signaled n ]

(* The unified API's native errors have the same printable, one-line form. *)
let test_unified_errors message last =
  List.iter
    (fun error ->
      let output = Fmt.str "%a" Vhs.pp_error error in
      check (String.is_valid_utf_8 output);
      if has_control output then failf "non-printable error: %S" output)
    Vhs.
      [
        Invalid message;
        Unsupported message;
        Timeout { wait = message; line = 1; last };
        Exited { status = `Exited 1; wait = message; line = 1; last };
      ]

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
      || String.exists (fun c -> c = '\'' || c = '\\') font
      || has_control font)
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
      match Vhs.tape ~settings ~outputs:[ "demo.gif" ] (actions @ [ last ]) with
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
  match Vhs.tape ~outputs:[ "demo.gif" ] [ Vhs.raw line; last ] with
  | Error _ -> ()
  | Ok t ->
      if not (vhs_validates t) then
        failf "accepted raw line rejected by VHS: %S" line

let test_padding_bound padding =
  let settings = Vhs.[ Padding padding; Width 1200; Height 600 ] in
  let expected = padding >= 0 && padding <= 300 in
  check (Result.is_ok (Vhs.tape ~settings ~outputs [ last ]) = expected)

(* With the default frame and padding, a nonnegative margin fits iff it
   is at most 240; no multiplication of a generated margin is needed. *)
let test_raw_margin_bound margin =
  let margin = margin land max_int in
  let line = Fmt.str "Set Margin %d" margin in
  let result = Vhs.tape ~outputs [ Vhs.raw line; last ] in
  if Result.is_ok result <> (margin <= 240) then
    failf "wrong frame bounds for %S" line

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
    | Error (Vhs.Invalid message) ->
        if not (String.starts_with ~prefix message) then
          failf "error does not identify %S: %s" bad message
    | Error e -> failf "not Invalid: %a" Vhs.pp_error e
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

let render_tape () = Result.get_ok (Vhs.tape ~outputs:[ "demo.gif" ] [ last ])

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
    Vhs.render ~vhs:executable
      ~clock:(Eio.Stdenv.mono_clock env)
      ~proc ~cwd:(Eio.Stdenv.cwd env) (render_tape ())
  with
  | Ok () -> ()
  | Error e -> failf "ambient environment enables upload: %a" Vhs.pp_error e

(* All documented ambient overrides are removed, while an unrelated
   application variable survives byte for byte. *)
let test_environment_isolation n =
  with_dir @@ fun env path ->
  let executable = path "recorder" in
  let value = string_of_int n in
  save env executable
    (Fmt.str
       "#!/bin/sh\n\
        cat > /dev/null\n\
        [ -z \
        \"${VHS_PUBLISH+x}${rod+x}${PS1+x}${PROMPT+x}${PROMPT_COMMAND+x}\" ] \
        && [ \"$VHS_TEST_KEEP\" = '%s' ]\n"
       value);
  let base =
    (Eio.Stdenv.process_mgr env
      :> [ `Generic ] Eio.Process.mgr_ty Eio.Resource.t)
  in
  let cleared = [ "VHS_PUBLISH"; "rod"; "PS1"; "PROMPT"; "PROMPT_COMMAND" ] in
  let inherited =
    Eio.Process.environment base
    |> Array.to_list
    |> List.filter (fun entry ->
        not
          (List.exists
             (fun key -> String.starts_with ~prefix:(key ^ "=") entry)
             ("VHS_TEST_KEEP" :: cleared)))
  in
  let inherited =
    Array.of_list
      (("VHS_TEST_KEEP=" ^ value)
       :: List.map (fun key -> key ^ "=" ^ value) cleared
      @ inherited)
  in
  let proc =
    Eio.Resource.T
      (Inherited.{ base; inherited }, Eio.Process.Pi.mgr (module Inherited))
  in
  match
    Vhs.render ~vhs:executable
      ~clock:(Eio.Stdenv.mono_clock env)
      ~proc ~cwd:(Eio.Stdenv.cwd env) (render_tape ())
  with
  | Ok () -> ()
  | Error e -> failf "environment contract: %a" Vhs.pp_error e

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
                 ~clock:(Eio.Stdenv.mono_clock env)
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

(* A tape whose shell writes its pid and that of a nested shell, which
   loops, then waits for what never shows. *)
let nested_shells =
  lazy
    (Result.get_ok
       (Vhs.tape ~settings:[ Typing_speed (Ms 1) ] ~outputs:[ "image.svg" ]
          Vhs.
            [
              type'
                "echo $$ > shell.pid; bash --norc --noprofile -c 'echo $$ > \
                 child.pid; while :; do sleep 0.1; done'\n";
              wait ~timeout:(Ms 10000) (Line "NEVER_READY");
            ]))

(* Fails if the shells of [nested_shells] are alive, after killing them. *)
let check_reaped cwd =
  let pids =
    List.map
      (fun name ->
        Eio.Path.load Eio.Path.(cwd / name) |> String.trim |> int_of_string)
      [ "shell.pid"; "child.pid" ]
  in
  let alive pid =
    try
      Unix.kill pid 0;
      true
    with Unix.Unix_error (Unix.ESRCH, _, _) -> false
  in
  let left = List.filter alive pids in
  List.iter
    (fun pid -> try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ())
    left;
  if left <> [] then
    failf "recording left shells alive: %a" Fmt.(Dump.list int) left

(* Losing a recording's switch also stops a command's nested shell. *)
let test_record_reaps delay callback_raises =
  with_dir @@ fun env path ->
  let clock = Eio.Stdenv.clock env in
  let cwd =
    Eio.Path.(Eio.Stdenv.fs env / Filename.dirname (path "shell.pid"))
  in
  let tape = Lazy.force nested_shells in
  let ready () =
    Sys.file_exists (path "child.pid")
    && String.trim (Eio.Path.load Eio.Path.(cwd / "child.pid")) <> ""
  in
  let cancel () =
    while not (ready ()) do
      Eio.Time.sleep clock 0.001
    done;
    Eio.Time.sleep clock (float_of_int (delay land 15) /. 1000.);
    if callback_raises then raise Exit;
    `Cancelled
  in
  let result =
    try
      Eio.Time.with_timeout_exn clock 5. (fun () ->
          Eio.Fiber.first
            (fun () ->
              `Recorded
                (Vhs.record
                   ~clock:(Eio.Stdenv.mono_clock env)
                   ~proc:(Eio.Stdenv.process_mgr env)
                   ~cwd tape))
            cancel)
    with Exit when callback_raises -> `Cancelled
  in
  (match result with
  | `Cancelled -> ()
  | `Recorded (Ok _) -> fail "recording ended before cancellation"
  | `Recorded (Error e) -> failf "%a" Vhs.pp_error e);
  check_reaped cwd

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

let contains ~sub s =
  let n = String.length sub in
  let rec go i =
    i + n <= String.length s && (String.sub s i n = sub || go (i + 1))
  in
  go 0

(* The exit status of vhs-cram on [args] and what it printed. cmdliner
   styles its messages unless [TERM] is [dumb]. *)
let run_cli env args =
  let cli =
    Option.value ~default:"vhs-cram" (Sys.getenv_opt "VHS_CRAM_TEST_EXE")
  in
  let output = Buffer.create 256 in
  let sink = Eio.Flow.buffer_sink output in
  let proc = Eio.Stdenv.process_mgr env in
  let env =
    Eio.Process.environment proc
    |> Eio.Process.Env.override [ ("TERM", Some "dumb") ]
  in
  let status =
    Eio.Switch.run @@ fun sw ->
    Eio.Process.spawn ~sw ~env ~stdout:sink ~stderr:sink proc (cli :: args)
    |> Eio.Process.await
  in
  (status, Buffer.contents output)

(* [output] says why vhs-cram refused [--wait-timeout=seconds]. *)
let check_refused seconds output =
  check (List.mem (string_of_int seconds) (String.split_on_char '"' output));
  (* cmdliner wraps the message. *)
  let words =
    String.split_on_char '\n' output
    |> List.concat_map (String.split_on_char ' ')
    |> List.filter (( <> ) "")
    |> String.concat " "
  in
  check
    (contains
       ~sub:
         (if seconds > 0 then
            "is too large: a wait lasts at most 9223372036 seconds"
          else "is not a positive number of seconds")
       words);
  check
    (List.exists
       (String.starts_with ~prefix:"vhs-cram: option '--wait-timeout':")
       (String.split_on_char '\n' output))

(* Command-line seconds must fit Go's duration and be strictly positive. *)
let test_cli_wait_timeout seconds =
  with_dir @@ fun env path ->
  save env (path "input.t") "  $ true\n";
  let status, output =
    run_cli env
      [
        "--print";
        "--output=demo.svg";
        "--wait-timeout=" ^ string_of_int seconds;
        path "input.t";
      ]
  in
  let valid = seconds > 0 && seconds <= 9_223_372_036 in
  let expected = if valid then 0 else 124 in
  (match status with
  | `Exited code when code = expected -> ()
  | status ->
      failf "timeout %d: expected %d, got %a: %s" seconds expected
        Eio.Process.pp_status status output);
  if valid then
    check
      (List.mem
         (Fmt.str "Set WaitTimeout %ds" seconds)
         (String.split_on_char '\n' output))
  else check_refused seconds output

let record_cram ?(setup = []) ?(prefix = []) env path src =
  let output = path "order.txt" in
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
         (prefix @ Vhs_cram.Cram.actions ~setup cram @ [ Vhs.sleep (Ms 3500) ]))
  in
  let result =
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 40. (fun () ->
        Vhs.render ?vhs:oracle
          ~clock:(Eio.Stdenv.mono_clock env)
          ~proc:(Eio.Stdenv.process_mgr env)
          ~cwd:(Eio.Stdenv.cwd env) t)
  in
  (match result with Ok () -> () | Error e -> failf "%a" Vhs.pp_error e);
  Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / output)

let check_output_order decoy =
  with_dir @@ fun env path ->
  let src =
    Fmt.str "  $ printf '%s'; sleep 3; echo FIRST_DONE\n  $ echo SECOND_DONE\n"
      decoy
  in
  let screen = record_cram env path src in
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

let test_output_order n = Fmt.kstr check_output_order "progress-%d>" (n land 255)

(* U+200B is ordinary Unicode output, even though the recorder uses it as
   the suffix of its private prompt. *)
let test_invisible_output_order n =
  let decoy = Buffer.create 32 in
  Fmt.kstr (Buffer.add_string decoy) "progress-%d>" (n land 255);
  Buffer.add_utf_8_uchar decoy (Uchar.of_int 0x200b);
  check_output_order (Buffer.contents decoy)

(* A setup's prompt hook must not undo the recorder's readiness prompt. *)
let test_setup_prompt_hook n =
  with_dir @@ fun env path ->
  let script = path "setup.sh" in
  save env script (Fmt.str "PROMPT_COMMAND='PS1=custom-%d'\n" (n land 255));
  let screen =
    record_cram ~setup:[ script ]
      ~prefix:[ Vhs.raw "Set WaitTimeout 3s" ]
      env path "  $ echo READY\n"
  in
  if not (String.split_on_char '\n' screen |> List.mem "READY") then
    fail "setup prevented the command from running"

(* A JSON theme is accepted or refused, never an exception, and a tape that
   accepts it plays it or names why not. *)
let test_json_theme s =
  let theme = Vhs.Theme ("{" ^ s) in
  match Vhs.tape ~settings:[ theme ] ~outputs:[ "a.gif" ] [ Vhs.type' "x" ] with
  | Error _ -> ()
  | Ok _ -> (
      match Vhs.svg ~settings:[ theme ] [] with
      | Ok _ | Error (Unsupported _) -> ()
      | Error e -> failf "%a" Vhs.pp_error e)

(* Unknown JSON values are skipped by Go, but still count towards the
   maximum nesting depth of 10,000, including the theme's root object. *)
let test_json_depth n =
  let depth = 9_997 + (n mod 7) in
  let theme =
    "{\"extra\":" ^ String.make depth '[' ^ "0" ^ String.make depth ']' ^ "}"
  in
  let expected = depth < 10_000 in
  let accepted =
    match Vhs.tape ~settings:[ Theme theme ] ~outputs:[ "a.txt" ] [ last ] with
    | Ok _ -> true
    | Error (Invalid _) -> false
    | Error e -> failf "%a" Vhs.pp_error e
  in
  if expected <> accepted then
    failf "JSON array depth %d: Go accepts=%b, tape accepts=%b" depth expected
      accepted

(* Check the shell's actual input bytes, independently of the tape parser
   and the emulator's rendering of combining marks and joined emoji. *)
let test_native_literal n =
  with_dir @@ fun env path ->
  let scripts =
    [|
      [ 0x0915; 0x093e ];
      [ 0x1f469; 0x200d; 0x1f4bb ];
      [ 0x65; 0x0301 ];
      [ 0x1100; 0x1161; 0x11a8 ];
      [ 0x2764; 0xfe0f ];
      [ 0x200b; 0x2060 ];
    |]
  in
  let b = Buffer.create 80 in
  Buffer.add_string b "quotes: \"'` $\\ ";
  for i = 0 to Array.length scripts - 1 do
    List.iter
      (fun u -> Buffer.add_utf_8_uchar b (Uchar.of_int u))
      scripts.((n + i) mod Array.length scripts);
    Buffer.add_char b ' '
  done;
  let payload = Buffer.contents b in
  let command = "printf '%s' " ^ Filename.quote payload ^ " > received\n" in
  let tape =
    match
      Vhs.tape
        ~settings:Vhs.[ Typing_speed (Ms 1) ]
        ~outputs:[ path "out.svg" ]
        Vhs.[ hidden [ type' command; wait Prompt ]; sleep (Ms 100) ]
    with
    | Ok tape -> tape
    | Error e -> failf "%a" Vhs.pp_error e
  in
  let cwd = Eio.Path.(Eio.Stdenv.fs env / Filename.dirname (path "received")) in
  (match
     Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
         Vhs.record
           ~clock:(Eio.Stdenv.mono_clock env)
           ~proc:(Eio.Stdenv.process_mgr env)
           ~cwd tape)
   with
  | Ok _ -> ()
  | Error e -> failf "%a" Vhs.pp_error e);
  check_eq ~pp:pp_string payload (Eio.Path.load Eio.Path.(cwd / "received"))

(* Zero typing delay cannot make Wait match the prompt that preceded the
   submitted command. A leading sleep ensures at least one real frame. *)
let test_zero_speed_prompt n =
  with_dir @@ fun env path ->
  let marker = Fmt.str "READY_%d" n in
  let settings =
    Vhs.[ Width 400; Height 200; Font_size 10; Padding 10; Typing_speed (Ms 0) ]
  in
  let tape =
    match
      Vhs.tape ~settings
        ~outputs:[ path "out.txt" ]
        Vhs.[ sleep (Ms 100); type' ("echo " ^ marker ^ "\n"); wait Prompt ]
    with
    | Ok t -> t
    | Error e -> failf "%a" Vhs.pp_error e
  in
  let cwd = Eio.Path.(Eio.Stdenv.fs env / Filename.dirname (path "out.txt")) in
  (match
     Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
         Vhs.record
           ~clock:(Eio.Stdenv.mono_clock env)
           ~proc:(Eio.Stdenv.process_mgr env)
           ~cwd tape)
   with
  | Ok () -> ()
  | Error e -> failf "%a" Vhs.pp_error e);
  let output = Eio.Path.load Eio.Path.(cwd / "out.txt") in
  if not (contains ~sub:("\n" ^ marker ^ "\n>") output) then
    failf "prompt wait returned without the output of echo %s" marker

(* Mixed outputs must be refused before spawning either engine or opening
   an earlier text output, including one whose parent directory is absent. *)
let test_output_preflight n =
  with_dir @@ fun env path ->
  let text = if n mod 2 = 0 then path "keep.txt" else path "absent/keep.txt" in
  let kinds = [| ".gif"; ".mp4"; ".webm"; ".png"; "/" |] in
  let movie = path ("movie" ^ kinds.(n mod Array.length kinds)) in
  let image = path "image.svg" in
  if n mod 2 = 0 then save env text "keep these bytes";
  let tape =
    match
      Vhs.tape ~outputs:[ text; movie; image ]
        Vhs.[ type' "touch played\n"; wait Prompt; sleep (Ms 100) ]
    with
    | Ok t -> t
    | Error e -> failf "%a" Vhs.pp_error e
  in
  let cwd = Eio.Path.(Eio.Stdenv.fs env / Filename.dirname image) in
  let clock = Eio.Stdenv.mono_clock env and proc = Eio.Stdenv.process_mgr env in
  let refused name item = function
    | Error (Vhs.Unsupported why) ->
        check (contains ~sub:name why);
        check (contains ~sub:item why)
    | Error e -> failf "%a" Vhs.pp_error e
    | Ok () -> fail "played incompatible outputs"
  in
  refused "Vhs.record" image
    (Vhs.render ~vhs:(path "missing-vhs") ~clock ~proc ~cwd tape);
  refused "Vhs.render" movie (Vhs.record ~clock ~proc ~cwd tape);
  check (not (Sys.file_exists (path "played")));
  check (not (Sys.file_exists image));
  if n mod 2 = 0 then
    check_eq ~pp:pp_string "keep these bytes"
      (Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / text))
  else check (not (Sys.file_exists (path "absent")))

let check_native_path env cwd marker (output, actual, rate) =
  if Sys.file_exists actual then Sys.remove actual;
  let settings =
    Vhs.
      [
        Width 400;
        Height 200;
        Font_size 10;
        Padding 10;
        Framerate rate;
        Typing_speed (Ms 0);
      ]
  in
  let tape =
    match
      Vhs.tape ~settings ~outputs:[ output ]
        Vhs.
          [
            type' ("echo " ^ marker ^ "\n");
            wait ~timeout:(Ms 5000) (Screen ("(?m)^" ^ marker ^ "$"));
            wait Prompt;
          ]
    with
    | Ok t -> t
    | Error e -> failf "%a" Vhs.pp_error e
  in
  (match
     Vhs.record
       ~clock:(Eio.Stdenv.mono_clock env)
       ~proc:(Eio.Stdenv.process_mgr env)
       ~cwd:(cwd :> Eio.Fs.dir_ty Eio.Path.t)
       tape
   with
  | Ok () -> ()
  | Error e -> failf "%a" Vhs.pp_error e);
  check (Sys.file_exists actual);
  check
    (contains
       ~sub:("\n" ^ marker ^ "\n")
       (Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / actual)))

(* A native recording writes the exact requested path even from a scoped
   cwd, with quoted Unicode names, at every positive frame-rate boundary. *)
let test_native_paths n =
  with_dir @@ fun env path ->
  let sub = path "cwd" in
  let nested = Filename.concat sub "nested" in
  Unix.mkdir sub 0o755;
  Unix.mkdir nested 0o755;
  let name = Fmt.str "out %d \u{03bb}'\".txt" n in
  let root_output = path name in
  let sub_output = Filename.concat sub name in
  let nested_output = Filename.concat nested name in
  Fun.protect ~finally:(fun () ->
      List.iter
        (fun p -> if Sys.file_exists p then Sys.remove p)
        [ sub_output; nested_output ];
      Unix.rmdir nested;
      Unix.rmdir sub)
  @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let cwd = Eio.Path.open_dir ~sw Eio.Path.(Eio.Stdenv.fs env / sub) in
  List.iter
    (check_native_path env cwd (Fmt.str "DONE_%d" n))
    [
      (name, sub_output, 1);
      ("../" ^ name, root_output, 100_001);
      (root_output, root_output, max_int);
      ("nested/" ^ name, nested_output, 50);
    ]

(* Properties that run [vhs] itself, so need [oracle]. *)
let with_vhs =
  [
    test_case "invisible output precedes next input" [ int8 ]
      test_invisible_output_order;
    test_case "setup prompt hooks preserve readiness" [ int8 ]
      test_setup_prompt_hook;
    test_case "command output precedes next input" [ int8 ] test_output_order;
    test_case "accepted tapes validate" [ bytes; int ] test_validates;
    test_case "accepted raw lines validate"
      [ list1 (pair uint8 bool) ]
      test_raw_validates;
  ]

(* Properties that run stand-ins for vhs and vhs-cram. *)
let with_processes =
  [
    test_case "native output paths and positive frame-rate boundaries" [ uint8 ]
      test_native_paths;
    test_case "mixed outputs are refused before playing or writing" [ uint8 ]
      test_output_preflight;
    test_case "zero-delay typing waits for the submitted command" [ uint8 ]
      test_zero_speed_prompt;
    test_case "native typing preserves mixed scripts and shell quotes" [ uint8 ]
      test_native_literal;
    test_case "CLI wait timeout bounds and diagnostics"
      [
        choose
          [
            int;
            const 0;
            const (-1);
            const 1;
            const 9_223_372_036;
            const 9_223_372_037;
            const 99_999_999_999;
          ];
      ]
      test_cli_wait_timeout;
    test_case "record cancellation and callback exceptions reap shells"
      [ int8; bool ] test_record_reaps;
    test_case "recording environment isolation" [ int ]
      test_environment_isolation;
    test_case "documented executable exit codes" [ int16 ] test_exit_codes;
    test_case "cancelled render reaps the child" [ int8 ] test_cancel_reaps;
    test_case "never publish implicitly"
      [ choose [ const "dumb"; const "xterm-256color"; const "" ] ]
      test_no_publication;
  ]

(* Properties of the frame and of the values [tape] accepts. *)
let bounds =
  [
    test_case "raw margin fits the frame"
      [ choose [ int; const max_int; const (max_int - 59); const 240 ] ]
      test_raw_margin_bound;
    test_case "errors identify the invalid item" [ int ] test_error_item;
    test_case "typed text cannot inject commands"
      [ choose [ bytes; const ""; const "λ中文👩🏽‍💻é"; const "\000"; const "\xff" ] ]
      test_no_injection;
    test_case "padding fits the frame"
      [ choose [ int; const max_int; const ((max_int / 2) + 1); const 300 ] ]
      test_padding_bound;
    test_case "type is exact"
      [ choose [ bytes; const "\xff"; const {|a"b'c`d|} ] ]
      test_type;
  ]

(* Properties of the tapes [tape] makes. *)
(* The library's UTF-8 readers against the stdlib's decoding. *)
let decoding =
  [
    test_case "control characters as the stdlib decodes them"
      [ bytes; choose [ const ""; const "\t\n"; const "\t\r" ] ]
      test_control_decoding;
    test_case "trailing spaces as the stdlib decodes them"
      [
        choose
          [
            bytes;
            map [ bytes; bytes ] (fun a b -> a ^ "\xe3\x80\x80\xc2" ^ b ^ " ");
          ];
      ]
      test_trim_end_decoding;
  ]

let tapes =
  [
    test_case "tape is total" [ bytes; bytes; int ] test_total;
    test_case "JSON themes obey Go nesting limits" [ uint8 ] test_json_depth;
    test_case "JSON themes"
      [
        choose
          [
            bytes;
            const {|"background": "#123"}|};
            const {|"red": "\ud83d\ude00", "RED": null}|};
          ];
      ]
      test_json_theme;
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
        choose [ const ""; const ".gif"; const ".png"; const "/"; const ".gfi" ];
      ]
      test_output;
    test_case "pp_error" [ bytes; int ] test_pp_error;
    test_case "unified errors are printable single lines" [ bytes; bytes ]
      test_unified_errors;
    test_case "keys" [ char ] test_keys;
    test_case "settings" [ int; bytes ] test_settings;
    test_case "order" [ bytes; bytes ] test_order;
  ]

let suite =
  ( "vhs",
    (if Option.is_some oracle then with_vhs else [])
    @ with_processes @ bounds @ decoding @ tapes )
