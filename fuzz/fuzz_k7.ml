(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Alcobar

let outputs = [ "demo.gif" ]
let max_ms = 9_223_372_036_854

(* VHS records no frame without a visible action. *)
let last = K7.sleep (Ms 100)
let last_line = "Sleep 100ms\n"

let tape ?settings ?(outputs = outputs) actions =
  K7.tape ?settings ~outputs (actions @ [ last ])
  |> Result.map (fun t ->
      let s = K7.to_string t in
      String.sub s 0 (String.length s - String.length last_line))

let lines ?settings actions =
  match tape ?settings actions with
  | Error _ -> None
  | Ok s -> (
      match String.split_on_char '\n' s with
      | _output :: rest -> Some (List.filter (( <> ) "") rest)
      | [] -> fail "empty tape")

let accepted ?settings actions = Option.is_some (lines ?settings actions)

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

let test_control_decoding s except =
  let control (u, _) =
    (u < 0x20 && not (String.contains except (Char.chr u)))
    || (u >= 0x7f && u <= 0x9f)
  in
  check_eq ~pp:Fmt.bool
    (List.exists control (decode s))
    (Tape_syntax.has_control ~except s)

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

let test_strip_format s =
  if String.is_valid_utf_8 s then (
    let t = Unicode.strip_format s in
    check_eq ~pp:Fmt.Dump.string t (Unicode.strip_format t);
    let selector = Unicode.is_selector in
    let rec scan i prev =
      if i < String.length t then (
        let d = String.get_utf_8_uchar t i in
        let u = Uchar.to_int (Uchar.utf_decode_uchar d) in
        if u = 0x200b || (u >= 0x2060 && u <= 0x2064) || u = 0xfeff then
          failf "invisible U+%04X in %S" u t;
        (if selector u then
           match prev with
           | Some p
             when p > 0x20
                  && (not (p >= 0x7f && p <= 0x9f))
                  && (not (selector p))
                  && p <> 0x200c && p <> 0x200d ->
               ()
           | _ -> failf "stray selector U+%04X in %S" u t);
        scan (i + Uchar.utf_decode_length d) (Some u))
    in
    scan 0 None)

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

let typed lines =
  String.concat ""
    (List.map
       (function "Tab" -> "\t" | "Enter" -> "\n" | line -> decode_type line)
       lines)

let test_type s =
  let typeable =
    String.is_valid_utf_8 s && not (has_control ~except:"\t\n" s)
  in
  match lines [ K7.type' s ] with
  | None -> if typeable then failf "rejected typeable %S" s
  | Some lines ->
      if not typeable then failf "accepted %S" s;
      let got = typed lines in
      if got <> s then failf "%S typed as %S" s got

let test_total s t n =
  let c = Char.chr (abs n mod 256) in
  let actions =
    K7.
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
    K7.
      [
        Font_size n;
        Font_family s;
        Width n;
        Height n;
        Theme t;
        Padding n;
        Typing_speed (Ms n);
        Framerate n;
        Wait_timeout (Ms n);
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
      ([], [ t ^ ".gif" ], List.map (fun a -> K7.hidden [ a ]) actions);
    ]

let check_reads t =
  let printed = K7.to_string t in
  match K7.of_string printed with
  | Ok t' ->
      let printed' = K7.to_string t' in
      if printed' <> printed then failf "%S read back as %S" printed printed'
  | Error e -> failf "%S not read back: %a" printed K7.pp_error e

let test_screenshot_alias n output =
  let path = Fmt.str "shot-%d.svg" n in
  let alias =
    String.concat "" (List.init (1 + (n mod 5)) (fun _ -> "./")) ^ path
  in
  let outputs, actions =
    if output then ([ path ], [ K7.raw ("Screenshot " ^ alias) ])
    else
      ( [ "animation.svg" ],
        [ K7.raw ("Screenshot " ^ path); K7.raw ("Screenshot " ^ alias) ] )
  in
  match K7.tape ~outputs (actions @ [ K7.sleep (Ms 100) ]) with
  | Error (Invalid _) -> ()
  | Error e -> failf "%a" K7.pp_error e
  | Ok _ -> failf "%S and %S name the same destination" path alias

let test_final_hidden_roundtrip n comment =
  let text = List.nth [ "secret"; "café"; "漢字"; "👍" ] (n mod 4) in
  let action = if comment then K7.raw ("# " ^ text) else K7.type' text in
  let actions = List.init (1 + (n mod 8)) (fun _ -> K7.hidden [ action ]) in
  match
    K7.tape ~outputs:[ "hidden.txt" ] K7.[ sleep (Ms 100); hidden actions ]
  with
  | Error e -> failf "%a" K7.pp_error e
  | Ok t -> check_reads t

let test_of_string s t n =
  (match K7.of_string s with Ok t -> check_reads t | Error _ -> ());
  let c = Char.chr (abs n mod 256) in
  let actions =
    K7.
      [
        raw t;
        type' s;
        key (Ctrl c);
        sleep (Ms n);
        wait (Line s);
        hidden [ raw s; type' t ];
      ]
  in
  let settings =
    K7.
      [
        Font_size n;
        Font_family s;
        Columns n;
        Theme t;
        Typing_speed (Ms n);
        Wait_timeout (Ms n);
      ]
  in
  List.iter
    (fun (settings, actions) ->
      match K7.tape ~settings ~outputs actions with
      | Ok t -> check_reads t
      | Error _ -> ())
    [ (settings, actions); ([], actions); (settings, [ K7.type' s ]) ]

let test_durations n =
  let in_range lo = n >= lo && n <= max_ms in
  (match lines [ K7.sleep (Ms n) ] with
  | Some [ l ] -> check (in_range 0 && l = Fmt.str "Sleep %dms" n)
  | Some _ -> fail "sleep printed several lines"
  | None -> check (not (in_range 0)));
  match lines [ K7.wait ~timeout:(Ms n) Prompt ] with
  | Some [ l ] -> check (in_range 1 && l = Fmt.str "Wait@%dms" n)
  | Some _ -> fail "wait printed several lines"
  | None -> check (not (in_range 1))

let test_screen re =
  match lines [ K7.wait (Screen re) ] with
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
      if not (accepted [ K7.wait (Screen body) ]) then
        failf "%S written as the invalid %S" re body
  | Some _ -> fail "wait printed several lines"

let test_raw line =
  (match lines [ K7.raw line ] with
  | Some [ l ] -> check (l = line)
  | Some [] -> check (line = "")
  | Some _ -> fail "raw printed several lines"
  | None -> ());
  (match lines [ K7.raw line ] with
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
  if late && accepted [ K7.key Enter; K7.raw line ] then
    failf "accepted %S after an action" line

let test_hidden texts depth =
  let actions = List.map K7.type' texts in
  let rec nest d a = if d <= 0 then a else nest (d - 1) [ K7.hidden a ] in
  match (lines actions, lines (nest (1 + (abs depth mod 4)) actions)) with
  | Some plain, Some nested ->
      if plain = [] then check (nested = [])
      else check (nested = ("Hide" :: plain) @ [ "Show" ])
  | None, None -> ()
  | _ -> fail "hidden changed acceptance"

let test_sizes w h p =
  (* [2 * p] overflows for large [p]; compare in a wider type. *)
  let fits size = Int64.(compare (of_int size) (mul 2L (of_int p))) >= 0 in
  (* GIF takes 65535 pixels a side; ffmpeg (8 w + 1024) (h + 128) < 2{^31}. *)
  let encodes =
    w <= 65535 && h <= 65535
    && Int64.(
         compare (mul (of_int ((8 * w) + 1024)) (of_int (h + 128))) 2147483647L)
       < 0
  in
  let ok = p >= 0 && fits w && fits h && encodes in
  let settings = K7.[ Padding p; Width w; Height h ] in
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

let test_pp_error s n =
  let pp e = Fmt.str "%a" K7.pp_error e in
  List.iter
    (fun e ->
      let printed = pp e in
      if has_control printed then failf "control character in %S" printed)
    [ K7.Io s; Executable_not_found s ];
  if String.is_valid_utf_8 s && not (has_control s) then
    check (pp (K7.Io s) = Unicode.strip_format s);
  List.iter
    (fun status ->
      let printed = pp (Vhs_failed { vhs = "vhs"; status; output = s }) in
      if has_control ~except:"\n" printed then
        failf "control character in %S" printed;
      let lines = String.split_on_char '\n' printed in
      check (List.length lines <= 11))
    [ `Exited n; `Signaled n ]

let test_unified_errors message last =
  List.iter
    (fun error ->
      let output = Fmt.str "%a" K7.pp_error error in
      check (String.is_valid_utf_8 output);
      if has_control output then failf "non-printable error: %S" output)
    K7.
      [
        Invalid message;
        Unsupported message;
        Timeout { wait = message; line = 1; file = None; last };
        Stuck { wait = message; line = 1; file = Some message; last };
        Exited
          { status = `Exited 1; wait = message; line = 1; file = None; last };
      ]

let test_keys c =
  let letter = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') in
  let ctrl = letter || String.contains {|@[\]^-.|} c in
  let alt = (letter && c <> 'm' && c <> 's') || String.contains "[]." c in
  check (accepted [ K7.key (Ctrl c) ] = ctrl);
  check (accepted [ K7.key (Alt c) ] = alt);
  match lines [ K7.key (Alt c) ] with
  | Some [ l ] -> check (String.equal l ("Alt+" ^ String.make 1 c))
  | Some _ -> fail "Alt printed several lines"
  | None -> ()

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

let test_order path text =
  match
    K7.tape ~settings:[ Width 1000 ]
      ~outputs:[ path ^ ".gif"; "x.txt" ]
      [ K7.type' text ]
  with
  | Error _ -> ()
  | Ok t -> (
      let s = K7.to_string t in
      check (String.equal s (Fmt.str "%a" K7.pp t));
      match String.split_on_char '\n' s with
      | o1 :: o2 :: set :: _ ->
          check (String.starts_with ~prefix:"Output " o1);
          check (String.equal o2 {|Output "x.txt"|});
          check (String.equal set "Set Width 1000")
      | _ -> fail "missing lines")

(* Set by the oracle alias only. *)
let oracle = Sys.getenv_opt "VHS_ORACLE"

let vhs_validates t =
  let path = Filename.temp_file "vhs-property" ".tape" in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      Out_channel.with_open_bin path (fun oc ->
          output_string oc (K7.to_string t));
      Sys.command
        (Filename.quote_command (Option.get oracle) [ "validate"; path ]
           ~stdout:"/dev/null" ~stderr:"/dev/null")
      = 0)

let test_validates text n =
  let open K7 in
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
      match K7.tape ~settings ~outputs:[ "demo.gif" ] (actions @ [ last ]) with
      | Error _ -> ()
      | Ok t ->
          if not (vhs_validates t) then
            failf "accepted tape rejected by VHS: %s" (K7.to_string t))
    candidates

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
  match K7.tape ~outputs:[ "demo.gif" ] [ K7.raw line; last ] with
  | Error _ -> ()
  | Ok t ->
      if not (vhs_validates t) then
        failf "accepted raw line rejected by VHS: %S" line

let test_padding_bound padding =
  let settings = K7.[ Padding padding; Width 1200; Height 600 ] in
  let expected = padding >= 0 && padding <= 300 in
  check (Result.is_ok (K7.tape ~settings ~outputs [ last ]) = expected)

(* With the default frame and padding, a margin fits iff it is at most 240. *)
let test_raw_margin_bound margin =
  let margin = margin land max_int in
  let line = Fmt.str "Set Margin %d" margin in
  let result = K7.tape ~outputs [ K7.raw line; last ] in
  if Result.is_ok result <> (margin <= 240) then
    failf "wrong frame bounds for %S" line

let test_no_injection text =
  let hostile = text ^ "\"'`\\\nOutput injected.gif\nHide\nSet Shell bash\n" in
  test_type hostile

let test_error_item n =
  let bad = Fmt.str "item-%d\001" (n land 65535) in
  let check_error prefix result =
    match result with
    | Ok _ -> failf "accepted invalid item %S" bad
    | Error (K7.Invalid message) ->
        if not (String.starts_with ~prefix message) then
          failf "error does not identify %S: %s" bad message
    | Error e -> failf "not Invalid: %a" K7.pp_error e
  in
  check_error
    (Fmt.str "Output %a:" Quote.pp (bad ^ ".gif"))
    (K7.tape ~outputs:[ "good.gif"; bad ^ ".gif" ] []);
  check_error
    (Fmt.str "Type %a:" Quote.pp bad)
    (K7.tape ~outputs [ K7.type' "good"; K7.hidden [ K7.type' bad ] ]);
  check_error
    (Fmt.str "Set FontFamily %a:" Quote.pp bad)
    (K7.tape ~settings:[ K7.Font_family bad ] ~outputs []);
  check_error
    (Fmt.str "Wait+Screen %a:" Quote.pp bad)
    (K7.tape ~outputs [ K7.wait (Screen bad) ])

(* A few turns of the scheduler, to vary where a cancellation lands. *)
let yields n =
  for _ = 0 to n land 15 do
    Eio.Fiber.yield ()
  done

(* On the clock of Native_test.with_clock, so that no verdict depends on
   real time. *)
let with_dir fn =
  Eio_main.run @@ fun env ->
  Native_test.with_clock @@ fun clock ->
  let env = Eio_unix.Stdenv.override env ~mono_clock:clock in
  let dir = Filename.temp_dir "k7-property" "" in
  Fun.protect
    ~finally:(fun () ->
      Array.iter
        (fun name -> Sys.remove (Filename.concat dir name))
        (Sys.readdir dir);
      Unix.rmdir dir)
    (fun () -> fn env (Filename.concat dir))

(* With a blocking close: under io_uring, Eio closes a file late, and
   running it at once fails with ETXTBSY. *)
let save _env path contents =
  Out_channel.with_open_gen [ Open_wronly; Open_creat; Open_trunc; Open_binary ]
    0o755 path (fun oc -> Out_channel.output_string oc contents)

let render_tape () = Result.get_ok (K7.tape ~outputs:[ "demo.gif" ] [ last ])

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
    K7.record ~engine:Vhs ~vhs:executable
      ~clock:(Eio.Stdenv.mono_clock env)
      ~proc ~cwd:(Eio.Stdenv.cwd env) (render_tape ())
  with
  | Ok () -> ()
  | Error e -> failf "ambient environment enables upload: %a" K7.pp_error e

(* The stand-in reads [env]: dash, Linux's /bin/sh, sets [PS1] in any shell. *)
let test_environment_isolation n =
  with_dir @@ fun env path ->
  let executable = path "recorder" in
  let value = string_of_int n in
  save env executable
    (Fmt.str
       "#!/bin/sh\n\
        cat > /dev/null\n\
        ! env | grep -E '^(VHS_PUBLISH|rod|PS1|PROMPT|PROMPT_COMMAND)=' \
        >/dev/null && [ \"$VHS_TEST_KEEP\" = '%s' ]\n"
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
    K7.record ~engine:Vhs ~vhs:executable
      ~clock:(Eio.Stdenv.mono_clock env)
      ~proc ~cwd:(Eio.Stdenv.cwd env) (render_tape ())
  with
  | Ok () -> ()
  | Error e -> failf "environment contract: %a" K7.pp_error e

let test_cancel_reaps delay =
  with_dir @@ fun env path ->
  let executable = path "recorder" in
  save env executable
    "#!/bin/sh\n\
     trap 'exit 0' TERM\n\
     cat > /dev/null\n\
     echo $$ > \"$0.pid\"\n\
     while :; do sleep 0.01; done\n";
  let result =
    Eio.Fiber.first
      (fun () ->
        `Rendered
          (K7.record ~engine:Vhs ~vhs:executable
             ~clock:(Eio.Stdenv.mono_clock env)
             ~proc:(Eio.Stdenv.process_mgr env)
             ~cwd:(Eio.Stdenv.cwd env) (render_tape ())))
      (fun () ->
        Native_test.until (fun () -> Sys.file_exists (executable ^ ".pid"));
        yields delay;
        `Cancelled)
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

let nested_shells =
  lazy
    (Result.get_ok
       (K7.tape ~settings:[ Typing_speed (Ms 1) ] ~outputs:[ "image.svg" ]
          K7.
            [
              type'
                "echo $$ > shell.pid; bash --norc --noprofile -c 'echo $$ > \
                 child.pid; while :; do sleep 0.1; done'\n";
              wait ~timeout:(Ms 10000) (Line "NEVER_READY");
            ]))

(* Only k7's signals end them, at a time of the kernel's choosing. *)
let check_reaped cwd =
  List.iter
    (fun name ->
      let pid =
        Eio.Path.load Eio.Path.(cwd / name) |> String.trim |> int_of_string
      in
      Native_test.until (fun () ->
          match Unix.kill pid 0 with
          | () -> false
          | exception Unix.Unix_error (Unix.ESRCH, _, _) -> true))
    [ "shell.pid"; "child.pid" ]

let test_record_reaps delay callback_raises =
  with_dir @@ fun env path ->
  let cwd =
    Eio.Path.(Eio.Stdenv.fs env / Filename.dirname (path "shell.pid"))
  in
  let tape = Lazy.force nested_shells in
  let ready () =
    Sys.file_exists (path "child.pid")
    && String.trim (Eio.Path.load Eio.Path.(cwd / "child.pid")) <> ""
  in
  let cancel () =
    Native_test.until ready;
    yields delay;
    if callback_raises then raise Exit;
    `Cancelled
  in
  let result =
    try
      Eio.Fiber.first
        (fun () ->
          `Recorded
            (K7.record ~engine:Native ~env:Native_test.no_job_control
               ~clock:(Eio.Stdenv.mono_clock env)
               ~proc:(Eio.Stdenv.process_mgr env)
               ~cwd tape))
        cancel
    with Exit when callback_raises -> `Cancelled
  in
  (match result with
  | `Cancelled -> ()
  | `Recorded (Ok _) -> fail "recording ended before cancellation"
  | `Recorded (Error e) -> failf "%a" K7.pp_error e);
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
  let cli = Option.value ~default:"k7" (Sys.getenv_opt "K7_TEST_EXE") in
  let eval expected args =
    let buffer = Buffer.create 256 in
    let sink = Eio.Flow.buffer_sink buffer in
    let received =
      Eio.Switch.run @@ fun sw ->
      let child =
        Eio.Process.spawn ~sw ~stdout:sink ~stderr:sink
          (Eio.Stdenv.process_mgr env)
          (cli :: "play" :: args)
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

(* cmdliner styles its messages unless [TERM] is [dumb]. *)
let run_cli env args =
  let cli = Option.value ~default:"k7" (Sys.getenv_opt "K7_TEST_EXE") in
  let output = Buffer.create 256 in
  let sink = Eio.Flow.buffer_sink output in
  let proc = Eio.Stdenv.process_mgr env in
  let env =
    Eio.Process.environment proc
    |> Eio.Process.Env.override [ ("TERM", Some "dumb") ]
  in
  let status =
    Eio.Switch.run @@ fun sw ->
    Eio.Process.spawn ~sw ~env ~stdout:sink ~stderr:sink proc
      (cli :: "play" :: args)
    |> Eio.Process.await
  in
  (status, Buffer.contents output)

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
       (String.starts_with ~prefix:"k7: option '--wait-timeout':")
       (String.split_on_char '\n' output))

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
         (Fmt.str "Set WaitTimeout %dms" (seconds * 1000))
         (String.split_on_char '\n' output))
  else check_refused seconds output

let record_cram ?(setup = []) ?(settings = []) env path src =
  let output = path "order.txt" in
  let cram = Result.get_ok (K7_cli.Cram.parse src) in
  let t =
    Result.get_ok
      (K7.tape
         ~settings:
           (K7.
              [
                Shell Bash;
                Width 480;
                Height 240;
                Padding 0;
                Font_size 12;
                Framerate 5;
                Typing_speed (Ms 5);
              ]
           @ settings)
         ~outputs:[ output ]
         (K7_cli.Cram.actions ~setup cram @ [ K7.sleep (Ms 3500) ]))
  in
  let result =
    K7.record ~engine:Vhs ?vhs:oracle
      ~clock:(Eio.Stdenv.mono_clock env)
      ~proc:(Eio.Stdenv.process_mgr env)
      ~cwd:(Eio.Stdenv.cwd env) t
  in
  (match result with Ok () -> () | Error e -> failf "%a" K7.pp_error e);
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
  match (position (decoy ^ "FIRST_DONE"), position "echo SECOND_DONE") with
  | Some first, Some second when first < second -> ()
  | _ -> failf "next command was typed before the first output for %S" src

let test_output_order n = Fmt.kstr check_output_order "progress-%d>" (n land 255)

let test_invisible_output_order n =
  let decoy = Buffer.create 32 in
  Fmt.kstr (Buffer.add_string decoy) "progress-%d>" (n land 255);
  Buffer.add_utf_8_uchar decoy (Uchar.of_int 0x200b);
  check_output_order (Buffer.contents decoy)

let test_setup_prompt_hook n =
  with_dir @@ fun env path ->
  let script = path "setup.sh" in
  save env script (Fmt.str "PROMPT_COMMAND='PS1=custom-%d'\n" (n land 255));
  let screen =
    record_cram ~setup:[ script ]
      ~settings:[ K7.Wait_timeout (Ms 3000) ]
      env path "  $ echo READY\n"
  in
  if not (String.split_on_char '\n' screen |> List.mem "READY") then
    fail "setup prevented the command from running"

let test_json_theme s =
  let theme = K7.Theme ("{" ^ s) in
  match K7.tape ~settings:[ theme ] ~outputs:[ "a.gif" ] [ K7.type' "x" ] with
  | Error _ -> ()
  | Ok _ -> (
      match K7.svg ~settings:[ theme ] [] with
      | Ok _ | Error (Unsupported _) -> ()
      | Error e -> failf "%a" K7.pp_error e)

(* Go skips unknown values but counts them in its 10,000-level limit. *)
let test_json_depth n =
  let depth = 9_997 + (n mod 7) in
  let theme =
    "{\"extra\":" ^ String.make depth '[' ^ "0" ^ String.make depth ']' ^ "}"
  in
  let expected = depth < 10_000 in
  let accepted =
    match K7.tape ~settings:[ Theme theme ] ~outputs:[ "a.txt" ] [ last ] with
    | Ok _ -> true
    | Error (Invalid _) -> false
    | Error e -> failf "%a" K7.pp_error e
  in
  if expected <> accepted then
    failf "JSON array depth %d: Go accepts=%b, tape accepts=%b" depth expected
      accepted

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
      K7.tape
        ~settings:K7.[ Typing_speed (Ms 1) ]
        ~outputs:[ path "out.svg" ]
        K7.[ hidden [ type' command; wait Prompt ]; sleep (Ms 100) ]
    with
    | Ok tape -> tape
    | Error e -> failf "%a" K7.pp_error e
  in
  let cwd = Eio.Path.(Eio.Stdenv.fs env / Filename.dirname (path "received")) in
  (match
     K7.record ~engine:Native ~env:Native_test.no_job_control
       ~clock:(Eio.Stdenv.mono_clock env)
       ~proc:(Eio.Stdenv.process_mgr env)
       ~cwd tape
   with
  | Ok _ -> ()
  | Error e -> failf "%a" K7.pp_error e);
  check_eq ~pp:pp_string payload (Eio.Path.load Eio.Path.(cwd / "received"))

(* The leading sleep gives at least one frame. *)
let test_zero_speed_prompt n =
  with_dir @@ fun env path ->
  let marker = Fmt.str "READY_%d" n in
  let settings =
    K7.[ Width 400; Height 200; Font_size 10; Padding 10; Typing_speed (Ms 0) ]
  in
  let tape =
    match
      K7.tape ~settings
        ~outputs:[ path "out.txt" ]
        K7.[ sleep (Ms 100); type' ("echo " ^ marker ^ "\n"); wait Prompt ]
    with
    | Ok t -> t
    | Error e -> failf "%a" K7.pp_error e
  in
  let cwd = Eio.Path.(Eio.Stdenv.fs env / Filename.dirname (path "out.txt")) in
  (match
     K7.record ~engine:Native ~env:Native_test.no_job_control
       ~clock:(Eio.Stdenv.mono_clock env)
       ~proc:(Eio.Stdenv.process_mgr env)
       ~cwd tape
   with
  | Ok () -> ()
  | Error e -> failf "%a" K7.pp_error e);
  let output = Eio.Path.load Eio.Path.(cwd / "out.txt") in
  if not (contains ~sub:("\n" ^ marker ^ "\n>") output) then
    failf "prompt wait returned without the output of echo %s" marker

let test_output_preflight n =
  with_dir @@ fun env path ->
  let text = if n mod 2 = 0 then path "keep.txt" else path "absent/keep.txt" in
  let kinds = [| ".gif"; ".mp4"; ".webm"; ".png"; "/" |] in
  let movie = path ("movie" ^ kinds.(n mod Array.length kinds)) in
  let image = path "image.svg" in
  if n mod 2 = 0 then save env text "keep these bytes";
  let tape =
    match
      K7.tape ~outputs:[ text; movie; image ]
        K7.[ type' "touch played\n"; wait Prompt; sleep (Ms 100) ]
    with
    | Ok t -> t
    | Error e -> failf "%a" K7.pp_error e
  in
  let cwd = Eio.Path.(Eio.Stdenv.fs env / Filename.dirname image) in
  let clock = Eio.Stdenv.mono_clock env and proc = Eio.Stdenv.process_mgr env in
  let refused name item = function
    | Error (K7.Unsupported why) ->
        check (contains ~sub:name why);
        check (contains ~sub:item why)
    | Error e -> failf "%a" K7.pp_error e
    | Ok () -> fail "played incompatible outputs"
  in
  refused "the Native engine" image
    (K7.record ~engine:Vhs ~vhs:(path "missing-vhs") ~clock ~proc ~cwd tape);
  refused "the Vhs engine" movie
    (K7.record ~engine:Native ~env:Native_test.no_job_control ~clock ~proc ~cwd
       tape);
  check (not (Sys.file_exists (path "played")));
  check (not (Sys.file_exists image));
  if n mod 2 = 0 then
    check_eq ~pp:pp_string "keep these bytes"
      (Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / text))
  else check (not (Sys.file_exists (path "absent")))

let check_native_path env cwd marker (output, actual, rate) =
  if Sys.file_exists actual then Sys.remove actual;
  let settings =
    K7.
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
      K7.tape ~settings ~outputs:[ output ]
        K7.
          [
            type' ("echo " ^ marker ^ "\n");
            wait ~timeout:(Ms 5000) (Screen ("(?m)^" ^ marker ^ "$"));
            wait Prompt;
          ]
    with
    | Ok t -> t
    | Error e -> failf "%a" K7.pp_error e
  in
  (match
     K7.record ~engine:Native ~env:Native_test.no_job_control
       ~clock:(Eio.Stdenv.mono_clock env)
       ~proc:(Eio.Stdenv.process_mgr env)
       ~cwd:(cwd :> Eio.Fs.dir_ty Eio.Path.t)
       tape
   with
  | Ok () -> ()
  | Error e -> failf "%a" K7.pp_error e);
  check (Sys.file_exists actual);
  check
    (contains
       ~sub:("\n" ^ marker ^ "\n")
       (Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / actual)))

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

(* The line discipline adds a carriage return before each newline. *)
let test_run_screen bytes =
  with_dir @@ fun env path ->
  let text =
    String.concat ""
      (List.map
         (fun c ->
           match c with
           | ' ' .. '~' -> String.make 1 c
           | '\000' .. '\031' -> "\n"
           | c -> Fmt.str "\027[3%dm" (Char.code c land 7))
         (List.of_seq (String.to_seq bytes)))
  in
  save env (path "out") text;
  let settings = K7.[ Columns 40; Rows 6; Font_size 10 ] in
  let last svg =
    match Svg_check.frames ~cell_width:7 ~cell_height:13 ~rows:6 svg with
    | Ok frames -> List.nth frames (List.length frames - 1)
    | Error e -> failf "not well-formed: %s" e
  in
  let crlf = String.concat "\r\n" (String.split_on_char '\n' text) in
  match
    ( K7.run ~settings ~show:""
        ~clock:(Eio.Stdenv.mono_clock env)
        ~proc:(Eio.Stdenv.process_mgr env)
        ~cwd:Eio.Path.(Eio.Stdenv.fs env / Filename.dirname (path "out"))
        [ "cat"; "out" ],
      K7.svg ~settings [ (Ms 0, crlf) ] )
  with
  | Ok (image, `Exited 0), Ok expected ->
      if last image <> last expected then
        failf "%S shows as\n%s\nnot\n%s" text (last image) (last expected)
  | Ok (_, _), Ok _ -> fail "cat did not exit with 0"
  | Error e, _ | _, Error e -> failf "%a" K7.pp_error e

let test_run_path_override n =
  with_dir @@ fun env path ->
  let name = Fmt.str "k7-path-only-%d" n in
  save env (path name) "#!/bin/sh\nprintf PATH_OVERRIDE_WORKED\n";
  Unix.chmod (path name) 0o755;
  let dir = Filename.dirname (path name) in
  match
    K7.run
      ~env:[ ("PATH", dir) ]
      ~clock:(Eio.Stdenv.mono_clock env)
      ~proc:(Eio.Stdenv.process_mgr env)
      ~cwd:Eio.Path.(Eio.Stdenv.fs env / dir)
      [ name ]
  with
  | Ok (image, `Exited 0) when contains ~sub:"PATH_OVERRIDE_WORKED" image -> ()
  | Ok _ -> fail "the executable in the overridden PATH did not run"
  | Error e -> failf "%a" K7.pp_error e

let test_run_signal_disposition n =
  with_dir @@ fun env _ ->
  let previous = Sys.signal Sys.sigpipe Sys.Signal_ignore in
  Fun.protect ~finally:(fun () -> Sys.set_signal Sys.sigpipe previous)
  @@ fun () ->
  match
    K7.run
      ~clock:(Eio.Stdenv.mono_clock env)
      ~proc:(Eio.Stdenv.process_mgr env)
      ~cwd:(Eio.Stdenv.cwd env)
      [ "/bin/sh"; "-c"; Fmt.str "kill -PIPE $$; printf IGNORED_%d" n ]
  with
  | Ok (_, `Signaled got) when got = Sys.sigpipe -> ()
  | Ok _ -> fail "the program inherited ignored SIGPIPE from the recorder"
  | Error e -> failf "%a" K7.pp_error e

let test_run_exec_error n =
  with_dir @@ fun env path ->
  save env (path "not-executable") "#!/bin/sh\nexit 0\n";
  Unix.chmod (path "not-executable") 0o644;
  let argv =
    List.nth
      [ [ "./not-executable" ]; [ "." ]; [ "/bin/echo"; "a\000b" ] ]
      (n mod 3)
  in
  match
    K7.run
      ~clock:(Eio.Stdenv.mono_clock env)
      ~proc:(Eio.Stdenv.process_mgr env)
      ~cwd:
        Eio.Path.(Eio.Stdenv.fs env / Filename.dirname (path "not-executable"))
      argv
  with
  | Error (K7.Io _) -> ()
  | Error e -> failf "%a" K7.pp_error e
  | Ok _ -> fail "exec failed but returned a recording and program status"

let test_run_cli_status n signaled =
  with_dir @@ fun env path ->
  let output = path "exit.svg" in
  let status = n land 255 in
  let cli = Option.value ~default:"k7" (Sys.getenv_opt "K7_TEST_EXE") in
  let sink = Eio.Flow.buffer_sink (Buffer.create 128) in
  let command =
    "printf EXIT_MARKER; "
    ^ if signaled then "kill -TERM $$" else Fmt.str "exit %d" status
  in
  let got =
    Eio.Switch.run @@ fun sw ->
    Eio.Process.spawn ~sw ~stdout:sink ~stderr:sink
      (Eio.Stdenv.process_mgr env)
      [ cli; "rec"; "--show"; ""; "-o"; output; "--"; "/bin/sh"; "-c"; command ]
    |> Eio.Process.await
  in
  check (got = `Exited (if signaled then 143 else status));
  let image = Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / output) in
  check (contains ~sub:"EXIT_MARKER" image)

let test_run_inherited_fd n =
  with_dir @@ fun env path ->
  let token = Fmt.str "PRIVATE_%d" n in
  save env (path "private-data") (token ^ "\n");
  let fd = Unix.openfile (path "private-data") [ O_RDONLY ] 0 in
  Unix.clear_close_on_exec fd;
  Fun.protect ~finally:(fun () -> Unix.close fd) @@ fun () ->
  let command =
    Fmt.str
      "n=3; while [ \"$n\" -lt 64 ]; do value=; IFS= read -r -t 1 -u \"$n\" \
       value 2>/dev/null; if [ \"$value\" = %s ]; then printf FD_LEAKED; fi; \
       n=$((n+1)); done; printf DONE"
      token
  in
  match
    K7.run ~show:""
      ~clock:(Eio.Stdenv.mono_clock env)
      ~proc:(Eio.Stdenv.process_mgr env)
      ~cwd:(Eio.Stdenv.cwd env)
      [ "/bin/bash"; "-c"; command ]
  with
  | Error e -> failf "%a" K7.pp_error e
  | Ok (image, `Exited 0) -> check (not (contains ~sub:"FD_LEAKED" image))
  | Ok _ -> fail "descriptor probe did not exit successfully"

let test_run_fd_cleanup n cancelled =
  with_dir @@ fun env _ ->
  let record () =
    let argv =
      if cancelled then [ "/bin/sh"; "-c"; "read value" ]
      else [ "/usr/bin/true" ]
    in
    let play () =
      match
        K7.run
          ~clock:(Eio.Stdenv.mono_clock env)
          ~proc:(Eio.Stdenv.process_mgr env)
          ~cwd:(Eio.Stdenv.cwd env) argv
      with
      | Ok (_, `Exited 0) -> ()
      | Ok _ -> fail "true did not exit successfully"
      | Error e -> failf "%a" K7.pp_error e
    in
    if cancelled then Eio.Fiber.first play (fun () -> yields n) else play ()
  in
  record ();
  let count () = Array.length (Sys.readdir "/dev/fd") in
  let before = count () in
  for _ = 0 to n mod 4 do
    record ()
  done;
  check_eq ~pp:pp_int before (count ())

let test_run_cancel_descendant cancelled =
  with_dir @@ fun env path ->
  let cwd =
    Eio.Path.(Eio.Stdenv.fs env / Filename.dirname (path "child.pid"))
  in
  let child = "trap '' HUP TERM; echo $$ > child.pid; exec sleep 100000" in
  let ready () =
    try String.trim (Eio.Path.load Eio.Path.(cwd / "child.pid")) <> ""
    with Eio.Io _ -> false
  in
  let play () =
    `Ran
      (K7.run
         ~clock:(Eio.Stdenv.mono_clock env)
         ~proc:(Eio.Stdenv.process_mgr env)
         ~cwd
         [
           "/bin/sh";
           "-c";
           ("trap '' HUP TERM; /bin/sh -c " ^ Filename.quote child
          ^ " & while [ ! -s child.pid ]; do sleep 0.01; done; "
           ^ if cancelled then "wait" else "exit 0");
         ])
  in
  let result =
    if not cancelled then play ()
    else
      Eio.Fiber.first play (fun () ->
          Native_test.until ready;
          `Cancelled)
  in
  (match result with
  | `Cancelled when cancelled -> ()
  | `Ran (Ok (_, `Exited 0)) when not cancelled -> ()
  | _ -> fail "run did not finish as expected");
  (* Only k7's SIGKILL ends it, at a time of the kernel's choosing. *)
  let pid =
    int_of_string (String.trim (Eio.Path.load Eio.Path.(cwd / "child.pid")))
  in
  Native_test.until (fun () ->
      match Unix.kill pid 0 with
      | () -> false
      | exception Unix.Unix_error (ESRCH, _, _) -> true)

let payloads =
  [
    "";
    "two words";
    "it's";
    "\"quoted\"";
    "$HOME";
    "a\nb";
    "a\n";
    "caf\xc3\xa9";
    "世界";
    "😀";
    "`echo replaced`";
    "a\tb";
    "a\x9b[2Jb";
    "\xc2\x9b";
    "\xff\xfe";
    "caf\xc3";
  ]

let test_command_line_roundtrip n =
  with_dir @@ fun env path ->
  let payload = List.nth payloads (n mod List.length payloads) in
  let settings = K7.[ Columns 200; Rows 6; Font_size 10; Padding 0 ] in
  let cwd = Eio.Path.(Eio.Stdenv.fs env / Filename.dirname (path "unused")) in
  let proc = Eio.Stdenv.process_mgr env in
  let image =
    match
      K7.run ~settings
        ~clock:(Eio.Stdenv.mono_clock env)
        ~proc ~cwd
        [ "/usr/bin/printf"; "%s"; payload ]
    with
    | Ok (image, `Exited 0) -> image
    | Ok (_, status) -> failf "printf: %a" Eio.Process.pp_status status
    | Error e -> failf "%a" K7.pp_error e
  in
  let first =
    match Svg_check.frames ~cell_width:7 ~cell_height:13 ~rows:6 image with
    | Ok (first :: _) -> first
    | Ok [] -> fail "no command frame"
    | Error e -> fail e
  in
  let command = Native_test.shown_command first in
  List.iter
    (fun shell ->
      let output = Buffer.create 128 in
      Eio.Process.run
        ~stdout:(Eio.Flow.buffer_sink output)
        proc [ shell; "-c"; command ];
      check_eq ~pp:Fmt.Dump.string payload (Buffer.contents output))
    Native_test.shells

(* Replaying a command from a saved recording must execute the named file,
   including when the shell gives its name a meaning in command position. *)
let test_executable_command_roundtrip n =
  Native_test.with_dir @@ fun env cwd ->
  let dir = Eio.Path.native_exn cwd in
  let name =
    match n mod 4 with
    | 0 -> "if"
    | 1 -> Fmt.str "K7_ARG_%d=value" n
    | 2 -> "-"
    | _ -> Fmt.str "k7-program-%d" n
  in
  Native_test.executable (Filename.concat dir name)
    "#!/bin/sh\nprintf 'EXECUTED\\n'\n";
  let vars = [ ("PATH", dir ^ ":/usr/bin:/bin") ] in
  let proc = Eio.Stdenv.process_mgr env in
  let settings = K7.[ Columns 100; Rows 4; Font_size 10; Padding 0 ] in
  let saved, status =
    match
      K7.capture ~settings ~env:vars
        ~clock:(Eio_mock.Clock.Mono.make ())
        ~proc ~cwd [ name ]
    with
    | Ok result -> result
    | Error e -> failf "%a" K7.pp_error e
  in
  check (status = `Exited 0);
  let saved = Result.get_ok (K7.cast_of_string (K7.string_of_cast saved)) in
  let image = Result.get_ok (K7.svg_of_casts ~settings [ saved ]) in
  let first =
    match Svg_check.frames ~cell_width:7 ~cell_height:13 ~rows:4 image with
    | Ok (first :: _) -> first
    | Ok [] -> fail "no command frame"
    | Error e -> fail e
  in
  let command = Native_test.shown_command first in
  List.iter
    (fun shell ->
      let output = Buffer.create 32 and error = Buffer.create 32 in
      Eio.Process.run ~cwd
        ~env:[| "PATH=" ^ dir ^ ":/usr/bin:/bin" |]
        ~is_success:(fun _ -> true)
        ~stdout:(Eio.Flow.buffer_sink output)
        ~stderr:(Eio.Flow.buffer_sink error)
        proc [ shell; "-c"; command ];
      if Buffer.contents output <> "EXECUTED\n" then
        failf "%s replays %S as %S (%s)" shell command (Buffer.contents output)
          (Buffer.contents error))
    Native_test.shells

let check_saved_command_argument argument =
  Native_test.with_dir @@ fun env cwd ->
  let proc = Eio.Stdenv.process_mgr env in
  let settings = K7.[ Columns 100; Rows 4; Font_size 10; Padding 0 ] in
  let recording, status =
    match
      K7.capture ~settings
        ~clock:(Eio_mock.Clock.Mono.make ())
        ~proc ~cwd
        [ "/usr/bin/printf"; "%s"; argument ]
    with
    | Ok result -> result
    | Error e -> failf "%a" K7.pp_error e
  in
  check (status = `Exited 0);
  let recording =
    Result.get_ok (K7.cast_of_string (K7.string_of_cast recording))
  in
  let image = Result.get_ok (K7.svg_of_casts ~settings [ recording ]) in
  let first =
    match Svg_check.frames ~cell_width:7 ~cell_height:13 ~rows:4 image with
    | Ok (first :: _) -> first
    | Ok [] -> fail "no command frame"
    | Error e -> fail e
  in
  let command = Native_test.shown_command first in
  List.iter
    (fun shell ->
      let output = Buffer.create 32 in
      Eio.Process.run
        ~stdout:(Eio.Flow.buffer_sink output)
        proc [ shell; "-c"; command ];
      check_eq ~pp:Fmt.Dump.string argument (Buffer.contents output))
    Native_test.shells

let test_command_format_roundtrip n =
  let scalar = [| 0x200b; 0x202e; 0x2060; 0xad; 0xfeff |].(n mod 5) in
  let buffer = Buffer.create 16 in
  Buffer.add_char buffer 'a';
  Buffer.add_utf_8_uchar buffer (Uchar.of_int scalar);
  Buffer.add_char buffer 'b';
  check_saved_command_argument (Buffer.contents buffer)

let test_command_octet_roundtrip n =
  check_saved_command_argument (String.make 1 (Char.chr (1 + (n mod 255))))

let test_capture_incomplete_roundtrip n tail =
  with_dir @@ fun env _ ->
  let suffix = List.nth [ "\xc3"; "\xe4\xb8"; "\xf0\x9f\x91" ] (tail mod 3) in
  let payload = String.make (n mod 8) 'a' ^ suffix in
  let captured =
    K7.capture ~show:""
      ~settings:K7.[ Columns 16; Rows 2 ]
      ~clock:(Eio.Stdenv.mono_clock env)
      ~proc:(Eio.Stdenv.process_mgr env)
      ~cwd:(Eio.Stdenv.cwd env)
      [ "printf"; "%s"; payload ]
  in
  match captured with
  | Error e -> failf "%a" K7.pp_error e
  | Ok (cast, _) -> (
      let saved = K7.string_of_cast cast in
      check (String.is_valid_utf_8 saved);
      match K7.cast_of_string saved with
      | Error e -> failf "%a" K7.pp_error e
      | Ok restored ->
          let image c =
            match K7.svg_of_casts [ c ] with
            | Ok s -> s
            | Error e -> failf "%a" K7.pp_error e
          in
          check_eq ~pp:Fmt.Dump.string (image cast) (image restored))

let test_cast_dryrun_destination n directory text =
  with_dir @@ fun env path ->
  save env (path "input.cast")
    ({|{"version":3,"term":{"cols":8,"rows":2}}|} ^ "\n");
  let extension = if text then ".txt" else ".svg" in
  let occupied = path (Fmt.str "occupied %d%s" n extension) in
  let destination =
    if directory then (
      Unix.mkdir occupied 0o755;
      occupied)
    else (
      save env (path "parent") "not a directory";
      path ("parent/out" ^ extension))
  in
  Fun.protect ~finally:(fun () -> if directory then Unix.rmdir occupied)
  @@ fun () ->
  let cli = Option.value ~default:"k7" (Sys.getenv_opt "K7_TEST_EXE") in
  let run dry_run =
    let sink = Eio.Flow.buffer_sink (Buffer.create 128) in
    Eio.Switch.run @@ fun sw ->
    let argv =
      [ cli; "play" ]
      @ (if dry_run then [ "--dry-run" ] else [])
      @ [ "-o"; destination; path "input.cast" ]
    in
    Eio.Process.spawn ~sw ~stdout:sink ~stderr:sink
      (Eio.Stdenv.process_mgr env)
      argv
    |> Eio.Process.await
  in
  check (run false = `Exited 123);
  check (run true = `Exited 123)

let record_with_domain surface live =
  let ready = Semaphore.Binary.make false in
  let finish = Semaphore.Binary.make false in
  let worker =
    Domain.spawn (fun () ->
        Semaphore.Binary.release ready;
        Semaphore.Binary.acquire finish)
  in
  Semaphore.Binary.acquire ready;
  let join () =
    Semaphore.Binary.release finish;
    Domain.join worker
  in
  if not live then join ();
  Fun.protect ~finally:(fun () -> if live then join ()) @@ fun () ->
  Native_test.with_dir @@ fun env cwd ->
  let clock = Eio_mock.Clock.Mono.make () in
  let proc = Eio.Stdenv.process_mgr env in
  let result =
    match surface mod 3 with
    | 0 -> Result.map (fun _ -> ()) (K7.capture ~clock ~proc ~cwd [ "true" ])
    | 1 -> Result.map (fun _ -> ()) (K7.run ~clock ~proc ~cwd [ "true" ])
    | _ ->
        let tape =
          Result.get_ok
            (K7.tape ~outputs:[ "out.svg" ] K7.[ type' "true\n"; wait Prompt ])
        in
        Native_test.play env cwd tape
  in
  match result with Ok () -> () | Error e -> failf "%a" K7.pp_error e

let native_domains =
  Native_test.subprocess "native domains" @@ function
  | [ surface; live ] ->
      record_with_domain (int_of_string surface) (bool_of_string live)
  | args -> failf "native domains: %a" Fmt.(Dump.list string) args

let test_native_domains surface live =
  let status = native_domains [ string_of_int surface; string_of_bool live ] in
  check (status = Unix.WEXITED 0)

let test_capture_valid_roundtrip status glyph =
  with_dir @@ fun env _ ->
  let glyph = List.nth [ "é"; "漢字"; "👍"; "é" ] (glyph mod 4) in
  let body = "\027[31m" ^ glyph ^ "\027[0m\nlast" in
  let payload =
    match status mod 3 with
    | 0 -> body
    | 1 -> "normal\027[?1049h" ^ body
    | _ -> "normal\027[?1049h" ^ body ^ "\027[?1049lEND"
  in
  let settings = K7.[ Columns 24; Rows 4; Font_size 10 ] in
  let result =
    K7.capture ~settings ~show:"demo"
      ~clock:(Eio.Stdenv.mono_clock env)
      ~proc:(Eio.Stdenv.process_mgr env)
      ~cwd:(Eio.Stdenv.cwd env)
      [
        "/bin/sh";
        "-c";
        Fmt.str "printf '%%s' \"$1\"; exit %d" status;
        "demo";
        payload;
      ]
  in
  match result with
  | Error e -> failf "%a" K7.pp_error e
  | Ok (cast, got) -> (
      check (got = `Exited status);
      let saved = K7.string_of_cast cast in
      check (String.is_valid_utf_8 saved);
      match K7.cast_of_string saved with
      | Error e -> failf "%a" K7.pp_error e
      | Ok restored ->
          let image c =
            match K7.svg_of_casts ~settings [ c ] with
            | Ok s -> s
            | Error e -> failf "%a" K7.pp_error e
          in
          let text c =
            match K7.text_of_casts ~settings [ c ] with
            | Ok s -> s
            | Error e -> failf "%a" K7.pp_error e
          in
          check_eq ~pp:Fmt.Dump.string (image cast) (image restored);
          check_eq ~pp:Fmt.Dump.string (text cast) (text restored))

let test_capture_script_fallback n =
  Native_test.with_dir @@ fun env cwd ->
  Native_test.executable
    (Eio.Path.native_exn Eio.Path.(cwd / "plain-script"))
    "printf '%s' \"$1\"; exit \"$2\"\n";
  let argument =
    List.nth
      [ ""; "two words"; "'\"$`\\"; "café"; "漢字👍"; "line\nbreak" ]
      (n mod 6)
  in
  let expected = "arg:" ^ argument ^ ":end\n" in
  match
    K7.capture ~show:""
      ~settings:K7.[ Columns 80; Rows 4 ]
      ~clock:(Eio_mock.Clock.Mono.make ())
      ~proc:(Eio.Stdenv.process_mgr env)
      ~cwd
      [
        "./plain-script"; "arg:" ^ argument ^ ":end"; string_of_int (n mod 128);
      ]
  with
  | Error e -> failf "%a" K7.pp_error e
  | Ok (cast, status) ->
      check (status = `Exited (n mod 128));
      check_eq ~pp:Fmt.Dump.string expected
        (Result.get_ok (K7.text_of_casts [ cast ]))

let test_capture_signal_mask n =
  Native_test.with_dir @@ fun env cwd ->
  let signal, name =
    List.nth
      [ (Sys.sigusr1, "USR1"); (Sys.sigusr2, "USR2"); (Sys.sigpipe, "PIPE") ]
      (n mod 3)
  in
  let previous = Unix.sigprocmask Unix.SIG_BLOCK [ signal ] in
  Fun.protect ~finally:(fun () ->
      ignore (Unix.sigprocmask Unix.SIG_SETMASK previous))
  @@ fun () ->
  match
    K7.capture ~show:""
      ~clock:(Eio_mock.Clock.Mono.make ())
      ~proc:(Eio.Stdenv.process_mgr env)
      ~cwd
      [ "/bin/sh"; "-c"; "kill -" ^ name ^ " $$; printf BLOCKED" ]
  with
  | Error e -> failf "%a" K7.pp_error e
  | Ok (cast, status) ->
      check (status = `Signaled signal);
      check_eq ~pp:Fmt.Dump.string ""
        (Result.get_ok (K7.text_of_casts [ cast ]));
      check (List.mem signal (Unix.sigprocmask Unix.SIG_BLOCK []))

let test_run_empty_path n =
  Native_test.with_dir @@ fun env cwd ->
  Native_test.executable
    (Eio.Path.native_exn Eio.Path.(cwd / "local-only"))
    "#!/bin/sh\nprintf LOCAL_PATH_WORKS\n";
  let path =
    List.nth [ ""; ":/k7-missing"; "/k7-missing:"; "/k7-a::/k7-b" ] (n mod 4)
  in
  match
    K7.capture ~show:""
      ~env:[ ("PATH", path) ]
      ~clock:(Eio_mock.Clock.Mono.make ())
      ~proc:(Eio.Stdenv.process_mgr env)
      ~cwd [ "local-only" ]
  with
  | Error e -> failf "PATH=%S: %a" path K7.pp_error e
  | Ok (cast, status) ->
      check (status = `Exited 0);
      check_eq ~pp:Fmt.Dump.string "LOCAL_PATH_WORKS\n"
        (Result.get_ok (K7.text_of_casts [ cast ]))

let test_capture_standard_fds mask =
  Native_test.with_dir @@ fun env cwd ->
  let fds = [ Unix.stdin; Unix.stdout; Unix.stderr ] in
  let saved = List.map (fun fd -> (fd, Unix.dup fd)) fds in
  let result =
    Fun.protect
      ~finally:(fun () ->
        List.iter (fun (fd, backup) -> Unix.dup2 backup fd) saved;
        List.iter (fun (_, backup) -> Unix.close backup) saved)
      (fun () ->
        List.iteri
          (fun i fd -> if mask land (1 lsl i) <> 0 then Unix.close fd)
          fds;
        K7.capture ~show:""
          ~clock:(Eio_mock.Clock.Mono.make ())
          ~proc:(Eio.Stdenv.process_mgr env)
          ~cwd
          [ "/bin/sh"; "-c"; "printf OUT; printf ERR >&2" ])
  in
  match result with
  | Error e -> failf "closed descriptors %d: %a" mask K7.pp_error e
  | Ok (cast, status) ->
      check (status = `Exited 0);
      check_eq ~pp:Fmt.Dump.string "OUTERR\n"
        (Result.get_ok (K7.text_of_casts [ cast ]))

let test_capture_theme_roundtrip n =
  Native_test.with_dir @@ fun env cwd ->
  let theme = List.nth [ "Dracula"; "Catppuccin Latte" ] (n mod 2) in
  let cast, status =
    match
      K7.capture ~show:""
        ~settings:K7.[ Columns 20; Rows 3; Theme theme ]
        ~clock:(Eio_mock.Clock.Mono.make ())
        ~proc:(Eio.Stdenv.process_mgr env)
        ~cwd
        [ "printf"; "\027[31mred\027[0m" ]
    with
    | Ok result -> result
    | Error e -> failf "%a" K7.pp_error e
  in
  check (status = `Exited 0);
  let restored =
    match K7.cast_of_string (K7.string_of_cast cast) with
    | Ok c -> c
    | Error e -> failf "%a" K7.pp_error e
  in
  let draw settings c =
    match K7.svg_of_casts ~settings [ c ] with
    | Ok image -> image
    | Error e -> failf "%a" K7.pp_error e
  in
  if draw K7.[ Theme theme ] cast <> draw [] restored then
    failf "the saved capture lost the %s theme" theme

let with_programs =
  [
    test_case "saved command images preserve each non-NUL argument byte"
      [ uint8 ] test_command_octet_roundtrip;
    test_case "saved command images preserve Unicode format arguments" [ uint8 ]
      test_command_format_roundtrip;
    test_case "saved command images preserve shell-sensitive executable names"
      [ uint8 ] test_executable_command_roundtrip;
    test_case "saved captures retain their selected theme" [ uint8 ]
      test_capture_theme_roundtrip;
    test_case "executable text without a shebang preserves arguments and status"
      [ uint8 ] test_capture_script_fallback;
    test_case "capture resets child signal masks and preserves the caller mask"
      [ uint8 ] test_capture_signal_mask;
    test_case "empty PATH components search the supplied cwd" [ uint8 ]
      test_run_empty_path;
    test_case "capture gives the child all standard descriptors"
      [ range 7 ]
      test_capture_standard_fds;
    test_case "saved captures preserve colour, Unicode, screens and exit codes"
      [ uint8; uint8 ] test_capture_valid_roundtrip;
    test_case "native recorders work with live or joined OCaml domains"
      [ uint8; bool ] test_native_domains;
    test_case "asciicast dry-run refuses unwritable output paths"
      [ uint8; bool; bool ] test_cast_dryrun_destination;
    test_case "saved captures preserve incomplete UTF-8 tails" [ uint8; uint8 ]
      test_capture_incomplete_roundtrip;
    test_case "the command read from SVG runs the same arguments" [ uint8 ]
      test_command_line_roundtrip;
    test_case "run searches the overridden PATH" [ uint8 ]
      test_run_path_override;
    test_case "run children have normal signal dispositions" [ uint8 ]
      test_run_signal_disposition;
    test_case "run reports exec errors instead of a program status" [ uint8 ]
      test_run_exec_error;
    test_case "run CLI writes the image for every program exit status"
      [ uint8; bool ] test_run_cli_status;
    test_case "run children cannot read the recorder's private descriptors"
      [ uint8 ] test_run_inherited_fd;
    test_case "run closes its descriptors after completion or cancellation"
      [ uint8; bool ] test_run_fd_cleanup;
    test_case "completed or cancelled run leaves no resistant descendant"
      [ bool ] test_run_cancel_descendant;
    test_case "a program's screen is the image of its bytes" [ bytes ]
      test_run_screen;
  ]

let test_check_directory_destination n =
  with_dir @@ fun env path ->
  let name, engine, actions =
    match n mod 4 with
    | 0 -> ("occupied.svg", K7.Native, [])
    | 1 -> ("occupied.txt", K7.Native, [])
    | 2 -> ("occupied.gif", K7.Vhs, [])
    | _ ->
        ( "occupied.svg",
          K7.Native,
          [ Fmt.kstr K7.raw "Screenshot %S" (path "occupied.svg") ] )
  in
  Unix.mkdir (path name) 0o755;
  Fun.protect ~finally:(fun () -> Unix.rmdir (path name)) @@ fun () ->
  let outputs = if actions = [] then [ path name ] else [ path "out.txt" ] in
  let t = Result.get_ok (K7.tape ~outputs (actions @ [ last ])) in
  let cwd = Eio.Path.(Eio.Stdenv.fs env / Filename.dirname (path name)) in
  match K7.check ~engine ~cwd t with
  | Error (Io message) -> check (contains ~sub:name message)
  | Error e -> failf "expected I/O error for %s: %a" name K7.pp_error e
  | Ok () -> failf "check accepted directory destination %s" name

let test_record_term n =
  with_dir @@ fun env path ->
  let term = List.nth [ "dumb"; "vt100"; "ansi" ] (n mod 3) in
  let cwd = Eio.Path.(Eio.Stdenv.fs env / Filename.dirname (path "term")) in
  let t =
    Result.get_ok
      (K7.tape ~settings:[ Typing_speed (Ms 0) ]
         ~outputs:[ path "out.txt" ]
         K7.[ type' "printf '%s' \"$TERM\" > term\n"; wait Prompt ])
  in
  (match
     K7.record ~engine:Native
       ~env:(("TERM", term) :: Native_test.no_job_control)
       ~clock:(Eio.Stdenv.mono_clock env)
       ~proc:(Eio.Stdenv.process_mgr env)
       ~cwd t
   with
  | Ok () -> ()
  | Error e -> failf "%a" K7.pp_error e);
  check_eq ~pp:Fmt.Dump.string term (Eio.Path.load Eio.Path.(cwd / "term"))

let test_vhs_path n =
  with_dir @@ fun env path ->
  let name = Fmt.str "k7-path-vhs-%d" n in
  save env (path name) "#!/bin/sh\n/bin/cat > /dev/null\n";
  let cwd = Eio.Path.(Eio.Stdenv.fs env / Filename.dirname (path name)) in
  match
    K7.record ~engine:Vhs ~vhs:name
      ~env:[ ("PATH", Filename.dirname (path name)) ]
      ~clock:(Eio.Stdenv.mono_clock env)
      ~proc:(Eio.Stdenv.process_mgr env)
      ~cwd (render_tape ())
  with
  | Ok () -> ()
  | Error e -> failf "executable %s on supplied PATH: %a" name K7.pp_error e

let test_check_preflight_parity n =
  with_dir @@ fun env path ->
  let cwd = Eio.Path.(Eio.Stdenv.fs env / Filename.dirname (path "out.svg")) in
  let engine, outputs, vars, actions =
    match n mod 6 with
    | 0 -> (K7.Native, [ path "out.gif" ], [], [])
    | 1 -> (K7.Vhs, [ path "out.svg" ], [], [])
    | 2 ->
        let invalid =
          List.nth
            [ ("", "v"); ("A=B", "v"); ("A\000B", "v"); ("A", "v\000") ]
            (n / 6 mod 4)
        in
        (K7.Native, [ path "out.svg" ], [ invalid ], [])
    | 3 -> (K7.Native, [ path "out.svg" ], [], [ K7.raw "Source missing.tape" ])
    | 4 ->
        ( K7.Native,
          [ path "out.svg" ],
          [],
          [ K7.raw "Set MarginFill missing.png" ] )
    | _ ->
        ( K7.Native,
          [ path "out.txt" ],
          [],
          [ K7.raw "Screenshot absent/shot.svg" ] )
  in
  let t =
    Result.get_ok
      (K7.tape ~outputs (actions @ K7.[ type' "touch played\n"; wait Prompt ]))
  in
  let entries () = List.sort compare (Eio.Path.read_dir cwd) in
  let before = entries () in
  let error = function
    | Ok () -> fail "invalid recording passed preflight"
    | Error e -> Fmt.str "%a" K7.pp_error e
  in
  let checked = error (K7.check ~engine ~env:vars ~cwd t) in
  check (before = entries ());
  let recorded =
    error
      (K7.record ~engine ~env:vars
         ~clock:(Eio.Stdenv.mono_clock env)
         ~proc:(Eio.Stdenv.process_mgr env)
         ~cwd t)
  in
  check_eq ~pp:Fmt.Dump.string checked recorded;
  check (before = entries ())

let test_cli_dry_run_preflight n =
  with_dir @@ fun env path ->
  let name, src, flags =
    match n mod 5 with
    | 0 ->
        ( "input.tape",
          "Sleep 100ms\n",
          [ "-o"; path "out.svg"; "-o"; path "out.gif" ] )
    | 1 ->
        ( "input.tape",
          "Source missing.tape\nSleep 100ms\n",
          [ "-o"; path "out.svg" ] )
    | 2 -> ("input.t", "  $ touch played\n", [])
    | 3 ->
        ( "input.tape",
          "Sleep 100ms\n",
          [ "-o"; path "out.svg"; "--setup"; path "setup.sh" ] )
    | _ ->
        ( "input.tape",
          "Screenshot absent/shot.svg\nSleep 100ms\n",
          [ "-o"; path "out.svg" ] )
  in
  save env (path name) src;
  save env (path "setup.sh") "true\n";
  let entries () =
    List.sort compare
      (Array.to_list (Sys.readdir (Filename.dirname (path name))))
  in
  let before = entries () in
  let checked, _ = run_cli env ([ "--dry-run" ] @ flags @ [ path name ]) in
  check (entries () = before);
  let recorded, _ = run_cli env (flags @ [ path name ]) in
  check (checked = `Exited 123);
  check (recorded = checked);
  check (entries () = before)

let test_cli_dry_run_pure n =
  with_dir @@ fun env path ->
  let command = "touch " ^ Filename.quote (path "played") in
  let name, src =
    if n mod 2 = 0 then ("input.t", Fmt.str "  $ %s\n" command)
    else ("input.tape", Fmt.str "Type %S\nEnter\nWait\nSleep 100ms\n" command)
  in
  save env (path name) src;
  let before =
    List.sort compare
      (Array.to_list (Sys.readdir (Filename.dirname (path name))))
  in
  let status, output =
    run_cli env [ "--dry-run"; "-o"; path "absent/out.svg"; path name ]
  in
  if status <> `Exited 0 then
    failf "dry-run: %a: %s" Eio.Process.pp_status status output;
  check_eq ~pp:Fmt.Dump.string "" output;
  check
    (List.sort compare
       (Array.to_list (Sys.readdir (Filename.dirname (path name))))
    = before)

let test_check_valid_is_pure n =
  with_dir @@ fun env path ->
  let cwd = Eio.Path.(Eio.Stdenv.fs env / Filename.dirname (path "unused")) in
  let output = Fmt.str "absent-%d/nested/out.svg" n in
  let t =
    Result.get_ok
      (K7.tape ~outputs:[ output ] K7.[ type' "touch played\n"; wait Prompt ])
  in
  (match K7.check ~env:[ ("TERM", "vt100") ] ~cwd t with
  | Ok () -> ()
  | Error e -> failf "%a" K7.pp_error e);
  check (Eio.Path.read_dir cwd = [])

let test_stuck_output_bound n =
  Native_test.with_dir @@ fun env cwd ->
  let complete = ref false and largest = ref 0 in
  let stuck output =
    largest := max !largest (String.length output);
    if Native_test.contains ~sub:"OUTPUT_COMPLETE\r\n" output then
      complete := true;
    !complete
  in
  let command =
    Fmt.str
      "head -c %d /dev/zero | tr '\\000' x; printf \"\\nOUTPUT_COMPLETE\\n\"; \
       read ignored\n"
      (65537 + (n * 128))
  in
  let tape =
    match
      K7.tape
        ~settings:K7.[ Columns 20; Rows 2; Typing_speed (Ms 0) ]
        ~outputs:[ "output.svg" ]
        K7.[ type' command; wait (Line "NEVER_READY") ]
    with
    | Ok t -> t
    | Error e -> failf "%a" K7.pp_error e
  in
  Native_test.with_clock ~release:(fun () -> !complete) @@ fun clock ->
  (match
     K7.record ~engine:Native ~env:Native_test.no_job_control ~stuck ~clock
       ~proc:(Eio.Stdenv.process_mgr env)
       ~cwd tape
   with
  | Error (Stuck _) -> ()
  | Error e -> failf "%a" K7.pp_error e
  | Ok () -> fail "the callback did not stop the wait");
  check !complete;
  if !largest > 65536 then
    failf "stuck callback received %d bytes, more than the last 64 KiB" !largest

let test_source_wait_line n =
  with_dir @@ fun env path ->
  let source =
    List.nth
      [ "inc.tape"; "included tape.tape"; "café.tape"; "./漢字.tape" ]
      (n mod 4)
  in
  let comments = 8 + (n mod 16) in
  let prefix = String.concat "" (List.init comments (fun _ -> "# comment\n")) in
  save env (path source)
    (prefix ^ "Output \"ignored.txt\"\nSleep 20ms\nWait+Line /NEVER_MATCH/\n");
  let tape =
    match
      K7.tape
        ~outputs:[ path "out.svg" ]
        K7.[ raw (Fmt.str {|Source "%s"|} source); sleep (Ms 100) ]
    with
    | Ok t -> t
    | Error e -> failf "%a" K7.pp_error e
  in
  let cwd = Eio.Path.(Eio.Stdenv.fs env / Filename.dirname (path "out.svg")) in
  match
    K7.record ~engine:Native ~env:Native_test.no_job_control
      ~stuck:(fun _ -> true)
      ~clock:(Eio.Stdenv.mono_clock env)
      ~proc:(Eio.Stdenv.process_mgr env)
      ~cwd tape
  with
  | Error (Stuck { line; file; wait; _ } as error) ->
      check_eq ~pp:Fmt.int (comments + 3) line;
      check_eq ~pp:Fmt.(Dump.option string) (Some source) file;
      check_eq ~pp:pp_string "Wait+Line /NEVER_MATCH/" wait;
      let message = Fmt.str "%a" K7.pp_error error in
      check (contains ~sub:(Fmt.str "line %d of" line) message);
      check (contains ~sub:source message)
  | Error e -> failf "%a" K7.pp_error e
  | Ok () -> fail "an impossible sourced wait succeeded"

let test_fractional_sleep_units n seconds =
  let count = 1 + (n mod 16) in
  let microseconds = if n land 16 = 0 then 500 else 1500 in
  let duration =
    if seconds then Fmt.str "0.%06ds" microseconds
    else Fmt.str "%d.5ms" (microseconds / 1000)
  in
  Native_test.with_dir @@ fun env cwd ->
  let tape =
    match
      K7.tape ~outputs:[ "out.svg" ]
        (List.init count (fun _ -> K7.raw ("Sleep " ^ duration)))
    with
    | Ok t -> t
    | Error e -> failf "%a" K7.pp_error e
  in
  let result, sleeps =
    Native_test.with_sleep_log @@ fun clock ->
    K7.record ~engine:Native ~env:Native_test.no_job_control ~clock
      ~proc:(Eio.Stdenv.process_mgr env)
      ~cwd tape
  in
  (match result with Ok () -> () | Error e -> failf "%a" K7.pp_error e);
  (* Exclude startup's timeout and cleanup's 10ms polling intervals. *)
  let total =
    List.filter (fun ns -> ns >= 0L && ns < 2_000_000L) sleeps
    |> List.fold_left Int64.add 0L
  in
  check_eq ~pp:Fmt.int64 (Int64.of_int (count * microseconds * 1000)) total

let test_long_duration_range n surface =
  let milliseconds = 4_611_686_018_428 + n in
  let expected = Int64.mul (Int64.of_int milliseconds) 1_000_000L in
  let setting, action, times_out =
    match surface mod 5 with
    | 0 -> ([], K7.sleep (Ms milliseconds), false)
    | 1 -> (K7.[ Typing_speed (Ms milliseconds) ], K7.type' "x", false)
    | 2 -> ([], K7.raw (Fmt.str "Type@%dms x" milliseconds), false)
    | 3 -> ([], K7.raw (Fmt.str "Enter@%dms" milliseconds), false)
    | _ -> ([], K7.wait ~timeout:(Ms milliseconds) (Screen "NEVER_MATCH"), true)
  in
  Native_test.with_dir @@ fun env cwd ->
  let tape =
    match K7.tape ~settings:setting ~outputs:[ "long.svg" ] [ action ] with
    | Ok t -> t
    | Error e -> failf "%a" K7.pp_error e
  in
  let result, sleeps =
    Native_test.with_sleep_log ~release:(fun ns -> ns > 1_000_000_000_000L)
    @@ fun clock ->
    K7.record ~engine:Native ~env:Native_test.no_job_control ~clock
      ~proc:(Eio.Stdenv.process_mgr env)
      ~cwd tape
  in
  (match (result, times_out) with
  | Ok (), false | Error (K7.Timeout _), true -> ()
  | Error e, _ -> failf "%a" K7.pp_error e
  | Ok (), true -> fail "an impossible wait succeeded");
  check_eq ~pp:Fmt.int64 expected (List.fold_left Int64.max 0L sleeps)

let test_typing_duration_units n surface =
  let nanoseconds = n mod 1000 in
  let duration =
    match n mod 3 with
    | 0 -> Fmt.str "0.%06dms" nanoseconds
    | 1 -> Fmt.str "0.%09ds" nanoseconds
    | _ -> Fmt.str "0.%06dms" nanoseconds
  in
  let text = "λ中🙂é" in
  let rec scalars i count =
    if i = String.length text then count
    else
      let d = String.get_utf_8_uchar text i in
      scalars (i + Uchar.utf_decode_length d) (count + 1)
  in
  let actions, count =
    match surface mod 3 with
    | 0 -> (K7.[ raw ("Set TypingSpeed " ^ duration); type' text ], scalars 0 0)
    | 1 -> ([ K7.raw (Fmt.str "Type@%s \"%s\"" duration text) ], scalars 0 0)
    | _ -> ([ K7.raw ("Space@" ^ duration ^ " 3") ], 3)
  in
  Native_test.with_dir @@ fun env cwd ->
  let actions = K7.raw (Fmt.str "Sleep 0.%09ds" nanoseconds) :: actions in
  let tape =
    match K7.tape ~outputs:[ "typing.svg" ] actions with
    | Ok t -> t
    | Error e -> failf "%a" K7.pp_error e
  in
  let result, sleeps =
    Native_test.with_sleep_log @@ fun clock ->
    K7.record ~engine:Native ~env:Native_test.no_job_control ~clock
      ~proc:(Eio.Stdenv.process_mgr env)
      ~cwd tape
  in
  (match result with Ok () -> () | Error e -> failf "%a" K7.pp_error e);
  let total =
    List.filter (fun ns -> ns >= 0L && ns < 1_000_000L) sleeps
    |> List.fold_left Int64.add 0L
  in
  check_eq ~pp:Fmt.int64 (Int64.of_int (nanoseconds * (count + 1))) total

let test_replace_fractional_speed n replacement =
  Native_test.with_dir @@ fun env cwd ->
  let milliseconds = replacement mod 5 in
  let source =
    Fmt.str "Output typing.svg\nSet TypingSpeed 0.%03dms\nType abc\n" (n + 1)
  in
  let tape =
    match
      Result.bind (K7.of_string source)
        (K7.with_settings K7.[ Typing_speed (Ms milliseconds) ])
    with
    | Ok t -> t
    | Error e -> failf "%a" K7.pp_error e
  in
  let result, sleeps =
    Native_test.with_sleep_log @@ fun clock ->
    K7.record ~engine:Native ~env:Native_test.no_job_control ~clock
      ~proc:(Eio.Stdenv.process_mgr env)
      ~cwd tape
  in
  (match result with Ok () -> () | Error e -> failf "%a" K7.pp_error e);
  let total =
    List.filter (fun ns -> ns >= 0L && ns < 10_000_000L) sleeps
    |> List.fold_left Int64.add 0L
  in
  check_eq ~pp:Fmt.int64 (Int64.of_int (3 * milliseconds * 1_000_000)) total

let test_require_executable n directory =
  Native_test.with_dir @@ fun env cwd ->
  let name = Fmt.str "required-%d" n in
  let path = Eio.Path.(cwd / name) in
  if directory then Eio.Path.mkdir ~perm:0o755 path
  else Eio.Path.save ~create:(`Or_truncate 0o644) path "data\n";
  let tape =
    match
      K7.tape
        ~settings:K7.[ Typing_speed (Ms 0) ]
        ~outputs:[ "require.svg" ]
        K7.
          [
            raw ("Require ./" ^ name);
            type' "touch ran\n";
            wait Prompt;
            sleep (Ms 100);
          ]
    with
    | Ok t -> t
    | Error e -> failf "%a" K7.pp_error e
  in
  let result = Native_test.play env cwd tape in
  check (not (Sys.file_exists (Eio.Path.native_exn Eio.Path.(cwd / "ran"))));
  match result with
  | Error (K7.Executable_not_found _) -> ()
  | Error e -> failf "%a" K7.pp_error e
  | Ok () -> fail "a non-executable prerequisite was accepted"

let test_noecho_prompt n =
  Native_test.with_dir @@ fun env cwd ->
  let marker = Fmt.str "DONE_%d" n in
  let tape =
    match
      K7.tape
        ~settings:K7.[ Columns 100; Rows 5; Typing_speed (Ms 0) ]
        ~outputs:[ "prompt.svg" ]
        K7.
          [
            type'
              ("stty -echo; echo READY; IFS= read -r line; stty echo; printf "
             ^ marker ^ "\n");
            wait (Screen "(?m)^READY$");
            type' (String.make (1 + (n mod 16)) 'x' ^ "\n");
            wait (Screen ("(?m)^" ^ marker ^ ">$"));
            wait ~timeout:(Ms 1) Prompt;
          ]
    with
    | Ok t -> t
    | Error e -> failf "%a" K7.pp_error e
  in
  match Native_test.play env cwd tape with
  | Ok () -> ()
  | Error e ->
      failf "an already observed new prompt must hold: %a" K7.pp_error e

let test_unmapped_modifier_keys n alt =
  Native_test.with_dir @@ fun env cwd ->
  let scalars = [| 0x00e9; 0x03bb; 0x4e2d; 0x1f642; 0x0301; 0x200b |] in
  let b = Buffer.create 32 in
  for i = 0 to n mod 8 do
    Buffer.add_utf_8_uchar b
      (Uchar.of_int scalars.((n + i) mod Array.length scalars))
  done;
  let modifier = if alt then "Alt" else "Shift" in
  let tape =
    match
      K7.tape
        ~settings:K7.[ Columns 100; Rows 5; Typing_speed (Ms 0) ]
        ~outputs:[ "keys.svg" ]
        K7.
          [
            type'
              "stty -echo; echo READY; IFS= read -r line; stty echo; printf \
               '%s' \"$line\" > keys; echo DONE\n";
            wait (Screen "(?m)^READY$");
            raw (modifier ^ "+\"" ^ Buffer.contents b ^ "\"");
            type' "X";
            key Enter;
            wait Prompt;
          ]
    with
    | Ok t -> t
    | Error e -> failf "%a" K7.pp_error e
  in
  (match Native_test.play env cwd tape with
  | Ok () -> ()
  | Error e -> failf "%a" K7.pp_error e);
  check_eq ~pp:pp_string "X" (Eio.Path.load Eio.Path.(cwd / "keys"))

let with_processes =
  [
    test_case "an observed new prompt holds after echo-disabled input" [ uint8 ]
      test_noecho_prompt;
    test_case "modifier strings omit Unicode absent from the VHS keymap"
      [ uint8; bool ] test_unmapped_modifier_keys;
    test_case "Require rejects files and directories that cannot execute"
      [ uint8; bool ] test_require_executable;
    test_case "with_settings replaces parsed fractional typing speeds"
      [ uint8; uint8 ] test_replace_fractional_speed;
    test_case "tiny and zero typing durations count Unicode scalars and keys"
      [ choose [ const 0; const 1; uint16 ]; uint8 ]
      test_typing_duration_units;
    test_case "all native durations preserve the upper half of Go time.Duration"
      [ uint16; uint8 ] test_long_duration_range;
    test_case "fractional VHS sleeps preserve their sum in either unit"
      [ uint8; bool ] test_fractional_sleep_units;
    test_case "sourced waits name their file and count its lines" [ uint8 ]
      test_source_wait_line;
    test_case "stuck callbacks receive only the last 64 KiB" [ uint8 ]
      test_stuck_output_bound;
    test_case "CLI dry-run and recording refuse the same preflight failures"
      [ uint8 ] test_cli_dry_run_preflight;
    test_case "successful CLI dry-run is silent and creates no output directory"
      [ uint8 ] test_cli_dry_run_pure;
    test_case "check rejects directory destinations for outputs and screenshots"
      [ uint8 ] test_check_directory_destination;
    test_case "record respects the explicit terminal environment" [ uint8 ]
      test_record_term;
    test_case "vhs is found on the supplied environment PATH" [ uint8 ]
      test_vhs_path;
    test_case "check and record agree on preflight errors without side effects"
      [ uint8 ] test_check_preflight_parity;
    test_case "successful check creates no directory and runs no tape" [ uint8 ]
      test_check_valid_is_pure;
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

let test_accessors_roundtrip n =
  let timeout = K7.Ms (List.nth [ 0; 1; 15_000; max_ms ] (n mod 4)) in
  let settings = K7.[ Columns (1 + n); Rows 6; Wait_timeout timeout ] in
  let outputs = [ Fmt.str "out-%d.txt" n ] in
  let t = Result.get_ok (K7.tape ~settings ~outputs [ last ]) in
  let parsed = Result.get_ok (K7.of_string (K7.to_string t)) in
  let printed settings =
    K7.to_string (Result.get_ok (K7.tape ~settings ~outputs:[] [ last ]))
  in
  let same_outputs = List.equal String.equal outputs in
  check_eq ~pp:pp_string (printed settings) (printed (K7.settings t));
  check (same_outputs (K7.outputs t));
  check_eq ~pp:pp_string (printed settings) (printed (K7.settings parsed));
  check (same_outputs (K7.outputs parsed));
  check_eq ~pp:Fmt.Dump.string (K7.to_string t) (K7.to_string parsed)

let test_replace_dimensions n =
  let get = function Ok t -> t | Error e -> failf "%a" K7.pp_error e in
  let columns = 1 + (n mod 80) and rows = 1 + (n mod 24) in
  let original =
    get
      (K7.tape
         ~settings:K7.[ Width 800; Height 400 ]
         ~outputs:[ "old.gif" ] [ last ])
  in
  let cells =
    get (K7.with_settings K7.[ Columns columns; Rows rows ] original)
  in
  check_eq ~pp:pp_string
    (Fmt.str "Output \"old.gif\"\nSet Columns %d\nSet Rows %d\nSleep 100ms\n"
       columns rows)
    (K7.to_string cells);
  let pixels = get (K7.with_settings K7.[ Width 800; Height 400 ] cells) in
  check_eq ~pp:pp_string (K7.to_string original) (K7.to_string pixels)

let test_replace_outputs n =
  let get = function Ok t -> t | Error e -> failf "%a" K7.pp_error e in
  let original =
    get
      (Fmt.kstr K7.of_string "Output old.gif\nSleep 100ms\nOutput late-%d.txt\n"
         n)
  in
  check_eq
    ~pp:Fmt.Dump.(list string)
    [ "old.gif"; Fmt.str "late-%d.txt" n ]
    (K7.outputs original);
  let replaced = get (K7.with_outputs [ "new.svg" ] original) in
  check_eq ~pp:pp_string "Sleep 100ms\n" (K7.to_string replaced);
  let none = get (K7.with_outputs [] original) in
  check_eq ~pp:pp_string "Sleep 100ms\n" (K7.to_string none)

(* A fake terminal: the program writes [output] into a buffer of
   [capacity] bytes and exits, and the terminal, once empty, reads as
   [closed] says: not ready, closed, or failed. The program runs while the
   reader asks whether it has exited, and the mock clock moves only when
   every fiber waits. *)
type fake_pty = {
  pending : Buffer.t;
  capacity : int;
  changed : Eio.Condition.t;
  mutable exited : bool;
}

(* Once empty and the program gone, the terminal reads as [closed] says:
   not ready, closed, or failed. *)
let fake_read t closed buf off len =
  if Buffer.length t.pending = 0 then
    match closed mod 3 with
    | 0 -> raise (Unix.Unix_error (EAGAIN, "read", ""))
    | _ when not t.exited -> raise (Unix.Unix_error (EAGAIN, "read", ""))
    | 1 -> 0
    | _ -> raise (Unix.Unix_error (EIO, "read", ""))
  else
    let n = min len (Buffer.length t.pending) in
    let rest = Buffer.sub t.pending n (Buffer.length t.pending - n) in
    Buffer.blit t.pending 0 buf off n;
    Buffer.clear t.pending;
    Buffer.add_string t.pending rest;
    Eio.Condition.broadcast t.changed;
    n

let rec fake_write t output i =
  if i < String.length output then (
    let n =
      min (t.capacity - Buffer.length t.pending) (String.length output - i)
    in
    if n = 0 then Eio.Condition.await_no_mutex t.changed
    else (
      Buffer.add_substring t.pending output i n;
      Eio.Condition.broadcast t.changed);
    fake_write t output (i + n))
  else (
    t.exited <- true;
    Eio.Condition.broadcast t.changed)

(* The program writes [output] into a fake terminal of [capacity] bytes and
   exits, and runs while the reader asks whether it has exited. The mock
   clock moves only when every fiber waits. *)
let test_reader output capacity closed size =
  Eio_mock.Backend.run_full @@ fun env ->
  let clock = Eio.Stdenv.mono_clock env in
  let t =
    {
      pending = Buffer.create 256;
      capacity = 1 + capacity;
      changed = Eio.Condition.create ();
      exited = false;
    }
  in
  let fed = Buffer.create 256 in
  let pause () = Eio.Time.Mono.sleep clock 0.02 in
  let ready () =
    Eio.Fiber.first
      (fun () ->
        while Buffer.length t.pending = 0 && not t.exited do
          Eio.Condition.await_no_mutex t.changed
        done)
      pause
  in
  Eio.Fiber.both
    (fun () -> fake_write t output 0)
    (fun () ->
      Pty_reader.run ~read:(fake_read t closed) ~ready ~pause
        ~exited:(fun () ->
          Eio.Fiber.yield ();
          t.exited)
        ~feed:(fun buf n -> Buffer.add_subbytes fed buf 0 n)
        (Bytes.create (1 + size)));
  check_eq ~pp:pp_string output (Buffer.contents fed)

let test_upper_half_repeat_preflight n =
  Native_test.with_dir @@ fun _env cwd ->
  let count = Int64.add 0x4000000000000000L (Int64.of_int n) in
  let source = Fmt.str "Output repeat.svg\nSpace@0ms %Ld\n" count in
  let tape =
    match K7.of_string source with
    | Ok t -> t
    | Error e -> failf "%a" K7.pp_error e
  in
  match K7.check ~engine:Native ~cwd tape with
  | Ok () -> ()
  | Error e -> failf "repeat %Ld: %a" count K7.pp_error e

let test_native_repeat_preflight n =
  Native_test.with_dir @@ fun _env cwd ->
  let count = 100001 + n in
  let source = Fmt.str "Output repeat.svg\nSpace@0ms %d\n" count in
  let tape =
    match K7.of_string source with
    | Ok t -> t
    | Error e -> failf "%a" K7.pp_error e
  in
  match K7.check ~engine:Native ~cwd tape with
  | Ok () -> ()
  | Error e -> failf "repeat %d: %a" count K7.pp_error e

let test_check_writes_nothing n =
  Native_test.with_dir @@ fun _env cwd ->
  let output = Fmt.str "missing-%d/deep/image.svg" n in
  let tape =
    match
      K7.tape ~outputs:[ output ]
        K7.
          [
            raw "Require k7-deliberately-missing-prerequisite";
            type' "touch ran\n";
            sleep (Ms 100);
          ]
    with
    | Ok t -> t
    | Error e -> failf "%a" K7.pp_error e
  in
  (match K7.check ~engine:Native ~env:[ ("PATH", "") ] ~cwd tape with
  | Ok () -> ()
  | Error e -> failf "%a" K7.pp_error e);
  check_eq
    ~pp:Fmt.(Dump.list string)
    []
    (Array.to_list (Sys.readdir (Eio.Path.native_exn cwd)))

let bounds =
  [
    test_case "native preflight accepts upper-half Go integer repeat counts"
      [ uint16 ] test_upper_half_repeat_preflight;
    test_case "successful preflight writes nothing and starts no program"
      [ uint8 ] test_check_writes_nothing;
    test_case "native repeat counts are not pixel sizes" [ uint16 ]
      test_native_repeat_preflight;
    test_case "the reader drains what a program wrote before it exited"
      [ bytes; uint8; uint8; uint8 ]
      test_reader;
    test_case "settings and outputs roundtrip including zero Wait_timeout"
      [ uint8 ] test_accessors_roundtrip;
    test_case "cell and pixel dimensions replace each other" [ uint8 ]
      test_replace_dimensions;
    test_case "replacing outputs replaces every Output line" [ uint8 ]
      test_replace_outputs;
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
    test_case "format characters and stray selectors are removed"
      [
        choose
          [
            bytes;
            map [ bytes; bytes ] (fun a b ->
                a ^ " \u{FE0F}\u{2764}\u{FE0F}\u{E0100}\u{200B}" ^ b
                ^ "\u{200D}\u{FE0E}");
          ];
      ]
      test_strip_format;
  ]

let tapes =
  [
    test_case "equivalent screenshot paths cannot overwrite another image"
      [ uint8; bool ] test_screenshot_alias;
    test_case "final hidden actions preserve tape serialization" [ uint8; bool ]
      test_final_hidden_roundtrip;
    test_case "tape is total" [ bytes; bytes; int ] test_total;
    test_case "of_string reads to_string"
      [
        choose [ bytes; const "Output a.gif\nSet Width 300\nHide\nType x\n" ];
        bytes;
        int;
      ]
      test_of_string;
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
  ( "k7",
    (if Option.is_some oracle then with_vhs else [])
    @ with_programs @ with_processes @ bounds @ decoding @ tapes )
