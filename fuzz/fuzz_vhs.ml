open Alcobar

let test_parse_no_exception src = ignore (Vhs.Cram.parse src)
let str = Fmt.str "%a" Vhs.pp

let builders s t n =
  let open Vhs in
  [
    ("comment", fun () -> comment s);
    ("require", fun () -> require s);
    ("output", fun () -> output s);
    ("output path", fun () -> output (s ^ ".gif"));
    ("env", fun () -> env s t);
    ("env value", fun () -> env "A" s);
    ("shell", fun () -> set (Shell s));
    ("font family", fun () -> set (Font_family s));
    ("theme", fun () -> set (Theme s));
    ("font size", fun () -> set (Font_size n));
    ("width", fun () -> set (Width n));
    ("height", fun () -> set (Height n));
    ("padding", fun () -> set (Padding n));
    ("framerate", fun () -> set (Framerate n));
    ("typing speed", fun () -> set (Typing_speed (Ms n)));
    ("type", fun () -> type' s);
    ("ctrl", fun () -> key (Ctrl (Char.chr (n land 255))));
    ("alt", fun () -> key (Alt (Char.chr (n land 255))));
    ("sleep", fun () -> sleep (Ms n));
    ("prompt timeout", fun () -> wait ~timeout:(Ms n) Prompt);
    ("line regexp", fun () -> wait (Line s));
    ("screen regexp", fun () -> wait ~timeout:(Ms n) (Screen s));
    ("screenshot", fun () -> screenshot s);
    ("source", fun () -> source s);
    ("tape source", fun () -> source (s ^ ".tape"));
    ("png path", fun () -> screenshot (s ^ ".png"));
    ("blank", fun () -> blank);
    ("hide", fun () -> hide);
    ("show", fun () -> show);
    ("prompt", fun () -> wait Prompt);
    ("raw", fun () -> raw s);
  ]

let test_builders_no_exception s t n =
  List.iter
    (fun (_, build) ->
      try ignore (str [ build () ]) with Invalid_argument _ -> ())
    (builders s t n);
  List.iter
    (fun key -> ignore (str [ Vhs.key key ]))
    Vhs.
      [
        Enter;
        Tab;
        Space;
        Backspace;
        Delete;
        Insert;
        Escape;
        Up;
        Down;
        Left;
        Right;
        Page_up;
        Page_down;
        Scroll_up;
        Scroll_down;
      ];
  match Vhs.Cram.parse s with
  | Error _ -> ()
  | Ok cram -> (
      try ignore (str (Vhs.Cram.items ~setup:[ t ] ~pause:(Ms n) cram))
      with Invalid_argument _ -> ())

let test_items_single_line s t n =
  List.iter
    (fun (name, build) ->
      match build () with
      | exception Invalid_argument _ -> ()
      | item ->
          let tape = str [ item ] in
          let line = String.sub tape 0 (String.length tape - 1) in
          if name = "type" then
            (* Type explicitly permits several statements when all three
               quotes occur. Each statement still needs a complete quoted
               payload on one physical line. *)
            String.split_on_char '\n' line
            |> List.iter (fun statement ->
                let len = String.length statement in
                check (len >= 7 && String.starts_with ~prefix:"Type " statement);
                let quote = statement.[5] in
                check (List.mem quote [ '\"'; '\''; '`' ]);
                check (statement.[len - 1] = quote);
                let payload = String.sub statement 6 (len - 7) in
                check (not (String.contains payload quote));
                check (not (String.contains payload '\r')))
          else if String.contains line '\n' || String.contains line '\r' then
            failf "%s accepted %S and emitted multiple lines: %S" name s tape)
    (builders s t n)

(* Independent decoder for the Type subset of VHS: no backslash escapes,
   and the matching quote ends the string. Concatenate split Type chunks. *)
let decode_type line =
  let n = String.length line in
  if n < 7 || not (String.starts_with ~prefix:"Type " line) then
    failf "not a Type line: %S" line;
  let quote = line.[5] in
  check (List.mem quote [ '"'; '\''; '`' ]);
  check (line.[n - 1] = quote);
  let payload = String.sub line 6 (n - 7) in
  check (not (String.contains payload quote));
  payload

let test_type_roundtrip text =
  match Vhs.type' text with
  | exception Invalid_argument _ -> ()
  | item ->
      let lines = String.split_on_char '\n' (str [ item ]) in
      let lines = List.filter (fun s -> s <> "") lines in
      let decoded = String.concat "" (List.map decode_type lines) in
      if decoded <> text then failf "Type roundtrip: %S became %S" text decoded

(* Make valid Unicode for generated shell commands without filtering out
   quotes. Invalid sequences consume one byte and become replacement text. *)
let vhs_utf8 s =
  let b = Buffer.create (String.length s) in
  let rec loop i =
    if i < String.length s then (
      let d = String.get_utf_8_uchar s i in
      let valid = Uchar.utf_decode_is_valid d in
      Buffer.add_utf_8_uchar b
        (if valid then Uchar.utf_decode_uchar d else Uchar.rep);
      loop (i + if valid then Uchar.utf_decode_length d else 1))
  in
  loop 0;
  Buffer.contents b

(* Map arbitrary bytes to typeable Unicode without dropping whitespace or
   quotes. This gives command-preservation tests meaningful accepted input. *)
let command_text s =
  String.map
    (fun c -> if Char.code c < 0x20 || c = '\x7f' then ' ' else c)
    (vhs_utf8 s)

let submitted_commands tape =
  let pending = Buffer.create 64 in
  let commands = ref [] in
  String.split_on_char '\n' tape
  |> List.iter (fun line ->
      if String.starts_with ~prefix:"Type " line then
        Buffer.add_string pending (decode_type line)
      else if line = "Enter" then (
        commands := Buffer.contents pending :: !commands;
        Buffer.clear pending));
  List.rev !commands

let test_cram_commands commands eol final_newline =
  let commands = List.map command_text commands in
  (* Empty output, prose, orphan continuations, and trailing spaces must
     never swallow the next command. Preserve empty commands too. *)
  let src =
    "prose" ^ eol ^ "  > orphan" ^ eol
    ^ String.concat
        (eol ^ "  " ^ eol)
        (List.map
           (fun cmd ->
             "  $ " ^ cmd ^ eol ^ "  > " ^ cmd ^ "  " ^ eol ^ "  " ^ eol
             ^ "  > ignored output continuation")
           commands)
    ^ if final_newline then eol else ""
  in
  match Vhs.Cram.parse src with
  | Error msg -> failf "typeable commands rejected: %s" msg
  | Ok cram ->
      let got = submitted_commands (str (Vhs.Cram.items cram)) in
      let expected =
        List.concat_map (fun cmd -> [ cmd; cmd ^ "  " ]) commands
      in
      if got <> expected then
        failf "commands lost in %S: expected %d, got %d" src
          (List.length expected) (List.length got)

let test_cram_arbitrary_commands src =
  match Vhs.Cram.parse src with
  | Error _ -> ()
  | Ok cram ->
      let starts =
        String.split_on_char '\n' src
        |> List.filter (String.starts_with ~prefix:"  $ ")
        |> List.map (fun line ->
            let last = String.length line - 1 in
            let stop = if line.[last] = '\r' then last else last + 1 in
            String.sub line 4 (stop - 4))
      in
      let got = submitted_commands (str (Vhs.Cram.items cram)) in
      (* Continuations submit additional lines, but every initial command
         must still occur in the same order. *)
      let rec subsequence expected actual =
        match (expected, actual) with
        | [], _ -> true
        | _, [] -> false
        | e :: es, a :: rest ->
            subsequence (if e = a then es else expected) rest
      in
      check (subsequence starts got)

let test_cram_tab_rejected text =
  match Vhs.Cram.parse ("  $ " ^ command_text text ^ "\targ\n") with
  | Error _ -> ()
  | Ok _ -> fail "parsed a command containing a tab that Type cannot represent"

let test_type_utf8 text =
  match Vhs.type' text with
  | exception Invalid_argument _ -> ()
  | _ ->
      if vhs_utf8 text <> text then
        failf "accepted invalid UTF-8 %S; VHS terminal input replaces its bytes"
          text

(* Item constructors document Invalid_argument; printers and parsers do not. *)
let accepted build law =
  match build () with
  | exception Invalid_argument _ -> ()
  | item -> law (str [ item ])

let quoted_payload prefix suffix tape =
  check (String.starts_with ~prefix tape);
  check (String.ends_with ~suffix tape);
  let first = String.length prefix in
  let last = String.length tape - String.length suffix in
  check (last >= first + 2);
  let q = tape.[first] in
  check (List.mem q [ '"'; '\''; '`' ]);
  check (tape.[last - 1] = q);
  String.sub tape (first + 1) (last - first - 2)

let test_builder_payloads s t =
  let open Vhs in
  accepted
    (fun () -> comment s)
    (fun tape -> check (tape = if s = "" then "#\n" else "# " ^ s ^ "\n"));
  accepted (fun () -> raw s) (fun tape -> check (tape = s ^ "\n"));
  List.iter
    (fun (prefix, build) ->
      accepted build (fun tape -> check (quoted_payload prefix "\n" tape = s)))
    [
      ("Require ", fun () -> require s);
      ("Output ", fun () -> output s);
      ("Screenshot ", fun () -> screenshot s);
      ("Source ", fun () -> source s);
      ("Set Shell ", fun () -> set (Shell s));
      ("Set FontFamily ", fun () -> set (Font_family s));
      ("Set Theme ", fun () -> set (Theme s));
    ];
  accepted
    (fun () -> env s t)
    (fun tape -> check (quoted_payload ("Env " ^ s ^ " ") "\n" tape = t));
  check (str [ blank; hide; show; wait Prompt ] = "\nHide\nShow\nWait\n")

let test_numeric_builders n =
  let open Vhs in
  List.iter
    (fun (prefix, build) ->
      if n < 0 then
        match build () with
        | exception Invalid_argument _ -> ()
        | _ -> failf "%s accepted negative %d" prefix n
      else check (str [ build () ] = prefix ^ string_of_int n ^ "\n"))
    [
      ("Set FontSize ", fun () -> set (Font_size n));
      ("Set Width ", fun () -> set (Width n));
      ("Set Height ", fun () -> set (Height n));
      ("Set Padding ", fun () -> set (Padding n));
      ("Set Framerate ", fun () -> set (Framerate n));
    ];
  let duration prefix build =
    accepted build (fun tape ->
        check (n >= 0);
        check (tape = prefix ^ string_of_int n ^ "ms\n"))
  in
  duration "Sleep " (fun () -> sleep (Ms n));
  duration "Set TypingSpeed " (fun () -> set (Typing_speed (Ms n)));
  duration "Wait@" (fun () -> wait ~timeout:(Ms n) Prompt)

let test_key_names c =
  let open Vhs in
  List.iter
    (fun (k, name) -> check (str [ key k ] = name ^ "\n"))
    [
      (Enter, "Enter");
      (Tab, "Tab");
      (Space, "Space");
      (Backspace, "Backspace");
      (Delete, "Delete");
      (Insert, "Insert");
      (Escape, "Escape");
      (Up, "Up");
      (Down, "Down");
      (Left, "Left");
      (Right, "Right");
      (Page_up, "PageUp");
      (Page_down, "PageDown");
      (Scroll_up, "ScrollUp");
      (Scroll_down, "ScrollDown");
    ];
  List.iter
    (fun (name, key) ->
      accepted
        (fun () -> Vhs.key key)
        (fun tape -> check (tape = name ^ "+" ^ String.make 1 c ^ "\n")))
    [ ("Ctrl", Ctrl c); ("Alt", Alt c) ]

let test_pp_composition texts =
  let items =
    List.filter_map
      (fun s ->
        match Vhs.type' s with
        | exception Invalid_argument _ -> None
        | item -> Some item)
      texts
  in
  check (str [] = "");
  check
    (str items = String.concat "" (List.map (fun item -> str [ item ]) items))

let test_pp_error s t n =
  List.iter
    (fun e -> ignore (Fmt.str "%a" Vhs.pp_error e))
    Vhs.
      [
        Executable_not_found s;
        Io s;
        Failed { vhs = s; status = `Exited n; stderr = t };
        Failed { vhs = s; status = `Signaled n; stderr = t };
      ];
  check (Fmt.str "%a" Vhs.pp_error (Vhs.Io s) = s)

let test_cram_eol commands =
  let commands = List.map command_text commands in
  let src eol = String.concat eol (List.map (fun s -> "  $ " ^ s) commands) in
  let convert s =
    match Vhs.Cram.parse s with
    | Error msg -> fail msg
    | Ok cram -> str (Vhs.Cram.items cram)
  in
  let lf = convert (src "\n") in
  check (lf = convert (src "\r\n"));
  check (lf = convert (src "\n" ^ "\n"));
  check (submitted_commands lf = commands)

let test_cram_pause src n =
  match Vhs.Cram.parse src with
  | Error _ -> ()
  | Ok cram ->
      let default = str (Vhs.Cram.items cram) in
      check (default = str (Vhs.Cram.items ~setup:[] ~pause:(Ms 1000) cram));
      if n >= 0 then (
        let tape = str (Vhs.Cram.items ~pause:(Ms n) cram) in
        check (submitted_commands tape = submitted_commands default);
        let sleeps =
          String.split_on_char '\n' tape
          |> List.filter (String.starts_with ~prefix:"Sleep ")
        in
        List.iter
          (fun line -> check (line = "Sleep " ^ string_of_int n ^ "ms"))
          sleeps)

let test_wait_agreement re n =
  let build w =
    match Vhs.wait ~timeout:(Ms n) w with
    | exception Invalid_argument _ -> None
    | item -> Some (str [ item ])
  in
  match (build (Line re), build (Screen re)) with
  | None, None -> ()
  | Some line, Some screen ->
      check (String.starts_with ~prefix:"Wait+Line" line);
      check (String.starts_with ~prefix:"Wait+Screen" screen);
      check
        (String.sub line 9 (String.length line - 9)
        = String.sub screen 11 (String.length screen - 11))
  | _ -> fail "Line and Screen disagree on accepted regexps or timeouts"

let test_cram_setup scripts text =
  let scripts = List.map command_text scripts in
  let command = command_text text in
  let cram =
    match Vhs.Cram.parse ("  $ " ^ command) with
    | Ok cram -> cram
    | Error msg -> fail msg
  in
  let tape = str (Vhs.Cram.items ~setup:scripts cram) in
  let shell_quote s =
    "'" ^ String.concat {|'\''|} (String.split_on_char '\'' s) ^ "'"
  in
  let expected =
    match scripts with
    | [] -> [ command ]
    | _ ->
        List.map (fun s -> "source " ^ shell_quote s) scripts
        @ [ "clear"; command ]
  in
  check (submitted_commands tape = expected);
  if scripts <> [] then (
    check (String.starts_with ~prefix:"Hide\n" tape);
    check
      (List.length (String.split_on_char '\n' tape |> List.filter (( = ) "Show"))
      = 1))

(* Cram.items has no documented exception, including for setup paths. *)
let test_cram_setup_total setup =
  match Vhs.Cram.parse "  $ true\n" with
  | Error msg -> fail msg
  | Ok cram -> ignore (str (Vhs.Cram.items ~setup cram))

let suite =
  ( "vhs",
    [
      test_case "cram items arbitrary setup does not raise"
        [ choose [ list bytes; const [ "\000" ] ] ]
        test_cram_setup_total;
      test_case "wait Line and Screen agree" [ bytes; int ] test_wait_agreement;
      test_case "cram setup preserves script order"
        [ list bytes; bytes ]
        test_cram_setup;
      test_case "builder payloads roundtrip" [ bytes; bytes ]
        test_builder_payloads;
      test_case "numeric settings preserve nonnegative values" [ int ]
        test_numeric_builders;
      test_case "keys preserve their names" [ char ] test_key_names;
      test_case "pp composes item lists" [ list bytes ] test_pp_composition;
      test_case "pp_error arbitrary variants" [ bytes; bytes; int ]
        test_pp_error;
      test_case "cram agrees across line endings" [ list bytes ] test_cram_eol;
      test_case "cram pause preserves commands and defaults" [ bytes; int ]
        test_cram_pause;
      test_case "parse arbitrary bytes" [ bytes ] test_parse_no_exception;
      test_case "builders only raise Invalid_argument" [ bytes; bytes; int ]
        test_builders_no_exception;
      test_case "accepted items are single lines"
        [ choose [ bytes; const {|a"b'c`d|} ]; bytes; int ]
        test_items_single_line;
      test_case "accepted Type text roundtrips"
        [ choose [ bytes; const "\xff"; const {|a"b'c`d|} ] ]
        test_type_roundtrip;
      test_case "cram preserves commands and continuations"
        [ list bytes; choose [ const "\n"; const "\r\n" ]; bool ]
        test_cram_commands;
      test_case "cram never loses an accepted command"
        [ choose [ bytes; map [ bytes ] (fun s -> "  $ " ^ s) ] ]
        test_cram_arbitrary_commands;
      test_case "cram reports untypeable tabs" [ bytes ] test_cram_tab_rejected;
      test_case "accepted Type payloads survive UTF-8 input"
        [ choose [ bytes; const "\xff" ] ]
        test_type_utf8;
    ] )
