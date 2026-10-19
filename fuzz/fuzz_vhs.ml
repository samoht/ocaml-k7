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

let suite =
  ( "vhs",
    [
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
