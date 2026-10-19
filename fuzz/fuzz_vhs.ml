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
    (fun (name, build) ->
      try ignore (str [ build () ]) with
      | Invalid_argument _ -> ()
      | exn -> failf "%s raised %s" name (Printexc.to_string exn))
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
      try ignore (str (Vhs.Cram.items ~setup:[ t ] ~pause:(Ms n) cram)) with
      | Invalid_argument _ -> ()
      | exn -> failf "Cram.items raised %s" (Printexc.to_string exn))

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
    ] )
