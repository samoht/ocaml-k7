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

let suite =
  ( "vhs",
    [
      test_case "parse arbitrary bytes" [ bytes ] test_parse_no_exception;
      test_case "builders only raise Invalid_argument" [ bytes; bytes; int ]
        test_builders_no_exception;
    ] )
