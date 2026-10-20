(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Native_test

let test_record () =
  with_dir @@ fun env cwd ->
  let t =
    tape
      Vhs.
        [
          type' "echo hello\n";
          wait Prompt;
          type' "printf '\\033[32mgreen\\033[0m\\n'\n";
          wait Prompt;
          sleep (Ms 100);
        ]
  in
  match record env cwd t with
  | Error e -> Alcotest.failf "%a" Vhs.pp_error e
  | Ok s ->
      let saved = Eio.Path.load Eio.Path.(cwd / "demo.svg") in
      Alcotest.(check string) "written to the output" s saved;
      check_contains "green" ~sub:{|class="p2">green|} s;
      Alcotest.(check string)
        "final screen"
        (screen
           [
             "> echo hello";
             "hello";
             "> printf '\\033[32mgreen\\033[0m\\n'";
             "green";
             ">";
           ])
        (last (frames s))

let test_timeout () =
  with_dir @@ fun env cwd ->
  let t =
    tape Vhs.[ type' "echo hi\n"; wait ~timeout:(Ms 300) (Screen "never") ]
  in
  match record env cwd t with
  | Error (Timeout { wait; last }) ->
      Alcotest.(check string) "wait" "Wait+Screen@300ms /never/" wait;
      check_contains "last screen" ~sub:"> echo hi\nhi" last
  | Ok _ -> Alcotest.fail "no timeout"
  | Error e -> Alcotest.failf "%a" Vhs.pp_error e

let test_hidden () =
  with_dir @@ fun env cwd ->
  let t =
    tape
      Vhs.
        [
          hidden [ type' "secret=1; clear\n"; wait Prompt ];
          type' "echo shown\n";
          wait Prompt;
          sleep (Ms 100);
        ]
  in
  match record env cwd t with
  | Error e -> Alcotest.failf "%a" Vhs.pp_error e
  | Ok s ->
      Alcotest.(check bool)
        "hidden typing not shown" false (contains ~sub:"secret" s);
      Alcotest.(check string)
        "final"
        (screen [ "> echo shown"; "shown"; ">" ])
        (last (frames s))

(* Scrolling shows the scrollback, and typing scrolls back down. *)
let test_scroll () =
  with_dir @@ fun env cwd ->
  let t =
    tape
      Vhs.
        [
          type' "seq 1 20\n";
          wait (Screen "20");
          wait Prompt;
          key Scroll_up;
          key Scroll_up;
          key Scroll_up;
          sleep (Ms 100);
        ]
  in
  match record env cwd t with
  | Error e -> Alcotest.failf "%a" Vhs.pp_error e
  | Ok s ->
      Alcotest.(check string)
        "scrolled"
        (String.concat "\n" (List.init 9 (fun i -> Int.to_string (i + 10))))
        (last (frames s))

(* Scrolling further up than the screen is high shows the scrollback in
   order. *)
let test_scroll_far () =
  with_dir @@ fun env cwd ->
  let t =
    tape
      Vhs.
        [
          type' "seq 1 30\n";
          wait (Line "^>$");
          key Scroll_up;
          raw "ScrollUp 11";
          sleep (Ms 100);
        ]
  in
  match record env cwd t with
  | Error e -> Alcotest.failf "%a" Vhs.pp_error e
  | Ok s ->
      Alcotest.(check string)
        "scrolled"
        (String.concat "\n" (List.init 9 (fun i -> Int.to_string (i + 11))))
        (last (frames s))

(* A screen wait reads the first lines of the buffer, as VHS's does: the
   scrollback first, of at most a thousand lines as in xterm.js, which
   [clear] empties. *)
let test_buffer () =
  with_dir @@ fun env cwd ->
  let shows re = Vhs.wait ~timeout:(Ms 5000) (Screen re) in
  let t =
    tape
      Vhs.
        [
          type' "seq 1 30\n";
          shows {|^> seq 1 30\n1\n2\n|};
          type' "seq 1 1100\n";
          shows {|^93\n94\n|};
          type' "seq 1 30; clear\n";
          shows {|^>\n\n|};
          type' "echo x\n";
          shows {|^> echo x\nx\n>|};
          key Scroll_up;
          sleep (Ms 100);
        ]
  in
  match record env cwd t with
  | Error e -> Alcotest.failf "%a" Vhs.pp_error e
  | Ok s ->
      Alcotest.(check string)
        "no scrollback after clear"
        (screen [ "> echo x"; "x"; ">" ])
        (last (frames s))

(* The lines [clear] erases stay erased once the scrollback is full. *)
let test_full_scrollback () =
  with_dir @@ fun env cwd ->
  let shows re = Vhs.wait ~timeout:(Ms 10000) (Screen re) in
  let t =
    tape
      Vhs.
        [
          type' "seq 1 9995; clear\n";
          shows {|^>\n\n|};
          type' "seq 1 1000\n";
          shows {|^> seq 1 1000\n1\n2\n|};
          sleep (Ms 100);
        ]
  in
  match record env cwd t with
  | Error e -> Alcotest.failf "%a" Vhs.pp_error e
  | Ok _ -> ()

(* After a scroll, a line wait reads the line as far above the cursor as
   the screen scrolled, as VHS does. *)
let test_scrolled_line () =
  with_dir @@ fun env cwd ->
  let t =
    tape
      Vhs.
        [
          type' "seq 1 30\n";
          wait (Line "^>$");
          key Scroll_up;
          wait ~timeout:(Ms 2000) (Line "^30$");
        ]
  in
  match record env cwd t with
  | Error e -> Alcotest.failf "%a" Vhs.pp_error e
  | Ok _ -> ()

(* A line ends at its last character that JavaScript's [trimEnd] keeps,
   which VHS calls on it: a no-break space is not one. *)
let test_trim () =
  with_dir @@ fun env cwd ->
  let t =
    tape
      Vhs.
        [
          type' "printf 'ab\\302\\240\\342\\200\\203'; sleep 3\n";
          wait ~timeout:(Ms 2000) (Line "^ab$");
        ]
  in
  match record env cwd t with
  | Error e -> Alcotest.failf "%a" Vhs.pp_error e
  | Ok _ -> ()

(* The text output holds the first lines of the buffer after each command
   VHS records, from the first action, or from the [Output] line that names
   it. *)
let test_text () =
  with_dir @@ fun env cwd ->
  let tape outputs actions =
    match
      Vhs.tape ~settings:(Vhs.Typing_speed (Ms 5) :: settings) ~outputs actions
    with
    | Ok t -> t
    | Error e -> Alcotest.fail e
  in
  let actions =
    Vhs.
      [
        hidden [ type' "x=1\n"; wait Prompt ];
        type' "sleep 1; echo hi";
        key Enter;
        wait Prompt;
      ]
  in
  let text path = screens (Eio.Path.load Eio.Path.(cwd / path)) in
  (match record env cwd (tape [ "demo.svg"; "demo.txt" ] actions) with
  | Error e -> Alcotest.failf "%a" Vhs.pp_error e
  | Ok _ -> (
      (* The echo of the typing can lag behind it, so the screens after the
         typing and the key are left out. *)
      match text "demo.txt" with
      | [ show; _; _; prompt ] ->
          Alcotest.(check string) "show" (screen [ "> x=1"; ">" ]) show;
          Alcotest.(check string)
            "prompt"
            (screen [ "> x=1"; "> sleep 1; echo hi"; "hi"; ">" ])
            prompt
      | l -> Alcotest.failf "%d screens" (List.length l)));
  match
    record env cwd
      (tape [ "demo.svg" ]
         (Vhs.raw "Sleep 0" :: Vhs.raw "Output \"later.ascii\"" :: actions))
  with
  | Error e -> Alcotest.failf "%a" Vhs.pp_error e
  | Ok _ ->
      Alcotest.(check int)
        "from the output" 5
        (List.length (text "later.ascii"))

(* A window bar takes the top of the window, with VHS's dots or rings. *)
let test_window_bar () =
  with_dir @@ fun env cwd ->
  let bar raws =
    match record env cwd (tape (List.map Vhs.raw raws @ [ Vhs.type' "x" ])) with
    | Ok s -> s
    | Error e -> Alcotest.failf "%a" Vhs.pp_error e
  in
  let s = bar [ "Set WindowBar Colorful" ] in
  List.iter
    (fun sub -> check_contains "dot" ~sub s)
    [
      {|<circle cx="15" cy="15" r="4.5" fill="#ff4f4d"/>|};
      {|<circle cx="30" cy="15" r="4.5" fill="#febb00"/>|};
      {|<circle cx="45" cy="15" r="4.5" fill="#00cc1d"/>|};
      (* 30 pixels fewer for the terminal: 8 rows of 29 pixels. *)
      {|<svg x="60" y="90" width="644" height="232">|};
    ];
  let s =
    bar [ "Set WindowBar RingsRight"; "Set WindowBarSize 40"; "Set Margin 5" ]
  in
  List.iter
    (fun sub -> check_contains "ring" ~sub s)
    [
      {|<circle cx="775" cy="25" r="7.5" fill="#333333"/>|};
      {|<circle cx="775" cy="25" r="5.5" class="e"/>|};
      {|<circle cx="753" cy="25" r="7.5" fill="#333333"/>|};
      {|<svg x="65" y="105" |};
    ]

(* The margin is an image, stretched to the frame, or none without a
   fill. *)
let test_margin_fill () =
  with_dir @@ fun env cwd ->
  Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(cwd / "m.png") "abcd";
  let play raws =
    match record env cwd (tape (List.map Vhs.raw raws @ [ Vhs.type' "x" ])) with
    | Ok s -> s
    | Error e -> Alcotest.failf "%a" Vhs.pp_error e
  in
  let s = play [ "Set Margin 20"; {|Set MarginFill "m.png"|} ] in
  check_contains "image"
    ~sub:
      {|<image width="800" height="400" preserveAspectRatio="none" href="data:image/png;base64,YWJjZA=="/>|}
    s;
  check_contains "inside the margin" ~sub:{|<svg x="80" y="80" |} s;
  let s = play [ "Set Margin 20"; {|Set MarginFill ""|} ] in
  check_contains "no margin" ~sub:{|<svg x="60" y="60" |} s;
  Alcotest.(check bool) "no image" false (contains ~sub:"<image" s)

(* zsh plays natively, with VHS's prompt. *)
let test_zsh () =
  with_dir @@ fun env cwd ->
  let t =
    match
      Vhs.tape
        ~settings:(Shell Zsh :: Typing_speed (Ms 5) :: settings)
        ~outputs:[ "demo.svg" ]
        Vhs.[ type' "echo $ZSH_NAME\n"; wait Prompt; sleep (Ms 100) ]
    with
    | Ok t -> t
    | Error e -> Alcotest.fail e
  in
  match record env cwd t with
  | Error e -> Alcotest.failf "%a" Vhs.pp_error e
  | Ok s ->
      check_contains "prompt colour" ~sub:{|fill="#5b56e0">&gt;|} s;
      Alcotest.(check string)
        "final"
        (screen [ "> echo $ZSH_NAME"; "zsh"; ">" ])
        (last (frames s))

(* fish plays natively, started as VHS starts it: no greeting, VHS's
   prompt and its typed command. fish is in CI; the test skips where it is
   not on the [PATH]. *)
let test_fish () =
  let on_path =
    List.exists
      (fun dir -> dir <> "" && Sys.file_exists (Filename.concat dir "fish"))
      (String.split_on_char ':'
         (Option.value (Sys.getenv_opt "PATH") ~default:""))
  in
  if not on_path then Alcotest.skip ();
  with_dir @@ fun env cwd ->
  let t =
    match
      Vhs.tape
        ~settings:(Shell Fish :: Typing_speed (Ms 5) :: settings)
        ~outputs:[ "demo.svg" ]
        Vhs.
          [
            type' "echo fish-$status\n";
            wait ~timeout:(Ms 10000) (Screen "(?m)^fish-0$");
            wait Prompt;
            sleep (Ms 100);
          ]
    with
    | Ok t -> t
    | Error e -> Alcotest.fail e
  in
  match record env cwd t with
  | Error e -> Alcotest.failf "%a" Vhs.pp_error e
  | Ok s ->
      let text = last (frames s) in
      Alcotest.(check bool) "no greeting" false (contains ~sub:"Welcome" text);
      check_contains "command" ~sub:"> echo fish-$status" text

(* A program that asks the terminal for its attributes gets xterm.js's
   answer. *)
let test_replies () =
  with_dir @@ fun env cwd ->
  let t =
    tape
      Vhs.
        [
          type' {|printf '\033[c'; IFS= read -rsd c da; echo "da${da#*[}"|};
          key Enter;
          wait ~timeout:(Ms 5000) (Screen {|(?m)^da\?1;2$|});
        ]
  in
  match record env cwd t with
  | Error e -> Alcotest.failf "%a" Vhs.pp_error e
  | Ok _ -> ()

(* Keys send what xterm sends: the line editor sees arrows, control and alt
   chords. *)
let test_keys () =
  with_dir @@ fun env cwd ->
  let t =
    tape
      Vhs.
        [
          type' "echo ac";
          key Left;
          type' "b";
          key (Ctrl 'a');
          key (Alt 'f');
          type' " X";
          key Enter;
          wait Prompt;
          sleep (Ms 100);
        ]
  in
  match record env cwd t with
  | Error e -> Alcotest.failf "%a" Vhs.pp_error e
  | Ok s ->
      Alcotest.(check string)
        "edited"
        (screen [ "> echo X abc"; "X abc"; ">" ])
        (last (frames s))

let test_unsupported () =
  with_dir @@ fun env cwd ->
  let t =
    match
      Vhs.tape ~settings:[ Shell Nushell ] ~outputs:[ "demo.svg" ]
        [ Vhs.type' "x" ]
    with
    | Ok t -> t
    | Error e -> Alcotest.fail e
  in
  match record env cwd t with
  | Error (Unsupported why) -> check_contains "names the line" ~sub:"Shell" why
  | Ok _ -> Alcotest.fail "played nushell"
  | Error e -> Alcotest.failf "%a" Vhs.pp_error e

(* A screenshot is a still SVG image of the next frame, also when it is
   taken while hidden, for the path with [.svg] in place of [.png]. *)
let test_screenshot () =
  with_dir @@ fun env cwd ->
  let t =
    tape
      Vhs.
        [
          hidden
            [ raw "Screenshot \"hidden.png\""; type' "true\n"; wait Prompt ];
          type' "echo one\n";
          wait Prompt;
          raw "Screenshot \"shot.png\"";
          type' "echo two\n";
          wait Prompt;
        ]
  in
  match record env cwd t with
  | Error e -> Alcotest.failf "%a" Vhs.pp_error e
  | Ok _ ->
      let still name = Eio.Path.load Eio.Path.(cwd / name) in
      let s = still "shot.svg" in
      Alcotest.(check (list string))
        "still"
        [ screen [ "> true"; "> echo one"; "one"; ">" ] ]
        (frames s);
      Alcotest.(check bool) "no blink" false (contains ~sub:".k{animation" s);
      Alcotest.(check (list string))
        "hidden"
        [ screen [ "> true"; ">" ] ]
        (frames (still "hidden.svg"))

(* A Source line plays the lines of its tape but their outputs, and is
   refused where VHS refuses it. *)
let test_source () =
  with_dir @@ fun env cwd ->
  let file name contents =
    Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(cwd / name) contents
  in
  file "inc.tape"
    "Output \"inc.txt\"\n# a comment\nType \"echo sourced\"\nEnter\nWait\n";
  file "nested.tape" "Source \"inc.tape\"\n";
  file "empty.tape" "";
  file "bad.tape" "Type\n";
  let play name =
    record env cwd (tape Vhs.[ raw ("Source " ^ name); sleep (Ms 100) ])
  in
  (match play {|"inc.tape"|} with
  | Ok s ->
      Alcotest.(check string)
        "sourced"
        (screen [ "> echo sourced"; "sourced"; ">" ])
        (last (frames s));
      Alcotest.(check bool)
        "no output" false
        (Sys.file_exists
           (Option.get (Eio.Path.native Eio.Path.(cwd / "inc.txt"))))
  | Error e -> Alcotest.failf "%a" Vhs.pp_error e);
  List.iter
    (fun (name, sub) ->
      match play name with
      | Error (Unsupported why | Io why) -> check_contains name ~sub why
      | Ok _ -> Alcotest.failf "%s played" name
      | Error e -> Alcotest.failf "%a" Vhs.pp_error e)
    [
      ({|"nested.tape"|}, "nested");
      ({|"empty.tape"|}, "empty");
      ({|"bad.tape"|}, "Type");
      ({|"missing.tape"|}, "missing.tape");
    ]

(* [after_settings tape txt] is [txt], a text output of VHS for [tape],
   without the screens it reads while it applies the settings, before it
   sizes the terminal: one per [Set], [Output] and [Require] line from the
   line of the text output, but [Set Shell]. *)
let after_settings tape txt =
  let lines = String.split_on_char '\n' (Vhs.to_string tape) in
  let rec header = function
    | l :: rest when String.starts_with ~prefix:"Set Shell " l -> header rest
    | l :: rest
      when List.exists
             (fun prefix -> String.starts_with ~prefix l)
             [ "Set "; "Output "; "Require " ] ->
        1 + header rest
    | _ -> 0
  in
  let rec from = function
    | l :: rest when String.ends_with ~suffix:".txt\"" l -> l :: rest
    | _ :: rest -> from rest
    | [] -> []
  in
  let n = header (from lines) in
  let sep = "\n" ^ separator ^ "\n" in
  let rec skip k i =
    if k = 0 then String.sub txt i (String.length txt - i)
    else
      let rec find j =
        if j + String.length sep > String.length txt then
          Alcotest.fail "fewer screens than settings"
        else if String.sub txt j (String.length sep) = sep then
          j + String.length sep
        else find (j + 1)
      in
      skip (k - 1) (find (max 0 (i - 1)))
  in
  skip n 0

(* [same_text env cwd t] records [t] with VHS and natively and checks
   that the native text output is VHS's, byte for byte, but for the screens
   VHS reads before it sizes the terminal. It is the native image. *)
let same_text env cwd t =
  let clock = Eio.Stdenv.mono_clock env and proc = Eio.Stdenv.process_mgr env in
  (match Vhs.render ?vhs:oracle ~clock ~proc ~cwd t with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%a" Vhs.pp_error e);
  let vhs = Eio.Path.load Eio.Path.(cwd / "oracle.txt") in
  match record env cwd t with
  | Error e -> Alcotest.failf "%a" Vhs.pp_error e
  | Ok s ->
      let native = Eio.Path.load Eio.Path.(cwd / "oracle.txt") in
      Alcotest.(check string) "text output" (after_settings t vhs) native;
      (native, s)

let oracle_tape ?(settings = []) actions =
  match
    Vhs.tape ~settings
      ~outputs:[ "oracle.txt"; "oracle.svg" ]
      (Vhs.raw "Set Columns 60" :: Vhs.raw "Set Rows 12" :: actions)
  with
  | Ok t -> t
  | Error e -> Alcotest.fail e

(* For a 60 x 12 terminal, with colours, a wide character, a combining mark,
   a trailing no-break space and scrolling. *)
let test_oracle () =
  with_dir @@ fun env cwd ->
  let t =
    oracle_tape
      Vhs.
        [
          type' "sleep 1; echo hello";
          key Enter;
          wait Prompt;
          type'
            "sleep 1; printf '\\033[31mred\\033[0m wide: \\346\\227\\245 \
             e\\314\\201nd\\302\\240\\n'";
          key Enter;
          wait Prompt;
          type' "sleep 1; seq 1 20";
          key Enter;
          wait Prompt;
          sleep (Ms 500);
        ]
  in
  let native, s = same_text env cwd t in
  Alcotest.(check int) "screens" 10 (List.length (screens native));
  match Svg_check.frames ~cell_width ~cell_height ~rows:12 s with
  | Error e -> Alcotest.fail e
  | Ok frames ->
      Alcotest.(check string)
        "final screen"
        (String.concat "\n"
           (List.init 11 (fun i -> Int.to_string (i + 10)) @ [ ">" ]))
        (last frames)

(* xterm's controls, which the native screen emulates where matrix 0.1.0
   lacks them: raw line feeds and indexes, a reverse index, scroll regions,
   line edits, character erases and repeats, no auto-wrap, origin mode, the
   cursor in the last column and DEC line drawing. *)
let test_oracle_controls () =
  with_dir @@ fun env cwd ->
  (* Each printf leaves the cursor at the start of the cleared last row,
     for the prompt. *)
  let printf s =
    Vhs.type'
      ("sleep 1; stty -onlcr; printf '" ^ s ^ {|\033[12;1H\033[K'; stty onlcr|})
  in
  let t =
    oracle_tape
      Vhs.
        [
          type' "sleep 1; clear";
          key Enter;
          wait Prompt;
          printf
            {|ab\ncd\033Dx\033Ey\r\n\033[2Mz\033[2;4r\033[4;1Hq\033Mr\033[r|};
          key Enter;
          wait Prompt;
          printf {|\033[8;1H12345\033[3D\033[2X\033[1b\033[L\033(0lqqk\033(B|};
          key Enter;
          wait Prompt;
          printf
            {|\033[?7l\033[10;55Habcdefghij\033[?7h\033[?6h\033[5;8r\033[2;3Ho\033[?6l\033[r|};
          key Enter;
          wait Prompt;
          printf {|\033[11;59Hxyz\bW\033[60GV|};
          key Enter;
          wait Prompt;
          sleep (Ms 500);
        ]
  in
  ignore (same_text env cwd t)

(* zsh, started as VHS starts it. *)
let test_oracle_zsh () =
  with_dir @@ fun env cwd ->
  let t =
    oracle_tape ~settings:[ Shell Zsh ]
      Vhs.
        [
          type' "sleep 1; echo hello";
          key Enter;
          wait Prompt;
          type' "print -P '%F{red}red%f'";
          key Enter;
          wait Prompt;
          sleep (Ms 500);
        ]
  in
  ignore (same_text env cwd t)

(* Stable screens isolate terminal interpretation from echo timing. The same
   tape, when VHS_ORACLE is set, checks the expected text against real VHS
   before checking both outputs of the native recorder. *)
let test_terminal_state () =
  with_dir @@ fun env cwd ->
  let command =
    String.concat ""
      [
        {|printf '\033[2J\033[H\033(0\0337\033(B\0338q\033(B|};
        {|\033[2;1Ha\tX\033[3g\r\tY|};
        {|\033[3;1H\344\270\255ab\r\033[P\033[10;1H'|};
      ]
  in
  let t =
    oracle_tape ~settings:[ Typing_speed (Ms 1) ]
      Vhs.[ hidden [ type' command; key Enter; wait Prompt ]; sleep (Ms 100) ]
  in
  let expected =
    String.concat "\n"
      ([
         "\xe2\x94\x80";
         "a" ^ String.make 7 ' ' ^ "X" ^ String.make 50 ' ' ^ "Y";
         " ab";
       ]
      @ List.init 6 (fun _ -> "")
      @ [ ">"; ""; "" ])
  in
  let text () = last_screen (Eio.Path.load Eio.Path.(cwd / "oracle.txt")) in
  (match oracle with
  | None -> ()
  | Some vhs -> (
      match
        Vhs.render ~vhs
          ~clock:(Eio.Stdenv.mono_clock env)
          ~proc:(Eio.Stdenv.process_mgr env)
          ~cwd t
      with
      | Error e -> Alcotest.failf "%a" Vhs.pp_error e
      | Ok () -> Alcotest.(check string) "VHS oracle" expected (text ())));
  match record env cwd t with
  | Error e -> Alcotest.failf "%a" Vhs.pp_error e
  | Ok s -> (
      match Svg_check.frames ~cell_width ~cell_height ~rows:12 s with
      | Error e -> Alcotest.fail e
      | Ok images ->
          Alcotest.(check (pair string string))
            "native text and SVG" (expected, expected)
            (text (), last images))

(* The expected screen is checked against real VHS when VHS_ORACLE is set.
   Keep the prompt below the payload so shell echo cannot affect its cells. *)
let check_terminal_payload payload lines =
  with_dir @@ fun env cwd ->
  let command = "printf '\\033[2J\\033[H" ^ payload ^ "\\033[10;1H'" in
  let t =
    oracle_tape ~settings:[ Typing_speed (Ms 1) ]
      Vhs.[ hidden [ type' command; key Enter; wait Prompt ]; sleep (Ms 200) ]
  in
  let expected =
    String.concat "\n"
      (lines @ List.init (9 - List.length lines) (fun _ -> "") @ [ ">"; ""; "" ])
  in
  let text () = last_screen (Eio.Path.load Eio.Path.(cwd / "oracle.txt")) in
  (match oracle with
  | None -> ()
  | Some vhs -> (
      match
        Vhs.render ~vhs
          ~clock:(Eio.Stdenv.mono_clock env)
          ~proc:(Eio.Stdenv.process_mgr env)
          ~cwd t
      with
      | Error e -> Alcotest.failf "%a" Vhs.pp_error e
      | Ok () -> Alcotest.(check string) "VHS oracle" expected (text ())));
  match record env cwd t with
  | Error e -> Alcotest.failf "%a" Vhs.pp_error e
  | Ok image -> (
      match Svg_check.frames ~cell_width ~cell_height ~rows:12 image with
      | Error e -> Alcotest.fail e
      | Ok images ->
          Alcotest.(check (pair string string))
            "native text and SVG" (expected, expected)
            (text (), last images))

(* REP repeats the complete preceding cell, including its combining marks. *)
let test_repeat_combining () =
  check_terminal_payload {|a\314\201\033[3b|}
    [ String.concat "" (List.init 4 (fun _ -> "a\xcc\x81")) ]

(* The UTF-8 encodings of C1 CSI, OSC and ST have their control meanings,
   just like ESC [, ESC ] and ESC backslash in xterm.js. *)
let test_c1_controls () =
  check_terminal_payload {|a\302\2332Cb\302\235title\302\234c|} [ "a  bc" ]

(* Erasure does not consume a pending wrap. ED starts beyond the final cell;
   EL 1 clears the row, but the following glyph still wraps to the next row. *)
let test_margin_erasure () =
  check_terminal_payload {|\033[60GX\033[JY\033[3;60HX\033[1KY|}
    [ String.make 59 ' ' ^ "X"; "Y"; ""; "Y" ]

(* Unlike 1049, modes 47 and 1047 share the cursor between buffers. *)
let test_alternate_cursor () =
  check_terminal_payload
    {|abc\033[?47hALT\033[?47lX\033[2;1Habc\033[?1047hALT\033[?1047lX|}
    [ "abc   X"; "abc   X" ]

let suite =
  ( "native",
    List.map
      (fun (name, fn) -> Alcotest.test_case name `Quick fn)
      ([
         ("record", test_record);
         ("timeout", test_timeout);
         ("hidden", test_hidden);
         ("scroll", test_scroll);
         ("scroll far", test_scroll_far);
         ("buffer", test_buffer);
         ("full scrollback", test_full_scrollback);
         ("scrolled line", test_scrolled_line);
         ("trim", test_trim);
         ("text", test_text);
         ("window bar", test_window_bar);
         ("margin fill", test_margin_fill);
         ("zsh", test_zsh);
         ("fish", test_fish);
         ("replies", test_replies);
         ("screenshot", test_screenshot);
         ("source", test_source);
         ("keys", test_keys);
         ("unsupported", test_unsupported);
         ("terminal state recording", test_terminal_state);
         ("repeat combining cell", test_repeat_combining);
         ("C1 terminal controls", test_c1_controls);
         ("right-margin erasure", test_margin_erasure);
         ("alternate-screen shared cursor", test_alternate_cursor);
       ]
      @
      if Option.is_some oracle then
        [
          ("vhs oracle", test_oracle);
          ("vhs oracle zsh", test_oracle_zsh);
          ("vhs oracle controls", test_oracle_controls);
        ]
      else []) )
