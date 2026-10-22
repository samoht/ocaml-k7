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

(* A prompt wait must observe the prompt after the submitted command, even
   when typing has no delay. The leading sleep gives the tape real frames. *)
let test_zero_speed_prompt () =
  with_dir @@ fun env cwd ->
  let t =
    match
      Vhs.tape
        ~settings:(Typing_speed (Ms 0) :: settings)
        ~outputs:[ "demo.txt" ]
        Vhs.[ sleep (Ms 100); type' "echo READY\n"; wait Prompt ]
    with
    | Ok t -> t
    | Error e -> Alcotest.failf "%a" Vhs.pp_error e
  in
  (match play env cwd t with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%a" Vhs.pp_error e);
  check_contains "completed command before the prompt wait returns"
    ~sub:"\nREADY\n>"
    (last_screen (Eio.Path.load Eio.Path.(cwd / "demo.txt")))

(* A screenshot of demo.png is a PNG image, which only render writes:
   record refuses the tape before it plays, rather than write a still to
   demo.svg, the path of the recording. *)
let test_screenshot_clash () =
  with_dir @@ fun env cwd ->
  let t =
    tape
      Vhs.
        [
          type' "echo hi\n";
          wait Prompt;
          raw "Screenshot \"demo.png\"";
          type' "echo again\n";
          wait Prompt;
          sleep (Ms 100);
        ]
  in
  match play env cwd t with
  | Error (Unsupported _) ->
      Alcotest.(check bool)
        "nothing written" false
        (Eio.Path.is_file Eio.Path.(cwd / "demo.svg"))
  | Error e -> Alcotest.failf "%a" Vhs.pp_error e
  | Ok () ->
      Alcotest.failf "Ok, and demo.svg is the still:\n%s"
        (Eio.Path.load Eio.Path.(cwd / "demo.svg"))

let test_timeout () =
  with_dir @@ fun env cwd ->
  let t =
    tape Vhs.[ type' "echo hi\n"; wait ~timeout:(Ms 300) (Screen "never") ]
  in
  match record env cwd t with
  | Error (Timeout { wait; line; last } as e) ->
      Alcotest.(check string) "wait" "Wait+Screen@300ms /never/" wait;
      (* The line of [Vhs.to_string t]: settings, then Type and Enter. *)
      Alcotest.(check string)
        "line" "Wait+Screen@300ms /never/"
        (List.nth (String.split_on_char '\n' (Vhs.to_string t)) (line - 1));
      check_contains "last screen" ~sub:"> echo hi\nhi" last;
      check_contains "names the line" ~sub:(Fmt.str "line %d," line)
        (Fmt.str "%a" Vhs.pp_error e);
      check_contains "how to wait longer"
        ~sub:"~timeout or the tape a Set WaitTimeout"
        (Fmt.str "%a" Vhs.pp_error e)
  | Ok _ -> Alcotest.fail "no timeout"
  | Error e -> Alcotest.failf "%a" Vhs.pp_error e

(* A shell that exits while a wait waits for it fails the wait at once,
   with its status, not after the wait's timeout. *)
let test_shell_exit () =
  with_dir @@ fun env cwd ->
  let t = tape Vhs.[ type' "exit 3\n"; wait ~timeout:(Ms 20_000) Prompt ] in
  let start = Unix.gettimeofday () in
  (match record env cwd t with
  | Error (Exited { status = `Exited 3; wait = "Wait@20000ms"; _ } as e) ->
      check_contains "message" ~sub:"the shell exited (status 3) while waiting"
        (Fmt.str "%a" Vhs.pp_error e)
  | Ok _ -> Alcotest.fail "no error"
  | Error e -> Alcotest.failf "%a" Vhs.pp_error e);
  Alcotest.(check bool) "at once" true (Unix.gettimeofday () -. start < 10.)

(* A wait's own timeout takes precedence over a raw [Set WaitTimeout], which
   the other waits take. *)
let test_timeout_precedence () =
  with_dir @@ fun env cwd ->
  let times_out ~within t =
    let start = Unix.gettimeofday () in
    (match record env cwd t with
    | Error (Timeout _) -> ()
    | Ok _ -> Alcotest.fail "no timeout"
    | Error e -> Alcotest.failf "%a" Vhs.pp_error e);
    Alcotest.(check bool)
      "timed out in time" true
      (Unix.gettimeofday () -. start < within)
  in
  times_out ~within:30.
    (tape
       Vhs.
         [
           raw "Set WaitTimeout 60s";
           type' "echo hi\n";
           wait ~timeout:(Ms 300) (Screen "never");
         ]);
  times_out ~within:30.
    (tape
       Vhs.
         [
           raw "Set WaitTimeout 300ms"; type' "echo hi\n"; wait (Screen "never");
         ])

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
    | Error e -> Alcotest.failf "%a" Vhs.pp_error e
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
  Eio.Path.save ~create:(`Or_truncate 0o644)
    Eio.Path.(cwd / "m.png")
    "\x89PNG\r\n\x1a\nabcd";
  let play raws =
    match record env cwd (tape (List.map Vhs.raw raws @ [ Vhs.type' "x" ])) with
    | Ok s -> s
    | Error e -> Alcotest.failf "%a" Vhs.pp_error e
  in
  let s = play [ "Set Margin 20"; {|Set MarginFill "m.png"|} ] in
  check_contains "image"
    ~sub:
      {|<image width="800" height="400" preserveAspectRatio="none" href="data:image/png;base64,iVBORw0KGgphYmNk"/>|}
    s;
  check_contains "inside the margin" ~sub:{|<svg x="80" y="80" |} s;
  let s = play [ "Set Margin 20"; {|Set MarginFill ""|} ] in
  check_contains "no margin" ~sub:{|<svg x="60" y="60" |} s;
  Alcotest.(check bool) "no image" false (contains ~sub:"<image" s)

(* The image of a margin is a regular file, followed through symlinks, of
   at most 10 MiB, with the signature of an image a browser draws; a
   [Source] tape is a regular file too. Anything else is refused before
   playing, without blocking on a FIFO. *)
let test_margin_fill_refused () =
  with_dir @@ fun env cwd ->
  let dir = Option.get (Eio.Path.native cwd) in
  let file name = Filename.concat dir name in
  Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(cwd / "text.png") "hi";
  Eio.Path.save ~create:(`Or_truncate 0o644)
    Eio.Path.(cwd / "big.png")
    ("\x89PNG\r\n\x1a\n" ^ String.make (10 * 1024 * 1024) 'x');
  Unix.symlink "/etc/hosts" (file "leak.png");
  Unix.symlink dir (file "dir.png");
  Unix.mkfifo (file "fifo.png") 0o644;
  Unix.mkfifo (file "fifo.tape") 0o644;
  let played = Eio.Path.(cwd / "played") in
  List.iter
    (fun (raw, sub) ->
      let t =
        tape
          [ Vhs.raw "Set Margin 20"; Vhs.raw raw; Vhs.type' "touch played\n" ]
      in
      match record env cwd t with
      | Error (Invalid msg) ->
          check_contains raw ~sub msg;
          Alcotest.(check bool) "not played" false (Eio.Path.is_file played)
      | Ok _ -> Alcotest.failf "%s played" raw
      | Error e -> Alcotest.failf "%s: %a" raw Vhs.pp_error e)
    [
      ({|Set MarginFill "text.png"|}, "not a PNG, JPEG, GIF, WebP or BMP image");
      ({|Set MarginFill "leak.png"|}, "not a PNG, JPEG, GIF, WebP or BMP image");
      ({|Set MarginFill "big.png"|}, "larger than 10 MiB");
      ({|Set MarginFill "dir.png"|}, "not a regular file");
      ({|Set MarginFill "fifo.png"|}, "not a regular file");
      ({|Source "fifo.tape"|}, "not a regular file");
    ]

(* Each engine reports an output it cannot write before it plays: a
   [Screenshot] needs its directory, and the other outputs a directory VHS
   can create, as it creates them. *)
let test_output_dirs () =
  with_dir @@ fun env cwd ->
  let dir = Option.get (Eio.Path.native cwd) in
  Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(cwd / "file") "";
  Eio.Path.mkdir ~perm:0o555 Eio.Path.(cwd / "ro");
  let played = Eio.Path.(cwd / "played") in
  let fake = Filename.concat dir "fake-vhs" in
  Eio.Path.save ~create:(`Or_truncate 0o755)
    Eio.Path.(cwd / "fake-vhs")
    (Fmt.str "#!/bin/sh\ncat > /dev/null\ntouch %s\n"
       (Filename.quote (Filename.concat dir "played")));
  let refused name ~engine ~outputs ~raws sub =
    let t =
      match
        Vhs.tape ~settings ~outputs
          (List.map Vhs.raw raws @ Vhs.[ type' "touch played\n"; wait Prompt ])
      with
      | Ok t -> t
      | Error e -> Alcotest.failf "%s: %a" name Vhs.pp_error e
    in
    match engine t with
    | Error (Vhs.Io msg) ->
        check_contains name ~sub msg;
        Alcotest.(check bool)
          (name ^ ": not played") false (Eio.Path.is_file played)
    | Ok () -> Alcotest.failf "%s: Ok" name
    | Error e -> Alcotest.failf "%s: %a" name Vhs.pp_error e
  in
  let record = play env cwd in
  let render t =
    Vhs.render ~vhs:fake
      ~clock:(Eio.Stdenv.mono_clock env)
      ~proc:(Eio.Stdenv.process_mgr env)
      ~cwd t
  in
  refused "record, file" ~engine:record ~outputs:[ "file/demo.svg" ] ~raws:[]
    "file/demo.svg: file is not a directory";
  refused "record, read-only" ~engine:record ~outputs:[ "ro/new/demo.svg" ]
    ~raws:[] "ro/new/demo.svg: cannot write in ro";
  refused "record, screenshot" ~engine:record ~outputs:[ "demo.svg" ]
    ~raws:[ {|Screenshot "none/shot.svg"|} ]
    "none/shot.svg: no directory none";
  refused "render, read-only" ~engine:render ~outputs:[ "ro/demo.gif" ] ~raws:[]
    "ro/demo.gif: cannot write in ro";
  refused "render, screenshot" ~engine:render ~outputs:[ "demo.gif" ]
    ~raws:[ {|Screenshot "none/shot.png"|} ]
    "none/shot.png: no directory none";
  refused "render, frames" ~engine:render ~outputs:[ "none/frames/" ] ~raws:[]
    "none/frames/: no directory none";
  (* VHS creates the directories of the other outputs, and so does record. *)
  let t =
    match
      Vhs.tape ~settings
        ~outputs:[ "a/b/demo.svg"; "c/demo.txt" ]
        Vhs.[ type' "true\n"; wait Prompt ]
    with
    | Ok t -> t
    | Error e -> Alcotest.failf "%a" Vhs.pp_error e
  in
  (match record t with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%a" Vhs.pp_error e);
  Alcotest.(check bool)
    "created" true
    (Eio.Path.is_file Eio.Path.(cwd / "a" / "b" / "demo.svg")
    && Eio.Path.is_file Eio.Path.(cwd / "c" / "demo.txt"))

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
    | Error e -> Alcotest.failf "%a" Vhs.pp_error e
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
    | Error e -> Alcotest.failf "%a" Vhs.pp_error e
  in
  match record env cwd t with
  | Error e -> Alcotest.failf "%a" Vhs.pp_error e
  | Ok s ->
      let text = last (frames s) in
      Alcotest.(check bool) "no greeting" false (contains ~sub:"Welcome" text);
      check_contains "command" ~sub:"> echo fish-$status" text

(* A program that asks the terminal for its attributes gets its
   answer. The terminal answers as soon as it reads the query, so echo is
   off before it, as a program sets it: the tty would otherwise echo a
   reply that arrives before [read] turns echo off itself. *)
let test_replies () =
  with_dir @@ fun env cwd ->
  let t =
    tape
      Vhs.
        [
          type'
            {|stty -echo; printf '\033[c'; IFS= read -rd c da; stty echo; echo "da${da#*[}"|};
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

(* Settings whose sizes or times leave the integer range are refused, or
   reduced to the loop they offset, rather than wrapping around. *)
let test_bounds () =
  with_dir @@ fun env cwd ->
  let play raws =
    match
      Vhs.tape ~outputs:[ "demo.svg" ]
        (List.map Vhs.raw raws @ [ Vhs.type' "x" ])
    with
    | Ok t -> record env cwd t
    | Error e -> Alcotest.failf "%a" Vhs.pp_error e
  in
  List.iter
    (fun raws ->
      match play raws with
      | Error (Unsupported _) -> ()
      | Ok _ -> Alcotest.failf "%s played" (String.concat "; " raws)
      | Error e -> Alcotest.failf "%a" Vhs.pp_error e)
    [
      [ "Set LetterSpacing 1000000000000000000"; "Set Columns 2000" ];
      [ "Set LineHeight 1000000000000000000" ];
      [ "Set Columns 2000"; "Set LetterSpacing 100" ];
      [ "Set PlaybackSpeed 0.0000000001" ];
    ];
  (match play [ "Set LoopOffset 1" ^ String.make 300 '0' ] with
  | Ok s ->
      Alcotest.(check bool) "valid delay" false (contains ~sub:"delay:--" s)
  | Error e -> Alcotest.failf "%a" Vhs.pp_error e);
  check_contains "late frame" ~sub:"99.9999%"
    (svg [ (Ms 0, "a"); (Ms 9_000_000_000_000, "b") ])

let test_unsupported () =
  with_dir @@ fun env cwd ->
  let t =
    match
      Vhs.tape ~settings:[ Shell Nushell ] ~outputs:[ "demo.svg" ]
        [ Vhs.type' "x" ]
    with
    | Ok t -> t
    | Error e -> Alcotest.failf "%a" Vhs.pp_error e
  in
  match record env cwd t with
  | Error (Unsupported why) -> check_contains "names the line" ~sub:"Shell" why
  | Ok _ -> Alcotest.fail "played nushell"
  | Error e -> Alcotest.failf "%a" Vhs.pp_error e

(* A screenshot of an [.svg] path is a still SVG image of the next frame at
   that path, also when it is taken while hidden. *)
let test_screenshot () =
  with_dir @@ fun env cwd ->
  let t =
    tape
      Vhs.
        [
          hidden
            [ raw "Screenshot \"hidden.svg\""; type' "true\n"; wait Prompt ];
          type' "echo one\n";
          wait Prompt;
          raw "Screenshot \"shot.svg\"";
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

(* [record] writes outputs where [render] has [vhs] write them: relative to
   [cwd], or anywhere an absolute or [..] path leads, even when [cwd] is a
   capability that does not reach there. *)
let test_output_paths () =
  with_dir @@ fun env top ->
  Eio.Path.mkdir ~perm:0o755 Eio.Path.(top / "sub");
  let top_dir = Option.get (Eio.Path.native top) in
  Eio.Path.save ~create:(`Or_truncate 0o644)
    Eio.Path.(top / "inc.tape")
    "Type \"echo sourced\"\nEnter\nWait\n";
  Eio.Switch.run @@ fun sw ->
  let cwd = Eio.Path.open_dir ~sw Eio.Path.(top / "sub") in
  let t =
    match
      Vhs.tape
        ~settings:(Vhs.Typing_speed (Ms 5) :: settings)
        ~outputs:[ "../up.svg"; Filename.concat top_dir "abs.txt" ]
        Vhs.[ raw {|Source "../inc.tape"|}; sleep (Ms 100) ]
    with
    | Ok t -> t
    | Error e -> Alcotest.failf "%a" Vhs.pp_error e
  in
  (match play env (cwd :> Eio.Fs.dir_ty Eio.Path.t) t with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%a" Vhs.pp_error e);
  Alcotest.(check string)
    "image"
    (screen [ "> echo sourced"; "sourced"; ">" ])
    (last (frames (Eio.Path.load Eio.Path.(top / "up.svg"))));
  check_contains "text" ~sub:"sourced"
    (Eio.Path.load Eio.Path.(top / "abs.txt"))

(* [record] refuses an output that only [render] writes before it plays
   anything. *)
let test_render_outputs () =
  with_dir @@ fun env cwd ->
  let played = Eio.Path.(cwd / "played") in
  List.iter
    (fun (outputs, raws, expected) ->
      let t =
        match
          Vhs.tape ~settings ~outputs
            (List.map Vhs.raw raws @ Vhs.[ type' "touch played\n"; wait Prompt ])
        with
        | Ok t -> t
        | Error e -> Alcotest.failf "%a" Vhs.pp_error e
      in
      (match play env cwd t with
      | Error (Unsupported _ as e) ->
          Alcotest.(check string)
            "names the output and render" expected
            (Fmt.str "%a" Vhs.pp_error e)
      | Error e -> Alcotest.failf "%a" Vhs.pp_error e
      | Ok () -> Alcotest.failf "%s recorded natively" (List.hd outputs));
      Alcotest.(check bool) "nothing played" false (Eio.Path.is_file played))
    [
      ( [ "demo.svg"; "demo.gif" ],
        [],
        {|not recorded natively: Output "demo.gif": a GIF, which Vhs.render records|}
      );
      ( [ "demo.mp4" ],
        [],
        {|not recorded natively: Output "demo.mp4": an MP4 video, which Vhs.render records|}
      );
      ( [ "frames/" ],
        [],
        {|not recorded natively: Output "frames/": PNG frames, which Vhs.render records|}
      );
      ( [ "demo.svg" ],
        [ {|Output "demo.webm"|} ],
        {|not recorded natively: Output "demo.webm": a WebM video, which Vhs.render records|}
      );
      ( [ "demo.svg" ],
        [ {|Screenshot "shot.png"|} ],
        {|not recorded natively: Screenshot "shot.png": a PNG image, which Vhs.render takes|}
      );
    ]

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
  (* The tape inline would be refused for its escape. *)
  file "control.tape" "Type \"\027]8;;x\007click\"\n";
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
      | Error (Invalid why | Io why) -> check_contains name ~sub why
      | Ok _ -> Alcotest.failf "%s played" name
      | Error e -> Alcotest.failf "%a" Vhs.pp_error e)
    [
      ({|"nested.tape"|}, "nested");
      ({|"empty.tape"|}, "empty");
      ({|"bad.tape"|}, "Type");
      ({|"control.tape"|}, "control character");
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
let same_text env cwd (vhs_tape, native_tape) =
  let clock = Eio.Stdenv.mono_clock env and proc = Eio.Stdenv.process_mgr env in
  (match Vhs.render ?vhs:oracle ~clock ~proc ~cwd vhs_tape with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%a" Vhs.pp_error e);
  let vhs = Eio.Path.load Eio.Path.(cwd / "oracle.txt") in
  match record ~image:"oracle.svg" env cwd native_tape with
  | Error e -> Alcotest.failf "%a" Vhs.pp_error e
  | Ok s ->
      let native = Eio.Path.load Eio.Path.(cwd / "oracle.txt") in
      Alcotest.(check string) "text output" (after_settings vhs_tape vhs) native;
      (native, s)

(* [oracle_tape ~settings actions] are the tapes of [actions] for {!Vhs.render},
   to [oracle.txt], and for {!Vhs.record}, also to [oracle.svg]. *)
let oracle_tape ?(settings = []) actions =
  let tape outputs =
    match
      Vhs.tape ~settings ~outputs
        (Vhs.raw "Set Columns 60" :: Vhs.raw "Set Rows 12" :: actions)
    with
    | Ok t -> t
    | Error e -> Alcotest.failf "%a" Vhs.pp_error e
  in
  (tape [ "oracle.txt" ], tape [ "oracle.txt"; "oracle.svg" ])

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

(* xterm's controls: raw line feeds and indexes, a reverse index, scroll
   regions, line edits, character erases and repeats, no auto-wrap, origin
   mode, the cursor in the last column and DEC line drawing. *)
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
          ~cwd (fst t)
      with
      | Error e -> Alcotest.failf "%a" Vhs.pp_error e
      | Ok () -> Alcotest.(check string) "VHS oracle" expected (text ())));
  match record ~image:"oracle.svg" env cwd (snd t) with
  | Error e -> Alcotest.failf "%a" Vhs.pp_error e
  | Ok s -> (
      match Svg_check.frames ~cell_width ~cell_height ~rows:12 s with
      | Error e -> Alcotest.fail e
      | Ok images ->
          Alcotest.(check (pair string string))
            "native text and SVG" (expected, expected)
            (text (), last images))

(* [rows lines] are the rows of a 60 x 12 screen that shows [lines], then
   the prompt on its tenth row. *)
let rows lines =
  lines @ List.init (9 - List.length lines) (fun _ -> "") @ [ ">"; ""; "" ]

(* [check_payload ?vhs ?text payload image] records a shell that clears the
   screen and prints [payload], then puts the prompt on the tenth row, below
   it, so shell echo cannot affect its cells. The native image shows [image],
   the native text output [text] (defaults to [image]), and, when
   VHS_ORACLE is set, VHS's text output [vhs] (defaults to [text]). *)
let check_payload ?vhs ?text payload image =
  let text = Option.value text ~default:image in
  let vhs = Option.value vhs ~default:text in
  let lines l = String.concat "\n" l in
  with_dir @@ fun env cwd ->
  let command = "printf '\\033[2J\\033[H" ^ payload ^ "\\033[10;1H'" in
  let t =
    oracle_tape ~settings:[ Typing_speed (Ms 1) ]
      Vhs.[ hidden [ type' command; key Enter; wait Prompt ]; sleep (Ms 200) ]
  in
  let output () = last_screen (Eio.Path.load Eio.Path.(cwd / "oracle.txt")) in
  (match oracle with
  | None -> ()
  | Some exe -> (
      match
        Vhs.render ~vhs:exe
          ~clock:(Eio.Stdenv.mono_clock env)
          ~proc:(Eio.Stdenv.process_mgr env)
          ~cwd (fst t)
      with
      | Error e -> Alcotest.failf "%a" Vhs.pp_error e
      | Ok () -> Alcotest.(check string) "VHS oracle" (lines vhs) (output ())));
  match record ~image:"oracle.svg" env cwd (snd t) with
  | Error e -> Alcotest.failf "%a" Vhs.pp_error e
  | Ok svg -> (
      match Svg_check.frames ~cell_width ~cell_height ~rows:12 svg with
      | Error e -> Alcotest.fail e
      | Ok images ->
          Alcotest.(check (pair string string))
            "native text and SVG"
            (lines text, lines image)
            (output (), last images))

let check_terminal_payload payload lines = check_payload payload (rows lines)

(* REP repeats the complete preceding cell, including its combining marks. *)
let test_repeat_combining () =
  check_terminal_payload {|a\314\201\033[3b|}
    [ String.concat "" (List.init 4 (fun _ -> "a\xcc\x81")) ]

(* The UTF-8 encodings of C1 CSI, OSC and ST have their control meanings,
   just like ESC [, ESC ] and ESC backslash in xterm.js. *)
let test_c1_controls () =
  check_terminal_payload {|a\302\2332Cb\302\235title\302\234c|} [ "a  bc" ]

(* Unlike 1049, modes 47 and 1047 share the cursor between buffers. *)
let test_alternate_cursor () =
  check_terminal_payload
    {|abc\033[?47hALT\033[?47lX\033[2;1Habc\033[?1047hALT\033[?1047lX|}
    [ "abc   X"; "abc   X" ]

(* DECSCA marks cells as protected. DECSEL erases only unprotected cells,
   as used by terminal forms and status displays. *)
let test_selective_erase () =
  check_terminal_payload {|\033[1"qabc\033[0"qdef\r\033[?2K|} [ "abc" ]

(* Reverse-wrap mode lets backspace cross a line that auto-wrap produced. *)
let test_reverse_wrap () =
  check_terminal_payload {|\033[?45h\033[60GAZ\b\bX|}
    [ String.make 59 ' ' ^ "X"; "Z" ]

(* The native terminal follows xterm, as matrix emulates it, where VHS's
   xterm.js departs from it. These are the differences that show in a
   recording, each with what VHS writes and what the native recorder does:
   - an erasure in the last column after a character there: xterm erases
     that column and ends the pending wrap, xterm.js starts beyond it and
     keeps the wrap pending;
   - CSI S: xterm keeps the lines it scrolls out in the scrollback, which the
     text output reads, and xterm.js drops them;
   - a restore after scrolling: xterm returns to the row the save was on,
     xterm.js to the buffer line, which moved up;
   - a restore brings back the character set designations in xterm; xterm.js
     brings back only the set in use, and the next shift invokes the
     designation made since;
   - ESC % G: xterm selects UTF-8 and keeps the designations, xterm.js also
     invokes ASCII into G0;
   - the two screens share the scroll region and the tab stops in xterm, and
     each has its own in xterm.js;
   - the cursor position report after a character in the last column: xterm
     reports that column and xterm.js the one beyond, and xterm's DECXCPR
     reply has the page, as the screen tests check;
   - a scroll of the alternate screen: VHS's text output reads xterm.js's
     buffer lines by their absolute index rather than the viewport, so it is
     a row off the screen its screenshots show, which the native one draws. *)
let known_differences =
  let blank n = List.init n (fun _ -> "") in
  let margin = String.make 59 ' ' in
  [
    ( "erase below at the margin",
      {|\033[60GX\033[JY|},
      rows [ margin ^ "X"; "Y" ],
      rows [ margin ^ "Y" ],
      None );
    ( "erase to the start at the margin",
      {|\033[60GX\033[1KY|},
      rows [ ""; "Y" ],
      rows [ margin ^ "Y" ],
      None );
    ( "scroll up",
      {|a\033[S|},
      rows [],
      rows [],
      Some ("a" :: List.take 11 (rows [])) );
    ( "restore after scrolling",
      {|\033[8;3H\0337\033[12;1H\n\n\0338X|},
      blank 2 @ List.take 10 (rows (blank 5 @ [ "  X" ])),
      rows (blank 7 @ [ "  X" ]),
      Some (blank 2 @ List.take 10 (rows (blank 7 @ [ "  X" ]))) );
    ( "restored designations",
      {|\033(0\0337\033(B\0338q\017q\033(B|},
      rows [ "\xe2\x94\x80q" ],
      rows [ "\xe2\x94\x80\xe2\x94\x80" ],
      None );
    ( "UTF-8",
      {|\033(0\033%%Gq\033(B|},
      rows [ "q" ],
      rows [ "\xe2\x94\x80" ],
      None );
    ( "shared scroll region",
      {|\033[?1049h\033[1;2r\033[?1049la\r\nb\r\nc\033[r|},
      rows [ "a"; "b"; "c" ],
      rows [ "b"; "c" ],
      Some ("a" :: List.take 11 (rows [ "b"; "c" ])) );
    ( "scrolled alternate screen",
      {|\033[?1049h\033[12;1HA\r\nB|},
      blank 10 @ [ ">"; "A" ],
      blank 9 @ [ ">"; "A"; "B" ],
      None );
    ( "shared tab stops",
      {|\033[?1049h\033[3g\033[?1049l\tX|},
      rows [ String.make 8 ' ' ^ "X" ],
      rows [ margin ^ "X" ],
      None );
  ]
  |> List.map (fun (name, payload, vhs, image, text) ->
      (name, fun () -> check_payload ~vhs ?text payload image))

let suite =
  ( "native",
    List.map
      (fun (name, fn) -> Alcotest.test_case name `Quick fn)
      ([
         ("record", test_record);
         ("zero-speed prompt wait", test_zero_speed_prompt);
         ("screenshot over the recording", test_screenshot_clash);
         ("timeout", test_timeout);
         ("shell exit", test_shell_exit);
         ("timeout precedence", test_timeout_precedence);
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
         ("margin fill refused", test_margin_fill_refused);
         ("output directories", test_output_dirs);
         ("zsh", test_zsh);
         ("fish", test_fish);
         ("replies", test_replies);
         ("screenshot", test_screenshot);
         ("source", test_source);
         ("output paths", test_output_paths);
         ("render outputs", test_render_outputs);
         ("keys", test_keys);
         ("unsupported", test_unsupported);
         ("bounds", test_bounds);
         ("terminal state recording", test_terminal_state);
         ("repeat combining cell", test_repeat_combining);
         ("C1 terminal controls", test_c1_controls);
         ("alternate-screen shared cursor", test_alternate_cursor);
         ("selective erasure preserves protected text", test_selective_erase);
         ("reverse wrap crosses an automatic line break", test_reverse_wrap);
       ]
      @ known_differences
      @
      if Option.is_some oracle then
        [
          ("vhs oracle", test_oracle);
          ("vhs oracle zsh", test_oracle_zsh);
          ("vhs oracle controls", test_oracle_controls);
        ]
      else []) )
