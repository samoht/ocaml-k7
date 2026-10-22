(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Native_test

let test_record () =
  with_dir @@ fun env cwd ->
  let t =
    tape
      K7.
        [
          type' "echo hello\n";
          wait Prompt;
          type' "printf '\\033[32mgreen\\033[0m\\n'\n";
          wait Prompt;
          sleep (Ms 100);
        ]
  in
  match record env cwd t with
  | Error e -> Alcotest.failf "%a" K7.pp_error e
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

(* The leading sleep gives the tape frames. *)
let test_zero_speed_prompt () =
  with_dir @@ fun env cwd ->
  let t =
    match
      K7.tape
        ~settings:(Typing_speed (Ms 0) :: settings)
        ~outputs:[ "demo.txt" ]
        K7.[ sleep (Ms 100); type' "echo READY\n"; wait Prompt ]
    with
    | Ok t -> t
    | Error e -> Alcotest.failf "%a" K7.pp_error e
  in
  (match play env cwd t with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%a" K7.pp_error e);
  check_contains "completed command before the prompt wait returns"
    ~sub:"\nREADY\n>"
    (last_screen (Eio.Path.load Eio.Path.(cwd / "demo.txt")))

let test_screenshot_clash () =
  with_dir @@ fun env cwd ->
  let t =
    tape
      K7.
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
  | Error e -> Alcotest.failf "%a" K7.pp_error e
  | Ok () ->
      Alcotest.failf "Ok, and demo.svg is the still:\n%s"
        (Eio.Path.load Eio.Path.(cwd / "demo.svg"))

let test_timeout () =
  with_dir @@ fun env cwd ->
  let t =
    tape K7.[ type' "echo hi\n"; wait ~timeout:(Ms 300) (Screen "never") ]
  in
  match record env cwd t with
  | Error (Timeout { wait; line; last } as e) ->
      Alcotest.(check string) "wait" "Wait+Screen@300ms /never/" wait;
      Alcotest.(check string)
        "line" "Wait+Screen@300ms /never/"
        (List.nth (String.split_on_char '\n' (K7.to_string t)) (line - 1));
      check_contains "last screen" ~sub:"> echo hi\nhi" last;
      check_contains "names the line" ~sub:(Fmt.str "line %d," line)
        (Fmt.str "%a" K7.pp_error e);
      check_contains "how to wait longer"
        ~sub:"~timeout or the tape a longer Wait_timeout"
        (Fmt.str "%a" K7.pp_error e)
  | Ok _ -> Alcotest.fail "no timeout"
  | Error e -> Alcotest.failf "%a" K7.pp_error e

let test_shell_exit () =
  with_dir @@ fun env cwd ->
  let t = tape K7.[ type' "exit 3\n"; wait ~timeout:(Ms 20_000) Prompt ] in
  match record env cwd t with
  | Error (Exited { status = `Exited 3; wait = "Wait@20000ms"; _ } as e) ->
      check_contains "message" ~sub:"the shell exited (status 3) while waiting"
        (Fmt.str "%a" K7.pp_error e)
  | Ok _ -> Alcotest.fail "no error"
  | Error e -> Alcotest.failf "%a" K7.pp_error e

(* A clock that jumps to the next sleep that is due, so that timeouts expire
   in their order and the time they took is exact. *)
let with_virtual_clock f =
  let clock = Eio_mock.Clock.Mono.make () in
  Eio.Switch.run @@ fun sw ->
  Eio.Fiber.fork_daemon ~sw (fun () ->
      let rec loop () =
        if not (Eio_mock.Clock.Mono.try_advance clock) then Eio.Fiber.yield ();
        loop ()
      in
      loop ());
  f clock

let test_timeout_precedence () =
  with_dir @@ fun env cwd ->
  let times_out ~within t =
    with_virtual_clock @@ fun clock ->
    (match K7.record ~clock ~proc:(Eio.Stdenv.process_mgr env) ~cwd t with
    | Error (Timeout _) -> ()
    | Ok _ -> Alcotest.fail "no timeout"
    | Error e -> Alcotest.failf "%a" K7.pp_error e);
    let s = Int64.to_float (Mtime.to_uint64_ns (Eio.Time.Mono.now clock)) in
    if s /. 1e9 >= within then Alcotest.failf "timed out after %gs" (s /. 1e9)
  in
  let tape timeout actions =
    Result.get_ok (K7.with_settings [ Wait_timeout timeout ] (tape actions))
  in
  times_out ~within:15.
    (tape (Ms 60_000)
       K7.[ type' "echo hi\n"; wait ~timeout:(Ms 300) (Screen "never") ]);
  times_out ~within:15.
    (tape (Ms 300) K7.[ type' "echo hi\n"; wait (Screen "never") ])

let test_hidden () =
  with_dir @@ fun env cwd ->
  let t =
    tape
      K7.
        [
          hidden [ type' "secret=1; clear\n"; wait Prompt ];
          type' "echo shown\n";
          wait Prompt;
          sleep (Ms 100);
        ]
  in
  match record env cwd t with
  | Error e -> Alcotest.failf "%a" K7.pp_error e
  | Ok s ->
      Alcotest.(check bool)
        "hidden typing not shown" false (contains ~sub:"secret" s);
      Alcotest.(check string)
        "final"
        (screen [ "> echo shown"; "shown"; ">" ])
        (last (frames s))

let test_scroll () =
  with_dir @@ fun env cwd ->
  let t =
    tape
      K7.
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
  | Error e -> Alcotest.failf "%a" K7.pp_error e
  | Ok s ->
      Alcotest.(check string)
        "scrolled"
        (String.concat "\n" (List.init 9 (fun i -> Int.to_string (i + 10))))
        (last (frames s))

let test_scroll_far () =
  with_dir @@ fun env cwd ->
  let t =
    tape
      K7.
        [
          type' "seq 1 30\n";
          wait (Line "^>$");
          key Scroll_up;
          raw "ScrollUp 11";
          sleep (Ms 100);
        ]
  in
  match record env cwd t with
  | Error e -> Alcotest.failf "%a" K7.pp_error e
  | Ok s ->
      Alcotest.(check string)
        "scrolled"
        (String.concat "\n" (List.init 9 (fun i -> Int.to_string (i + 11))))
        (last (frames s))

(* As VHS, scrollback first, at most a thousand lines as in xterm.js. *)
let test_buffer () =
  with_dir @@ fun env cwd ->
  let shows re = K7.wait ~timeout:(Ms 5000) (Screen re) in
  let t =
    tape
      K7.
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
  | Error e -> Alcotest.failf "%a" K7.pp_error e
  | Ok s ->
      Alcotest.(check string)
        "no scrollback after clear"
        (screen [ "> echo x"; "x"; ">" ])
        (last (frames s))

let test_full_scrollback () =
  with_dir @@ fun env cwd ->
  let shows re = K7.wait ~timeout:(Ms 10000) (Screen re) in
  let t =
    tape
      K7.
        [
          type' "seq 1 9995; clear\n";
          shows {|^>\n\n|};
          type' "seq 1 1000\n";
          shows {|^> seq 1 1000\n1\n2\n|};
          sleep (Ms 100);
        ]
  in
  match record env cwd t with
  | Error e -> Alcotest.failf "%a" K7.pp_error e
  | Ok _ -> ()

let test_scrolled_line () =
  with_dir @@ fun env cwd ->
  let t =
    tape
      K7.
        [
          type' "seq 1 30\n";
          wait (Line "^>$");
          key Scroll_up;
          wait ~timeout:(Ms 2000) (Line "^30$");
        ]
  in
  match record env cwd t with
  | Error e -> Alcotest.failf "%a" K7.pp_error e
  | Ok _ -> ()

(* VHS trims lines with JavaScript's [trimEnd], which drops a no-break space. *)
let test_trim () =
  with_dir @@ fun env cwd ->
  let t =
    tape
      K7.
        [
          type' "printf 'ab\\302\\240\\342\\200\\203'; sleep 3\n";
          wait ~timeout:(Ms 2000) (Line "^ab$");
        ]
  in
  match record env cwd t with
  | Error e -> Alcotest.failf "%a" K7.pp_error e
  | Ok _ -> ()

let test_text () =
  with_dir @@ fun env cwd ->
  let tape outputs actions =
    match
      K7.tape ~settings:(K7.Typing_speed (Ms 5) :: settings) ~outputs actions
    with
    | Ok t -> t
    | Error e -> Alcotest.failf "%a" K7.pp_error e
  in
  let actions =
    K7.
      [
        hidden [ type' "x=1\n"; wait Prompt ];
        type' "sleep 1; echo hi";
        key Enter;
        wait Prompt;
      ]
  in
  let text path = screens (Eio.Path.load Eio.Path.(cwd / path)) in
  (match record env cwd (tape [ "demo.svg"; "demo.txt" ] actions) with
  | Error e -> Alcotest.failf "%a" K7.pp_error e
  | Ok _ -> (
      (* The echo can lag behind the typing. *)
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
         (K7.raw "Sleep 0" :: K7.raw "Output \"later.ascii\"" :: actions))
  with
  | Error e -> Alcotest.failf "%a" K7.pp_error e
  | Ok _ ->
      Alcotest.(check int)
        "from the output" 5
        (List.length (text "later.ascii"))

let test_window_bar () =
  with_dir @@ fun env cwd ->
  let bar raws =
    match record env cwd (tape (List.map K7.raw raws @ [ K7.type' "x" ])) with
    | Ok s -> s
    | Error e -> Alcotest.failf "%a" K7.pp_error e
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

let test_margin_fill () =
  with_dir @@ fun env cwd ->
  Eio.Path.save ~create:(`Or_truncate 0o644)
    Eio.Path.(cwd / "m.png")
    "\x89PNG\r\n\x1a\nabcd";
  let play raws =
    match record env cwd (tape (List.map K7.raw raws @ [ K7.type' "x" ])) with
    | Ok s -> s
    | Error e -> Alcotest.failf "%a" K7.pp_error e
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
        tape [ K7.raw "Set Margin 20"; K7.raw raw; K7.type' "touch played\n" ]
      in
      match record env cwd t with
      | Error (Invalid msg) ->
          check_contains raw ~sub msg;
          Alcotest.(check bool) "not played" false (Eio.Path.is_file played)
      | Ok _ -> Alcotest.failf "%s played" raw
      | Error e -> Alcotest.failf "%s: %a" raw K7.pp_error e)
    [
      ({|Set MarginFill "text.png"|}, "not a PNG, JPEG, GIF, WebP or BMP image");
      ({|Set MarginFill "leak.png"|}, "not a PNG, JPEG, GIF, WebP or BMP image");
      ({|Set MarginFill "big.png"|}, "larger than 10 MiB");
      ({|Set MarginFill "dir.png"|}, "not a regular file");
      ({|Set MarginFill "fifo.png"|}, "not a regular file");
      ({|Source "fifo.tape"|}, "not a regular file");
    ]

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
        K7.tape ~settings ~outputs
          (List.map K7.raw raws @ K7.[ type' "touch played\n"; wait Prompt ])
      with
      | Ok t -> t
      | Error e -> Alcotest.failf "%s: %a" name K7.pp_error e
    in
    match engine t with
    | Error (K7.Io msg) ->
        check_contains name ~sub msg;
        Alcotest.(check bool)
          (name ^ ": not played") false (Eio.Path.is_file played)
    | Ok () -> Alcotest.failf "%s: Ok" name
    | Error e -> Alcotest.failf "%s: %a" name K7.pp_error e
  in
  let record = play env cwd in
  let render t =
    K7.render ~vhs:fake
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
  let t =
    match
      K7.tape ~settings
        ~outputs:[ "a/b/demo.svg"; "c/demo.txt" ]
        K7.[ type' "true\n"; wait Prompt ]
    with
    | Ok t -> t
    | Error e -> Alcotest.failf "%a" K7.pp_error e
  in
  (match record t with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%a" K7.pp_error e);
  Alcotest.(check bool)
    "created" true
    (Eio.Path.is_file Eio.Path.(cwd / "a" / "b" / "demo.svg")
    && Eio.Path.is_file Eio.Path.(cwd / "c" / "demo.txt"))

let on_path exe =
  List.exists
    (fun dir -> dir <> "" && Sys.file_exists (Filename.concat dir exe))
    (String.split_on_char ':'
       (Option.value (Sys.getenv_opt "PATH") ~default:""))

let test_zsh () =
  if not (on_path "zsh") then Alcotest.skip ();
  with_dir @@ fun env cwd ->
  let t =
    match
      K7.tape
        ~settings:(Shell Zsh :: Typing_speed (Ms 5) :: settings)
        ~outputs:[ "demo.svg" ]
        K7.[ type' "echo $ZSH_NAME\n"; wait Prompt; sleep (Ms 100) ]
    with
    | Ok t -> t
    | Error e -> Alcotest.failf "%a" K7.pp_error e
  in
  match record env cwd t with
  | Error e -> Alcotest.failf "%a" K7.pp_error e
  | Ok s ->
      check_contains "prompt colour" ~sub:{|fill="#5b56e0">&gt;|} s;
      Alcotest.(check string)
        "final"
        (screen [ "> echo $ZSH_NAME"; "zsh"; ">" ])
        (last (frames s))

let test_fish () =
  if not (on_path "fish") then Alcotest.skip ();
  with_dir @@ fun env cwd ->
  let t =
    match
      K7.tape
        ~settings:(Shell Fish :: Typing_speed (Ms 5) :: settings)
        ~outputs:[ "demo.svg" ]
        K7.
          [
            type' "echo fish-$status\n";
            wait ~timeout:(Ms 10000) (Screen "(?m)^fish-0$");
            wait Prompt;
            sleep (Ms 100);
          ]
    with
    | Ok t -> t
    | Error e -> Alcotest.failf "%a" K7.pp_error e
  in
  match record env cwd t with
  | Error e -> Alcotest.failf "%a" K7.pp_error e
  | Ok s ->
      let text = last (frames s) in
      Alcotest.(check bool) "no greeting" false (contains ~sub:"Welcome" text);
      check_contains "command" ~sub:"> echo fish-$status" text

(* The terminal answers at once, so echo goes off before the query: the tty
   would otherwise echo the reply. *)
let test_replies () =
  with_dir @@ fun env cwd ->
  let t =
    tape
      K7.
        [
          type'
            {|stty -echo; printf '\033[c'; IFS= read -rd c da; stty echo; echo "da${da#*[}"|};
          key Enter;
          wait ~timeout:(Ms 5000) (Screen {|(?m)^da\?1;2$|});
        ]
  in
  match record env cwd t with
  | Error e -> Alcotest.failf "%a" K7.pp_error e
  | Ok _ -> ()

let test_keys () =
  with_dir @@ fun env cwd ->
  let t =
    tape
      K7.
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
  | Error e -> Alcotest.failf "%a" K7.pp_error e
  | Ok s ->
      Alcotest.(check string)
        "edited"
        (screen [ "> echo X abc"; "X abc"; ">" ])
        (last (frames s))

let test_modified_arrows () =
  with_dir @@ fun env cwd ->
  let t =
    tape
      K7.
        [
          type' "stty -echo; cat -v; stty echo\n";
          sleep (Ms 300);
          raw "Ctrl+Up";
          raw "Ctrl+Alt+Up";
          raw "Ctrl+Shift+Down";
          raw "Ctrl+Alt+Shift+Left";
          key Enter;
          key (Ctrl 'd');
          wait Prompt;
          sleep (Ms 100);
        ]
  in
  match record env cwd t with
  | Error e -> Alcotest.failf "%a" K7.pp_error e
  | Ok s ->
      check_contains "bytes" ~sub:"^[[1;5A^[[1;7A^[[1;6B^[[1;8D"
        (last (frames s))

let test_bounds () =
  with_dir @@ fun env cwd ->
  let play ?settings raws =
    match
      K7.tape ?settings ~outputs:[ "demo.svg" ]
        (List.map K7.raw raws @ [ K7.type' "x" ])
    with
    | Ok t -> record env cwd t
    | Error e -> Alcotest.failf "%a" K7.pp_error e
  in
  List.iter
    (fun (settings, raws) ->
      match play ~settings raws with
      | Error (Unsupported _) -> ()
      | Ok _ -> Alcotest.failf "%s played" (String.concat "; " raws)
      | Error e -> Alcotest.failf "%a" K7.pp_error e)
    [
      ([ K7.Columns 2000 ], [ "Set LetterSpacing 1000000000000000000" ]);
      ([], [ "Set LineHeight 1000000000000000000" ]);
      ([ K7.Columns 2000 ], [ "Set LetterSpacing 100" ]);
      ([], [ "Set PlaybackSpeed 0.0000000001" ]);
    ];
  (match play [ "Set LoopOffset 1" ^ String.make 300 '0' ] with
  | Ok s ->
      Alcotest.(check bool) "valid delay" false (contains ~sub:"delay:--" s)
  | Error e -> Alcotest.failf "%a" K7.pp_error e);
  check_contains "late frame" ~sub:"99.9999%"
    (svg [ (Ms 0, "a"); (Ms 9_000_000_000_000, "b") ])

let test_unsupported () =
  with_dir @@ fun env cwd ->
  let t =
    match
      K7.tape ~settings:[ Shell Nushell ] ~outputs:[ "demo.svg" ]
        [ K7.type' "x" ]
    with
    | Ok t -> t
    | Error e -> Alcotest.failf "%a" K7.pp_error e
  in
  match record env cwd t with
  | Error (Unsupported why) -> check_contains "names the line" ~sub:"Shell" why
  | Ok _ -> Alcotest.fail "played nushell"
  | Error e -> Alcotest.failf "%a" K7.pp_error e

let test_screenshot () =
  with_dir @@ fun env cwd ->
  let t =
    tape
      K7.
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
  | Error e -> Alcotest.failf "%a" K7.pp_error e
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
      K7.tape
        ~settings:(K7.Typing_speed (Ms 5) :: settings)
        ~outputs:[ "../up.svg"; Filename.concat top_dir "abs.txt" ]
        K7.[ raw {|Source "../inc.tape"|}; sleep (Ms 100) ]
    with
    | Ok t -> t
    | Error e -> Alcotest.failf "%a" K7.pp_error e
  in
  (match play env (cwd :> Eio.Fs.dir_ty Eio.Path.t) t with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%a" K7.pp_error e);
  Alcotest.(check string)
    "image"
    (screen [ "> echo sourced"; "sourced"; ">" ])
    (last (frames (Eio.Path.load Eio.Path.(top / "up.svg"))));
  check_contains "text" ~sub:"sourced"
    (Eio.Path.load Eio.Path.(top / "abs.txt"))

let test_render_outputs () =
  with_dir @@ fun env cwd ->
  let played = Eio.Path.(cwd / "played") in
  List.iter
    (fun (outputs, raws, expected) ->
      let t =
        match
          K7.tape ~settings ~outputs
            (List.map K7.raw raws @ K7.[ type' "touch played\n"; wait Prompt ])
        with
        | Ok t -> t
        | Error e -> Alcotest.failf "%a" K7.pp_error e
      in
      (match play env cwd t with
      | Error (Unsupported _ as e) ->
          Alcotest.(check string)
            "names the output and render" expected
            (Fmt.str "%a" K7.pp_error e)
      | Error e -> Alcotest.failf "%a" K7.pp_error e
      | Ok () -> Alcotest.failf "%s recorded natively" (List.hd outputs));
      Alcotest.(check bool) "nothing played" false (Eio.Path.is_file played))
    [
      ( [ "demo.svg"; "demo.gif" ],
        [],
        {|not recorded natively: Output "demo.gif": a GIF, which K7.render records|}
      );
      ( [ "demo.mp4" ],
        [],
        {|not recorded natively: Output "demo.mp4": an MP4 video, which K7.render records|}
      );
      ( [ "frames/" ],
        [],
        {|not recorded natively: Output "frames/": PNG frames, which K7.render records|}
      );
      ( [ "demo.svg" ],
        [ {|Output "demo.webm"|} ],
        {|not recorded natively: Output "demo.webm": a WebM video, which K7.render records|}
      );
      ( [ "demo.svg" ],
        [ {|Screenshot "shot.png"|} ],
        {|not recorded natively: Screenshot "shot.png": a PNG image, which K7.render takes|}
      );
    ]

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
  file "control.tape" "Type \"\027]8;;x\007click\"\n";
  let play name =
    record env cwd (tape K7.[ raw ("Source " ^ name); sleep (Ms 100) ])
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
  | Error e -> Alcotest.failf "%a" K7.pp_error e);
  List.iter
    (fun (name, sub) ->
      match play name with
      | Error (Invalid why | Io why) -> check_contains name ~sub why
      | Ok _ -> Alcotest.failf "%s played" name
      | Error e -> Alcotest.failf "%a" K7.pp_error e)
    [
      ({|"nested.tape"|}, "nested");
      ({|"empty.tape"|}, "empty");
      ({|"bad.tape"|}, "Type");
      ({|"control.tape"|}, "control character");
      ({|"missing.tape"|}, "missing.tape");
    ]

(* VHS reads a screen per [Set] but [Set Shell], [Output] and [Require] line
   before it sizes the terminal. *)
let after_settings tape txt =
  let lines = String.split_on_char '\n' (K7.to_string tape) in
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

let same_text env cwd (vhs_tape, native_tape) =
  let clock = Eio.Stdenv.mono_clock env and proc = Eio.Stdenv.process_mgr env in
  (match K7.render ?vhs:oracle ~clock ~proc ~cwd vhs_tape with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%a" K7.pp_error e);
  let vhs = Eio.Path.load Eio.Path.(cwd / "oracle.txt") in
  match record ~image:"oracle.svg" env cwd native_tape with
  | Error e -> Alcotest.failf "%a" K7.pp_error e
  | Ok s ->
      let native = Eio.Path.load Eio.Path.(cwd / "oracle.txt") in
      Alcotest.(check string) "text output" (after_settings vhs_tape vhs) native;
      (native, s)

let oracle_tape ?(settings = []) actions =
  let tape outputs =
    match
      K7.tape ~settings:(K7.[ Columns 60; Rows 12 ] @ settings) ~outputs actions
    with
    | Ok t -> t
    | Error e -> Alcotest.failf "%a" K7.pp_error e
  in
  (tape [ "oracle.txt" ], tape [ "oracle.txt"; "oracle.svg" ])

let test_oracle () =
  with_dir @@ fun env cwd ->
  let t =
    oracle_tape
      K7.
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

let test_oracle_controls () =
  with_dir @@ fun env cwd ->
  let printf s =
    K7.type'
      ("sleep 1; stty -onlcr; printf '" ^ s ^ {|\033[12;1H\033[K'; stty onlcr|})
  in
  let t =
    oracle_tape
      K7.
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

let test_oracle_zsh () =
  with_dir @@ fun env cwd ->
  let t =
    oracle_tape ~settings:[ Shell Zsh ]
      K7.
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
      K7.[ hidden [ type' command; key Enter; wait Prompt ]; sleep (Ms 100) ]
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
        K7.render ~vhs
          ~clock:(Eio.Stdenv.mono_clock env)
          ~proc:(Eio.Stdenv.process_mgr env)
          ~cwd (fst t)
      with
      | Error e -> Alcotest.failf "%a" K7.pp_error e
      | Ok () -> Alcotest.(check string) "VHS oracle" expected (text ())));
  match record ~image:"oracle.svg" env cwd (snd t) with
  | Error e -> Alcotest.failf "%a" K7.pp_error e
  | Ok s -> (
      match Svg_check.frames ~cell_width ~cell_height ~rows:12 s with
      | Error e -> Alcotest.fail e
      | Ok images ->
          Alcotest.(check (pair string string))
            "native text and SVG" (expected, expected)
            (text (), last images))

let rows lines =
  lines @ List.init (9 - List.length lines) (fun _ -> "") @ [ ">"; ""; "" ]

let check_payload ?vhs ?text payload image =
  let text = Option.value text ~default:image in
  let vhs = Option.value vhs ~default:text in
  let lines l = String.concat "\n" l in
  with_dir @@ fun env cwd ->
  let command = "printf '\\033[2J\\033[H" ^ payload ^ "\\033[10;1H'" in
  let t =
    oracle_tape ~settings:[ Typing_speed (Ms 1) ]
      K7.[ hidden [ type' command; key Enter; wait Prompt ]; sleep (Ms 200) ]
  in
  let output () = last_screen (Eio.Path.load Eio.Path.(cwd / "oracle.txt")) in
  (match oracle with
  | None -> ()
  | Some exe -> (
      match
        K7.render ~vhs:exe
          ~clock:(Eio.Stdenv.mono_clock env)
          ~proc:(Eio.Stdenv.process_mgr env)
          ~cwd (fst t)
      with
      | Error e -> Alcotest.failf "%a" K7.pp_error e
      | Ok () -> Alcotest.(check string) "VHS oracle" (lines vhs) (output ())));
  match record ~image:"oracle.svg" env cwd (snd t) with
  | Error e -> Alcotest.failf "%a" K7.pp_error e
  | Ok svg -> (
      match Svg_check.frames ~cell_width ~cell_height ~rows:12 svg with
      | Error e -> Alcotest.fail e
      | Ok images ->
          Alcotest.(check (pair string string))
            "native text and SVG"
            (lines text, lines image)
            (output (), last images))

let check_terminal_payload payload lines = check_payload payload (rows lines)

let test_repeat_combining () =
  check_terminal_payload {|a\314\201\033[3b|}
    [ String.concat "" (List.init 4 (fun _ -> "a\xcc\x81")) ]

let test_c1_controls () =
  check_terminal_payload {|a\302\2332Cb\302\235title\302\234c|} [ "a  bc" ]

let test_alternate_cursor () =
  check_terminal_payload
    {|abc\033[?47hALT\033[?47lX\033[2;1Habc\033[?1047hALT\033[?1047lX|}
    [ "abc   X"; "abc   X" ]

let test_selective_erase () =
  check_terminal_payload {|\033[1"qabc\033[0"qdef\r\033[?2K|} [ "abc" ]

let test_reverse_wrap () =
  check_terminal_payload {|\033[?45h\033[60GAZ\b\bX|}
    [ String.make 59 ' ' ^ "X"; "Z" ]

(* Where VHS's xterm.js departs from xterm, the native recorder follows xterm,
   as matrix emulates it: each case has VHS's screen, then the native one. *)
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

let run ?clock ?vars ?show env cwd argv =
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 60. @@ fun () ->
  K7.run ~settings ?env:vars ?show
    ~clock:
      (match clock with
      | Some c -> (c :> Eio.Time.Mono.ty Eio.Resource.t)
      | None -> (Eio.Stdenv.mono_clock env :> Eio.Time.Mono.ty Eio.Resource.t))
    ~proc:(Eio.Stdenv.process_mgr env)
    ~cwd argv

let script cwd name src =
  Eio.Path.save ~create:(`Or_truncate 0o755) Eio.Path.(cwd / name) src

let times svg =
  let number s i =
    let j = ref i in
    while
      !j < String.length s && (s.[!j] = '.' || Char.Ascii.is_digit s.[!j])
    do
      incr j
    done;
    (float_of_string (String.sub s i (!j - i)), !j)
  in
  let rec find sub i =
    if i + String.length sub > String.length svg then None
    else if String.sub svg i (String.length sub) = sub then Some i
    else find sub (i + 1)
  in
  let duration =
    match find "{animation:s " 0 with
    | Some i -> fst (number svg (i + 13))
    | None -> Alcotest.fail "no animation"
  in
  let rec keyframes i acc =
    match find "%{transform:translateX(" i with
    | None -> List.rev (List.tl acc)
    | Some j ->
        let k = ref (j - 1) in
        while svg.[!k] = '.' || Char.Ascii.is_digit svg.[!k] do
          decr k
        done;
        let p, _ = number svg (!k + 1) in
        keyframes (j + 1) ((p *. duration /. 100.) :: acc)
  in
  (duration, keyframes 0 [])

(* The program prints nothing until the clock reads 500ms, so its output and
   its exit land on that frame, whatever the machine's load. *)
let test_run_program () =
  with_dir @@ fun env cwd ->
  script cwd "demo.sh"
    "#!/bin/sh\n\
     : > started\n\
     while [ ! -e go ]; do sleep 0.01; done\n\
     printf '\\033[31mred\\033[0m\\n'\n\
     printf 'done\\n'\n\
     exit 3\n";
  let clock = Eio_mock.Clock.Mono.make () in
  let wall = Eio.Stdenv.clock env in
  let result, () =
    Eio.Fiber.pair
      (fun () -> run ~clock env cwd [ "./demo.sh" ])
      (fun () ->
        Eio.Time.with_timeout_exn wall 30. (fun () ->
            while
              not (Sys.file_exists (Eio.Path.native_exn cwd ^ "/started"))
            do
              Eio.Time.sleep wall 0.01
            done);
        Eio_mock.Clock.Mono.set_time clock (Mtime.of_uint64_ns 500_000_000L);
        Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(cwd / "go") "")
  in
  match result with
  | Error e -> Alcotest.failf "%a" K7.pp_error e
  | Ok (s, status) ->
      Alcotest.(check bool) "status" true (status = `Exited 3);
      check_contains "red" ~sub:{|class="p1">red|} s;
      Alcotest.(check (list string))
        "frames"
        [ screen [ "$ ./demo.sh" ]; screen [ "$ ./demo.sh"; "red"; "done" ] ]
        (frames s);
      let duration, starts = times (unprefixed s) in
      Alcotest.(check (list (float 0.)))
        "starts" [ 0.; 500. ]
        (List.map Float.round starts);
      Alcotest.(check (float 0.))
        "the last screen holds a second" 1500. duration

let test_run_fast () =
  with_dir @@ fun env cwd ->
  Eio.Path.save ~create:(`Or_truncate 0o644)
    Eio.Path.(cwd / "out")
    "one\ntwo\nthree\n";
  match run env cwd [ "cat"; "out" ] with
  | Error e -> Alcotest.failf "%a" K7.pp_error e
  | Ok (s, status) ->
      Alcotest.(check bool) "status" true (status = `Exited 0);
      Alcotest.(check string)
        "image"
        (screen [ "$ cat out"; "one"; "two"; "three" ])
        (last (frames s))

let check_run ?vars name argv ~shown status lines =
  with_dir @@ fun env cwd ->
  match run ?vars env cwd argv with
  | Error e -> Alcotest.failf "%s: %a" name K7.pp_error e
  | Ok (s, got) ->
      Alcotest.(check bool) (name ^ ": status") true (got = status);
      Alcotest.(check string) name (screen (shown :: lines)) (last (frames s))

let test_run_env () =
  check_run "environment"
    ~vars:[ ("VHS_TEST", "caf\xc3\xa9 au lait") ]
    [ "sh"; "-c"; {|printf %s "$VHS_TEST|$TERM"|} ]
    ~shown:{|$ sh -c 'printf %s "$VHS_TEST|$TERM"'|} (`Exited 0)
    [ "caf\xc3\xa9 au lait|xterm-256color" ]

let test_run_path_override () =
  with_dir @@ fun env cwd ->
  script cwd "k7-path-only" "#!/bin/sh\nprintf PATH_OVERRIDE_WORKED\n";
  let path = Eio.Path.native_exn cwd in
  match run ~vars:[ ("PATH", path) ] ~show:"" env cwd [ "k7-path-only" ] with
  | Error e -> Alcotest.failf "%a" K7.pp_error e
  | Ok (image, status) ->
      Alcotest.(check bool)
        "executable in overridden PATH runs" true
        (status = `Exited 0);
      Alcotest.(check string)
        "program output"
        (screen [ "PATH_OVERRIDE_WORKED" ])
        (last (frames image))

let test_run_sigpipe () =
  let previous = Sys.signal Sys.sigpipe Sys.Signal_ignore in
  Fun.protect ~finally:(fun () -> Sys.set_signal Sys.sigpipe previous)
  @@ fun () ->
  check_run "normal terminal SIGPIPE disposition"
    [ "/bin/sh"; "-c"; "kill -PIPE $$; printf SIGPIPE_WAS_IGNORED" ]
    ~shown:"$ /bin/sh -c 'kill -PIPE $$; printf SIGPIPE_WA"
    (`Signaled Sys.sigpipe) [ "S_IGNORED'" ]

let test_record_sigpipe () =
  let previous = Sys.signal Sys.sigpipe Sys.Signal_ignore in
  Fun.protect ~finally:(fun () -> Sys.set_signal Sys.sigpipe previous)
  @@ fun () ->
  with_dir @@ fun env cwd ->
  let t = tape K7.[ type' "yes | head -1\n"; wait Prompt; sleep (Ms 100) ] in
  match record env cwd t with
  | Error e -> Alcotest.failf "%a" K7.pp_error e
  | Ok image ->
      Alcotest.(check bool)
        "a normal pipeline has no broken-pipe diagnostic" false
        (contains ~sub:"Broken pipe" image)

let test_run_exec_errors () =
  with_dir @@ fun env cwd ->
  Eio.Path.save ~create:(`Or_truncate 0o644)
    Eio.Path.(cwd / "not-executable")
    "#!/bin/sh\nprintf SHOULD_NOT_RUN\n";
  let outcomes =
    List.map
      (fun (cwd, argv) ->
        match run env cwd argv with
        | Error (K7.Io _) -> "Io"
        | Error e -> Fmt.str "%a" K7.pp_error e
        | Ok (_, status) -> (
            match status with
            | `Exited n -> Fmt.str "exit %d" n
            | `Signaled n -> Fmt.str "signal %d" n))
      [
        (cwd, [ "./not-executable" ]);
        (cwd, [ "." ]);
        (cwd, [ "/bin/echo"; "a\000b" ]);
        (Eio.Path.(cwd / "missing-directory"), [ "/usr/bin/true" ]);
      ]
  in
  Alcotest.(check (list string))
    "exec and chdir failures are I/O errors" [ "Io"; "Io"; "Io"; "Io" ] outcomes

let test_run_inherited_fd () =
  with_dir @@ fun env cwd ->
  Eio.Path.save ~create:(`Or_truncate 0o600)
    Eio.Path.(cwd / "private-data")
    "FD_SENTINEL\n";
  let fd =
    Unix.openfile
      (Eio.Path.native_exn Eio.Path.(cwd / "private-data"))
      [ O_RDONLY ] 0
  in
  Unix.clear_close_on_exec fd;
  Fun.protect ~finally:(fun () -> Unix.close fd) @@ fun () ->
  script cwd "inspect-fds.sh"
    "#!/bin/bash\n\
     n=3\n\
     while [ \"$n\" -lt 64 ]; do\n\
     value=\n\
     IFS= read -r -t 1 -u \"$n\" value 2>/dev/null\n\
     if [ \"$value\" = FD_SENTINEL ]; then printf FD_LEAKED; fi\n\
     n=$((n+1))\n\
     done\n\
     printf DONE\n";
  match run ~show:"" env cwd [ "./inspect-fds.sh" ] with
  | Error e -> Alcotest.failf "%a" K7.pp_error e
  | Ok (image, _) ->
      Alcotest.(check string)
        "only standard descriptors reach the program" (screen [ "DONE" ])
        (last (frames image))

let test_run_errors () =
  with_dir @@ fun env cwd ->
  let error name ?vars argv check =
    match run ?vars env cwd argv with
    | Error e when check e -> ()
    | Error e -> Alcotest.failf "%s: %a" name K7.pp_error e
    | Ok _ -> Alcotest.failf "%s: ran" name
  in
  let invalid = function K7.Invalid _ -> true | _ -> false in
  error "no command" [] invalid;
  error "not found" [ "vhs-no-such-program" ] (function
    | Executable_not_found "vhs-no-such-program" -> true
    | _ -> false);
  error "not found relative to cwd" [ "./demo.sh" ] (function
    | Executable_not_found _ -> true
    | _ -> false);
  error "empty name" ~vars:[ ("", "x") ] [ "true" ] invalid;
  error "name with =" ~vars:[ ("A=B", "x") ] [ "true" ] invalid;
  error "NUL" ~vars:[ ("A", "x\000y") ] [ "true" ] invalid;
  match
    K7.run ~settings:[ Width 800; Columns 80 ]
      ~clock:(Eio.Stdenv.mono_clock env)
      ~proc:(Eio.Stdenv.process_mgr env)
      ~cwd [ "true" ]
  with
  | Error (Invalid _) -> ()
  | Error e -> Alcotest.failf "width and columns: %a" K7.pp_error e
  | Ok _ -> Alcotest.fail "width and columns: ran"

let size svg =
  let key = {|viewBox="|} in
  let rec find i =
    if i + String.length key > String.length svg then Alcotest.fail "no viewBox"
    else if String.sub svg i (String.length key) = key then
      let j = i + String.length key in
      String.sub svg j (String.index_from svg j '"' - j)
    else find (i + 1)
  in
  find 0

let test_run_command_line () =
  with_dir @@ fun env cwd ->
  let shown = {|$ printf '%s\n' 'hello world'|} in
  match run env cwd [ "printf"; {|%s\n|}; "hello world" ] with
  | Error e -> Alcotest.failf "%a" K7.pp_error e
  | Ok (s, _) ->
      let first = List.hd (frames s) in
      if not (String.starts_with ~prefix:shown first) then
        Alcotest.failf "first frame: %S" first;
      Alcotest.(check string)
        "last frame"
        (screen [ shown; "hello world" ])
        (last (frames s));
      check_contains "prompt" ~sub:{|fill="#5a56e0">$</tspan>|} s;
      check_contains "command"
        ~sub:{|<tspan x="28 42 56 70 84 98">printf</tspan>|} s

let test_run_rows () =
  with_dir @@ fun env cwd ->
  let n = Native_test.rows - 2 in
  match run env cwd [ "seq"; string_of_int n ] with
  | Error e -> Alcotest.failf "%a" K7.pp_error e
  | Ok (s, _) ->
      Alcotest.(check string)
        "last frame"
        (screen
           (Fmt.str "$ seq %d" n :: List.init n (fun i -> Int.to_string (i + 1))))
        (last (frames s));
      Alcotest.(check string) "size" (size (svg [])) (size s)

let test_run_show () =
  with_dir @@ fun env cwd ->
  let image ?show argv =
    match run ?show env cwd argv with
    | Ok (s, _) -> last (frames s)
    | Error e -> Alcotest.failf "%a" K7.pp_error e
  in
  Alcotest.(check string)
    "show"
    (screen [ "$ dune exec -- ./main.exe"; "hi" ])
    (image ~show:"dune exec -- ./main.exe" [ "echo"; "hi" ]);
  Alcotest.(check string)
    "hidden" (screen [ "hi" ])
    (image ~show:"" [ "echo"; "hi" ]);
  Alcotest.(check string)
    "quoted"
    (screen
       [
         {|$ printf '%s|' '' 'it'\''s' $'a\nb' |} ^ "caf\xc3\xa9";
         "|it's|a";
         "b|caf\xc3\xa9|";
       ])
    (image [ "printf"; "%s|"; ""; "it's"; "a\nb"; "caf\xc3\xa9" ]);
  match run ~show:"a\027[2Jb" env cwd [ "true" ] with
  | Error (Invalid _) -> ()
  | Error e -> Alcotest.failf "control: %a" K7.pp_error e
  | Ok _ -> Alcotest.fail "control: ran"

let test_run_signal () =
  check_run "killed"
    [ "sh"; "-c"; "echo bye; kill -TERM $$" ]
    ~shown:{|$ sh -c 'echo bye; kill -TERM $$'|} (`Signaled Sys.sigterm)
    [ "bye" ]

let test_run_cancel () =
  with_dir @@ fun env cwd ->
  let clock = Eio.Stdenv.clock env in
  let pid_file = Eio.Path.(cwd / "run.pid") in
  let ready () =
    match String.trim (Eio.Path.load pid_file) with
    | "" -> false
    | _ -> true
    | exception Eio.Io _ -> false
  in
  let result =
    Eio.Fiber.first
      (fun () ->
        `Ran (run env cwd [ "sh"; "-c"; "echo $$ > run.pid; exec sleep 30" ]))
      (fun () ->
        while not (ready ()) do
          Eio.Time.sleep clock 0.001
        done;
        `Cancelled)
  in
  (match result with
  | `Cancelled -> ()
  | `Ran _ -> Alcotest.fail "the program ended before cancellation");
  let pid = int_of_string (String.trim (Eio.Path.load pid_file)) in
  match Unix.kill pid 0 with
  | () ->
      (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
      Alcotest.failf "process %d left alive" pid
  | exception Unix.Unix_error (ESRCH, _, _) -> ()

let test_run_cancel_descendant () =
  with_dir @@ fun env cwd ->
  let clock = Eio.Stdenv.clock env in
  script cwd "parent.sh"
    "#!/bin/sh\n\
     trap '' HUP TERM\n\
     /bin/sh -c 'trap \"\" HUP TERM; echo $$ > child.pid; exec sleep 30' &\n\
     wait\n";
  let pid_file = Eio.Path.(cwd / "child.pid") in
  let ready () =
    try String.trim (Eio.Path.load pid_file) <> "" with Eio.Io _ -> false
  in
  let result =
    Eio.Time.with_timeout_exn clock 60. @@ fun () ->
    Eio.Fiber.first
      (fun () -> `Ran (run env cwd [ "./parent.sh" ]))
      (fun () ->
        while not (ready ()) do
          Eio.Time.sleep clock 0.001
        done;
        `Cancelled)
  in
  let pid = int_of_string (String.trim (Eio.Path.load pid_file)) in
  let alive =
    try
      Unix.kill pid 0;
      true
    with Unix.Unix_error (ESRCH, _, _) -> false
  in
  (if alive then try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
  Alcotest.(check bool) "recording was cancelled" true (result = `Cancelled);
  Alcotest.(check bool) "cancelled recording leaves no descendant" false alive

let test_record_env_names () =
  with_dir @@ fun env cwd ->
  List.iter
    (fun line ->
      match
        Result.bind
          (K7.tape ~settings ~outputs:[ "demo.svg" ]
             K7.[ raw line; type' "echo hi\n"; wait Prompt ])
          (play env cwd)
      with
      | Error _ -> ()
      | Ok () -> Alcotest.failf "%s: recorded" line)
    [ {|Env "A=B" "x"|}; {|Env "" "x"|} ]

let test_run_concurrent_terminals () =
  with_dir @@ fun env cwd ->
  script cwd "ttys.sh"
    "#!/bin/bash\n\
     for f in /dev/fd/*; do n=${f##*/}; if [ \"$n\" -gt 2 ] && [ -t \"$n\" ]; \
     then printf 'LEAK%s ' \"$n\"; fi; done\n\
     printf DONE\n";
  let clock = Eio.Stdenv.clock env in
  let _, second =
    Eio.Fiber.pair
      (fun () -> run env cwd [ "sleep"; "2" ])
      (fun () ->
        Eio.Time.sleep clock 0.5;
        run ~show:"" env cwd [ "./ttys.sh" ])
  in
  match second with
  | Error e -> Alcotest.failf "%a" K7.pp_error e
  | Ok (image, _) ->
      Alcotest.(check string)
        "no other terminal reaches the program" (screen [ "DONE" ])
        (last (frames image))

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
         ("modified arrows", test_modified_arrows);
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
         ("run a program", test_run_program);
         ("run a program that exits at once", test_run_fast);
         ("run environment", test_run_env);
         ("run searches the overridden PATH", test_run_path_override);
         ("run restores SIGPIPE", test_run_sigpipe);
         ("record restores SIGPIPE", test_record_sigpipe);
         ("run reports exec failures as I/O errors", test_run_exec_errors);
         ( "run does not inherit recorder file descriptors",
           test_run_inherited_fd );
         ("run errors", test_run_errors);
         ("run a killed program", test_run_signal);
         ("run shows the command line", test_run_command_line);
         ("run keeps the rows", test_run_rows);
         ("run shows another line", test_run_show);
         ("run cancelled", test_run_cancel);
         ( "run cancellation reaps resistant descendants",
           test_run_cancel_descendant );
         ("record refuses Env names", test_record_env_names);
         ("concurrent runs keep their terminals", test_run_concurrent_terminals);
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
