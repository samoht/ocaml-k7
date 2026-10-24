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
      check_contains "green" ~sub:{|-p2">green|} s;
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

let test_stuck_output_bound () =
  with_dir @@ fun env cwd ->
  let complete = ref false and largest = ref 0 in
  let stuck output =
    largest := max !largest (String.length output);
    if contains ~sub:"OUTPUT_COMPLETE\r\n" output then complete := true;
    !complete
  in
  let t =
    tape
      K7.
        [
          type'
            "head -c 70000 /dev/zero | tr '\\000' x; printf \
             \"\\nOUTPUT_COMPLETE\\n\"; read ignored\n";
          wait (Line "NEVER_READY");
        ]
  in
  with_clock ~release:(fun () -> !complete) @@ fun clock ->
  (match
     K7.record ~engine:Native ~env:no_job_control ~stuck ~clock
       ~proc:(Eio.Stdenv.process_mgr env)
       ~cwd t
   with
  | Error (Stuck _) -> ()
  | Error e -> Alcotest.failf "%a" K7.pp_error e
  | Ok () -> Alcotest.fail "the callback did not stop the wait");
  Alcotest.(check bool)
    "the complete fixed-size output reached the callback" true !complete;
  Alcotest.(check bool)
    (Fmt.str "callback gets at most the last 64 KiB, got %d bytes" !largest)
    true (!largest <= 65536)

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
    tape
      K7.
        [
          type' "echo hi\n";
          wait (Screen "(?m)^hi$");
          wait ~timeout:(Ms 300) (Screen "never");
        ]
  in
  match record env cwd t with
  | Error (Timeout { wait; line; last; _ } as e) ->
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

(* Typed at once, before the clock moves. *)
let start = K7.type' "touch started\n"

(* The clock stands still until [start] has run, so that the shell's first
   prompt arrives in its time. *)
let started cwd =
  let path = Eio.Path.native_exn cwd ^ "/started" in
  if Sys.file_exists path then Sys.remove path;
  fun () -> Sys.file_exists path

let test_timeout_precedence () =
  with_dir @@ fun env cwd ->
  let times_out ~within t =
    with_clock ~hold:0. ~release:(started cwd) @@ fun clock ->
    (match
       K7.record ~engine:Native ~env:no_job_control ~clock
         ~proc:(Eio.Stdenv.process_mgr env)
         ~cwd t
     with
    | Error (Timeout _) -> ()
    | Ok _ -> Alcotest.fail "no timeout"
    | Error e -> Alcotest.failf "%a" K7.pp_error e);
    let s = Int64.to_float (Mtime.to_uint64_ns (Eio.Time.Mono.now clock)) in
    if s /. 1e9 >= within then Alcotest.failf "timed out after %gs" (s /. 1e9)
  in
  let tape timeout actions =
    Result.get_ok
      (K7.with_settings
         [ Wait_timeout timeout; Typing_speed (Ms 0) ]
         (tape (start :: actions)))
  in
  times_out ~within:15.
    (tape (Ms 60_000)
       K7.[ type' "echo hi\n"; wait ~timeout:(Ms 300) (Screen "never") ]);
  times_out ~within:15.
    (tape (Ms 300) K7.[ type' "echo hi\n"; wait (Screen "never") ])

(* The waits never time out, so the frames are the prompt at 0ms and the
   command at 500ms, in a recording of a second, however late the shell's
   output arrives. A frame lasts at least 10ms at 50 frames a second, so a
   speed above 2 plays as 2. *)
let test_playback_speed () =
  with_dir @@ fun env cwd ->
  let loop speed =
    with_clock @@ fun clock ->
    let t =
      Result.get_ok
        (K7.tape
           ~settings:(Typing_speed (Ms 0) :: settings)
           ~outputs:[ "demo.svg" ]
           K7.
             [
               raw ("Set PlaybackSpeed " ^ speed);
               sleep (Ms 500);
               type' "echo hi\n";
               wait Prompt;
               sleep (Ms 500);
             ])
    in
    match
      K7.record ~engine:Native ~env:no_job_control ~clock
        ~proc:(Eio.Stdenv.process_mgr env)
        ~cwd t
    with
    | Error e -> Alcotest.failf "%a" K7.pp_error e
    | Ok () ->
        let svg = Eio.Path.load Eio.Path.(cwd / "demo.svg") in
        let suffix = "ms steps(1,end) infinite" in
        let n = String.length suffix in
        let rec find i =
          if i + n > String.length svg then Alcotest.fail "no animation"
          else if String.sub svg i n = suffix then i
          else find (i + 1)
        in
        let j = find 0 in
        let k = ref j in
        while Char.Ascii.is_digit svg.[!k - 1] do
          decr k
        done;
        int_of_string (String.sub svg !k (j - !k))
  in
  Alcotest.(check int) "real time" 1000 (loop "1");
  Alcotest.(check int) "twice as fast" 500 (loop "2");
  Alcotest.(check int) "clamped" 500 (loop "100000");
  Alcotest.(check int) "slower" 4000 (loop "0.25")

(* A shell that shows no prompt fails the recording within the tape's wait
   timeout, by the clock of record. *)
let test_first_prompt () =
  with_dir @@ fun env cwd ->
  with_clock ~hold:Float.infinity @@ fun clock ->
  let t =
    Result.get_ok
      (K7.tape
         ~settings:(K7.Wait_timeout (Ms 300) :: settings)
         ~outputs:[ "demo.svg" ]
         K7.[ raw {|Env PS1 ""|}; type' "touch played\n"; sleep (Ms 100) ])
  in
  (match
     K7.record ~engine:Native ~env:no_job_control ~clock
       ~proc:(Eio.Stdenv.process_mgr env)
       ~cwd t
   with
  | Error (Timeout { line = 0; _ } as e) ->
      Alcotest.(check string)
        "message"
        "timed out waiting for the shell's first prompt; give the tape a \
         longer Wait_timeout"
        (Fmt.str "%a" K7.pp_error e)
  | Ok () -> Alcotest.fail "recorded without a prompt"
  | Error e -> Alcotest.failf "%a" K7.pp_error e);
  Alcotest.(check bool)
    "nothing typed" false
    (Eio.Path.is_file Eio.Path.(cwd / "played"));
  Alcotest.(check bool)
    "within the wait timeout" true
    (Mtime.to_uint64_ns (Eio.Time.Mono.now clock) < 5_000_000_000L)

(* In the environment of Native_test.no_job_control. *)
let test_no_job_control () =
  with_dir @@ fun env cwd ->
  match
    record env cwd (tape K7.[ type' "echo \"flags $-\"\n"; wait Prompt ])
  with
  | Error e -> Alcotest.failf "%a" K7.pp_error e
  | Ok s -> (
      match
        List.find_opt
          (String.starts_with ~prefix:"flags ")
          (String.split_on_char '\n' (last (frames s)))
      with
      | Some flags ->
          Alcotest.(check bool) flags false (String.contains flags 'm')
      | None -> Alcotest.fail "no flags")

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

(* As VHS, a tape that ends hidden ends on its last shown frame. *)
let test_hidden_end () =
  with_dir @@ fun env cwd ->
  let t =
    tape
      K7.
        [
          type' "echo shown\n";
          wait Prompt;
          hidden [ type' "echo bye\n"; wait Prompt ];
        ]
  in
  match record env cwd t with
  | Error e -> Alcotest.failf "%a" K7.pp_error e
  | Ok s ->
      Alcotest.(check bool)
        "hidden output not shown" false (contains ~sub:"bye" s);
      Alcotest.(check string)
        "final"
        (screen [ "> echo shown"; "shown"; ">" ])
        (last (frames s))

(* A frame in synchronized output that reaches the terminal in two reads
   shows once complete. *)
let test_synchronized () =
  with_dir @@ fun env cwd ->
  let t =
    tape
      K7.
        [
          type' "printf '\\033[?2026hHA''LF'; read x; printf '\\033[?2026l'\n";
          wait (Screen "(?m)^HALF$");
          sleep (Ms 100);
          type' "done\n";
          wait Prompt;
        ]
  in
  match record env cwd t with
  | Error e -> Alcotest.failf "%a" K7.pp_error e
  | Ok s ->
      let half f = List.mem "HALF" (String.split_on_char '\n' f) in
      Alcotest.(check bool) "half frame" false (List.exists half (frames s));
      check_contains "whole frame" ~sub:"\nHALFdone\n" (last (frames s))

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
  match record env cwd (tape [ "demo.svg"; "demo.txt" ] actions) with
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
      | l -> Alcotest.failf "%d screens" (List.length l))

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
      {|<circle cx="775" cy="25" r="5.5" fill="#171717"/>|};
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
  executable
    (Eio.Path.native_exn Eio.Path.(cwd / "fake-vhs"))
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
    K7.record ~engine:Vhs ~vhs:fake
      ~clock:(Eio_mock.Clock.Mono.make ())
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

(* The screen wait observes the new prompt before the line wait starts.
   Neither wait can race the child or depend on a particular PTY split. *)
let test_prompt_after_noecho_screen_wait () =
  with_dir @@ fun env cwd ->
  let t =
    tape
      K7.
        [
          type'
            "stty -echo; echo READY; IFS= read -r line; stty echo; printf DONE\n";
          wait (Screen "(?m)^READY$");
          type' "x\n";
          wait (Screen "(?m)^DONE>$");
          wait ~timeout:(Ms 1) Prompt;
        ]
  in
  match play env cwd t with
  | Ok () -> ()
  | Error e ->
      Alcotest.failf "the new prompt is already on screen: %a" K7.pp_error e

(* VHS's modifier keymap has no non-ASCII key: Shift+"é" is accepted
   by its parser but emits nothing. The following literal Type still runs. *)
let test_shift_unmapped_unicode () =
  with_dir @@ fun env cwd ->
  let t =
    tape
      K7.
        [
          type'
            "stty -echo; echo READY; IFS= read -r line; stty echo; printf '%s' \
             \"$line\" > keys; echo DONE\n";
          wait (Screen "(?m)^READY$");
          raw "Shift+\"é\"";
          type' "X";
          key Enter;
          wait Prompt;
        ]
  in
  (match play env cwd t with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%a" K7.pp_error e);
  Alcotest.(check string)
    "an unmapped modifier key emits no bytes, as in VHS" "X"
    (Eio.Path.load Eio.Path.(cwd / "keys"))

let test_modified_arrows () =
  with_dir @@ fun env cwd ->
  let t =
    tape
      K7.
        [
          type' "stty -echo; echo READY; cat -v; stty echo\n";
          wait (Screen "(?m)^READY$");
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

(* Shift does not change the character typed, Alt sends the unshifted key
   after ESC, and Ctrl sends nothing for a key xterm.js has no control for. *)
let test_modified_keymap () =
  with_dir @@ fun env cwd ->
  let t =
    tape
      K7.
        [
          type' "stty -echo; echo READY; cat -v; stty echo\n";
          wait (Screen "(?m)^READY$");
          raw "Shift+\"a\"";
          type' "|";
          raw "Alt+\"A!\"";
          type' "|";
          raw "Ctrl+@";
          type' "|";
          raw "Ctrl+Shift+@";
          type' "|";
          raw "Ctrl+^";
          type' "|";
          raw "Shift+\"s\"";
          type' "|";
          raw "Shift+\"a\xc3\xa9\xe2\x86\x90\"";
          type' "|";
          key Enter;
          key (Ctrl 'd');
          wait Prompt;
          sleep (Ms 100);
        ]
  in
  match record env cwd t with
  | Error e -> Alcotest.failf "%a" K7.pp_error e
  | Ok s ->
      check_contains "bytes" ~sub:"a|^[a^[1||^@|^^||a^[[1;2D|" (last (frames s))

let test_require_non_executable () =
  with_dir @@ fun env cwd ->
  Eio.Path.save ~create:(`Or_truncate 0o644)
    Eio.Path.(cwd / "not-executable")
    "data\n";
  let t =
    tape
      K7.
        [
          raw "Require ./not-executable";
          type' "touch ran\n";
          wait Prompt;
          sleep (Ms 100);
        ]
  in
  let result = play env cwd t in
  Alcotest.(check bool)
    "a failed executable prerequisite prevents all actions" false
    (Sys.file_exists (Eio.Path.native_exn Eio.Path.(cwd / "ran")));
  match result with
  | Error (K7.Executable_not_found "./not-executable") -> ()
  | Error e -> Alcotest.failf "%a" K7.pp_error e
  | Ok () -> Alcotest.fail "Require accepted a file without execute permission"

(* VHS's non-negative repeat counts are Go ints, including values that
   exceed OCaml's tagged int range. Preflight need not execute the count. *)
let test_upper_half_key_repeat () =
  with_dir @@ fun _env cwd ->
  let source = "Output repeat.svg\nSpace@0ms 4611686018427387904\n" in
  let t =
    match K7.of_string source with
    | Ok t -> t
    | Error e -> Alcotest.failf "%a" K7.pp_error e
  in
  match K7.check ~engine:Native ~cwd t with
  | Ok () -> ()
  | Error e -> Alcotest.failf "valid VHS repeat count: %a" K7.pp_error e

let test_unparsed_key_repeat () =
  with_dir @@ fun env cwd ->
  let t =
    tape
      K7.
        [
          type'
            "stty -echo; echo READY; IFS= read -r line; stty echo; printf '%s' \
             \"$line\" > keys; echo DONE\n";
          wait (Screen "(?m)^READY$");
          raw "Space@0ms 1.5";
          type' "a";
          raw "Space@0ms 99999999999999999999";
          type' "b";
          raw "Space@0ms 0";
          type' "c";
          key Enter;
          wait Prompt;
        ]
  in
  (match play env cwd t with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%a" K7.pp_error e);
  Alcotest.(check string)
    "a count Go's Atoi rejects presses once" " a bc"
    (Eio.Path.load Eio.Path.(cwd / "keys"))

let test_large_key_repeat () =
  with_dir @@ fun _env cwd ->
  let t =
    match K7.of_string "Output repeat.svg\nSpace@0ms 100001\n" with
    | Ok t -> t
    | Error e -> Alcotest.failf "%a" K7.pp_error e
  in
  match K7.check ~engine:Native ~cwd t with
  | Ok () -> ()
  | Error e ->
      Alcotest.failf "a valid repeat count was refused: %a" K7.pp_error e

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
  check_contains "late frame" ~sub:"99.99999999977%{"
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
        {|not recorded natively: Output "demo.gif": a GIF, which the Vhs engine records|}
      );
      ( [ "demo.mp4" ],
        [],
        {|not recorded natively: Output "demo.mp4": an MP4 video, which the Vhs engine records|}
      );
      ( [ "frames/" ],
        [],
        {|not recorded natively: Output "frames/": PNG frames, which the Vhs engine records|}
      );
      ( [ "demo.svg"; "demo.webm" ],
        [],
        {|not recorded natively: Output "demo.webm": a WebM video, which the Vhs engine records|}
      );
      ( [ "demo.svg" ],
        [ {|Screenshot "shot.png"|} ],
        {|not recorded natively: Screenshot "shot.png": a PNG image, which the Vhs engine takes|}
      );
    ]

let test_source_wait_line () =
  with_dir @@ fun env cwd ->
  Eio.Path.save ~create:(`Or_truncate 0o644)
    Eio.Path.(cwd / "inc.tape")
    "# included tape\n\
     Output \"ignored.txt\"\n\
     Sleep 20ms\n\
     Wait+Line /NEVER_MATCH/\n";
  let t = tape K7.[ raw {|Source "inc.tape"|}; sleep (Ms 100) ] in
  with_clock @@ fun clock ->
  match
    K7.record ~engine:Native ~env:no_job_control
      ~stuck:(fun _ -> true)
      ~clock
      ~proc:(Eio.Stdenv.process_mgr env)
      ~cwd t
  with
  | Error (Stuck { line; wait; _ }) ->
      Alcotest.(check string) "the included wait" "Wait+Line /NEVER_MATCH/" wait;
      Alcotest.(check int)
        "line in inc.tape, including comments and Output" 4 line
  | Error e -> Alcotest.failf "%a" K7.pp_error e
  | Ok () -> Alcotest.fail "an impossible wait succeeded"

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
  let clock = Eio_mock.Clock.Mono.make ()
  and proc = Eio.Stdenv.process_mgr env in
  (match K7.record ~engine:Vhs ?vhs:oracle ~clock ~proc ~cwd vhs_tape with
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
        K7.record ~engine:Vhs ~vhs
          ~clock:(Eio_mock.Clock.Mono.make ())
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
        K7.record ~engine:Vhs ~vhs:exe
          ~clock:(Eio_mock.Clock.Mono.make ())
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

let run ?clock ?vars ?show ?delay env cwd argv =
  let run clock =
    K7.run ~settings ?env:vars ?show ?delay ~clock
      ~proc:(Eio.Stdenv.process_mgr env)
      ~cwd argv
  in
  match clock with Some c -> run (c :> clock) | None -> with_clock run

let script cwd name src =
  executable (Eio.Path.native_exn Eio.Path.(cwd / name)) src

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
   its exit land on that frame, after the line's 500ms, whatever the
   machine's load. *)
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
  let result, () =
    Eio.Fiber.pair
      (fun () -> run ~clock env cwd [ "./demo.sh" ])
      (fun () ->
        until (fun () -> Sys.file_exists (Eio.Path.native_exn cwd ^ "/started"));
        Eio_mock.Clock.Mono.set_time clock (Mtime.of_uint64_ns 500_000_000L);
        Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(cwd / "go") "")
  in
  match result with
  | Error e -> Alcotest.failf "%a" K7.pp_error e
  | Ok (s, status) ->
      Alcotest.(check bool) "status" true (status = `Exited 3);
      check_contains "red" ~sub:{|-p1">red|} s;
      Alcotest.(check (list string))
        "frames"
        [ screen [ "$ ./demo.sh" ]; screen [ "$ ./demo.sh"; "red"; "done" ] ]
        (frames s);
      let duration, starts = times (unprefixed s) in
      Alcotest.(check (list (float 0.)))
        "starts" [ 0.; 1000. ]
        (List.map Float.round starts);
      Alcotest.(check (float 0.))
        "the last screen holds a second" 2000. duration

(* The clock never moves, so the program's output arrives at 0ms. *)
(* What a program writes at once, more than the terminal holds, is all read
   before run returns. The clock stands still. *)
let test_run_burst () =
  with_dir @@ fun env cwd ->
  Eio.Path.save ~create:(`Or_truncate 0o644)
    Eio.Path.(cwd / "burst")
    (String.make 65_536 'x' ^ "\r\027[2KFINAL_MARKER");
  let settings = K7.[ Columns 40; Rows 6; Font_size 10 ] in
  let last image =
    match Svg_check.frames ~cell_width:7 ~cell_height:13 ~rows:6 image with
    | Ok frames -> List.nth frames (List.length frames - 1)
    | Error e -> Alcotest.fail e
  in
  let expected =
    K7.svg ~settings
      [
        ( Ms 0,
          String.concat "\r\n"
            (List.init 5 (fun _ -> String.make 40 'x') @ [ "FINAL_MARKER" ]) );
      ]
  in
  match
    ( K7.run ~settings ~show:""
        ~clock:(Eio_mock.Clock.Mono.make ())
        ~proc:(Eio.Stdenv.process_mgr env)
        ~cwd [ "/bin/cat"; "burst" ],
      expected )
  with
  | Ok (image, `Exited 0), Ok expected ->
      Alcotest.(check string) "last screen" (last expected) (last image)
  | Error e, _ | _, Error e -> Alcotest.failf "%a" K7.pp_error e
  | Ok (_, status), _ -> Alcotest.failf "cat: %a" Eio.Process.pp_status status

let test_run_delay () =
  with_dir @@ fun env cwd ->
  let clock = Eio_mock.Clock.Mono.make () in
  let image ?show ?delay name =
    match run ~clock ?show ?delay env cwd [ "echo"; "hi" ] with
    | Error e -> Alcotest.failf "%s: %a" name K7.pp_error e
    | Ok (s, _) -> s
  in
  let line = screen [ "$ echo hi" ] and both = screen [ "$ echo hi"; "hi" ] in
  let check ?delay name beat =
    let s = image ?delay name in
    Alcotest.(check (list string)) (name ^ ": frames") [ line; both ] (frames s);
    Alcotest.(check (list (float 0.)))
      (name ^ ": starts") [ 0.; beat ]
      (List.map Float.round (snd (times (unprefixed s))))
  in
  check "default" 500.;
  check "longer" ~delay:(Ms 2000) 2000.;
  Alcotest.(check (list string))
    "at once" [ both ]
    (frames (image ~delay:(Ms 0) "at once"));
  Alcotest.(check (list string))
    "no line"
    [ screen [ "hi" ] ]
    (frames (image ~show:"" "no line"));
  match run ~clock ~delay:(Ms (-1)) env cwd [ "true" ] with
  | Error (Invalid msg) ->
      Alcotest.(check string) "negative" "delay -1ms: negative duration" msg
  | Ok _ -> Alcotest.fail "negative delay accepted"
  | Error e -> Alcotest.failf "%a" K7.pp_error e

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

let test_run_empty_path_component () =
  with_dir @@ fun env cwd ->
  script cwd "k7-local-only" "#!/bin/sh\nprintf LOCAL_PATH_WORKS\n";
  match
    run
      ~vars:[ ("PATH", ":/k7-missing-directory") ]
      ~show:"" env cwd [ "k7-local-only" ]
  with
  | Error e -> Alcotest.failf "%a" K7.pp_error e
  | Ok (image, status) ->
      Alcotest.(check bool) "the local program ran" true (status = `Exited 0);
      Alcotest.(check string)
        "an empty PATH component searches cwd"
        (screen [ "LOCAL_PATH_WORKS" ])
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
      ~clock:(Eio_mock.Clock.Mono.make ())
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
         {|$ printf '%s|' '' 'it'\''s' 'a|};
         {|> b' caf|} ^ "\xc3\xa9";
         "|it's|a";
         "b|caf\xc3\xa9|";
       ])
    (image [ "printf"; "%s|"; ""; "it's"; "a\nb"; "caf\xc3\xa9" ]);
  Alcotest.(check string)
    "a newline that ends an argument"
    (screen [ {|$ printf '%s|' 'a|}; "> '"; "a"; "|" ])
    (image [ "printf"; "%s|"; "a\n" ]);
  Alcotest.(check string)
    "a shown newline, as in the default"
    (image [ "printf"; "a\nb" ])
    (image ~show:"printf 'a\nb'" [ "printf"; "a\nb" ]);
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
        until ready;
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
  script cwd "parent.sh"
    "#!/bin/sh\n\
     trap '' HUP TERM\n\
     /bin/sh -c 'trap \"\" HUP TERM; echo $$ > child.pid; exec sleep 100000' &\n\
     wait\n";
  let pid_file = Eio.Path.(cwd / "child.pid") in
  let ready () =
    try String.trim (Eio.Path.load pid_file) <> "" with Eio.Io _ -> false
  in
  let result =
    Eio.Fiber.first
      (fun () -> `Ran (run env cwd [ "./parent.sh" ]))
      (fun () ->
        until ready;
        `Cancelled)
  in
  Alcotest.(check bool) "recording was cancelled" true (result = `Cancelled);
  (* Only k7's SIGKILL ends it, at a time of the kernel's choosing. *)
  let pid = int_of_string (String.trim (Eio.Path.load pid_file)) in
  until (fun () ->
      match Unix.kill pid 0 with
      | () -> false
      | exception Unix.Unix_error (ESRCH, _, _) -> true)

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

let test_record_env () =
  with_dir @@ fun env cwd ->
  let t =
    tape
      K7.
        [
          type' "echo \"$GREETING\"\n";
          wait ~timeout:(Ms 10_000) (Screen "(?m)^bonjour$");
        ]
  in
  match
    with_clock @@ fun clock ->
    K7.record ~engine:Native
      ~env:(("GREETING", "bonjour") :: no_job_control)
      ~clock
      ~proc:(Eio.Stdenv.process_mgr env)
      ~cwd t
  with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%a" K7.pp_error e

let test_record_term_override () =
  with_dir @@ fun env cwd ->
  let t =
    tape K7.[ type' "printf '%s' \"$TERM\" > received-term\n"; wait Prompt ]
  in
  (match
     with_clock @@ fun clock ->
     K7.record ~engine:Native
       ~env:(("TERM", "vt100") :: no_job_control)
       ~clock
       ~proc:(Eio.Stdenv.process_mgr env)
       ~cwd t
   with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%a" K7.pp_error e);
  Alcotest.(check string)
    "the shell gets the explicit TERM" "vt100"
    (Eio.Path.load Eio.Path.(cwd / "received-term"))

let test_check () =
  with_dir @@ fun _ cwd ->
  let check ?engine name outputs expected =
    match K7.tape ~settings ~outputs K7.[ type' "touch played\n" ] with
    | Error e -> Alcotest.failf "%s: %a" name K7.pp_error e
    | Ok t ->
        let got =
          match K7.check ?engine ~cwd t with
          | Ok () -> "ok"
          | Error e -> Fmt.str "%a" K7.pp_error e
        in
        Alcotest.(check string) name expected got
  in
  check "svg" [ "demo.svg"; "demo.txt" ] "ok";
  check "gif" [ "demo.gif" ] "ok";
  check "native gif" ~engine:Native [ "demo.gif" ]
    {|not recorded natively: Output "demo.gif": a GIF, which the Vhs engine records|};
  check "vhs svg" ~engine:Vhs [ "demo.svg" ]
    {|not recorded by vhs: Output "demo.svg": an SVG image, which the Native engine records|};
  check "frames" [ "none/frames/" ] {|none/frames/: no directory none|};
  Alcotest.(check bool)
    "nothing written" false
    (List.exists
       (fun f -> Eio.Path.is_file Eio.Path.(cwd / f))
       [ "demo.svg"; "demo.txt"; "played" ])

let test_output_directory_preflight () =
  with_dir @@ fun env cwd ->
  Eio.Path.mkdir ~perm:0o755 Eio.Path.(cwd / "occupied.svg");
  let t =
    Result.get_ok
      (K7.tape ~settings ~outputs:[ "occupied.svg" ]
         K7.[ type' "touch played\n"; wait Prompt ])
  in
  let checked = K7.check ~engine:Native ~cwd t in
  let recorded = play env cwd t in
  (match recorded with
  | Error (Io _) -> ()
  | Error e -> Alcotest.failf "expected I/O error: %a" K7.pp_error e
  | Ok () -> Alcotest.fail "recorded over a directory");
  Alcotest.(check bool)
    "unwritable output is refused before playing" false
    (Eio.Path.is_file Eio.Path.(cwd / "played"));
  Alcotest.(check (result unit string))
    "check returns the recording's preflight error"
    (Result.map_error (Fmt.str "%a" K7.pp_error) recorded)
    (Result.map_error (Fmt.str "%a" K7.pp_error) checked)

let test_screenshot_real_path () =
  with_dir @@ fun env cwd ->
  let dir = Unix.realpath (Eio.Path.native_exn cwd) in
  Unix.symlink "." (Filename.concat dir "here");
  let refused shot =
    let t =
      match
        K7.tape ~settings ~outputs:[ "demo.svg" ]
          K7.
            [
              type' "touch played\n";
              Fmt.kstr raw "Screenshot %S" shot;
              wait Prompt;
            ]
      with
      | Ok t -> t
      | Error e -> Alcotest.failf "%s: %a" shot K7.pp_error e
    in
    let expected =
      Fmt.str "Screenshot %S: the path of an output or another screenshot" shot
    in
    let message = Result.map_error (Fmt.to_to_string K7.pp_error) in
    Alcotest.(check (result unit string))
      (shot ^ " is refused by check")
      (Error expected)
      (message (K7.check ~engine:Native ~cwd t));
    Alcotest.(check (result unit string))
      (shot ^ " is refused by record")
      (Error expected)
      (message (play env cwd t))
  in
  refused (Filename.concat dir "demo.svg");
  refused "here/demo.svg";
  Alcotest.(check bool)
    "nothing played" false
    (Eio.Path.is_file Eio.Path.(cwd / "played"))

(* The $ COMMAND line quotes each argument so that the line runs the same
   argv; a byte that is not UTF-8 is written in octal, as a control character
   is. *)
let test_run_command_line_invalid_utf_8 () =
  with_dir @@ fun env cwd ->
  let proc = Eio.Stdenv.process_mgr env in
  let arg = "a\x9b[2Jb" in
  let image =
    match
      with_clock @@ fun clock ->
      K7.run
        ~settings:K7.[ Columns 200; Rows 6; Font_size 10; Padding 0 ]
        ~clock ~proc ~cwd
        [ "/usr/bin/printf"; "%s"; arg ]
    with
    | Ok (image, `Exited 0) -> image
    | Ok (_, status) -> Alcotest.failf "printf: %a" Eio.Process.pp_status status
    | Error e -> Alcotest.failf "%a" K7.pp_error e
  in
  let line =
    match Svg_check.frames ~cell_width:7 ~cell_height:13 ~rows:6 image with
    | Ok (first :: _) -> List.hd (String.split_on_char '\n' first)
    | Ok [] -> Alcotest.fail "no frame"
    | Error e -> Alcotest.fail e
  in
  let command = String.sub line 2 (String.length line - 2) in
  let output = Buffer.create 16 in
  Eio.Process.run
    ~stdout:(Eio.Flow.buffer_sink output)
    proc
    [ "/bin/bash"; "-c"; command ];
  Alcotest.(check string)
    (Fmt.str "%S runs the same argv" line)
    (String.escaped arg)
    (String.escaped (Buffer.contents output))

(* The first program runs until the second one is done. *)
let test_run_concurrent_terminals () =
  with_dir @@ fun env cwd ->
  script cwd "first.sh"
    "#!/bin/sh\n: > first\nwhile [ ! -e second ]; do sleep 0.01; done\n";
  script cwd "ttys.sh"
    "#!/bin/bash\n\
     for f in /dev/fd/*; do n=${f##*/}; if [ \"$n\" -gt 2 ] && [ -t \"$n\" ]; \
     then printf 'LEAK%s ' \"$n\"; fi; done\n\
     printf DONE\n\
     : > second\n";
  let _, second =
    Eio.Fiber.pair
      (fun () -> run env cwd [ "./first.sh" ])
      (fun () ->
        until (fun () -> Eio.Path.is_file Eio.Path.(cwd / "first"));
        run ~show:"" env cwd [ "./ttys.sh" ])
  in
  match second with
  | Error e -> Alcotest.failf "%a" K7.pp_error e
  | Ok (image, _) ->
      Alcotest.(check string)
        "no other terminal reaches the program" (screen [ "DONE" ])
        (last (frames image))

let capture ?show ?(clock = Eio_mock.Clock.Mono.make ()) env cwd argv =
  match
    K7.capture ~settings ?show ~clock
      ~proc:(Eio.Stdenv.process_mgr env)
      ~cwd argv
  with
  | Ok r -> r
  | Error e -> Alcotest.failf "%a" K7.pp_error e

let lines s = String.split_on_char '\n' s

let exited name n = function
  | `Exited m -> Alcotest.(check int) name n m
  | `Signaled s -> Alcotest.failf "%s: signal %d" name s

let signaled name n = function
  | `Signaled s when Int.equal s n -> ()
  | status -> Alcotest.failf "%s: %a" name Eio.Process.pp_status status

(* The recording holds the size, the command, the output at its times and the
   exit; drawn, it is the image of K7.run. *)
let test_capture () =
  with_dir @@ fun env cwd ->
  script cwd "demo.sh"
    "#!/bin/sh\n\
     : > started\n\
     while [ ! -e go ]; do sleep 0.01; done\n\
     printf '\\033[31mred\\033[0m\\n'\n\
     exit 3\n";
  let clock = Eio_mock.Clock.Mono.make () in
  let (cast, status), () =
    Eio.Fiber.pair
      (fun () -> capture ~clock env cwd [ "./demo.sh" ])
      (fun () ->
        until (fun () -> Sys.file_exists (Eio.Path.native_exn cwd ^ "/started"));
        Eio_mock.Clock.Mono.set_time clock (Mtime.of_uint64_ns 500_000_000L);
        Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(cwd / "go") "")
  in
  exited "status" 3 status;
  let src = K7.string_of_cast cast in
  Alcotest.(check (list string))
    "asciicast v3"
    [
      {|{"version":3,"term":{"cols":46,"rows":9,"type":"xterm-256color"},"command":"./demo.sh"}|};
      {|[0.500,"o","\u001B[31mred\u001B[0m\r\n"]|};
      {|[0.000,"x","3"]|};
      "";
    ]
    (lines src);
  Alcotest.(check string)
    "the last screen" "$ ./demo.sh\nred\n"
    (Result.get_ok (K7.text_of_casts [ cast ]));
  let svg = Result.get_ok (K7.svg_of_casts ~settings [ cast ]) in
  Alcotest.(check (list string))
    "frames"
    [ screen [ "$ ./demo.sh" ]; screen [ "$ ./demo.sh"; "red" ] ]
    (frames svg);
  let cast, _ = capture ~show:"" env cwd [ "true" ] in
  Alcotest.(check (list string))
    "no command"
    [
      {|{"version":3,"term":{"cols":46,"rows":9,"type":"xterm-256color"}}|};
      {|[0.000,"x","0"]|};
      "";
    ]
    (lines (K7.string_of_cast cast))

(* A character split across two writes is written whole, and a byte that is
   not UTF-8 as U+FFFD. *)
let test_capture_utf_8 () =
  with_dir @@ fun env cwd ->
  let cast, _ =
    capture ~show:"" env cwd
      [ "sh"; "-c"; "printf '\\303'; sleep 0.2; printf '\\251\\377\\n'" ]
  in
  let src = K7.string_of_cast cast in
  Alcotest.(check bool) "valid UTF-8" true (String.is_valid_utf_8 src);
  match K7.cast_of_string src with
  | Ok c ->
      Alcotest.(check string)
        "read back" "é\xef\xbf\xbd\n"
        (Result.get_ok (K7.text_of_casts [ c ]))
  | Error e -> Alcotest.failf "%a" K7.pp_error e

let test_capture_incomplete_utf_8_round_trip () =
  with_dir @@ fun env cwd ->
  let cast, status = capture ~show:"" env cwd [ "printf"; "%s"; "\xc3" ] in
  exited "printf" 0 status;
  let saved = K7.string_of_cast cast in
  Alcotest.(check bool) "the cast is UTF-8" true (String.is_valid_utf_8 saved);
  let restored =
    match K7.cast_of_string saved with
    | Ok c -> c
    | Error e -> Alcotest.failf "%a" K7.pp_error e
  in
  Alcotest.(check string)
    "saving the capture preserves its image"
    (Result.get_ok (K7.svg_of_casts [ cast ]))
    (Result.get_ok (K7.svg_of_casts [ restored ]))

let test_capture_theme_cursor () =
  with_dir @@ fun env cwd ->
  let theme =
    {|{"background": "#102030", "foreground": "#eeeeee", "cursor": "#ff0000", "cursorAccent": "#00ff00"}|}
  in
  let settings = K7.[ Columns 20; Rows 3; Theme theme ] in
  let cast, _ =
    match
      K7.capture ~settings ~show:""
        ~clock:(Eio_mock.Clock.Mono.make ())
        ~proc:(Eio.Stdenv.process_mgr env)
        ~cwd [ "printf"; "x" ]
    with
    | Ok r -> r
    | Error e -> Alcotest.failf "%a" K7.pp_error e
  in
  let restored = Result.get_ok (K7.cast_of_string (K7.string_of_cast cast)) in
  Alcotest.(check string)
    "the saved capture keeps the cursor colours"
    (Result.get_ok (K7.svg_of_casts ~settings [ cast ]))
    (Result.get_ok (K7.svg_of_casts [ restored ]))

let cram ?env:vars ?stop ?(clock = Eio_mock.Clock.Mono.make ()) env cwd argv =
  match
    K7.cram ?env:vars ?stop ~clock ~proc:(Eio.Stdenv.process_mgr env) ~cwd argv
  with
  | Ok r -> r
  | Error e -> Alcotest.failf "%a" K7.pp_error e

(* What dune writes for each command of a cram test: the output and the
   error, without escape sequences or the carriage return before a line feed,
   each line indented, then the exit code unless 0. *)
let test_cram () =
  with_dir @@ fun env cwd ->
  script cwd "out.sh"
    "#!/bin/sh\n\
     printf 'a\\n\\nb  \\n'\n\
     printf 'err\\n' >&2\n\
     printf '\\033[1mbold\\033[0m\\r\\n'\n\
     printf 'x\\ry\\n'\n\
     printf '\\303\\n'\n\
     printf 'no eol'\n\
     exit 3\n";
  let t, status = cram env cwd [ "./out.sh" ] in
  exited "status" 3 status;
  Alcotest.(check string)
    "a cram test"
    "  $ ./out.sh\n\
    \  a\n\
    \  \n\
    \  b  \n\
    \  err\n\
    \  bold\n\
    \  x\ry\n\
    \  \xc3\n\
    \  no eol\n\
    \  [3]\n"
    t;
  let t, status = cram env cwd [ "cat"; "-"; "a b" ] in
  exited "cat" 1 status;
  (match String.split_on_char '\n' t with
  | [ cmd; err; code; "" ] ->
      Alcotest.(check string) "quoted" "  $ cat - 'a b'" cmd;
      Alcotest.(check bool)
        "nothing printed, stdin empty" true
        (String.starts_with ~prefix:"  cat: " err);
      Alcotest.(check string) "exit code" "  [1]" code
  | _ -> Alcotest.failf "cat: %S" t);
  Alcotest.(check string)
    "environment" "  $ printenv K7_TEST\n  set\n"
    (fst (cram ~env:[ ("K7_TEST", "set") ] env cwd [ "printenv"; "K7_TEST" ]));
  let t = fst (cram env cwd [ "sh"; "-c"; "kill -9 $$" ]) in
  Alcotest.(check bool)
    "killed" true
    (String.starts_with ~prefix:"  $ sh -c 'kill -9 $$'\n" t
    && String.ends_with ~suffix:"\n  [137]\n" t)

(* An exec argument cannot contain NUL. Going through sh must not turn an
   impossible argv into a successful command with a truncated argument. *)
let test_cram_nul_argument () =
  with_dir @@ fun env cwd ->
  match
    K7.cram
      ~clock:(Eio_mock.Clock.Mono.make ())
      ~proc:(Eio.Stdenv.process_mgr env)
      ~cwd
      [ "printf"; "%s"; "before\000after" ]
  with
  | Error (Invalid _ | Io _) -> ()
  | Error e -> Alcotest.failf "unexpected error: %a" K7.pp_error e
  | Ok (source, status) ->
      Alcotest.failf "ran an argv containing NUL: %a\n%s" Eio.Process.pp_status
        status source

(* dune runs a cram test with sh, which is dash on Debian, and reads its
   [  > ] lines as more of the command. *)
let test_cram_control_argument () =
  with_dir @@ fun env cwd ->
  let proc = Eio.Stdenv.process_mgr env in
  let received = Eio.Path.(cwd / "received") in
  List.iter
    (fun argument ->
      let source, status =
        match
          K7.cram
            ~clock:(Eio_mock.Clock.Mono.make ())
            ~proc ~cwd
            [
              "/bin/sh";
              "-c";
              "printf '%s' \"$1\" > received";
              "k7-argv";
              argument;
            ]
        with
        | Ok r -> r
        | Error e -> Alcotest.failf "%S: %a" argument K7.pp_error e
      in
      exited "status" 0 status;
      let command = cram_command source in
      List.iter
        (fun shell ->
          Eio.Path.unlink received;
          Eio.Process.run proc ~cwd [ shell; "-c"; command ];
          Alcotest.(check string)
            (Fmt.str "%s reads %S" shell source)
            argument (Eio.Path.load received))
        shells)
    [
      "\r\r";
      "a\nb";
      "a\n";
      "\n";
      "a \n\n b\r\n";
      "-\x1b%s\\'\"$`\xc2\x9b\xff caf\xc3\xa9";
      "=ls";
    ]

(* A shell word in executable position must still name a program. *)
let check_cram_executable name =
  with_dir @@ fun env cwd ->
  let dir = Eio.Path.native_exn cwd in
  executable (Filename.concat dir name) "#!/bin/sh\nprintf 'EXECUTED\\n'\n";
  let source, status =
    cram ~env:[ ("PATH", dir ^ ":/usr/bin:/bin") ] env cwd [ name ]
  in
  exited "the named executable runs" 0 status;
  Alcotest.(check bool)
    "the executable's output is recorded" true
    (contains ~sub:"\n  EXECUTED\n" source)

let test_cram_reserved_executable () = check_cram_executable "if"
let test_cram_assignment_executable () = check_cram_executable "FOO=bar"
let test_cram_option_executable () = check_cram_executable "-"

(* The displayed command must preserve even an invisible argument character. *)
let test_run_command_format_argument () =
  with_dir @@ fun env cwd ->
  let argument = "a\u{200b}b" in
  let image, status =
    match
      run
        ~clock:(Eio_mock.Clock.Mono.make ())
        env cwd
        [ "printf"; "%s"; argument ]
    with
    | Ok result -> result
    | Error e -> Alcotest.failf "%a" K7.pp_error e
  in
  exited "printf" 0 status;
  let command = shown_command (List.hd (frames image)) in
  List.iter
    (fun shell ->
      let output = Buffer.create 32 in
      Eio.Process.run
        ~stdout:(Eio.Flow.buffer_sink output)
        (Eio.Stdenv.process_mgr env)
        [ shell; "-c"; command ];
      Alcotest.(check string)
        (shell ^ " replays the shown argument")
        argument (Buffer.contents output))
    shells

(* The background loop prints once the shell has gone, so it prints nothing
   if it stops with the shell. *)
let test_cram_stop () =
  with_dir @@ fun env cwd ->
  let script =
    "(while kill -0 $$ 2>/dev/null; do sleep 0.01; done; echo late) & : > \
     started; wait"
  in
  let stop, set = Eio.Promise.create () in
  let (t, status), () =
    Eio.Fiber.pair
      (fun () -> cram ~stop env cwd [ "sh"; "-c"; script ])
      (fun () ->
        until (fun () -> Eio.Path.is_file Eio.Path.(cwd / "started"));
        Eio.Promise.resolve set ())
  in
  signaled "terminated" Sys.sigterm status;
  Alcotest.(check string)
    "the background loop is stopped too"
    (Fmt.str "  $ sh -c '%s'\n  [143]\n" script)
    t

(* The inner shell ignores SIGTERM and holds the output open, so cram returns
   only once the SIGKILL a second later stops it. The status is that of the
   outer sh: killed where sh execs its last command (bash), terminated where
   it does not (dash). *)
let test_cram_kill () =
  with_dir @@ fun env cwd ->
  let script = "trap '' TERM; : > started; while :; do sleep 0.01; done" in
  let clock = Eio_mock.Clock.Mono.make () in
  let stop, set = Eio.Promise.create () in
  let (_, status), () =
    Eio.Fiber.pair
      (fun () -> cram ~stop ~clock env cwd [ "sh"; "-c"; script ])
      (fun () ->
        until (fun () -> Eio.Path.is_file Eio.Path.(cwd / "started"));
        Eio.Promise.resolve set ();
        until (fun () -> Eio_mock.Clock.Mono.try_advance clock))
  in
  match status with
  | `Signaled s when Int.equal s Sys.sigkill || Int.equal s Sys.sigterm -> ()
  | status -> Alcotest.failf "killed: %a" Eio.Process.pp_status status

(* The command and its background job hold the FIFO open for writing, so a
   read of it finds no writer only once both have exited. *)
let test_cram_cancel () =
  with_dir @@ fun env cwd ->
  let dir = Eio.Path.native_exn cwd in
  let fifo = Filename.concat dir "held" in
  Unix.mkfifo fifo 0o600;
  let held = Unix.openfile fifo [ O_RDONLY; O_NONBLOCK ] 0 in
  Fun.protect ~finally:(fun () -> Unix.close held) @@ fun () ->
  let script =
    "exec 3>held; sleep 100008 & echo $! > bg.pid; : > started; wait"
  in
  let result =
    Eio.Fiber.first
      (fun () -> `Ran (cram env cwd [ "sh"; "-c"; script ]))
      (fun () ->
        until (fun () -> Eio.Path.is_file Eio.Path.(cwd / "started"));
        `Cancelled)
  in
  Alcotest.(check bool) "cancelled" true (result = `Cancelled);
  match Unix.read held (Bytes.create 1) 0 1 with
  | 0 -> ()
  | _ -> Alcotest.fail "the command wrote to the FIFO"
  | exception Unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) ->
      let pid =
        int_of_string (String.trim (Eio.Path.load Eio.Path.(cwd / "bg.pid")))
      in
      (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
      Alcotest.fail "the background job outlives the cancellation"

(* A job left in the background ends with the shell. *)
let test_cram_background () =
  with_dir @@ fun env cwd ->
  Alcotest.(check string)
    "the shell's output" "  $ sh -c 'echo started; sleep 100007 &'\n  started\n"
    (fst (cram env cwd [ "sh"; "-c"; "echo started; sleep 100007 &" ]))

(* Dune reads these as commands and continuations, rather than expected
   output. The recorded command must remain the only command in its test. *)
let test_cram_output_is_data () =
  with_dir @@ fun env cwd ->
  List.iter
    (fun output ->
      let source, status = cram env cwd [ "printf"; {|%s\n|}; output ] in
      exited "printf" 0 status;
      let header = List.hd (String.split_on_char '\n' source) ^ "\n" in
      let actions src =
        let t = Result.get_ok (K7_cli.Cram.parse src) in
        let tape =
          Result.get_ok
            (K7.tape ~outputs:[ "out.svg" ] (K7_cli.Cram.actions ~setup:[] t))
        in
        K7.to_string tape |> String.split_on_char '\n'
        |> List.filter (String.starts_with ~prefix:"Type ")
      in
      Alcotest.(check (list string))
        ("output stays data: " ^ output)
        (actions header) (actions source))
    [ "$ echo injected"; "> echo injected" ]

let capture_after_domain =
  subprocess "capture after a domain" @@ fun _ ->
  let worker = Domain.spawn (fun () -> ()) in
  Domain.join worker;
  with_dir @@ fun env cwd ->
  let _, status = capture ~show:"" env cwd [ "true" ] in
  exited "true" 0 status

let test_capture_after_domain () =
  let status = capture_after_domain [] in
  Alcotest.(check bool)
    "capture still works after a domain has joined" true
    (status = Unix.WEXITED 0)

(* A recorder in a daemon may have no standard input or standard output.
   Restore both before making assertions so Alcotest keeps its own output. *)
let test_capture_closed_standard_fds () =
  with_dir @@ fun env cwd ->
  let stdin = Unix.dup Unix.stdin and stdout = Unix.dup Unix.stdout in
  let result =
    Fun.protect
      ~finally:(fun () ->
        Unix.dup2 stdin Unix.stdin;
        Unix.dup2 stdout Unix.stdout;
        Unix.close stdin;
        Unix.close stdout)
      (fun () ->
        Unix.close Unix.stdin;
        Unix.close Unix.stdout;
        K7.capture ~show:""
          ~clock:(Eio_mock.Clock.Mono.make ())
          ~proc:(Eio.Stdenv.process_mgr env)
          ~cwd
          [ "/bin/sh"; "-c"; "printf STDOUT_WORKS" ])
  in
  match result with
  | Error e -> Alcotest.failf "%a" K7.pp_error e
  | Ok (cast, status) ->
      exited "stdout is usable after exec" 0 status;
      Alcotest.(check string)
        "the child's standard output is recorded" "STDOUT_WORKS\n"
        (Result.get_ok (K7.text_of_casts [ cast ]))

(* VHS Sleep accepts fractional milliseconds. Their sum must survive even
   when each individual pause is shorter than one SVG frame. *)
let test_fractional_sleeps () =
  with_dir @@ fun env cwd ->
  let t =
    Result.get_ok
      (K7.tape ~outputs:[ "out.svg" ]
         (List.init 100 (fun _ -> K7.raw "Sleep 1.5ms")))
  in
  let result, sleeps =
    with_sleep_log @@ fun clock ->
    K7.record ~engine:Native ~env:no_job_control ~clock
      ~proc:(Eio.Stdenv.process_mgr env)
      ~cwd t
  in
  (match result with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%a" K7.pp_error e);
  (* Startup's timeout and cleanup's polling intervals exceed 2ms. *)
  let total =
    List.filter (fun ns -> ns >= 0L && ns < 2_000_000L) sleeps
    |> List.fold_left Int64.add 0L
  in
  Alcotest.(check int64) "100 pauses of 1.5ms last 150ms" 150_000_000L total

(* Observe the requested span, without waiting centuries on a real clock. *)
let test_long_sleep () =
  with_dir @@ fun env cwd ->
  let milliseconds = 9_223_372_036_854 in
  let t =
    Result.get_ok
      (K7.tape ~outputs:[ "long.svg" ] [ K7.sleep (Ms milliseconds) ])
  in
  let result, sleeps =
    with_sleep_log ~release:(fun ns -> ns > 1_000_000_000_000L) @@ fun clock ->
    K7.record ~engine:Native ~env:no_job_control ~clock
      ~proc:(Eio.Stdenv.process_mgr env)
      ~cwd t
  in
  (match result with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%a" K7.pp_error e);
  Alcotest.(check int64)
    "the documented maximum duration reaches the clock"
    (Int64.mul (Int64.of_int milliseconds) 1_000_000L)
    (List.fold_left Int64.max 0L sleeps)

let test_replace_fractional_typing_speed () =
  with_dir @@ fun env cwd ->
  let source = "Output typing.svg\nSet TypingSpeed 0.5ms\nType abc\n" in
  let t =
    Result.get_ok
      (Result.bind (K7.of_string source)
         (K7.with_settings K7.[ Typing_speed (Ms 7) ]))
  in
  let result, sleeps =
    with_sleep_log @@ fun clock ->
    K7.record ~engine:Native ~env:no_job_control ~clock
      ~proc:(Eio.Stdenv.process_mgr env)
      ~cwd t
  in
  (match result with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%a" K7.pp_error e);
  Alcotest.(check (list int64))
    "with_settings replaces the parsed speed for each character"
    [ 7_000_000L; 7_000_000L; 7_000_000L ]
    (List.filter (fun ns -> ns >= 0L && ns < 10_000_000L) sleeps)

let suite =
  ( "native",
    List.map
      (fun (name, fn) -> Alcotest.test_case name `Quick fn)
      ([
         ( "capture with closed standard descriptors",
           test_capture_closed_standard_fds );
         ("large key repeat counts validate", test_large_key_repeat);
         ("upper-half key repeat count", test_upper_half_key_repeat);
         ("unparsed key repeat counts", test_unparsed_key_repeat);
         ("Require refuses non-executable files", test_require_non_executable);
         ("fractional sleeps retain their duration", test_fractional_sleeps);
         ("long sleeps retain their duration", test_long_sleep);
         ( "replace a fractional typing speed",
           test_replace_fractional_typing_speed );
         ("record", test_record);
         ("stuck callback receives at most 64 KiB", test_stuck_output_bound);
         ("zero-speed prompt wait", test_zero_speed_prompt);
         ("screenshot over the recording", test_screenshot_clash);
         ("timeout", test_timeout);
         ("shell exit", test_shell_exit);
         ("timeout precedence", test_timeout_precedence);
         ("playback speed", test_playback_speed);
         ("first prompt", test_first_prompt);
         ("no job control", test_no_job_control);
         ("hidden", test_hidden);
         ("hidden end", test_hidden_end);
         ("synchronized output", test_synchronized);
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
         ("sourced waits name their own line", test_source_wait_line);
         ("output paths", test_output_paths);
         ("render outputs", test_render_outputs);
         ("keys", test_keys);
         ( "prompt after a no-echo screen wait",
           test_prompt_after_noecho_screen_wait );
         ("Shift ignores unmapped Unicode keys", test_shift_unmapped_unicode);
         ("modifiers type the VHS keymap", test_modified_keymap);
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
         ("run searches empty PATH components", test_run_empty_path_component);
         ("run restores SIGPIPE", test_run_sigpipe);
         ("record restores SIGPIPE", test_record_sigpipe);
         ("run reports exec failures as I/O errors", test_run_exec_errors);
         ( "run does not inherit recorder file descriptors",
           test_run_inherited_fd );
         ("run errors", test_run_errors);
         ("run a killed program", test_run_signal);
         ("run shows the command line", test_run_command_line);
         ("run shows the command line alone first", test_run_delay);
         ("run reads a burst of 64 KiB", test_run_burst);
         ("run keeps the rows", test_run_rows);
         ("run shows another line", test_run_show);
         ("run cancelled", test_run_cancel);
         ("capture", test_capture);
         ("capture after an OCaml domain", test_capture_after_domain);
         ("capture keeps UTF-8 whole", test_capture_utf_8);
         ( "capture round-trips incomplete UTF-8",
           test_capture_incomplete_utf_8_round_trip );
         ("capture keeps a theme's cursor colours", test_capture_theme_cursor);
         ("cram", test_cram);
         ("cram refuses NUL in argv", test_cram_nul_argument);
         ( "cram quotes a reserved executable name",
           test_cram_reserved_executable );
         ( "cram quotes an assignment executable name",
           test_cram_assignment_executable );
         ("cram records an executable named dash", test_cram_option_executable);
         ( "shown commands preserve Unicode format arguments",
           test_run_command_format_argument );
         ( "cram quotes control characters for any sh",
           test_cram_control_argument );
         ("cram stops the command's process group", test_cram_stop);
         ("cram kills what ignores SIGTERM", test_cram_kill);
         ("cram cancelled stops the process group", test_cram_cancel);
         ("cram ends the command's background jobs", test_cram_background);
         ("cram output stays data", test_cram_output_is_data);
         ( "run cancellation reaps resistant descendants",
           test_run_cancel_descendant );
         ("record refuses Env names", test_record_env_names);
         ("record environment", test_record_env);
         ("record respects TERM override", test_record_term_override);
         ("check", test_check);
         ("output directory preflight", test_output_directory_preflight);
         ("screenshot real path", test_screenshot_real_path);
         ( "command line quotes invalid UTF-8",
           test_run_command_line_invalid_utf_8 );
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
