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
  | Ok _ ->
      Alcotest.(check (list string))
        "screens"
        [
          screen [ "> x=1"; ">" ];
          screen [ "> x=1"; "> sleep 1; echo hi" ];
          screen [ "> x=1"; "> sleep 1; echo hi"; "" ];
          screen [ "> x=1"; "> sleep 1; echo hi"; "hi"; ">" ];
        ]
        (text "demo.txt"));
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
  let t = tape Vhs.[ raw "Screenshot shot.png"; type' "x" ] in
  match record env cwd t with
  | Error (Unsupported why) ->
      check_contains "names the line" ~sub:"Screenshot" why
  | Ok _ -> Alcotest.fail "played a screenshot"
  | Error e -> Alcotest.failf "%a" Vhs.pp_error e

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

(* The native text output is VHS's, byte for byte, but for the screens VHS
   reads before it sizes the terminal, for a 60 x 12 terminal. *)
let test_oracle () =
  with_dir @@ fun env cwd ->
  let t =
    match
      Vhs.tape
        ~outputs:[ "oracle.txt"; "oracle.svg" ]
        Vhs.
          [
            raw "Set Columns 60";
            raw "Set Rows 12";
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
    with
    | Ok t -> t
    | Error e -> Alcotest.fail e
  in
  let clock = Eio.Stdenv.mono_clock env and proc = Eio.Stdenv.process_mgr env in
  (match Vhs.render ?vhs:oracle ~clock ~proc ~cwd t with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%a" Vhs.pp_error e);
  let vhs = Eio.Path.load Eio.Path.(cwd / "oracle.txt") in
  match record env cwd t with
  | Error e -> Alcotest.failf "%a" Vhs.pp_error e
  | Ok s -> (
      let native = Eio.Path.load Eio.Path.(cwd / "oracle.txt") in
      Alcotest.(check int) "screens" 10 (List.length (screens native));
      Alcotest.(check string) "text output" (after_settings t vhs) native;
      match Svg_check.frames ~cell_width ~cell_height ~rows:12 s with
      | Error e -> Alcotest.fail e
      | Ok frames ->
          Alcotest.(check string)
            "final screen"
            (String.concat "\n"
               (List.init 11 (fun i -> Int.to_string (i + 10)) @ [ ">" ]))
            (last frames))

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
         ("keys", test_keys);
         ("unsupported", test_unsupported);
       ]
      @ if Option.is_some oracle then [ ("vhs oracle", test_oracle) ] else [])
  )
