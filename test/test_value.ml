let expect name expected got = Alcotest.(check string) name expected got

let rejects name item =
  match Vhs.Value.(to_string (tape [ item ])) with
  | s -> Alcotest.failf "%s: printed %S" name s
  | exception Invalid_argument _ -> ()

let test_basic_tape () =
  let open Vhs.Value in
  let tape =
    tape
      [
        comment "render from repo root";
        output "demo.gif";
        blank ();
        set (Shell "bash");
        set (Font_size 18);
        set (Width 1000);
        set (Height 560);
        set (Theme "Builtin Dark");
        type' "dune test";
        enter ();
        sleep (S 1.5);
      ]
  in
  expect "tape"
    {|# render from repo root
Output "demo.gif"

Set Shell "bash"
Set FontSize 18
Set Width 1000
Set Height 560
Set Theme "Builtin Dark"
Type "dune test"
Enter
Sleep 1.5s
|}
    (to_string tape)

let test_escaping () =
  let open Vhs.Value in
  let tape =
    tape
      [
        type' {|printf "hello\n" && echo \done|};
        env "PROMPT" {|a "quoted" value|};
      ]
  in
  expect "escaping"
    "Type 'printf \"hello\\n\" && echo \\done'\n\
     Env PROMPT 'a \"quoted\" value'\n"
    (to_string tape)

let test_type_control () =
  let open Vhs.Value in
  expect "newline is Enter"
    "Type 'echo \"a\"'\nEnter\nType \"Output evil.gif\"\n"
    (to_string (tape [ type' "echo \"a\"\nOutput evil.gif" ]));
  expect "tab is Tab" "Type \"a\"\nTab\nType \"b\"\nEnter\n"
    (to_string (tape [ type' "a\tb\n" ]));
  expect "empty" "Type \"\"\n" (to_string (tape [ type' "" ]));
  rejects "control byte" (type' "bad\x01byte")

let test_quoted_control () =
  let open Vhs.Value in
  rejects "output" (output "a.gif\nOutput evil.gif");
  rejects "env value" (env "A" "x\ry");
  rejects "theme" (set (Theme "a\x00"))

(* A docker format string forces double quotes and VHS types it verbatim, so
   the [\t] must reach the shell as one backslash. *)
let test_backslash_verbatim () =
  let open Vhs.Value in
  let tape =
    tape [ type' {|docker ps --format 'table {{.Names}}\t{{.Image}}'|} ]
  in
  expect "backslash"
    "Type \"docker ps --format 'table {{.Names}}\\t{{.Image}}'\"\n"
    (to_string tape)

let test_both_quotes () =
  let open Vhs.Value in
  expect "backticks" "Type `echo \"it's\"`\n"
    (to_string (tape [ type' {|echo "it's"|} ]))

let test_three_quotes () =
  let open Vhs.Value in
  expect "split type" "Type `a\"b'c`\nType \"`d\"\n"
    (to_string (tape [ type' {|a"b'c`d|} ]));
  rejects "output" (output {|a"b'c`d|})

let test_unquoted () =
  let open Vhs.Value in
  let evil = "x\nOutput evil.gif" in
  rejects "comment" (comment evil);
  rejects "require" (require evil);
  rejects "require word" (require "git Output e.gif");
  rejects "env key" (env evil "v");
  rejects "custom name" (set (Custom ("A B", "1")));
  rejects "custom value" (set (Custom ("A", "1\r")));
  rejects "ctrl" (key (Ctrl "c Enter"));
  rejects "raw key" (key (Raw_key evil));
  rejects "raw command" (raw_command evil);
  rejects "raw line" (raw_line evil);
  expect "raw line kept" "Set Margin 20\n"
    (to_string (tape [ set (Custom ("Margin", "20")) ]))

let test_durations () =
  let open Vhs.Value in
  expect "decimal" "Sleep 0.00001s\nSleep 2s\nSleep 0s\nSleep 25ms\n"
    (to_string
       (tape [ sleep (S 1e-5); sleep (S 2.); sleep (S (-0.)); sleep (Ms 25) ]));
  rejects "negative" (sleep (S (-1.)));
  rejects "nan" (sleep (S Float.nan));
  rejects "infinity" (set (Typing_speed (S Float.infinity)));
  rejects "negative ms" (sleep (Ms (-1)))

let test_keys_and_waits () =
  let open Vhs.Value in
  let tape =
    tape
      [
        key (Ctrl "c");
        key (Ctrl_alt "Delete");
        wait (Wait_screen "ready");
        screenshot ~path:"frame.png" ();
        source "common.tape";
      ]
  in
  expect "keys and waits"
    "Ctrl+c\n\
     Ctrl+Alt+Delete\n\
     Wait+Screen /ready/\n\
     Screenshot \"frame.png\"\n\
     Source \"common.tape\"\n"
    (to_string tape)

let suite =
  ( "value",
    [
      Alcotest.test_case "basic tape" `Quick test_basic_tape;
      Alcotest.test_case "escaping" `Quick test_escaping;
      Alcotest.test_case "backslash verbatim" `Quick test_backslash_verbatim;
      Alcotest.test_case "both quotes" `Quick test_both_quotes;
      Alcotest.test_case "three quotes" `Quick test_three_quotes;
      Alcotest.test_case "type control" `Quick test_type_control;
      Alcotest.test_case "quoted control" `Quick test_quoted_control;
      Alcotest.test_case "unquoted payloads" `Quick test_unquoted;
      Alcotest.test_case "durations" `Quick test_durations;
      Alcotest.test_case "keys and waits" `Quick test_keys_and_waits;
    ] )
