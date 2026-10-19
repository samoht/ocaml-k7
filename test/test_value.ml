let expect name expected got = Alcotest.(check string) name expected got

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
        type' "bad\x01byte";
      ]
  in
  expect "escaping"
    "Type 'printf \"hello\\n\" && echo \\done'\n\
     Env PROMPT 'a \"quoted\" value'\n\
     Type \"bad\\x01byte\"\n"
    (to_string tape)

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
  match to_string (tape [ output {|a"b'c`d|} ]) with
  | s -> Alcotest.failf "printed %S" s
  | exception Invalid_argument _ -> ()

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
      Alcotest.test_case "keys and waits" `Quick test_keys_and_waits;
    ] )
