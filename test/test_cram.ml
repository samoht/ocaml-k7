let expect name expected got = Alcotest.(check string) name expected got

let test_minimal () =
  let src = "  $ ./main.exe\n  +hello, world\n" in
  let tape = Vhs.Cram.to_tape ~output:"demo.gif" (Vhs.Cram.parse src) in
  expect "minimal"
    {|Output "demo.gif"
Type "./main.exe"
Enter
Wait+Screen /\+hello, world/
Sleep 3s
|}
    (Vhs.to_string tape)

let test_full () =
  let src =
    "Boot the demo.\n\n\
    \  $ ./main.exe\n\
    \  +hello, world\n\
    \  $ echo done\n\
    \  done\n\
    \  [0]\n"
  in
  let tape =
    let open Vhs in
    Cram.to_tape
      ~settings:[ Shell "bash"; Width 900 ]
      ~setup_scripts:[ "setup.sh" ] ~output:"demo.gif" (Cram.parse src)
  in
  expect "full"
    {|Output "demo.gif"
Set Shell "bash"
Set Width 900
Hide
Type "source 'setup.sh'"
Enter
Type "clear"
Enter
Show
Type "./main.exe"
Enter
Wait+Screen /\+hello, world/
Sleep 1s
Type "echo done"
Enter
Wait+Screen /done/
Sleep 3s
|}
    (Vhs.to_string tape)

let test_slash () =
  let src = "  $ ls\n  a/b\n" in
  let tape = Vhs.Cram.to_tape ~output:"demo.gif" (Vhs.Cram.parse src) in
  expect "slash"
    "Output \"demo.gif\"\nType \"ls\"\nEnter\nWait+Screen /a\\x2fb/\nSleep 3s\n"
    (Vhs.to_string tape)

let test_setup_quoted () =
  let tape =
    Vhs.Cram.to_tape ~setup_scripts:[ "my setup.sh" ] ~output:"demo.gif" []
  in
  expect "quoted"
    "Output \"demo.gif\"\n\
     Hide\n\
     Type \"source 'my setup.sh'\"\n\
     Enter\n\
     Type \"clear\"\n\
     Enter\n\
     Show\n"
    (Vhs.to_string tape)

let test_parse () =
  let src = "prose is ignored\n\n  $ echo hi\n  hi\n  $ false\n  [1]\n" in
  let steps = Vhs.Cram.parse src in
  Alcotest.(check int) "step count" 2 (List.length steps);
  match steps with
  | [ a; b ] ->
      expect "cmd a" "echo hi" (String.concat "\n" a.Vhs.Cram.command);
      expect "out a" "hi" (String.concat "\n" a.Vhs.Cram.output);
      expect "cmd b" "false" (String.concat "\n" b.Vhs.Cram.command);
      Alcotest.(check (option int)) "exit b" (Some 1) b.Vhs.Cram.exit_code
  | _ -> Alcotest.fail "expected two steps"

let test_crlf () =
  let steps = Vhs.Cram.parse "  $ echo hi\r\n  hi\r\n" in
  let tape = Vhs.Cram.to_tape ~output:"demo.gif" steps in
  expect "crlf"
    "Output \"demo.gif\"\nType \"echo hi\"\nEnter\nWait+Screen /hi/\nSleep 3s\n"
    (Vhs.to_string tape)

let suite =
  ( "cram",
    [
      Alcotest.test_case "minimal" `Quick test_minimal;
      Alcotest.test_case "full" `Quick test_full;
      Alcotest.test_case "slash" `Quick test_slash;
      Alcotest.test_case "setup quoted" `Quick test_setup_quoted;
      Alcotest.test_case "parse" `Quick test_parse;
      Alcotest.test_case "crlf" `Quick test_crlf;
    ] )
