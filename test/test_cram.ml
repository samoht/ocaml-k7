let expect name expected got = Alcotest.(check string) name expected got

(* A single command and its expected output become keystrokes plus a wait on
   the last output line. No settings and no setup: the minimal translation. *)
let test_minimal () =
  let src = "  $ ./main.exe\n  +hello, world\n" in
  let tape = Vhs.Cram.to_tape ~output:"demo.gif" (Vhs.Cram.parse src) in
  expect "minimal"
    {|Output "demo.gif"
Type "./main.exe"
Enter
Wait+Screen /\+hello, world/
Sleep 1s
|}
    (Vhs.to_string tape)

(* Settings become Set lines; setup scripts are sourced inside a hidden block
   before the first command; prose, blank lines, and the [N] exit annotation
   are dropped; the wait targets the last non-empty output line. *)
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
Type "source setup.sh"
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
Sleep 1s
|}
    (Vhs.to_string tape)

(* parse keeps only the command blocks: their command line(s), the expected
   output with the two-space indent removed, and any exit annotation. *)
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

let suite =
  ( "cram",
    [
      Alcotest.test_case "minimal" `Quick test_minimal;
      Alcotest.test_case "full" `Quick test_full;
      Alcotest.test_case "parse" `Quick test_parse;
    ] )
