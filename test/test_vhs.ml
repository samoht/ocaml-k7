let expect name expected got = Alcotest.(check string) name expected got
let str = Fmt.str "%a" Vhs.pp

let rejects name item =
  match item () with
  | _ -> Alcotest.failf "%s: accepted" name
  | exception Invalid_argument _ -> ()

let test_basic_tape () =
  let open Vhs in
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
    (str tape)

let test_escaping () =
  let open Vhs in
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
    (str tape)

let test_type_control () =
  let open Vhs in
  expect "newline is Enter"
    "Type 'echo \"a\"'\nEnter\nType \"Output evil.gif\"\n"
    (str (tape [ type' "echo \"a\"\nOutput evil.gif" ]));
  expect "tab is Tab" "Type \"a\"\nTab\nType \"b\"\nEnter\n"
    (str (tape [ type' "a\tb\n" ]));
  expect "empty" "Type \"\"\n" (str (tape [ type' "" ]));
  rejects "control byte" (fun () -> type' "bad\x01byte")

let test_quoted_control () =
  let open Vhs in
  rejects "output" (fun () -> output "a.gif\nOutput evil.gif");
  rejects "env value" (fun () -> env "A" "x\ry");
  rejects "theme" (fun () -> set (Theme "a\x00"))

(* A docker format string forces double quotes and VHS types it verbatim, so
   the [\t] must reach the shell as one backslash. *)
let test_backslash_verbatim () =
  let open Vhs in
  let tape =
    tape [ type' {|docker ps --format 'table {{.Names}}\t{{.Image}}'|} ]
  in
  expect "backslash"
    "Type \"docker ps --format 'table {{.Names}}\\t{{.Image}}'\"\n" (str tape)

let test_both_quotes () =
  let open Vhs in
  expect "backticks" "Type `echo \"it's\"`\n"
    (str (tape [ type' {|echo "it's"|} ]))

let test_three_quotes () =
  let open Vhs in
  expect "split type" "Type `a\"b'c`\nType \"`d\"\n"
    (str (tape [ type' {|a"b'c`d|} ]));
  rejects "output" (fun () -> output {|a"b'c`d|})

let test_unquoted () =
  let open Vhs in
  let evil = "x\nOutput evil.gif" in
  rejects "comment" (fun () -> comment evil);
  rejects "require" (fun () -> require evil);
  rejects "env key" (fun () -> env evil "v");
  rejects "custom name" (fun () -> set (Custom ("A B", "1")));
  rejects "custom value" (fun () -> set (Custom ("A", "1\r")));
  rejects "ctrl digit" (fun () -> key (Ctrl '1'));
  rejects "alt backslash" (fun () -> key (Alt '\\'));
  rejects "raw" (fun () -> raw evil);
  expect "raw line kept" "Set Margin 20\n"
    (str (tape [ set (Custom ("Margin", "20")) ]))

let test_durations () =
  let open Vhs in
  expect "decimal" "Sleep 0.00001s\nSleep 2s\nSleep 0s\nSleep 25ms\n"
    (str
       (tape [ sleep (S 1e-5); sleep (S 2.); sleep (S (-0.)); sleep (Ms 25) ]));
  rejects "negative" (fun () -> sleep (S (-1.)));
  rejects "nan" (fun () -> sleep (S Float.nan));
  rejects "infinity" (fun () -> set (Typing_speed (S Float.infinity)));
  rejects "negative ms" (fun () -> sleep (Ms (-1)))

let test_vhs_rules () =
  let open Vhs in
  rejects "negative width" (fun () -> set (Width (-5)));
  rejects "output without extension" (fun () -> output "demo");
  expect "output dir" "Output \"frames/\"\n" (str (tape [ output "frames/" ]));
  rejects "dotted dir" (fun () -> output "a.b/c");
  rejects "screenshot gif" (fun () -> screenshot "x.gif");
  rejects "env name" (fun () -> env "A=B" "x");
  rejects "env digit" (fun () -> env "1A" "x");
  rejects "trailing backslash" (fun () -> wait (Line {|a\|}));
  expect "require quoted" "Require \"dune\"\n" (str (tape [ require "dune" ]))

let test_wait_slash () =
  let open Vhs in
  expect "slash" "Wait+Screen /a\\x2fb\\x2fc\\\\\\x2f/\n"
    (str (tape [ wait (Screen {|a/b\/c\\/|}) ]));
  rejects "line break" (fun () -> wait (Line "a\nOutput evil.gif"))

let test_keys_and_waits () =
  let open Vhs in
  let tape =
    tape
      [
        key (Ctrl 'c');
        key (Ctrl '\\');
        key (Alt '.');
        key Page_up;
        wait (Screen "ready");
        wait Prompt;
        wait ~timeout:(Ms 500) Prompt;
        wait ~timeout:(S 2.) (Line "ok");
        screenshot "frame.png";
        source "common.tape";
      ]
  in
  expect "keys and waits"
    "Ctrl+c\n\
     Ctrl+\\\n\
     Alt+.\n\
     PageUp\n\
     Wait+Screen /ready/\n\
     Wait\n\
     Wait@500ms\n\
     Wait+Line@2s /ok/\n\
     Screenshot \"frame.png\"\n\
     Source \"common.tape\"\n"
    (str tape)

let suite =
  ( "vhs",
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
      Alcotest.test_case "vhs rules" `Quick test_vhs_rules;
      Alcotest.test_case "wait slash" `Quick test_wait_slash;
      Alcotest.test_case "keys and waits" `Quick test_keys_and_waits;
    ] )

let cram src =
  match Vhs.Cram.parse src with Ok t -> t | Error msg -> Alcotest.fail msg

let test_minimal () =
  let src = "  $ ./main.exe\n  +hello, world\n" in
  let tape = Vhs.Cram.to_tape ~output:"demo.gif" (cram src) in
  expect "minimal" {|Output "demo.gif"
Type "./main.exe"
Enter
Wait
Sleep 3s
|}
    (str tape)

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
      ~setup_scripts:[ "setup.sh" ] ~output:"demo.gif" (cram src)
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
Wait
Sleep 1s
Type "echo done"
Enter
Wait
Sleep 3s
|}
    (str tape)

let test_setup_quoted () =
  let tape =
    Vhs.Cram.to_tape ~setup_scripts:[ "my setup.sh" ] ~output:"demo.gif"
      (cram "")
  in
  expect "quoted"
    "Output \"demo.gif\"\n\
     Hide\n\
     Type \"source 'my setup.sh'\"\n\
     Enter\n\
     Type \"clear\"\n\
     Enter\n\
     Show\n"
    (str tape)

let test_parse () =
  let src =
    "prose is ignored\n\n\
    \  $ echo hi\n\
    \  hi\n\
    \  $ printf a\\\n\
    \  > b\n\
    \  ab\n\
    \  [1]\n"
  in
  expect "cram" "  $ echo hi\n  hi\n  $ printf a\\\n  > b\n  ab\n  [1]\n"
    (Fmt.str "%a" Vhs.Cram.pp (cram src))

let test_parse_control () =
  match Vhs.Cram.parse "  $ echo \x01\n" with
  | Ok _ -> Alcotest.fail "parsed an untypeable command"
  | Error _ -> ()

let test_crlf () =
  let steps = cram "  $ echo hi\r\n  hi\r\n" in
  let tape = Vhs.Cram.to_tape ~output:"demo.gif" steps in
  expect "crlf" "Output \"demo.gif\"\nType \"echo hi\"\nEnter\nWait\nSleep 3s\n"
    (str tape)

let cram_suite =
  ( "cram",
    [
      Alcotest.test_case "minimal" `Quick test_minimal;
      Alcotest.test_case "full" `Quick test_full;
      Alcotest.test_case "setup quoted" `Quick test_setup_quoted;
      Alcotest.test_case "parse" `Quick test_parse;
      Alcotest.test_case "parse control" `Quick test_parse_control;
      Alcotest.test_case "crlf" `Quick test_crlf;
    ] )

let render ?vhs tape =
  Eio_main.run @@ fun env ->
  Vhs.render ~proc:(Eio.Stdenv.process_mgr env) ?vhs tape

let test_render_stdin () =
  match render ~vhs:"sh" (Vhs.tape [ Vhs.raw "exit 3" ]) with
  | Error (Failed { vhs = "sh"; status = `Exited 3 }) -> ()
  | Ok () -> Alcotest.fail "sh did not read the tape"
  | Error e -> Alcotest.failf "unexpected %a" Vhs.pp_error e

let test_render_not_found () =
  match render ~vhs:"vhs-no-such-executable" (Vhs.tape []) with
  | Error (Executable_not_found _) -> ()
  | Ok () -> Alcotest.fail "ran a missing executable"
  | Error e -> Alcotest.failf "unexpected %a" Vhs.pp_error e

let run_suite =
  ( "run",
    [
      Alcotest.test_case "render stdin" `Quick test_render_stdin;
      Alcotest.test_case "render not found" `Quick test_render_not_found;
    ] )
