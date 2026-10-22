Usage errors are reported by the command line parser, whose wording
varies across cmdliner versions.

  $ k7 cram demo.t 2> /dev/null
  [124]

  $ k7 cram --output demo.gif missing.t 2> /dev/null
  [124]

  $ printf '  $ echo hi\n  hi\n' > demo.t
  $ k7 cram --setup missing.sh --output demo.gif demo.t 2> /dev/null
  [124]

k7 reports its version, which a release substitutes.

  $ k7 --version | wc -l | tr -d ' '
  1

Numbers are plain decimal digits, as the messages say.

  $ k7 cram --print --cols 0x50 --output demo.gif demo.t 2> /dev/null
  [124]
  $ k7 cram --print --wait-timeout 1_5 --output demo.gif demo.t 2> /dev/null
  [124]
  $ k7 cram --print --rows 0b1 --output demo.gif demo.t 2> /dev/null
  [124]
  $ k7 cram --print --font-size +16 --output demo.gif demo.t 2> /dev/null
  [124]

--print shows the tape instead of running vhs.

  $ k7 cram --print --output demo.gif demo.t
  Output "demo.gif"
  Set Shell "bash"
  Hide
  Env K7_PROMPT `bind 'set input-meta on'; bind 'set output-meta on'; bind 'set convert-meta off'; __k7_n=; __k7_prompt() { PS1='\[\e[38;2;90;86;224m\]> \[\342\201\240\342\201\240\342\200\213\342\201\240\342\200\213\342\200\213\342\200\213\342\201\240\342\200\213\342\201\240\342\200\213\342\200\213\342\201\240\342\200\213\342\201\240\342\201\240\e[0m\]'; __k7_n=$__k7_n'\342\200\213'; PS2='> \[\342\201\240\342\201\240\342\200\213\342\201\240\342\200\213\342\200\213\342\200\213\342\201\240\342\200\213\342\201\240\342\200\213\342\200\213\342\201\240\342\200\213\342\201\240\342\201\240'$__k7_n'\]'; }; __k7=$(printf '%s\n' ${PROMPT_COMMAND+"${PROMPT_COMMAND[@]}"} __k7_prompt); unset PROMPT_COMMAND; PROMPT_COMMAND=$__k7; unset __k7 K7_PROMPT`
  Type 'eval "$K7_PROMPT"; clear'
  Enter
  Wait+Line /\x{2060}\x{2060}\x{200B}\x{2060}\x{200B}\x{200B}\x{200B}\x{2060}\x{200B}\x{2060}\x{200B}\x{200B}\x{2060}\x{200B}\x{2060}\x{2060}$/
  Show
  Type "echo hi"
  Enter
  Wait+Line /\x{2060}\x{2060}\x{200B}\x{2060}\x{200B}\x{200B}\x{200B}\x{2060}\x{200B}\x{2060}\x{200B}\x{200B}\x{2060}\x{200B}\x{2060}\x{2060}$/
  Sleep 1000ms

A reader that stops before the end of a tape larger than a pipe buffer
ends --print quietly, as SIGPIPE would; a standard output that cannot be
written is an error.

  $ i=0; while [ $i -lt 4000 ]; do echo "  \$ echo line $i"; i=$((i + 1)); done > big.t
  $ { k7 cram --print --output demo.gif big.t; echo $? > status; } | head -n 1
  Output "demo.gif"
  $ cat status
  0
  $ k7 cram --print --output demo.gif big.t >&- 2> err
  [123]

The reason is the system's, such as "Bad file descriptor" on macOS and
"Invalid argument" on Linux.

  $ cut -d: -f1,2 err
  k7: standard output

A command the tape cannot type is an error, not an exception.

  $ printf '  $ echo \001\n' > bad.t
  $ k7 cram --output demo.gif bad.t
  k7: bad.t: line 1: "echo \x01" holds a control character
  [123]

So is a test without commands, and an output path VHS cannot read.

  $ printf 'Only prose.\n' > empty.t
  $ k7 cram --output demo.gif empty.t
  k7: empty.t: no command: no line starts with "  $ "
  [123]

  $ k7 cram --output demo demo.t
  k7: Output "demo": neither an extension nor a trailing slash
  [123]

A failing vhs is reported with what it printed.

  $ cat > fake-vhs <<'EOF'
  > #!/bin/sh
  > cat > /dev/null
  > echo 'Error: no browser'
  > exit 1
  > EOF
  $ chmod +x fake-vhs
  $ k7 cram --vhs ./fake-vhs --output demo.gif demo.t
  k7: demo.t: ./fake-vhs exited with code 1
    Error: no browser
  [123]

A wait of vhs that times out is said in words, without the regular
expression of the prompt mark, with the option that waits longer.

  $ cat > slow-vhs <<'EOF'
  > #!/bin/sh
  > cat > /dev/null
  > printf 'Error: timeout waiting for "Line /\\x{200B}$/" to match /\\x{200B}$/; last value was: > sleep 99\n'
  > exit 1
  > EOF
  $ chmod +x slow-vhs
  $ k7 cram --vhs ./slow-vhs --wait-timeout 2 --output demo.gif demo.t
  k7: demo.t: vhs timed out after 2 seconds waiting for the prompt after a command; last value was: > sleep 99; --wait-timeout gives it longer
  [123]

Setup and clear finish while the recording is still hidden, after
resetting the prompt that the waits rely on.

  $ printf 'PS1=x\n' > setup.sh
  $ k7 cram --print --setup setup.sh --output demo.gif demo.t
  Output "demo.gif"
  Set Shell "bash"
  Hide
  Env K7_PROMPT `bind 'set input-meta on'; bind 'set output-meta on'; bind 'set convert-meta off'; __k7_n=; __k7_prompt() { PS1='\[\e[38;2;90;86;224m\]> \[\342\201\240\342\201\240\342\201\240\342\200\213\342\201\240\342\200\213\342\201\240\342\201\240\342\201\240\342\201\240\342\200\213\342\201\240\342\200\213\342\201\240\342\200\213\342\200\213\e[0m\]'; __k7_n=$__k7_n'\342\201\240'; PS2='> \[\342\201\240\342\201\240\342\201\240\342\200\213\342\201\240\342\200\213\342\201\240\342\201\240\342\201\240\342\201\240\342\200\213\342\201\240\342\200\213\342\201\240\342\200\213\342\200\213'$__k7_n'\]'; }; __k7=$(printf '%s\n' ${PROMPT_COMMAND+"${PROMPT_COMMAND[@]}"} __k7_prompt); unset PROMPT_COMMAND; PROMPT_COMMAND=$__k7; unset __k7 K7_PROMPT`
  Type `source './setup.sh'; eval "$K7_PROMPT"; clear`
  Enter
  Wait+Line /\x{2060}\x{2060}\x{2060}\x{200B}\x{2060}\x{200B}\x{2060}\x{2060}\x{2060}\x{2060}\x{200B}\x{2060}\x{200B}\x{2060}\x{200B}\x{200B}$/
  Show
  Type "echo hi"
  Enter
  Wait+Line /\x{2060}\x{2060}\x{2060}\x{200B}\x{2060}\x{200B}\x{2060}\x{2060}\x{2060}\x{2060}\x{200B}\x{2060}\x{200B}\x{2060}\x{200B}\x{200B}$/
  Sleep 1000ms

An .svg output is recorded without vhs, with the same prompt and mark.

  $ k7 cram --print --output demo.svg demo.t | grep -v '^Env'
  Set Shell "bash"
  Hide
  Type 'eval "$K7_PROMPT"; clear'
  Enter
  Wait+Line /\x{2060}\x{2060}\x{200B}\x{2060}\x{200B}\x{200B}\x{200B}\x{2060}\x{200B}\x{2060}\x{200B}\x{200B}\x{2060}\x{200B}\x{2060}\x{2060}$/
  Show
  Type "echo hi"
  Enter
  Wait+Line /\x{2060}\x{2060}\x{200B}\x{2060}\x{200B}\x{200B}\x{200B}\x{2060}\x{200B}\x{2060}\x{200B}\x{200B}\x{2060}\x{200B}\x{2060}\x{2060}$/
  Sleep 1000ms
  $ k7 cram --vhs ./missing-vhs --output demo.svg demo.t
  $ grep -c '<svg xmlns' demo.svg
  1

A GIF needs vhs, and k7 says where to get it.

  $ k7 cram --vhs ./missing-vhs --output demo.gif demo.t
  k7: demo.t: executable not found: ./missing-vhs; a GIF or a video needs Charm's VHS, see https://github.com/charmbracelet/vhs#installation, and an .svg output does not
  [123]

The terminal's size, font and theme are settings of the tape, and each
--env variable an Env line of its hidden setup.

  $ k7 cram --print --cols 100 --rows 20 --font-size 16 --theme Dracula --env 'GREETING=bonjour à tous' --output demo.gif demo.t | head -n 8
  Output "demo.gif"
  Set Shell "bash"
  Set Columns 100
  Set Rows 20
  Set FontSize 16
  Set Theme "Dracula"
  Hide
  Env "GREETING" "bonjour à tous"
  $ k7 cram --print --env 'A=B"C'"'"'D`' --output demo.gif demo.t
  k7: --env "A=B\"C'D`": holds all three quote characters
  [123]
  $ k7 cram --print --env NOVALUE --output demo.gif demo.t 2> /dev/null
  [124]

A native recording has the columns asked for and the variables set.

  $ printf '  $ echo "$GREETING"; printf "%%099d|\\n" 0\n' > env.t
  $ k7 cram --cols 100 --env GREETING=bonjour --output env.txt env.t
  $ grep -c '^bonjour$' env.txt
  3
  $ grep -c '^0*|$' env.txt
  3

k7 run records a program, with no shell and nothing typed, and exits
with the program's exit code once the image is written.

  $ k7 run -o run.svg --cols 20 --rows 3 -- sh -c 'printf "hello\n"; exit 3'
  [3]
  $ grep -c '>hello<' run.svg
  1
  $ grep -q '&gt; ' run.svg || echo no prompt
  no prompt
  $ k7 run -o run.gif -- true
  k7: "run.gif": not .svg, the animated SVG image k7 run writes
  [123]
  $ k7 run -o run.svg -- k7-no-such-program
  k7: executable not found: k7-no-such-program
  [123]
  $ k7 run -o missing/run.svg -- true
  k7: "missing/run.svg": no directory "missing"
  [123]
  $ k7 run -o run.svg 2> /dev/null
  [124]

The image shows the command line first, shell-quoted where needed, then
the program's output. --show replaces the line and --show "" hides it.

  $ k7 run -o cmd.svg --cols 40 --rows 3 -- printf '%s\n' 'hello world'
  $ grep -o '<desc id="k[0-9a-f]*d">.*' cmd.svg | sed 's/id="[^"]*"//'
  <desc >$ printf '%s\n' 'hello world'
  $ grep -o '^[^<]*</desc>' cmd.svg
  hello world</desc>
  $ k7 run -o show.svg --cols 40 --rows 3 --show 'dune exec -- ./main.exe' -- printf 'hi\n'
  $ grep -o '<desc id="k[0-9a-f]*d">.*' show.svg | sed 's/id="[^"]*"//'
  <desc >$ dune exec -- ./main.exe
  $ k7 run -o hide.svg --cols 40 --rows 3 --show '' -- printf 'hi\n'
  $ grep -o '<desc id="k[0-9a-f]*d">[^<]*</desc>' hide.svg | sed 's/id="[^"]*"//'
  <desc >hi</desc>

k7 tape reads a tape in VHS's syntax; the command line replaces its
settings and outputs.

  $ cat > demo.tape <<'EOF'
  > Output demo.gif
  > Set FontSize 18
  > Set Width 900
  > Type "echo $GREETING"
  > Enter
  > Sleep 500ms
  > EOF
  $ k7 tape --print --cols 90 -o demo.svg demo.tape
  Set FontSize 18
  Set Columns 90
  Type "echo $GREETING"
  Enter
  Sleep 500ms
  $ k7 tape --cols 40 --env GREETING=salut -o tape.txt demo.tape
  $ grep -c '^salut$' tape.txt
  2
  $ printf 'Output a.gif\nType "x\n' > bad.tape
  $ k7 tape bad.tape
  k7: bad.tape: line 2: "Type \"x": unterminated string
  [123]
