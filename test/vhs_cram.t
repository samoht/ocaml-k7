Usage errors are reported by the command line parser, whose wording
varies across cmdliner versions.

  $ vhs-cram demo.t 2> /dev/null
  [124]

  $ vhs-cram --output demo.gif missing.t 2> /dev/null
  [124]

  $ printf '  $ echo hi\n  hi\n' > demo.t
  $ vhs-cram --setup missing.sh --output demo.gif demo.t 2> /dev/null
  [124]

--print shows the tape instead of running vhs.

  $ vhs-cram --print --output demo.gif demo.t
  Output "demo.gif"
  Set Shell "bash"
  Hide
  Type "PS1='\[\e[38;2;90;86;224m\]>\[\342\200\213\] \[\e[0m\]'; clear"
  Enter
  Wait+Line /\x{200B}$/
  Show
  Type "echo hi"
  Enter
  Wait+Line /\x{200B}$/
  Sleep 1000ms

A command the tape cannot type is an error, not an exception.

  $ printf '  $ echo \001\n' > bad.t
  $ vhs-cram --output demo.gif bad.t
  vhs-cram: bad.t: line 1: "echo \001" holds a control character
  [123]

So is a test without commands, and an output path VHS cannot read.

  $ printf 'Only prose.\n' > empty.t
  $ vhs-cram --output demo.gif empty.t
  vhs-cram: empty.t: no command
  [123]

  $ vhs-cram --output demo demo.t
  vhs-cram: Output "demo": neither an extension nor a trailing slash
  [123]

A failing vhs is reported with what it printed.

  $ cat > fake-vhs <<'EOF'
  > #!/bin/sh
  > cat > /dev/null
  > echo 'Error: timeout waiting for "Line" to match >$'
  > exit 1
  > EOF
  $ chmod +x fake-vhs
  $ vhs-cram --vhs ./fake-vhs --output demo.gif demo.t
  vhs-cram: ./fake-vhs exited with code 1
    Error: timeout waiting for "Line" to match >$
  [123]

Setup and clear finish while the recording is still hidden, after
resetting the prompt that the waits rely on.

  $ printf 'PS1=x\n' > setup.sh
  $ vhs-cram --print --setup setup.sh --output demo.gif demo.t
  Output "demo.gif"
  Set Shell "bash"
  Hide
  Type "source 'setup.sh'; PS1='\[\e[38;2;90;86;224m\]>\[\342\200\213\] \[\e[0m\]'; clear"
  Enter
  Wait+Line /\x{200B}$/
  Show
  Type "echo hi"
  Enter
  Wait+Line /\x{200B}$/
  Sleep 1000ms
