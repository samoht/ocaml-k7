Usage errors are reported by the command line parser, whose wording
varies across cmdliner versions.

  $ vhs-cram demo.t 2> /dev/null
  [124]

  $ vhs-cram --output demo.gif missing.t 2> /dev/null
  [124]

  $ printf '  $ echo hi\n  hi\n' > demo.t
  $ vhs-cram --setup missing.sh --output demo.gif demo.t 2> /dev/null
  [124]

The tape is written to the standard input of vhs, so cat shows it.

  $ vhs-cram --vhs cat --output demo.gif demo.t
  Output "demo.gif"
  Set Shell "bash"
  Type "echo hi"
  Enter
  Wait
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

A failing vhs is reported with its standard error.

  $ cat > fake-vhs <<'EOF'
  > #!/bin/sh
  > cat > /dev/null
  > echo 'Error: timeout waiting for "Line" to match >$' >&2
  > exit 1
  > EOF
  $ chmod +x fake-vhs
  $ vhs-cram --vhs ./fake-vhs --output demo.gif demo.t
  vhs-cram: ./fake-vhs exited with code 1: Error: timeout waiting for "Line" to match >$
  [123]

A setup script is sourced while the recording is hidden.

  $ printf ':\n' > setup.sh
  $ vhs-cram --vhs cat --setup setup.sh --output demo.gif demo.t
  Output "demo.gif"
  Set Shell "bash"
  Hide
  Type "source 'setup.sh'"
  Enter
  Type "clear"
  Enter
  Show
  Type "echo hi"
  Enter
  Wait
  Sleep 1000ms
