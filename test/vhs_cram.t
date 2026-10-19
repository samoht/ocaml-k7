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
  vhs-cram: "echo \001" holds a control character
  [123]

So is a test without commands, and an output path VHS cannot read.

  $ printf 'Only prose.\n' > empty.t
  $ vhs-cram --output demo.gif empty.t
  vhs-cram: empty.t: no command
  [123]

  $ vhs-cram --output demo demo.t
  vhs-cram: Vhs.output: "demo" has no extension and does not end with a slash
  [123]

A failing vhs is reported.

  $ vhs-cram --vhs false --output demo.gif demo.t
  vhs-cram: false exited with code 1
  [123]
