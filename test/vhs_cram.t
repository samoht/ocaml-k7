Usage errors are reported by the command line parser, whose wording
varies across cmdliner versions.

  $ vhs-cram demo.t 2> /dev/null
  [124]

  $ vhs-cram --output demo.gif missing.t 2> /dev/null
  [124]

A command the tape cannot type is an error, not an exception.

  $ printf '  $ echo \001\n' > bad.t
  $ vhs-cram --output demo.gif bad.t
  vhs-cram: "echo \001" holds a control character
  [1]

So is an output path VHS cannot read.

  $ printf '  $ echo hi\n' > demo.t
  $ vhs-cram --output demo demo.t
  vhs-cram: Vhs.output: "demo" has no extension and does not end with a slash
  [1]

A failing vhs is reported.

  $ printf '  $ echo hi\n  hi\n' > demo.t
  $ vhs-cram --vhs false --output demo.gif demo.t
  vhs-cram: false exited with code 1
  [1]
