Usage errors are reported by the command line parser, whose wording
varies across cmdliner versions.

  $ vhs-cram demo.t 2> /dev/null
  [124]

  $ vhs-cram --output demo.gif missing.t 2> /dev/null
  [124]

A command the tape cannot type is an error, not an exception.

  $ printf '  $ echo \001\n' > bad.t
  $ vhs-cram --output demo.gif bad.t
  vhs-cram: Type "echo \001" holds a control character
  [1]

A failing vhs is reported, and the temporary tape is removed.

  $ printf '  $ echo hi\n  hi\n' > demo.t
  $ mkdir tmp
  $ TMPDIR=$PWD/tmp vhs-cram --vhs false --output demo.gif demo.t 2> err
  [1]
  $ sed 's/vhs-cram-.*\.tape/TAPE/' err
  vhs-cram: false $TESTCASE_ROOT/tmp/TAPE failed: Exited (code 1)
  $ ls tmp
