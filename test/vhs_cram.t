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
  Env VHS_CRAM_PROMPT `__vhs_n=; __vhs_prompt() { PS1='\[\e[38;2;90;86;224m\]> \[\342\201\240\342\201\240\342\200\213\342\201\240\342\200\213\342\200\213\342\200\213\342\201\240\342\200\213\342\201\240\342\200\213\342\200\213\342\201\240\342\200\213\342\201\240\342\201\240\e[0m\]'; __vhs_n=$__vhs_n'\342\200\213'; PS2='> \[\342\201\240\342\201\240\342\200\213\342\201\240\342\200\213\342\200\213\342\200\213\342\201\240\342\200\213\342\201\240\342\200\213\342\200\213\342\201\240\342\200\213\342\201\240\342\201\240'$__vhs_n'\]'; }; __vhs=$(printf '%s\n' ${PROMPT_COMMAND+"${PROMPT_COMMAND[@]}"} __vhs_prompt); unset PROMPT_COMMAND; PROMPT_COMMAND=$__vhs; unset __vhs VHS_CRAM_PROMPT`
  Type 'eval "$VHS_CRAM_PROMPT"; clear'
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
  $ { vhs-cram --print --output demo.gif big.t; echo $? > status; } | head -n 1
  Output "demo.gif"
  $ cat status
  0
  $ vhs-cram --print --output demo.gif big.t >&-
  vhs-cram: standard output: Bad file descriptor
  [123]

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
  Env VHS_CRAM_PROMPT `__vhs_n=; __vhs_prompt() { PS1='\[\e[38;2;90;86;224m\]> \[\342\201\240\342\201\240\342\201\240\342\200\213\342\201\240\342\200\213\342\201\240\342\201\240\342\201\240\342\201\240\342\200\213\342\201\240\342\200\213\342\201\240\342\200\213\342\200\213\e[0m\]'; __vhs_n=$__vhs_n'\342\201\240'; PS2='> \[\342\201\240\342\201\240\342\201\240\342\200\213\342\201\240\342\200\213\342\201\240\342\201\240\342\201\240\342\201\240\342\200\213\342\201\240\342\200\213\342\201\240\342\200\213\342\200\213'$__vhs_n'\]'; }; __vhs=$(printf '%s\n' ${PROMPT_COMMAND+"${PROMPT_COMMAND[@]}"} __vhs_prompt); unset PROMPT_COMMAND; PROMPT_COMMAND=$__vhs; unset __vhs VHS_CRAM_PROMPT`
  Type `source 'setup.sh'; eval "$VHS_CRAM_PROMPT"; clear`
  Enter
  Wait+Line /\x{2060}\x{2060}\x{2060}\x{200B}\x{2060}\x{200B}\x{2060}\x{2060}\x{2060}\x{2060}\x{200B}\x{2060}\x{200B}\x{2060}\x{200B}\x{200B}$/
  Show
  Type "echo hi"
  Enter
  Wait+Line /\x{2060}\x{2060}\x{2060}\x{200B}\x{2060}\x{200B}\x{2060}\x{2060}\x{2060}\x{2060}\x{200B}\x{2060}\x{200B}\x{2060}\x{200B}\x{200B}$/
  Sleep 1000ms
