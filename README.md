# k7

k7 records terminal sessions as animated SVG images: a cram test, any
program, or a tape. It runs them on a pseudo-terminal and draws what the
terminal shows with CSS alone, with no browser, so the image plays in an
HTML `<img>` and in a README on GitHub. When you want a GIF or a video
instead, k7 writes tapes for Charm's [VHS](https://github.com/charmbracelet/vhs),
checks them against VHS's own rules, and runs `vhs` on them.

![A terminal where four commands run: echo prints "Recorded without a browser"; printf prints "bold green" in bold green, "blue" in blue and "inverse" in reverse video; printf prints wide Chinese characters, an e with a combining accent and a thumbs-up emoji; and echo $((6 * 7)) prints 42](doc/demo.svg)

The recording above is the cram test [doc/demo.t](doc/demo.t), recorded with
`dune exec -- k7 cram --output doc/demo.svg doc/demo.t`.

## Installation

```
opam install k7
```

A GIF or a video also needs [VHS](https://github.com/charmbracelet/vhs#installation).

## Recording from the command line

`k7 cram` records a cram test, the `.t` files dune runs: it types each
command into bash and waits for the prompt to come back before the next one.

```sh
$ printf '  $ echo hello\n  hello\n' > hello.t
$ k7 cram --output hello.svg hello.t
```

`k7 run` records a program, with no shell and nothing typed. Each screen
shows at the time the program printed it, and `k7 run` exits with the
program's exit code.

```sh
$ k7 run --output ls.svg -- ls -l
```

`k7 tape` records a tape in VHS's `.tape` syntax.

```sh
$ printf 'Output hello.svg\nType "echo hello"\nEnter\nSleep 1s\n' > hello.tape
$ k7 tape hello.tape
```

Each subcommand takes `--cols`, `--rows`, `--font-size`, `--theme` and
`--env VAR=VALUE`. An `.svg` output, and the text outputs `.txt`, `.ascii`
and `.test`, are recorded by k7 itself; `k7 cram` and `k7 tape` run `vhs`
for the others. `k7 --help` and `k7 COMMAND --help` give the details.

## Recording from OCaml

`K7.record` plays a tape in bash, zsh or fish and writes its `.svg` output.
`K7.svg` draws the same image from output given with its times, such as
bytes a test captured, and returns it. Both take their settings from the
same values; `K7.run` records a program the same way.

```ocaml
let record_ls env =
  let ( let* ) = Result.bind in
  let* tape =
    K7.tape
      ~settings:K7.[ Columns 80; Rows 12 ]
      ~outputs:[ "ls.svg" ]
      K7.[ type' "ls\n"; wait Prompt; sleep (Ms 1000) ]
  in
  K7.record
    ~clock:(Eio.Stdenv.mono_clock env)
    ~proc:(Eio.Stdenv.process_mgr env)
    ~cwd:(Eio.Stdenv.cwd env) tape

let image =
  K7.svg
    ~settings:K7.[ Columns 40; Rows 4 ]
    K7.[ (Ms 0, "$ ls\r\n"); (Ms 500, "a.txt  b.txt\r\n") ]
```

## Writing tapes for VHS

A tape is a list of settings, the files to write, and a list of actions.
`K7.tape` checks it as VHS's parser would, so a mistake shows before
anything runs, and `K7.pp` prints it in the format VHS reads:

```ocaml
# let tape =
    K7.tape
      ~settings:K7.[ Font_size 18; Width 1000; Height 560 ]
      ~outputs:[ "demo.gif" ]
      K7.[ type' "dune test\n"; wait Prompt; sleep (Ms 2000) ];;
val tape : (K7.t, K7.error) result = Ok <abstr>
# Fmt.pr "%a" K7.pp (Result.get_ok tape);;
Output "demo.gif"
Set FontSize 18
Set Width 1000
Set Height 560
Type "dune test"
Enter
Wait
Sleep 2000ms
- : unit = ()
# K7.tape ~outputs:[ "demo" ] [ K7.sleep (Ms 100) ];;
- : (K7.t, K7.error) result =
Error
 (K7.Invalid "Output \"demo\": neither an extension nor a trailing slash")
```

The checks cover the syntax of every line, quoting, durations, regular
expressions, key names, frame sizes and outputs; `K7.raw` passes a line
`K7` has no value for, and it is checked too. `K7.render` runs `vhs` on a
tape, which needs `ttyd`, `ffmpeg` and a browser, and writes the `.gif`,
`.mp4` and `.webm` outputs. `K7.record` writes the `.svg` outputs instead;
both write the text outputs and the paths the tape names, and each refuses
an output only the other writes. [lib/k7.mli](lib/k7.mli) states the
checks, the contract of each function and what the native recorder plays.

## Known limitations

- k7 0.9.0 depends on the released matrix 0.1.0 for its pseudo-terminals,
  and carries a temporary private copy of matrix's terminal core, from
  mosaic commit 858724d6 (90a3f220 and the fixes of invariant-hq/mosaic
  PRs #104-#108), in [lib/matrix_backport](lib/matrix_backport/README.md).
  matrix 0.1.0 lacks emulator fixes the native recorder needs, such as
  keeping zero-width characters, moving within scroll regions, setting tab
  stops and replying to device queries. The copy goes away once matrix 0.2.0
  is released. No test is skipped or narrowed for matrix 0.1.0, since the
  tests run against the copy.
- The native terminal answers the device attribute, status and cursor
  position queries, but not DECRQM or the OSC 11 background colour query,
  so the queries fish 4 sends as it starts go unanswered. CI tests fish 3.7
  only.
- A program that sets the cursor's shape with DECSCUSR still gets a block
  cursor in the SVG image, as the terminal does not keep the shape.
- Under heavy load on macOS, `/bin/bash` 3.2 can print `bash: child setpgid
  (N to N): Operation not permitted` as it starts a command. The command
  still runs, but the line shows on the screen and can make a wait on the
  screen fail. It is rare, and its cause is not known yet.
- Emoji take the cells xterm.js, which VHS uses, gives them: each regional
  indicator of a flag takes a cell, a family joined with U+200D shows as
  its separate emoji, and an emoji made wide by U+FE0F can overlap the cell
  after it.
- `K7.render` reports a wait that times out as `K7.Failed`, with VHS's
  message in its output, where `K7.record` returns `K7.Timeout`.
- `k7 run` types nothing, so a program that waits for input waits until
  `k7` is interrupted, and it stops recording when the program exits, so
  what a background process prints after that is not in the image.
- A JSON theme is read with [jsont](https://erratique.ch/software/jsont),
  which refuses invalid UTF-8 and an unpaired surrogate escape, such as
  `"\ud800"`, in a string. VHS reads them with Go's `encoding/json`, which
  replaces them with U+FFFD, so `K7.tape` refuses such a theme where VHS
  would not.

## Credits

The tapes are for [VHS](https://github.com/charmbracelet/vhs) by
[Charm](https://charm.land/) (MIT), whose themes the SVG images use and
whose parser defines the syntax, key names and checks that k7 follows. The `.t` format comes from Mercurial's test runner, made popular by
Brodie Rao's cram, and is the one dune runs. The native recorder runs on the
pseudo-terminals and the terminal emulator of Thibaut Mattio's
[matrix](https://github.com/tmattio/mosaic) (ISC); until matrix 0.2.0 is
released, the emulator is a copy of matrix's unreleased terminal core in
[lib/matrix_backport](lib/matrix_backport/README.md).

## License

ISC. See [LICENSE.md](LICENSE.md).
