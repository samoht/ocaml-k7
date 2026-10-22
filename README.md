# k7

k7 records terminal sessions as SVG images animated with CSS alone. It
runs a cram test or a program on a pseudo-terminal and draws what the
terminal shows, without a browser. The image plays in an HTML `<img>`
element and in a README on GitHub. k7 also reads the tapes of Charm's
[VHS](https://github.com/charmbracelet/vhs). It records a tape itself as an
SVG image, and runs `vhs` on it for a GIF or a video.

![A terminal where four commands run: echo prints "Recorded without a browser"; printf prints "bold green" in bold green, "blue" in blue and "inverse" in reverse video; printf prints wide Chinese characters, an e with a combining accent and a thumbs-up emoji; and echo $((6 * 7)) prints 42](doc/demo.svg)

The image above is the cram test [doc/demo.t](doc/demo.t), recorded with
`dune exec -- k7 cram --output doc/demo.svg doc/demo.t`.

## Installation

```
opam install k7
```

GIF and video outputs also need [VHS](https://github.com/charmbracelet/vhs#installation).

## Command line

`k7 cram` records a cram test, the `.t` format that dune runs. It types each
command into bash, in a new empty temporary directory as dune does, and
waits for the prompt before it types the next.

```sh
$ printf '  $ echo hello\n  hello\n' > hello.t
$ k7 cram --output hello.svg hello.t
```

`k7 run` records a program, with no shell and nothing typed. The image's
first line is `$ ` and the command, which `--show` replaces or, empty,
hides. Each screen shows at the time the program printed it. `k7 run` exits
with the program's exit code; an error of k7 itself prints `k7:` and the
reason on standard error, and exits 123.

```sh
$ k7 run --output ls.svg -- ls -l
```

`k7 tape` records a tape in VHS's `.tape` syntax.

```sh
$ printf 'Output hello.svg\nType "echo hello"\nEnter\nSleep 1s\n' > hello.tape
$ k7 tape hello.tape
```

Each subcommand takes `--columns`, `--rows`, `--font-size`, `--theme` and
`--env VAR=VALUE`, and `k7 cram` and `k7 tape` take `--wait-timeout`. k7 writes `.svg` outputs and the text outputs (`.txt`,
`.ascii` and `.test`) itself, and `k7 cram` and `k7 tape` run `vhs` for the
others. `k7 cram` and `k7 tape` call `K7.record`, and `k7 run` calls `K7.run`.
`k7 COMMAND --help` gives the details.

## OCaml

`K7.record` writes the outputs of a tape. Its `Native` engine plays the
tape in bash, zsh or fish and writes the SVG and text outputs, and its
`Vhs` engine runs `vhs`; `K7.record` picks the one that writes the outputs
of the tape, unless `~engine` names one. `K7.svg` returns the image of output given with its times, such as
the bytes a test captured. `K7.run` records a program as `k7 run` does.

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

`K7.tape` checks a tape as VHS's parser would, so a mistake shows before
anything runs, and `K7.pp` prints the tape in the syntax VHS reads.

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

[lib/k7.mli](lib/k7.mli) lists the checks. A line that `K7` has no value
for goes in with `K7.raw`, and `K7.tape` checks it too.

`K7.record ~engine:Vhs` runs `vhs` on a tape and writes its `.gif`,
`.mp4` and `.webm` outputs; `vhs` needs `ttyd`, `ffmpeg` and a browser.
`K7.record ~engine:Native` writes the `.svg` outputs instead. Both write
the text outputs, and each refuses, before it plays anything, an output
that only the other writes.

## Known limitations

k7 0.9.0 carries a copy of matrix's unreleased terminal emulator in
[lib/matrix_backport](lib/matrix_backport/README.md), with fixes that
matrix 0.1.0 lacks. The copy goes once matrix 0.2.0 is released.

k7 differs from VHS in these cases:

- The native terminal answers the device attribute, status and cursor
  position queries, but not DECRQM or the OSC 11 background colour query,
  so the queries fish 4 sends as it starts go unanswered. CI tests fish 3.7
  only.
- A cursor shape set with DECSCUSR is drawn as a block.
- The `Vhs` engine reports a wait that times out as `K7.Vhs_failed`, with
  VHS's message in its output, where the `Native` engine returns
  `K7.Timeout`.
- `K7.tape` refuses a JSON theme with invalid UTF-8 or an unpaired
  surrogate escape such as `"\ud800"`, which VHS reads as U+FFFD.

Other limitations:

- Emoji take the cells that xterm.js, which VHS uses, gives them. Each
  regional indicator of a flag takes a cell, a family joined with U+200D
  shows as its separate emoji, and an emoji made wide by U+FE0F can overlap
  the next cell.
- `k7 run` types nothing, so a program that waits for input waits until
  `k7` is interrupted, and then writes what it recorded. Recording stops
  when the program exits, so what a background process prints after that
  is not in the image.
- Under heavy load on macOS, `/bin/bash` 3.2 can print `bash: child setpgid
  (N to N): Operation not permitted` as it starts a command. The command
  still runs, but the line shows on the screen and can make a wait fail.
  The cause is not known yet.

## Credits

The tape format is that of [VHS](https://github.com/charmbracelet/vhs) by
[Charm](https://charm.land/) (MIT). k7 follows its parser for the syntax,
the key names and the checks, and the SVG images use its themes. The `.t`
format comes from Mercurial's test runner, made popular by Brodie Rao's
cram. The native recorder runs on the pseudo-terminals and the terminal
emulator of Thibaut Mattio's [matrix](https://github.com/tmattio/mosaic)
(ISC).

## License

ISC. See [LICENSE.md](LICENSE.md).
