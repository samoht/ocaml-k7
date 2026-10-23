# k7

k7 records terminal sessions and draws them as SVG images animated with
CSS alone, without a browser, so they play in an HTML `<img>` element and
in a README on GitHub. `k7 rec` runs any program on a pseudo-terminal and
keeps what it printed, with its times, as an image, a recording, the text
of its last screen or a cram test. `k7 play` draws a recording later, with
other settings if need be, and plays the cram tests that dune runs and the
tapes of Charm's [VHS](https://github.com/charmbracelet/vhs) by typing
their commands into a shell.

A recording is in asciinema's asciicast format, so `k7 play` also draws
what [asciinema](https://asciinema.org/) records, in its v2 and v3 formats.
k7 reads and checks VHS tapes as VHS does, and runs `vhs` on one for a GIF
or a video.

![A terminal where four commands run: echo prints "Recorded without a browser"; printf prints "bold green" in bold green, "blue" in blue and "inverse" in reverse video; printf prints wide Chinese characters, an e with a combining accent and a thumbs-up emoji; and echo $((6 * 7)) prints 42](doc/demo.svg)

The image above is the cram test [doc/demo.t](doc/demo.t), recorded with
`dune exec -- k7 play --output doc/demo.svg doc/demo.t`.

## Installation

```
opam install k7
```

GIF and video outputs also need [VHS](https://github.com/charmbracelet/vhs#installation).

## Command line

`k7 rec` runs a program on a pseudo-terminal, with no shell and nothing
typed, and writes each output of `-o` in the format of its extension. An
`.svg` output is the animated image. Its first line is `$ ` and the
command, which `--show` replaces or, empty, hides; that line shows alone
for half a second, as if typed, or for `--delay` milliseconds, and each
screen then shows at the time the program printed it. A `.cast` output is
the recording in asciicast v3, a `.txt` output the text of the last
screen, and a `.t` output a cram test of the command, run with `sh -c` on
pipes as dune runs it, so that dune reads it back unchanged. `k7 rec` exits
with the program's exit code; an error of k7 itself prints `k7:` and the
reason on standard error, and exits 123.

```sh
$ k7 rec --output ls.svg -- ls -l
$ k7 rec --output hello.cast --output hello.t -- printf 'hello\n'
```

The cram test `hello.t` then holds the command and what it printed:

```
  $ printf 'hello\n'
  hello
```

`k7 play FILE` plays a tape (`.tape`), a cram test (`.t`) or an
asciicast recording (`.cast`), by the extension of `FILE` in any case,
and refuses an option that does not apply to that kind. On a recording, it
runs nothing: it draws the output at its times, rounded to the
millisecond, so the same recording gives the same image, byte for byte,
and `k7 play` of the `.cast` that `k7 rec` wrote gives the image that
`k7 rec` would have. The header's command shows first, as for `k7 rec`,
and `--show` and `--delay` apply as they do there. A `.txt` output is the
text of the last screen.

```sh
$ k7 play --theme Dracula --output hello.svg hello.cast
```

Several recordings play one after the other in one image, as commands
typed in turn, such as a program run with two different options. One
`--show` replaces every command, and one per recording each in turn.

```sh
$ k7 rec --output dark.cast -- printf 'dark\n'
$ k7 rec --output light.cast -- printf 'light\n'
$ k7 play --output both.svg dark.cast light.cast
```

A cram test, the `.t` format that dune runs, gives the commands to type
into bash, in a new empty temporary directory as dune does; `k7 play`
waits for the prompt before it types the next. `--setup`, `--dir` and
`--wait-timeout` apply to cram tests only.

```sh
$ printf '  $ echo hello\n  hello\n' > hello.t
$ k7 play --output hello.svg hello.t
```

A tape is in VHS's `.tape` syntax, and `-o` replaces its `Output` lines.
`--dry-run` checks a tape or a cram test, with the options, as a
recording would, without recording it.

```sh
$ printf 'Output hello.svg\nType "echo hello"\nEnter\nSleep 1s\n' > hello.tape
$ k7 play hello.tape
```

Both subcommands take `--columns`, `--rows`, `--font-size`, `--theme`
and `--framerate`, and `--env VAR=VALUE` for the program of `k7 rec` or
the shell of a tape or a cram test. On a tape or a cram test, k7 writes
`.svg` outputs and the text outputs (`.txt`, `.ascii` and `.test`)
itself, and runs `vhs` for the others. `k7 COMMAND --help` gives the
details.

## OCaml

`K7.capture` records a program as `k7 rec` does and returns the recording,
which `K7.svg_of_casts` draws and `K7.text_of_casts` reads the last screen
of; `K7.cast_of_string` and `K7.string_of_cast` read and write it in
asciicast. `K7.run` records a program and returns the image at once, and
`K7.cram` returns the cram test of a command. `K7.svg` returns the image of
output given with its times, such as the bytes a test captured.

`K7.record` writes the outputs of a tape. Its `Native` engine plays the
tape in bash, zsh or fish and writes the SVG and text outputs, and its
`Vhs` engine runs `vhs`; `K7.record` picks the one that writes the outputs
of the tape, unless `~engine` names one.

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

let record_hello env =
  let ( let* ) = Result.bind in
  let* cast, _status =
    K7.capture
      ~clock:(Eio.Stdenv.mono_clock env)
      ~proc:(Eio.Stdenv.process_mgr env)
      ~cwd:(Eio.Stdenv.cwd env)
      [ "printf"; "hello\n" ]
  in
  K7.svg_of_casts ~settings:K7.[ Theme "Dracula" ] [ cast ]
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
- `k7 rec` types nothing, so a program that waits for input waits until
  `k7` is interrupted, and then writes what it recorded. Recording stops
  when the program exits, so what a background process prints after that
  is not in the image.
- A cram test that `k7 rec` writes of a command killed by a signal does not
  pass under dune: dune's shell then prints a line with the process ID,
  which changes on each run.
- A recording that resizes the terminal keeps what fits on the screen in
  use; while a program uses the alternate screen, the normal screen behind
  it is cleared.
- Under heavy load on macOS, `/bin/bash` 3.2 can print `bash: child setpgid
  (N to N): Operation not permitted` as it starts a command. The command
  still runs, but the line shows on the screen and can make a wait fail.
  The cause is not known yet.

## Credits

The `.cast` format is the asciicast format of
[asciinema](https://asciinema.org/), by Marcin Kulik. The tape format is
that of [VHS](https://github.com/charmbracelet/vhs) by
[Charm](https://charm.land/) (MIT). k7 follows its parser for the syntax,
the key names and the checks, and the SVG images use its themes. The `.t`
format comes from Mercurial's test runner, made popular by Brodie Rao's
cram. The native recorder runs on the pseudo-terminals and the terminal
emulator of Thibaut Mattio's [matrix](https://github.com/tmattio/mosaic)
(ISC).

## License

ISC. See [LICENSE.md](LICENSE.md).
