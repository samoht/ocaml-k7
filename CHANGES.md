## 0.9.0

- Initial version.
- `K7.record` records a tape with one of two engines: `Vhs` runs `vhs`,
  and `Native` records without VHS, as an animated SVG image drawn with
  CSS only. By default it picks the engine that writes the outputs of the
  tape. Both engines have one contract: each writes the outputs
  and screenshots at the paths the tape names, relative to its directory,
  absolute and `..` paths included, creates their missing directories as
  VHS does, and refuses, before it plays anything, an output or a
  screenshot only the other writes, or one whose directory it cannot write.
  `K7.svg` draws the image of timed output, with the settings `K7.tape`
  checks.
- The `k7` command has two subcommands. `k7 rec` records any program, and
  `k7 play FILE` plays a tape (`.tape`) or a cram test (`.t`), or draws
  asciicast recordings (`.cast`), by the extension of `FILE` in any case,
  and refuses an option that does not apply to that kind. Each writes an
  animated SVG image, and `k7 play` runs `vhs` for the other outputs of a
  tape or a cram test, or when `--engine vhs` says so.
  `--columns`, `--rows`, `--font-size`, `--theme` and `--framerate` set
  the terminal of each, and `--env` the environment of the program or the
  shell.
- `K7.run` records a program on a pseudo-terminal, with no shell and
  nothing typed, and returns the animated SVG image of its screens below
  its command line, timed as they arrived, with how the program exited.
  `?show` (`--show`) replaces the command line, or hides it, and the line
  shows alone for `?delay` (`--delay`, 500ms by default), as if typed.
- `k7 rec -o FILE` writes each output by its extension: `.svg` for the
  image, `.cast` for the recording in asciicast v3, with the colours of
  `--theme` in its header, which `k7 play` draws as the same image byte
  for byte, with other options if need be, `.txt`
  for the text of the last screen, and `.t` for a cram test of the command,
  run with `sh -c` on pipes as dune runs it, with its output and its exit
  code as dune writes them, but for an output line dune would read as a
  command, which gets one more space of indentation. `K7.capture` records a program as an asciicast
  recording, and `K7.run` draws it with `K7.svg_of_casts`; `K7.cram` runs a
  command and gives its cram test. Each starts the program in a process
  group of its own, which gets SIGTERM when the program exits, when
  `?stop` resolves or when the calling fiber is cancelled, then SIGKILL a
  second later by the `~clock` it takes.
- `K7.cast_of_string` reads a recording in asciicast v2 or v3, as
  asciinema 2 and 3 and console's `CONSOLE_CAST` write it, and
  `K7.svg_of_casts` (`k7 play FILE.cast`) draws it as `K7.svg` draws its
  output events, each at its time rounded to the millisecond, so the same
  recording gives the same image, byte for byte. The terminal has the size
  of the header and follows the resize events, on an image of the largest
  size, unless `--columns` and `--rows` size it; the header's theme gives
  its colours unless `--theme` does; a pause longer than the header's
  `idle_time_limit` lasts that limit; and the last screen holds for a
  second after the last event, the exit included. The header's command
  shows first, as `k7 rec` shows its own, and `?show` and `?delay`
  (`--show`, `--delay`) replace it and set the pause before the output.
  `K7.text_of_casts` (`k7 play -o FILE.txt FILE.cast`) is the text of the
  last screen. Several recordings (`k7 play -o FILE.svg A.cast B.cast`)
  play one after the other on one terminal, each with its command line,
  the next starting once the last screen of the one before has held for a
  second; one `--show` replaces every command, and one per recording each
  in turn.
- `K7.check` returns the error `K7.record` would before it records,
  without starting a shell or `vhs`.
- `K7.run` and `K7.record` take `?env`, variables for the environment of
  the program or the shell, which `--env` sets for each subcommand. Both
  clear an inherited `COLUMNS` and `LINES`, so that the program reads the
  size of its terminal.
- `K7.run`, `K7.capture`, `K7.cram` and `K7.record` take `?stop`, which
  ends a recording and keeps what it recorded; `k7 play` and `k7 rec`
  resolve it on SIGINT or SIGTERM, then exit 130 or 143.
- `K7.record`, `K7.run`, `K7.capture` and `K7.cram` take an
  `Eio_unix.Process.mgr`, such as the `Eio.Stdenv.process_mgr` of the
  environment that `Eio_main.run` gives.
- `K7.record` takes `?stuck`, a test of what the shell wrote since the
  last wait, which fails a native recording at once with `K7.Stuck`;
  `k7 play` reports a cram command that leaves bash at its continuation
  prompt as soon as bash shows it, rather than when the wait times out.
- `k7 play --dry-run` checks a tape or a cram test, with the options and
  outputs given, without recording it.
- `k7 play` runs the commands of a cram test in a new empty temporary
  directory, as dune does, or in the directory of `--dir`.
- `K7.of_string` reads a tape in the `.tape` syntax, every `Output` line
  as an output, its settings as `K7.setting` values and `Hide`/`Show` as
  `K7.hidden`, and `K7.with_settings` and `K7.with_outputs` replace a
  tape's settings and outputs, which `K7.settings` and `K7.outputs`
  return. A tape may have no output until it plays, so a tape without an
  `Output` line reads, and `k7 play -o` gives it one. A tape that ends
  hidden ends on its last shown screen, as with VHS, not on the screen
  its hidden actions leave, such as the shell's prompt after a program
  quits.
- The native recorder plays bash, zsh and fish as VHS starts them, VHS's
  window bar, named and JSON themes, margin colours and images, `Screenshot`
  of an `.svg` path (a still SVG image), `Source` and the `.txt`, `.ascii`
  and `.test` outputs, byte for byte as VHS writes them but for the screens
  VHS reads before it sizes the terminal. Its wait patterns match as Go's
  do, with Unicode case folding and ASCII word boundaries, and its waits
  read the lines VHS reads, never the prompt a command was typed at. A wait
  that times out is `K7.Timeout`, naming its line, and a shell that exits
  while a wait waits is `K7.Exited`, at once.
- The native terminal is matrix's emulator, read as VHS's xterm.js reads
  bytes where it departs from xterm: each screen has its own scroll region
  and tab stops, a restore returns to the line of the buffer and leaves the
  designations of the character sets, `ESC % G` selects ASCII, an erasure at
  the right margin keeps the wrap pending, the cursor reports count the
  column beyond the last, and `CSI S` keeps no scrollback. A frame drawn
  in synchronized output (DEC private mode 2026) shows once complete,
  when the program resets the mode or 150ms after it set it, even when it
  reaches the terminal in several reads.
- `K7.Columns` and `K7.Rows` size the terminal in cells, as VHS's
  `Set Columns` and `Set Rows` do, in place of `K7.Width` and
  `K7.Height`.
- `K7.Wait_timeout` sets how long a wait without a timeout of its own
  waits, as VHS's `Set WaitTimeout` does; `--wait-timeout` sets it for
  a cram test.
- `K7.tape` refuses a JSON theme that Go or jsont does not read, more
  rows or columns than VHS can measure, a frame larger than the encoder of
  a GIF, MP4 or WebM output takes, and a screenshot with the path of an
  output or of another screenshot.
- `k7 play` keeps the typed non-ASCII text of a cram test without a
  UTF-8 locale, and waits for the same prompt mark natively as with VHS.
- The library needs OCaml 5.4, for matrix's pseudo-terminals and terminal
  emulator, and matrix 0.1.0. The emulator is a private copy of matrix's
  terminal core at mosaic 858724d6, whose fixes matrix's main has since
  made in its own way and not released yet.
- The README lists the known limitations of this release: the private copy
  of matrix's terminal core, the queries and cursor shapes the native
  terminal does not handle yet, a rare `setpgid` error from macOS's bash 3.2
  under load, the timed-out wait the `Vhs` engine reports as
  `Vhs_failed`, and the JSON themes with invalid UTF-8 or unpaired
  surrogates that jsont refuses and Go reads, the input and background
  output `k7 rec` does not record, the cram test of a command killed by a
  signal, whose line from dune's shell changes on each run, and the normal
  screen that a resize clears behind the alternate screen.
