## 0.9.0

- Initial version.
- `Vhs.record` records a tape without VHS, as an animated SVG image drawn
  with CSS only, with the contract of `Vhs.render`: each writes the outputs
  of the tape relative to its directory, absolute and `..` paths included,
  and refuses, before it plays anything, an output only the other writes.
  `Vhs.svg` draws the image of timed output, with the settings `Vhs.tape`
  checks. `vhs-cram`
  records an `.svg` output this way, waiting for the same prompt mark as
  with VHS.
- The native recorder plays bash, zsh and fish as VHS starts them, VHS's
  window bar, named and JSON themes, margin colours and images, `Screenshot`
  (a still SVG image at an `.svg` path), `Source` and the `.txt`, `.ascii` and `.test`
  outputs, byte for byte as VHS writes them but for the screens VHS reads
  before it sizes the terminal. Its wait patterns match as Go's do, with
  Unicode case folding and ASCII word boundaries, and its waits read the
  lines VHS reads.
- The native terminal is matrix's emulator, read as VHS's xterm.js reads
  bytes where it departs from xterm: each screen has its own scroll region
  and tab stops, a restore returns to the line of the buffer and leaves the
  designations of the character sets, `ESC % G` selects ASCII, an erasure at
  the right margin keeps the wrap pending, the cursor reports count the
  column beyond the last, and `CSI S` keeps no scrollback.
- `Vhs.tape` refuses a JSON theme that Go or jsont does not read, more
  rows or columns than VHS can measure, and a frame larger than the
  encoder of a GIF, MP4 or WebM output takes.
- `vhs-cram` keeps typed non-ASCII text without a UTF-8 locale.
- The library needs OCaml 5.4, for matrix's pseudo-terminals and terminal
  emulator, and matrix 0.1.0. The emulator is a private copy of matrix's
  terminal core at mosaic 858724d6 (90a3f220 and the fixes of PRs
  #104-#108), whose fixes are not released yet.
- The README lists the known limitations of this release: the private copy
  of matrix's terminal core, the queries and cursor shapes the native
  terminal does not handle yet, a rare `setpgid` error from macOS's bash 3.2
  under load, the timed-out wait `Vhs.render` reports as `Failed`, and the
  JSON themes with invalid UTF-8 or unpaired surrogates that jsont refuses
  and Go reads.
