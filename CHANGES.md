## 1.0.0

- Initial version.
- `Vhs.record` records a tape without VHS, as an animated SVG image drawn
  with CSS only, and writes it to the tape's `.svg` outputs; `Vhs.svg` draws
  the image of timed output. `vhs-cram` records an `.svg` output this way,
  waiting for the same prompt mark as with VHS.
- The native recorder plays bash and zsh as VHS starts them, VHS's window
  bar, named and JSON themes, margin colours and images, `Screenshot` (a
  still SVG image), `Source` and the `.txt`, `.ascii` and `.test` outputs,
  byte for byte as VHS writes them but for the screens VHS reads before it
  sizes the terminal. Its wait patterns match as Go's do, with Unicode case
  folding and ASCII word boundaries, and its waits read the lines VHS reads.
- The native terminal is matrix's emulator, read as VHS's xterm.js reads
  bytes where it departs from xterm: each screen has its own scroll region
  and tab stops, a restore returns to the line of the buffer and leaves the
  designations of the character sets, `ESC % G` selects ASCII, an erasure at
  the right margin keeps the wrap pending, the cursor reports count the
  column beyond the last, and `CSI S` keeps no scrollback.
- `Vhs.tape` refuses a JSON theme that Go does not read, more rows or
  columns than VHS can measure, and a frame larger than the encoder of a
  GIF, MP4 or WebM output takes.
- `vhs-cram` keeps typed non-ASCII text without a UTF-8 locale.
- The library needs OCaml 5.4, for matrix's pseudo-terminals and terminal
  emulator, and matrix 0.2.0.
