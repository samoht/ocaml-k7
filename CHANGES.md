## 0.10.0 (10/11/2026)

- k7 installs on Windows, where `k7 play` draws asciicast recordings.
  Recording a program, a tape or a cram test needs a Unix pseudo-terminal,
  and fails there (#4).

- k7 now needs matrix 0.2.0, so it cannot share a switch with a package that
  needs an older matrix (#5).

- A SUB byte in a recording now shows a reversed question mark, and the
  national character sets apply only once the program selects national mode,
  as on a VT220 (#5).

- A recording whose header gives a width or height too large for an `int`,
  such as 2^62, is now refused, instead of read as a negative size that
  `string_of_cast` writes and `cast_of_string` refuses (#2).

## 0.9.0 (27/10/2026)

- Initial version.
