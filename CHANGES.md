## unreleased

- k7 now needs matrix 0.2.0, and no longer carries a copy of its terminal
  emulator (#5).

- A recording whose header gives a width or height too large for an `int`,
  such as 2^62, is now refused, instead of read as a negative size that
  `string_of_cast` writes and `cast_of_string` refuses (#2).

## 0.9.0 (27/10/2026)

- Initial version.
