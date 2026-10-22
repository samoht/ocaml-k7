# matrix_backport

A temporary verbatim copy of matrix's terminal core at
[invariant-hq/mosaic](https://github.com/invariant-hq/mosaic)
([samoht/mosaic](https://github.com/samoht/mosaic)) commit
`3ccffa89b915646a7bebafa3a39edd0f62f659eb` (branch `vte-wrap-zero-width`),
which keeps a zero-width character at the wrap limit on top of
`90a3f220997fc14819623a5cb98afca7a3d29bb6`.

It exists because the released matrix 0.1.0 lacks the terminal fixes that
vhs's emulator needs: those merged on invariant-hq/mosaic since 0.1.0, the
PR stack #46-#65, #78-#89 and #94-#96 on top of them, and that last commit,
not yet proposed upstream. It is deleted
when vhs moves to matrix 0.2.0 (the `next` branch).

**Do not edit these files: fix upstream instead.** dune builds the
directory as vendored code (`vendored_dirs` in `../dune`), so it is built
without warnings and is skipped by `dune fmt` (also listed in
`.ocamlformat-ignore`), merlint and dupfind (`.dupfind.toml`).

## Contents

The files are copied byte for byte, flattened into this directory:

| Upstream directory  | Upstream library | Files                                                                                           |
| ------------------- | ---------------- | ----------------------------------------------------------------------------------------------- |
| `matrix/lib/ansi/`  | `matrix.ansi`    | `ansi`, `attr`, `color`, `escape`, `parser`, `segment`, `sgr_state`, `style`, `writer`          |
| `matrix/lib/text/`  | `matrix.text`    | `matrix_text`, `unicode`, `unicode_data`, `uuseg_grapheme_cluster`                              |
| `matrix/lib/grid/`  | `matrix.grid`    | `matrix_grid`, `border`, `grapheme_store`, `grapheme_tracker`, `grid_data`, `packed_cell`       |
| `matrix/lib/vte/`   | `matrix.vte`     | `vte`                                                                                           |

each as its `.ml` and `.mli` where upstream has one, and `LICENSE` from the
repository root. Only `dune` and this README are vhs's own.

The four upstream libraries are one wrapped library, `matrix_backport`, so
that none of their modules clashes with matrix 0.1.0's (`Ansi`, `Grid`,
`Glyph`, `Vte`) in a program that links both. vhs reaches them as
`Matrix_backport.Ansi`, `Matrix_backport.Matrix_grid`,
`Matrix_backport.Matrix_text` and `Matrix_backport.Vte`, and takes only
`matrix.pty` from the released matrix 0.1.0.

## Edits

None: the files compile unchanged in the wrapped library.
