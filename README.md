# vhs

Generate Charmbracelet VHS `.tape` files from OCaml and run the `vhs`
command-line tool on them.

[VHS](https://github.com/charmbracelet/vhs) records a scripted terminal
session as a GIF, MP4 or WebM file. Its input is a `.tape` file with one
command per line, which is easy to write by hand and hard to keep in step with
the tool it shows. This library models every tape line as an OCaml
value, so a demo can come from the same code or test that checks the tool,
and the printed tape is stable enough to review in a diff. Rendering still goes
through the external `vhs` binary, which must be on the `PATH`.

## Installation

<!-- $MDX skip -->
```sh
$ opam install vhs
```

If opam cannot find the package, it has not reached the public
`opam-repository` yet. Add the overlay repository first:

<!-- $MDX skip -->
```sh
$ opam repo add samoht https://tangled.org/gazagnaire.org/opam-overlay.git
$ opam update
$ opam install vhs
```

## Usage

A tape is a list of items in output order. Settings come before the first
terminal action, as VHS expects:

```ocaml
let tape =
  Vhs.tape
    [
      Vhs.output "demo.gif";
      Vhs.set (Shell "bash");
      Vhs.set (Font_size 18);
      Vhs.set (Width 1000);
      Vhs.set (Height 560);
      Vhs.type' "dune test";
      Vhs.enter ();
      Vhs.sleep (S 2.);
    ]

let () = Vhs.write_file ~path:"demo.tape" tape
```

`Vhs.render` writes the tape and then runs `vhs` on it through an Eio process
manager. A missing executable, a non-zero exit and any other I/O failure all
come back as a `Vhs.error`:

```ocaml
Eio_main.run @@ fun env ->
let proc = Eio.Stdenv.process_mgr env in
match Vhs.render ~proc ~path:"demo.tape" tape with
| Ok () -> ()
| Error e -> Fmt.epr "%a@." Vhs.pp_error e
```

`Vhs.publish` runs `vhs publish` and returns the share links VHS prints.

## Recording a cram test

A cram test is a `.t` file of shell commands, each on a line starting with `  $
`, followed by the output they are expected to print; dune replays the commands
and diffs the output. The `vhs-cram` executable turns such a cram transcript
into a recording. Each `$` command is typed and submitted, and the tape waits
for the last line of its expected output before moving on, so the GIF shows the
output the test checks. A setup script runs hidden before the first command:

<!-- $MDX skip -->
```sh
$ vhs-cram --setup setup.sh --output demo.gif demo.t
```

The terminal is 1100 pixels wide unless `--width` says otherwise. The same
conversion is available as a library through `Vhs.Cram.parse` and
`Vhs.Cram.to_tape`.

## License

ISC. See [LICENSE.md](LICENSE.md).
