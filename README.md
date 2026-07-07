# vhs

Generate Charmbracelet VHS `.tape` files from OCaml and optionally run the
`vhs` command-line tool.

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

Rendering is a thin wrapper around the external `vhs` binary:

```ocaml
Eio_main.run @@ fun env ->
let proc = Eio.Stdenv.process_mgr env in
match Vhs.render ~proc ~path:"demo.tape" tape with
| Ok () -> ()
| Error e -> Fmt.epr "%a@." Vhs.pp_error e
```

## Installation

Install with opam:

<!-- $MDX skip -->
```sh
$ opam install vhs
```

If opam cannot find the package, it may not yet be released in the public
`opam-repository`. Add the overlay repository, then install it:

<!-- $MDX skip -->
```sh
$ opam repo add samoht https://tangled.org/gazagnaire.org/opam-overlay.git
$ opam update
$ opam install vhs
```
