(lang dune 3.21)
(using mdx 0.4)
(name vhs)

(generate_opam_files true)
(implicit_transitive_deps false)

(license ISC)
(authors "Thomas Gazagnaire <thomas@gazagnaire.org>")
(maintainers "Thomas Gazagnaire <thomas@gazagnaire.org>")
(source (github samoht/ocaml-vhs))

(package
 (name vhs)
 (synopsis "Generate and run Charmbracelet VHS terminal tapes")
 (tags (command-line-utilities format eio))
 (description
  "Small OCaml library for generating Charmbracelet VHS .tape files and \
   invoking the vhs command-line tool. Tapes are checked values, \
   so generated demos are deterministic and reviewable.")
 (depends
  (ocaml (>= 5.2))
  (eio (>= 1.0))
  (eio_main (>= 1.0))
  fmt
  (cmdliner (>= 1.1.0))
  (alcotest :with-test)
  (mdx :with-test)
  (odoc :with-doc)))
