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
  "Build VHS terminal recordings as OCaml values, check them against VHS's \
   own parser rules before VHS runs, print them as .tape files and run vhs \
   on them. The vhs-cram command records a cram test.")
 (depends
  (ocaml (>= 5.2))
  (eio (>= 1.4))
  (eio_main (>= 1.4))
  fmt
  (cmdliner (>= 2.0.0))
  (alcotest :with-test)
  (alcobar :with-test)
  (mdx :with-test)
  (odoc :with-doc)))
