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
 (synopsis
  "Generate and run VHS terminal tapes, or record them as SVG without VHS")
 (tags (command-line-utilities format eio))
 (description
  "Build VHS terminal recordings as OCaml values, check them against VHS's \
   own parser rules before VHS runs, print them as .tape files and run vhs \
   on them to record a GIF or a video. A native recorder plays a tape in \
   bash, zsh or fish on a pseudo-terminal and draws an SVG image animated \
   with CSS alone, with no vhs, browser or ffmpeg. The k7 command \
   records a cram test, a program or a tape.")
 (depends
  (ocaml (>= 5.4))
  (eio (>= 1.6))
  (eio_main (>= 1.6))
  (fmt (>= 0.8.7))
  (cmdliner (>= 2.0.0))
  (alcotest :with-test)
  (mtime (>= 2.0.0))
  (cstruct (>= 6.0.1))
  ; Only matrix.pty comes from matrix 0.1.0: the terminal core is the
  ; private copy in lib/matrix_backport until matrix 0.2.0 is on opam.
  (matrix (and (>= 0.1.0) (< 0.2.0)))
  (re (>= 1.10.0))
  (jsont (>= 0.4.0))
  (bytesrw (>= 0.1.0))
  (markup (and (>= 1.0.0) :with-test))
  (alcobar (and :with-test (>= 0.3.2)))
  (mdx :with-test)
  (odoc :with-doc)))
