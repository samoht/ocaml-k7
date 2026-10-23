(lang dune 3.21)
(using mdx 0.4)
(name k7)

(generate_opam_files true)
(implicit_transitive_deps false)

(license ISC)
(authors "Thomas Gazagnaire <thomas@gazagnaire.org>")
(maintainers "Thomas Gazagnaire <thomas@gazagnaire.org>")
(source (github samoht/ocaml-k7))

(package
 (name k7)
 (synopsis
  "Record terminal sessions and draw them as animated SVG images")
 (tags (command-line-utilities format eio))
 (description
  "k7 records any program, a cram test or a tape on a pseudo-terminal and \
   draws it as an SVG image animated with CSS alone, with no browser or \
   ffmpeg. It writes a program's recording in asciinema's asciicast format, \
   the text of its last screen or a cram test of it, and draws asciicast v2 \
   and v3 recordings, such as asciinema's. It also builds tapes for Charm's \
   VHS as OCaml values, checks them against VHS's own parser rules and runs \
   vhs on them to record a GIF or a video.")
 (depends
  (ocaml (>= 5.4))
  (eio (>= 1.6))
  (eio_main (>= 1.6))
  (fmt (>= 0.8.7))
  (cmdliner (>= 2.0.0))
  (alcotest :with-test)
  (mtime (>= 2.0.0))
  (cstruct (>= 6.0.1))
  (matrix (and (>= 0.1.0) (< 0.2.0)))
  (re (>= 1.10.0))
  (jsont (>= 0.4.0))
  (bytesrw (>= 0.1.0))
  (markup (and (>= 1.0.0) :with-test))
  (alcobar (and :with-test (>= 0.3.2)))
  (mdx :with-test)
  (odoc :with-doc)))
