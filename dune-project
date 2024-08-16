(lang dune 3.15)
(name sibyl)
(using menhir 3.0)
(generate_opam_files true)
(package
 (name sibyl)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml containers dune menhir)
 (tags
  (topics "to describe" your project)))
