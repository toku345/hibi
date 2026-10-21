(lang dune 3.24)

(name hibi)

(formatting
 (enabled_for ocaml))

(generate_opam_files true)

(package
 (name hibi)
 (synopsis "A small CLI for reflecting on your working day")
 (description "Answer four short questions and keep a private local JSON record.")
 (depends
  (ocaml (>= 4.14))
  yojson
  ptime
  (lambda-term (>= 3.4.1))
  lwt
  react
  zed
  (qcheck-core (and :with-test (>= 0.91)))
  (conf-python-3 :with-test)))
