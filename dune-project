(lang dune 3.17)
(name ocaml_os)
(generate_opam_files true)
(package
 (name ocaml_os)
 (synopsis "An experimental 32-bit x86 interpreter")
 (depends
  (ocaml (>= 5.2))
  (base (>= v0.17))
  zarith
  (ocamlformat (and :with-test (= 0.29.0)))))
