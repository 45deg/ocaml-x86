(lang dune 3.17)
(name ocaml_os)
(generate_opam_files true)
(package
 (name ocaml_os)
 (synopsis "An experimental 32-bit x86 interpreter")
 (depends
  (ocaml (>= 4.14))
  (base (>= v0.16))
  zarith))
