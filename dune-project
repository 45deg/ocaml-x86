(lang dune 3.17)
(name ocaml-x86)
(generate_opam_files true)
(package
 (name ocaml-x86)
 (synopsis "An experimental 32-bit x86 interpreter")
 (depends
  (ocaml (>= 4.14))
  (base (>= v0.16))
  zarith))
