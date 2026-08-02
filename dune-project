(lang dune 3.14)
(name clausecraft)
(generate_opam_files true)

(package
 (name clausecraft)
 (synopsis "A compact OCaml SAT solver and Sudoku encoder")
 (description
  "Clausecraft parses DIMACS CNF files, solves them with DPLL, and demonstrates Sudoku encoding.")
 (depends
  (ocaml (>= 4.14))
  (dune (>= 3.14))
  (alcotest :with-test)))