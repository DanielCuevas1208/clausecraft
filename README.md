# Clausecraft

Clausecraft is a compact OCaml SAT solver with a Sudoku encoding demo.

It reads standard DIMACS CNF files and reports satisfiable or unsatisfiable results.

This supporting project shows parser design, recursive search, independent model checks, and a practical Sudoku encoding.

## First release

This release implements a DIMACS parser and a DPLL solver with unit propagation.

The solver selects high-occurrence variables and explores both truth values.

The Sudoku encoder uses one Boolean variable for each cell and digit.

The CLI reports clause counts, search statistics, and solve time.

## Architecture

- `lib/cnf.ml` parses DIMACS headers, comments, literals, and clause terminators.
- `lib/solver.ml` performs recursive DPLL search and unit propagation.
- `lib/sudoku.ml` encodes and decodes standard 9 by 9 Sudoku puzzles.
- `bin/main.ml` exposes the `solve` and `sudoku` commands.
- `test/test_clausecraft.ml` checks models with an independent clause evaluator.

The library keeps parsing, solving, and Sudoku concerns separate.

## Setup

Use OCaml 4.14 or newer, Dune 3.14 or newer, and Opam.

Install dependencies with Opam.

```sh
opam install . --deps-only --with-test --locked
```

Build the project.

```sh
dune build
```

Run the tests.

```sh
dune test
```

The repository tracks `clausecraft.opam.locked` for direct dependency versions.

## Run the demos

Solve a DIMACS file.

```sh
dune exec clausecraft -- solve examples/benchmarks/tiny-sat.cnf
```

Encode and solve the Sudoku fixture.

```sh
dune exec clausecraft -- sudoku examples/sudoku.sdk
```

The Sudoku command prints the solved grid and solver statistics.

## Sample output

The small SAT fixture produces this stable result shape:

```text
variables: 3
clauses: 2
result: SAT
model: 1 2 3 0
decisions: 2
propagations: 1
conflicts: 0
solve_time_ms: <machine-dependent>
```

The time value changes with the local machine.

The Sudoku command prints nine solved rows after its result line.

## Tests and validation

Each SAT test uses a separate clause evaluator.

Tests cover parser errors, SAT, UNSAT, bundled files, Sudoku, and model validity.

CI runs the build, test suite, and both demos on OCaml 5.2.

Local status: this workspace lacks `ocamlc`, `dune`, and `opam`.

No local test, typecheck, or build result is claimed.

## Limitations

This first release uses full clause scans during propagation.

It does not yet implement watched literals, conflict learning, or restart scheduling.

The solver targets small and educational benchmark files.

The Sudoku encoder supports standard 9 by 9 puzzles only.

## Roadmap

1. Add watched-literal propagation as an independent release.
2. Add conflict analysis and learned clauses as an independent release.
3. Add restart scheduling and a larger benchmark set as an independent release.

## License

Clausecraft uses the MIT License.