# Clausecraft

Clausecraft is an OCaml SAT solver for standard DIMACS CNF files.

It also encodes and solves standard 9 by 9 Sudoku puzzles.

## Value

Clausecraft provides a small, readable CDCL-ready solver foundation.

The project demonstrates parsing, watched-literal propagation, recursive search, model checking, and Sudoku encoding.

## Architecture

- `lib/cnf.ml` parses DIMACS headers, comments, literals, and clause terminators.
- `lib/solver.ml` initializes two watches per clause for each solve call.
- The solver queues newly assigned literals and inspects only clauses watching falsified literals.
- The solver moves watches when possible and enqueues unit literals otherwise.
- `lib/sudoku.ml` encodes and decodes standard 9 by 9 Sudoku puzzles.
- `bin/main.ml` provides the `solve` and `sudoku` commands.
- `test/test_clausecraft.ml` checks models with an independent clause evaluator.

The library keeps parsing, solving, and Sudoku concerns separate.

## Setup

Use OCaml 4.14 or newer, Dune 3.14 or newer, and Opam.

Install the locked dependencies.

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

The SAT command prints this result shape.

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

The time value depends on the local machine.

The Sudoku command prints nine solved rows after its result line.

## Tests and validation

Tests cover parser errors, SAT, UNSAT, empty clauses, watched-literal chains, bundled files, Sudoku, and model validity.

Each SAT test uses a separate clause evaluator.

CI runs the build, test suite, and both demos on OCaml 5.2.

Local validation is limited because this workspace lacks `ocamlc`, `dune`, and `opam`.

The available local check was `git diff --check`.

## Limitations

The solver does not yet analyze conflicts or retain learned clauses.

It does not yet schedule restarts.

Variable selection still scans clauses, although propagation uses watched literals.

The solver targets small and educational benchmark files.

The Sudoku encoder supports standard 9 by 9 puzzles only.

## Roadmap

1. Complete watched-literal propagation. **Complete in this release.**
2. Add conflict analysis and learned clauses.
3. Add restart scheduling and a larger benchmark set.

## License

Clausecraft uses the MIT License.
