open Clausecraft

let project_root =
  match Sys.getenv_opt "DUNE_SOURCEROOT" with
  | Some root -> root
  | None -> Sys.getcwd ()

let fixture path = Filename.concat project_root path
let check_independent_model cnf model =
  let contains literal = List.exists (fun candidate -> candidate = literal) model in
  let clause_satisfied clause = List.exists contains clause in
  List.length model = Cnf.variable_count cnf
  && List.for_all clause_satisfied (Cnf.clauses cnf)

let test_dimacs_parser () =
  let cnf =
    Cnf.parse_string
      "c comment\np cnf 3 2\n1 -2\n0\n2 3 0\n"
  in
  Alcotest.(check int) "variable count" 3 (Cnf.variable_count cnf);
  Alcotest.(check int) "clause count" 2 (Cnf.clause_count cnf);
  Alcotest.(check (list (list int))) "clauses" [ [ 1; -2 ]; [ 2; 3 ] ] (Cnf.clauses cnf)

let test_dimacs_errors () =
  let malformed () = Cnf.parse_string "p cnf 1 1\n1\n" in
  Alcotest.check_raises "unterminated clause"
    (Cnf.Parse_error "DIMACS ended before a clause terminator 0")
    malformed

let test_sat_model () =
  let cnf = Cnf.parse_string "p cnf 3 2\n1 -2 0\n2 3 0\n" in
  match Solver.solve cnf with
  | Solver.Unsatisfiable _ -> Alcotest.fail "expected a satisfying assignment"
  | Solver.Satisfiable (assignment, stats) ->
      let model = Solver.assignment_to_literals assignment in
      Alcotest.(check bool) "independent clause verification" true
        (check_independent_model cnf model);
      Alcotest.(check int) "all variables receive a value" 3 (List.length model);
      Alcotest.(check bool) "statistics are available" true (stats.Solver.propagations > 0)

let test_unsat_model () =
  let cnf = Cnf.parse_string "p cnf 1 2\n1 0\n-1 0\n" in
  match Solver.solve cnf with
  | Solver.Satisfiable _ -> Alcotest.fail "expected an unsatisfiable result"
  | Solver.Unsatisfiable stats ->
      Alcotest.(check bool) "conflict found" true (stats.Solver.conflicts > 0)

let test_bundled_benchmarks () =
  let sat_path = fixture "examples/benchmarks/tiny-sat.cnf" in
  let unsat_path = fixture "examples/benchmarks/tiny-unsat.cnf" in
  let paths = [ sat_path; unsat_path ] in
  List.iter
    (fun path ->
      let cnf = Cnf.parse_file path in
      match Solver.solve cnf with
      | Solver.Unsatisfiable _ when path = sat_path ->
          Alcotest.fail "tiny-sat.cnf should be satisfiable"
      | Solver.Satisfiable (_, _) when path = unsat_path ->
          Alcotest.fail "tiny-unsat.cnf should be unsatisfiable"
      | Solver.Satisfiable (assignment, _) ->
          let model = Solver.assignment_to_literals assignment in
          Alcotest.(check bool) "benchmark model verification" true
            (check_independent_model cnf model)
      | Solver.Unsatisfiable _ -> ())
    paths

let test_sudoku_encoding_and_solution () =
  let puzzle =
    Sudoku.parse_string
      "530070000\n600195000\n098000060\n800060003\n400803001\n700020006\n060000280\n000419005\n000080079\n"
  in
  let cnf = Sudoku.encode puzzle in
  Alcotest.(check int) "Sudoku variable count" 729 (Cnf.variable_count cnf);
  Alcotest.(check bool) "Sudoku has structural clauses" true
    (Cnf.clause_count cnf > 10000);
  match Sudoku.solve puzzle with
  | Solver.Unsatisfiable _ -> Alcotest.fail "the bundled Sudoku should be satisfiable"
  | Solver.Satisfiable (assignment, _) ->
      let solution = Sudoku.decode assignment in
      Alcotest.(check bool) "Sudoku solution validity" true
        (Sudoku.is_valid_solution solution);
      Alcotest.(check string) "Sudoku solution"
        "534678912\n672195348\n198342567\n859761423\n426853791\n713924856\n961537284\n287419635\n345286179"
        (Sudoku.to_string solution);
      Alcotest.(check bool) "CNF model verification" true
        (Solver.satisfies cnf assignment)

let () =
  Alcotest.run "clausecraft"
    [
      ( "CNF",
        [
          Alcotest.test_case "parses DIMACS" `Quick test_dimacs_parser;
          Alcotest.test_case "rejects malformed input" `Quick test_dimacs_errors;
          Alcotest.test_case "verifies a SAT model independently" `Quick test_sat_model;
          Alcotest.test_case "detects UNSAT" `Quick test_unsat_model;
          Alcotest.test_case "solves bundled benchmarks" `Quick test_bundled_benchmarks;
        ] );
      ("Sudoku", [ Alcotest.test_case "encodes and solves a puzzle" `Quick test_sudoku_encoding_and_solution ]);
    ]