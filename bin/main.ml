open Clausecraft

let usage () =
  Printf.eprintf
    "Usage:\n  clausecraft solve FILE.cnf\n  clausecraft sudoku FILE.sdk\n";
  exit 2

let format_model assignment =
  Solver.assignment_to_literals assignment
  |> List.map string_of_int
  |> String.concat " "

let print_statistics stats elapsed =
  Printf.printf "decisions: %d\n" stats.Solver.decisions;
  Printf.printf "propagations: %d\n" stats.Solver.propagations;
  Printf.printf "conflicts: %d\n" stats.Solver.conflicts;
  Printf.printf "solve_time_ms: %.3f\n" (elapsed *. 1000.)

let solve_file path =
  let cnf = Cnf.parse_file path in
  let started = Unix.gettimeofday () in
  let result = Solver.solve cnf in
  let elapsed = Unix.gettimeofday () -. started in
  Printf.printf "variables: %d\n" (Cnf.variable_count cnf);
  Printf.printf "clauses: %d\n" (Cnf.clause_count cnf);
  match result with
  | Solver.Satisfiable (assignment, stats) ->
      print_endline "result: SAT";
      Printf.printf "model: %s 0\n" (format_model assignment);
      print_statistics stats elapsed
  | Solver.Unsatisfiable stats ->
      print_endline "result: UNSAT";
      print_statistics stats elapsed

let sudoku_file path =
  let puzzle = Sudoku.parse_file path in
  let cnf = Sudoku.encode puzzle in
  let started = Unix.gettimeofday () in
  let result = Solver.solve cnf in
  let elapsed = Unix.gettimeofday () -. started in
  match result with
  | Solver.Satisfiable (assignment, stats) ->
      print_endline "result: SAT";
      Printf.printf "cnf_variables: %d\n" (Cnf.variable_count cnf);
      Printf.printf "cnf_clauses: %d\n" (Cnf.clause_count cnf);
      print_endline (Sudoku.to_string (Sudoku.decode assignment));
      print_statistics stats elapsed
  | Solver.Unsatisfiable stats ->
      print_endline "result: UNSAT";
      Printf.printf "cnf_variables: %d\n" (Cnf.variable_count cnf);
      Printf.printf "cnf_clauses: %d\n" (Cnf.clause_count cnf);
      print_statistics stats elapsed

let () =
  match Array.to_list Sys.argv with
  | [ _; "solve"; path ] ->
      (try solve_file path with
      | Cnf.Parse_error message ->
          Printf.eprintf "parse error: %s\n" message;
          exit 1
      | Sys_error message ->
          Printf.eprintf "file error: %s\n" message;
          exit 1)
  | [ _; "sudoku"; path ] ->
      (try sudoku_file path with
      | Sudoku.Invalid_puzzle message ->
          Printf.eprintf "Sudoku error: %s\n" message;
          exit 1
      | Sys_error message ->
          Printf.eprintf "file error: %s\n" message;
          exit 1)
  | _ -> usage ()