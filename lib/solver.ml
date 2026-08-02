type assignment = int array

type statistics = {
  decisions : int;
  propagations : int;
  conflicts : int;
}

type result =
  | Satisfiable of assignment * statistics
  | Unsatisfiable of statistics

type clause_state =
  | Satisfied
  | Unit of int
  | Open
  | Conflict

let literal_value assignment literal =
  let variable = abs literal in
  let value = assignment.(variable) in
  if value = 0 then None
  else Some (if literal > 0 then value = 1 else value = -1)

let inspect_clause assignment clause =
  let satisfied = ref false in
  let unassigned = ref [] in
  List.iter
    (fun literal ->
      match literal_value assignment literal with
      | Some true -> satisfied := true
      | Some false -> ()
      | None -> unassigned := literal :: !unassigned)
    clause;
  if !satisfied then Satisfied
  else
    match !unassigned with
    | [] -> Conflict
    | [ literal ] -> Unit literal
    | _ -> Open

(* Keep the original helper available for callers that use the inferred module interface.
   The solver below uses watched literals for its own propagation loop. *)
let propagate cnf assignment propagations =
  let rec pass () =
    let changed = ref false in
    let contradiction = ref false in
    List.iter
      (fun clause ->
        match inspect_clause assignment clause with
        | Satisfied | Open -> ()
        | Conflict -> contradiction := true
        | Unit literal ->
            let variable = abs literal in
            let required_value = if literal > 0 then 1 else -1 in
            if assignment.(variable) = 0 then (
              assignment.(variable) <- required_value;
              incr propagations;
              changed := true)
            else if assignment.(variable) <> required_value then contradiction := true)
      cnf.Cnf.clauses;
    if !contradiction then false else if !changed then pass () else true
  in
  pass ()

let choose_variable cnf assignment =
  let scores = Array.make (cnf.Cnf.variable_count + 1) 0 in
  List.iter
    (fun clause ->
      match inspect_clause assignment clause with
      | Satisfied -> ()
      | Unit _ | Open ->
          List.iter
            (fun literal ->
              let variable = abs literal in
              if assignment.(variable) = 0 then scores.(variable) <- scores.(variable) + 1)
            clause
      | Conflict -> ())
    cnf.Cnf.clauses;
  let selected = ref 0 in
  for variable = 1 to cnf.Cnf.variable_count do
    if scores.(variable) > scores.(!selected) then selected := variable
  done;
  if !selected = 0 then
    let rec first_unassigned variable =
      if variable > cnf.Cnf.variable_count then None
      else if assignment.(variable) = 0 then Some variable
      else first_unassigned (variable + 1)
    in
    first_unassigned 1
  else Some !selected

let solve cnf =
  let decisions = ref 0 in
  let propagations = ref 0 in
  let conflicts = ref 0 in
  let statistics () = { decisions = !decisions; propagations = !propagations; conflicts = !conflicts } in
  let clauses = Array.of_list (List.map Array.of_list cnf.Cnf.clauses) in
  let watch_a = Array.make (Array.length clauses) 0 in
  let watch_b = Array.make (Array.length clauses) 0 in
  let watch_lists = Array.make (2 * cnf.Cnf.variable_count + 2) [] in
  let initial_units = ref [] in
  let has_empty_clause = ref false in
  let literal_index literal =
    let variable = abs literal in
    (2 * variable) + if literal < 0 then 1 else 0
  in
  let add_watch literal clause_index =
    let index = literal_index literal in
    watch_lists.(index) <- clause_index :: watch_lists.(index)
  in
  Array.iteri
    (fun clause_index clause ->
      match Array.length clause with
      | 0 -> has_empty_clause := true
      | 1 ->
          watch_a.(clause_index) <- 0;
          watch_b.(clause_index) <- 0;
          add_watch clause.(0) clause_index;
          initial_units := clause.(0) :: !initial_units
      | _ ->
          watch_a.(clause_index) <- 0;
          watch_b.(clause_index) <- 1;
          add_watch clause.(0) clause_index;
          add_watch clause.(1) clause_index)
    clauses;
  let literal_is_true assignment literal =
    let value = assignment.(abs literal) in
    (literal > 0 && value = 1) || (literal < 0 && value = -1)
  in
  let literal_is_false assignment literal =
    let value = assignment.(abs literal) in
    (literal > 0 && value = -1) || (literal < 0 && value = 1)
  in
  let propagate assignment initial_literals =
    let pending = Queue.create () in
    let contradiction = ref !has_empty_clause in
    let enqueue literal =
      let variable = abs literal in
      let required_value = if literal > 0 then 1 else -1 in
      match assignment.(variable) with
      | 0 ->
          assignment.(variable) <- required_value;
          incr propagations;
          Queue.add literal pending;
          true
      | value -> value = required_value
    in
    let enqueue_initial literal =
      let variable = abs literal in
      let required_value = if literal > 0 then 1 else -1 in
      match assignment.(variable) with
      | 0 ->
          assignment.(variable) <- required_value;
          incr propagations;
          Queue.add literal pending
      | value when value = required_value -> Queue.add literal pending
      | _ -> contradiction := true
    in
    List.iter enqueue_initial initial_literals;
    let process_false_literal false_literal =
      let list_index = literal_index false_literal in
      let rec process remaining kept =
        match remaining with
        | [] -> watch_lists.(list_index) <- List.rev kept
        | clause_index :: rest ->
            let clause = clauses.(clause_index) in
            let first = watch_a.(clause_index) in
            let second = watch_b.(clause_index) in
            let false_position =
              if clause.(first) = false_literal then Some first
              else if clause.(second) = false_literal then Some second
              else None
            in
            (match false_position with
            | None -> process rest kept
            | Some false_position ->
                let other_position = if false_position = first then second else first in
                let other_literal = clause.(other_position) in
                if literal_is_true assignment other_literal then process rest (clause_index :: kept)
                else
                  let rec find_replacement position =
                    if position >= Array.length clause then None
                    else if position = false_position || position = other_position then
                      find_replacement (position + 1)
                    else if literal_is_false assignment clause.(position) then
                      find_replacement (position + 1)
                    else Some position
                  in
                  (match find_replacement 0 with
                  | Some replacement ->
                      if first = false_position then watch_a.(clause_index) <- replacement
                      else watch_b.(clause_index) <- replacement;
                      add_watch clause.(replacement) clause_index;
                      process rest kept
                  | None ->
                      if literal_is_false assignment other_literal then contradiction := true
                      else if not (enqueue other_literal) then contradiction := true;
                      process rest (clause_index :: kept)))
      in
      process watch_lists.(list_index) []
    in
    let rec drain () =
      if !contradiction then false
      else if Queue.is_empty pending then true
      else (
        process_false_literal (- (Queue.take pending));
        drain ())
    in
    drain ()
  in
  let rec search assignment initial_literals =
    if not (propagate assignment initial_literals) then (
      incr conflicts;
      None)
    else
      match choose_variable cnf assignment with
      | None -> Some assignment
      | Some variable ->
          incr decisions;
          let try_value value =
            let branch = Array.copy assignment in
            branch.(variable) <- value;
            let literal = if value = 1 then variable else -variable in
            search branch [ literal ]
          in
          match try_value 1 with
          | Some model -> Some model
          | None -> try_value (-1)
  in
  let initial = Array.make (cnf.Cnf.variable_count + 1) 0 in
  match search initial !initial_units with
  | Some model -> Satisfiable (model, statistics ())
  | None -> Unsatisfiable (statistics ())

let assignment_to_literals assignment =
  let literals = ref [] in
  for variable = 1 to Array.length assignment - 1 do
    match assignment.(variable) with
    | 1 -> literals := variable :: !literals
    | -1 -> literals := -variable :: !literals
    | 0 -> ()
    | _ -> invalid_arg "assignment contains a value other than -1, 0, or 1"
  done;
  List.rev !literals

let value assignment variable =
  if variable <= 0 || variable >= Array.length assignment then invalid_arg "variable is out of range"
  else assignment.(variable)

let model result =
  match result with
  | Satisfiable (assignment, _) -> Some assignment
  | Unsatisfiable _ -> None

let statistics result =
  match result with
  | Satisfiable (_, stats) | Unsatisfiable stats -> stats

let satisfies cnf assignment =
  Array.length assignment = cnf.Cnf.variable_count + 1
  && List.for_all
       (fun clause ->
         List.exists
           (fun literal ->
             match literal_value assignment literal with
             | Some true -> true
             | Some false | None -> false)
           clause)
       cnf.Cnf.clauses
