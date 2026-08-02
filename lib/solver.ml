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
  let rec search assignment =
    if not (propagate cnf assignment propagations) then (
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
            search branch
          in
          match try_value 1 with
          | Some model -> Some model
          | None -> try_value (-1)
  in
  let initial = Array.make (cnf.Cnf.variable_count + 1) 0 in
  match search initial with
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