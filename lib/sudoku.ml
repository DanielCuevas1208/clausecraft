type puzzle = int array array

exception Invalid_puzzle of string

let size = 9

let validate puzzle =
  if Array.length puzzle <> size then raise (Invalid_puzzle "a Sudoku puzzle needs nine rows");
  Array.iter
    (fun row ->
      if Array.length row <> size then raise (Invalid_puzzle "each Sudoku row needs nine cells");
      Array.iter
        (fun value ->
          if value < 0 || value > size then raise (Invalid_puzzle "Sudoku values must range from 0 to 9"))
        row)
    puzzle

let parse_string input =
  let parse_row line =
    let values = ref [] in
    String.iter
      (fun character ->
        if character = '.' then values := 0 :: !values
        else if character >= '0' && character <= '9' then
          values := (Char.code character - Char.code '0') :: !values
        else if character = ' ' || character = '\t' || character = '\r' then ()
        else raise (Invalid_puzzle "Sudoku rows accept digits, dots, and spaces"))
      line;
    let row = Array.of_list (List.rev !values) in
    if Array.length row <> size then raise (Invalid_puzzle "each Sudoku row needs nine cells");
    row
  in
  let lines =
    String.split_on_char '\n' input
    |> List.filter (fun line -> String.trim line <> "")
  in
  if List.length lines <> size then raise (Invalid_puzzle "a Sudoku puzzle needs nine non-empty rows");
  let puzzle = Array.of_list (List.map parse_row lines) in
  validate puzzle;
  puzzle

let parse_file path =
  let channel = open_in path in
  let rec read_lines acc =
    try read_lines (input_line channel :: acc) with
    | End_of_file -> List.rev acc
  in
  try
    let lines = read_lines [] in
    close_in channel;
    parse_string (String.concat "\n" lines)
  with
  | exn ->
      close_in_noerr channel;
      raise exn

let variable row column digit =
  if row < 0 || row >= size || column < 0 || column >= size || digit < 1 || digit > size then
    invalid_arg "Sudoku variable is out of range";
  (row * size + column) * size + digit

let add_exactly_one clauses variables =
  clauses := variables :: !clauses;
  let rec add_pairs = function
    | [] -> ()
    | first :: rest ->
        List.iter (fun other -> clauses := [ -first; -other ] :: !clauses) rest;
        add_pairs rest
  in
  add_pairs variables

let encode puzzle =
  validate puzzle;
  let clauses = ref [] in
  for row = 0 to size - 1 do
    for column = 0 to size - 1 do
      let variables = List.init size (fun offset -> variable row column (offset + 1)) in
      add_exactly_one clauses variables
    done
  done;
  for row = 0 to size - 1 do
    for digit = 1 to size do
      let variables = List.init size (fun column -> variable row column digit) in
      add_exactly_one clauses variables
    done
  done;
  for column = 0 to size - 1 do
    for digit = 1 to size do
      let variables = List.init size (fun row -> variable row column digit) in
      add_exactly_one clauses variables
    done
  done;
  for box_row = 0 to 2 do
    for box_column = 0 to 2 do
      for digit = 1 to size do
        let variables =
          List.init size (fun offset ->
              let row = (box_row * 3) + (offset / 3) in
              let column = (box_column * 3) + (offset mod 3) in
              variable row column digit)
        in
        add_exactly_one clauses variables
      done
    done
  done;
  for row = 0 to size - 1 do
    for column = 0 to size - 1 do
      let value = puzzle.(row).(column) in
      if value <> 0 then clauses := [ variable row column value ] :: !clauses
    done
  done;
  { Cnf.variable_count = size * size * size; clauses = List.rev !clauses }

let decode assignment =
  let puzzle = Array.init size (fun _ -> Array.make size 0) in
  for row = 0 to size - 1 do
    for column = 0 to size - 1 do
      let digit = ref 1 in
      while !digit <= size && Solver.value assignment (variable row column !digit) <> 1 do
        incr digit
      done;
      if !digit > size then raise (Invalid_puzzle "the model does not contain a complete Sudoku");
      puzzle.(row).(column) <- !digit
    done
  done;
  puzzle

let is_valid_solution puzzle =
  validate puzzle;
  let valid_group values =
    let seen = Array.make (size + 1) false in
    Array.for_all
      (fun value ->
        if value = 0 || seen.(value) then false
        else (
          seen.(value) <- true;
          true))
      values
  in
  let valid_rows = Array.for_all (fun row -> valid_group row) puzzle in
  let valid_columns =
    let valid = ref true in
    for column = 0 to size - 1 do
      let values = Array.init size (fun row -> puzzle.(row).(column)) in
      if not (valid_group values) then valid := false
    done;
    !valid
  in
  let valid_boxes =
    let valid = ref true in
    for box_row = 0 to 2 do
      for box_column = 0 to 2 do
        let values =
          Array.init size (fun offset ->
              puzzle.((box_row * 3) + (offset / 3)).((box_column * 3) + (offset mod 3)))
        in
        if not (valid_group values) then valid := false
      done
    done;
    !valid
  in
  valid_rows && valid_columns && valid_boxes

let solve puzzle = Solver.solve (encode puzzle)

let to_string puzzle =
  validate puzzle;
  Array.to_list puzzle
  |> List.map (fun row -> Array.to_list row |> List.map string_of_int |> String.concat "")
  |> String.concat "\n"