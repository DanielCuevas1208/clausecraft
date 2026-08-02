type literal = int
type clause = literal list

type t = {
  variable_count : int;
  clauses : clause list;
}

exception Parse_error of string

let words line =
  let is_space = function
    | ' ' | '\t' | '\r' -> true
    | _ -> false
  in
  let length = String.length line in
  let rec skip index =
    if index < length && is_space line.[index] then skip (index + 1) else index
  in
  let rec take index =
    if index < length && not (is_space line.[index]) then take (index + 1)
    else index
  in
  let rec collect index acc =
    let start = skip index in
    if start = length then List.rev acc
    else
      let finish = take start in
      collect finish (String.sub line start (finish - start) :: acc)
  in
  collect 0 []

let parse_integer ~line_number token =
  try int_of_string token with
  | Failure _ ->
      raise (Parse_error (Printf.sprintf "line %d: expected an integer, got %S" line_number token))

let parse_header ~line_number tokens =
  match tokens with
  | [ "p"; "cnf"; variables; clauses ] ->
      let variable_count = parse_integer ~line_number variables in
      let clause_count = parse_integer ~line_number clauses in
      if variable_count < 0 then
        raise (Parse_error (Printf.sprintf "line %d: variable count cannot be negative" line_number));
      if clause_count < 0 then
        raise (Parse_error (Printf.sprintf "line %d: clause count cannot be negative" line_number));
      variable_count, clause_count
  | _ ->
      raise
        (Parse_error
           (Printf.sprintf
              "line %d: expected a DIMACS header in the form 'p cnf <variables> <clauses>'"
              line_number))

let parse_lines lines =
  let header = ref None in
  let current_clause = ref [] in
  let clauses = ref [] in
  let clause_count = ref 0 in
  let add_clause ~line_number =
    match !header with
    | None -> raise (Parse_error (Printf.sprintf "line %d: missing DIMACS header" line_number))
    | Some (_, expected_count) ->
        if !clause_count >= expected_count then
          raise (Parse_error (Printf.sprintf "line %d: found more clauses than the header declares" line_number));
        clauses := List.rev !current_clause :: !clauses;
        current_clause := [];
        incr clause_count
  in
  let process_literal ~line_number literal =
    match !header with
    | None -> raise (Parse_error (Printf.sprintf "line %d: missing DIMACS header" line_number))
    | Some (variable_count, _) ->
        if literal = 0 then add_clause ~line_number
        else if literal < -variable_count || literal > variable_count then
          raise
            (Parse_error
               (Printf.sprintf
                  "line %d: literal %d exceeds the declared variable range 1..%d"
                  line_number literal variable_count))
        else current_clause := literal :: !current_clause
  in
  let process_line line_number line =
    let trimmed = String.trim line in
    if trimmed = "" || trimmed.[0] = 'c' then ()
    else
      match words line with
      | [] -> ()
      | first :: _ when first = "p" ->
          if !header <> None then
            raise (Parse_error (Printf.sprintf "line %d: duplicate DIMACS header" line_number));
          header := Some (parse_header ~line_number (words line))
      | tokens ->
          if !header = None then
            raise (Parse_error (Printf.sprintf "line %d: DIMACS data appeared before the header" line_number));
          List.iter (fun token -> process_literal ~line_number (parse_integer ~line_number token)) tokens
  in
  List.iteri (fun index line -> process_line (index + 1) line) lines;
  match !header with
  | None -> raise (Parse_error "missing DIMACS header")
  | Some (variable_count, expected_count) ->
      if !current_clause <> [] then raise (Parse_error "DIMACS ended before a clause terminator 0");
      if !clause_count <> expected_count then
        raise
          (Parse_error
             (Printf.sprintf
                "header declares %d clauses, but the file contains %d" expected_count !clause_count));
      { variable_count; clauses = List.rev !clauses }

let parse_string input = parse_lines (String.split_on_char '\n' input)

let parse_file path =
  let channel = open_in path in
  let rec read_lines acc =
    try read_lines (input_line channel :: acc) with
    | End_of_file -> List.rev acc
  in
  try
    let lines = read_lines [] in
    close_in channel;
    parse_lines lines
  with
  | exn ->
      close_in_noerr channel;
      raise exn

let variable_count cnf = cnf.variable_count
let clauses cnf = cnf.clauses
let clause_count cnf = List.length cnf.clauses