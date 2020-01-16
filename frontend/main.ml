let read_ast_from_file f =
  let inchan = open_in f in
  try Lexing.from_channel inchan |> Parser.exp Lexer.token
  with e ->
    close_in inchan;
    raise e

let print_transformations f =
  let ast_syntax = read_ast_from_file f in
  Printf.printf "AST read from file %s: \n" f;
  Syntax.to_string ast_syntax |> print_string;
  print_newline ();
  print_newline ();

  let ast_knorm = Knorm.of_syntax ast_syntax in
  Printf.printf "AST after k-normalization:\n";
  Knorm.to_string ast_knorm |> print_string;
  print_newline ();
  print_newline ();

  let ast_alpha = Alpha.convert ast_knorm [] in
  Printf.printf "AST after alpha-conversion:\n";
  Knorm.to_string ast_alpha |> print_string;
  print_newline ();
  print_newline ();

  let ast_nested_let = NestedLetReduction.reduction ast_alpha in
  Printf.printf "AST after let-reduction:\n";
  Knorm.to_string ast_nested_let |> print_string;
  print_newline ();
  print_newline ();

  let closure_prog = Closure.prog_of_knorm ast_nested_let in
  Printf.printf "AST after closure conversion:\n";
  Closure.prog_to_string closure_prog |> print_string;
  print_newline ();
  print_newline ();

  let asml_prog = Asml.of_closure_prog closure_prog in
  let var_reg = Register.program_to_reg asml_prog [] in
  let modified_pg_test = Register.modify_program asml_prog var_reg in
  print_string (Asml.to_string_f (Asml.prog_to_fd modified_pg_test));
  print_newline ()

let print_ast l =
  print_string (Asml.to_string_f (AsmlParser.fundef AsmlLexer.token l));
  print_newline ()


let file f =
  let inchan = open_in f in
  try
    print_ast (Lexing.from_channel inchan);
    close_in inchan
  with e ->
    close_in inchan
    raise e;
  (*me*)

let () =
  let files = ref [] in
  Arg.parse []
    (fun s -> files := !files @ [ s ])
    (Printf.sprintf "usage: %s filenames" Sys.argv.(0))
  List.iter (fun f -> ignore (print_transformations f)) !files
