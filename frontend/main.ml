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

  let ast_closure = Closure.con ast_nested_let in
  Printf.printf "AST after closure conversion:\n";
  Closure.to_string ast_closure |> print_string;
  print_newline ();
  print_newline ()

let () =
  let files = ref [] in
  Arg.parse []
    (fun s -> files := !files @ [ s ])
    (Printf.sprintf "usage: %s filenames" Sys.argv.(0));
  List.iter (fun f -> print_transformations f) !files
