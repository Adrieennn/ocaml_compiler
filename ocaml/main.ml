let print_ast l =
  print_string (Syntax.to_string ((*Knorm.of_syntax*) Parser.exp Lexer.token l));
  print_newline ()

let file f =
  let inchan = open_in f in
  try
    print_ast (Lexing.from_channel inchan);
    close_in inchan
  with e ->
    close_in inchan;
    raise e

let print_knorm f =  (*print_reducedLet*)
  let inchan = open_in f in
  try
    let knorm_ast (*reducedLet_ast*)=
      Lexing.from_channel inchan
      |> Parser.exp Lexer.token
      |> Knorm.of_syntax
      (*|> NestedLetReduction.red*)
    in
    print_string (Knorm.to_string knorm_ast); (*(NestedLetReduction.to_string reducedLet_ast);*)
    print_newline ();
    print_newline ();

    close_in inchan
  with e ->
    close_in inchan;
    raise e

let () =
  let files = ref [] in
  Arg.parse []
    (fun s -> files := !files @ [ s ])
    (Printf.sprintf "usage: %s filenames" Sys.argv.(0));
  List.iter (fun f -> ignore (print_knorm f)) !files  (*print_reducedLet*)
