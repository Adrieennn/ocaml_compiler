let print_ast l =
  print_string (Syntax.to_string (Parser.exp Lexer.token l)); print_newline ()

let file f = 
  let inchan = open_in f in
  try
    print_ast (Lexing.from_channel inchan);
    close_in inchan
  with e -> (close_in inchan; raise e)

let print_untyped_and_typed_ast f =
  let inchan = open_in f in
  try
    let ast = Lexing.from_channel inchan
              |> Parser.exp Lexer.token in

    print_string (Syntax.to_string ast);
    print_newline ();
    print_newline ();
    print_newline ();

    let typed_ast = Typing.typed_ast ast in
    print_string (Syntax.to_string typed_ast);
    print_newline ();

    close_in inchan
  with e -> (close_in inchan; raise e)

let () = 
  let files = ref [] in
  Arg.parse
    [ ]
    (fun s -> files := !files @ [s])
    (Printf.sprintf "usage: %s filenames" Sys.argv.(0));
  List.iter
    (fun f -> ignore (print_untyped_and_typed_ast f))
    !files
