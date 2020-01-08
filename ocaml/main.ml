let print_ast l =
  print_string (Syntax.to_string (Parser.exp Lexer.token l)); print_newline ()

let file f = 
  let inchan = open_in f in
  try
    print_ast (Lexing.from_channel inchan);
    close_in inchan
  with e -> (close_in inchan; raise e)

let print_equation f =
  let inchan = open_in f in
  try
    let ast = Lexing.from_channel inchan
              |> Parser.exp Lexer.token in
    let equations = Typing.gen_equations (Typing.TypingEnvironment.default () ) ast Type.Unit in
    List.iter print_string (List.map Typing.TypingEquation.to_string equations);
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
    (fun f -> ignore (print_equation f))
    !files
