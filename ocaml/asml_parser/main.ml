open Asml

let print_ast l =
  print_string (Asml.to_string_f (Parser.fundef Lexer.token l)); print_newline ()

let file f = 
  let inchan = open_in f in
  try
    print_ast (Lexing.from_channel inchan);
    close_in inchan
  with e -> (close_in inchan; raise e)

let () = 
  let files = ref [] in
  Arg.parse
    [ ]
    (fun s -> files := !files @ [s])
    (Printf.sprintf "usage: %s filenames" Sys.argv.(0));
  List.iter
    (fun f -> ignore (file f))
    !files;;
  (* let program =
    Program
      ( [],
        [],
        Let
          ( ("x", Type.gentyp ()),
            Int 0,
            Let
              ( ("y", Type.gentyp ()),
                Int 1,
                Let (("z", Type.gentyp ()), Add ("x", Var "y"), Ans Unit) ) ) )
  in *)
  let empty_prog = Program ( [], [], Ans Unit ) in
  let chann = open_in "test.asml" in
  let fundef_test = Parser.fundef Lexer.token (Lexing.from_channel chann) in
  let pg_test = fd_to_prog fundef_test empty_prog in
  let var_reg = Asml.program_to_reg pg_test [] in
  let pg_from_fd_test = prog_to_fd pg_test in
  List.iter (fun (s1, s2) -> Printf.printf "(%s, %s) " s1 s2) var_reg;
  print_newline ();
  print_string (Asml.to_string_f pg_from_fd_test)
;;
print_newline ()
