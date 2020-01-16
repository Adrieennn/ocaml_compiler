let output_file = ref "out.s"

let disp_help = ref false

let disp_version = ref false

let typecheck_only = ref false

let disp_asml = ref false

let speclist =
  [
    ("-o", Arg.Set_string output_file, "Outputs to file <file>");
    ("-h", Arg.Set disp_help, "Display help");
    ("-v", Arg.Set disp_version, "Display compiler's version");
    ("-t", Arg.Set typecheck_only, "Only do typechecking");
    ("-asml", Arg.Set disp_asml, "Print asml");
  ]

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
  Closure.to_string' ast_closure |> print_string;
  print_newline ();
  print_newline ()

let print_ast l =
  print_string (Asml.to_string_f (AsmlParser.fundef AsmlLexer.token l));
  print_newline ()

let chann_to_asm chann =
  (* empty program with empty main body *)
  let empty_prog = Asml.Program ([], [], Ans Unit) in
  (* Parsing the test ASML file chann *)
  let fundef_test =
    AsmlParser.fundef AsmlLexer.token (Lexing.from_channel chann)
  in
  (* converting a fundef to a program *)
  let pg_test = Asml.fd_to_prog fundef_test empty_prog in
  (* associating each variable in main body of program with an offset from the
   * frame pointer *)
  let var_reg = Register.program_to_reg pg_test [] in
  (*
  List.iter (fun (s1, s2) -> Printf.printf "(%s, %d) " s1 s2) var_reg;
  print_newline ();
  *)
  (* replace each variable by its fp offset in program *)
  let modified_pg_test = Register.modify_program pg_test var_reg in

  (* let pg_from_fd_test = prog_to_fd pg_test in *)
  (*
  print_string (Asml.to_string_f (Asml.prog_to_fd modified_pg_test));
  print_newline ();
  *)
  print_string (Asm.prog_to_asm modified_pg_test);
  let armfile = open_out !output_file in
  output_string armfile (Asm.prog_to_asm modified_pg_test);
  close_out armfile

let file f =
  let inchan = open_in f in
  try
    chann_to_asm inchan;
    close_in inchan
  with e ->
    close_in inchan;
    raise e

let () =
  let usage_msg = Printf.sprintf "usage: %s filenames" Sys.argv.(0) in
  let files = ref [] in
  Arg.parse speclist (fun s -> files := !files @ [ s ]) usage_msg;
  print_string (string_of_bool !disp_version);
  if List.length !files = 0 then (
    Printf.printf "%s" usage_msg;
    exit 1 )
  else List.iter (fun f -> ignore (file f)) !files
