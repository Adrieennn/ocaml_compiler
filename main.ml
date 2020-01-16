let output_file = ref None

let set_output_file s = output_file := Some s

let disp_help = ref false

let disp_version () =
  let inchan = Unix.open_process_in "git rev-parse --short HEAD" in
  let version = input_line inchan in
  close_in inchan;
  Printf.printf "mincamlc compiler, version %s\n" version;
  exit 0

let typecheck_only = ref false

let disp_asml = ref false

let speclist =
  [
    ("-o", Arg.String set_output_file, "Outputs to file <file>");
    ( "-h",
      Arg.Unit
        (fun () ->
          Printf.printf "Please run --help instead.\n";
          exit 0),
      "Display help" );
    ("-v", Arg.Unit disp_version, "Display compiler's version");
    ("-t", Arg.Set typecheck_only, "Only do typechecking");
    ("-asml", Arg.Set disp_asml, "Print asml");
  ]

let read_ast_from_file f =
  let inchan = open_in f in
  try Lexing.from_channel inchan |> Parser.exp Lexer.token
  with e ->
    close_in inchan;
    raise e

(*
let print_transformations f =
  let ast_syntax = read_ast_from_file f in
  Printf.printf "AST read from file %s: \n" f;
  Syntax.to_string ast_syntax |> print_string;
  print_newline ();
  print_newline ();

  let type_ast = Typing.typed_ast ast_syntax in

  let ast_knorm = Knorm.of_syntax type_ast in
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

  let ast_closure = Closure.prog_of_knorm ast_nested_let in
  Printf.printf "AST after closure conversion:\n";
  Closure.prog_to_string ast_closure |> print_string;
  print_newline ();
  print_newline ();

  let asml_prog = Asml.of_closure_prog ast_closure in
  let fd_test = Asml.prog_to_fd asml_prog in
  print_string (Asml.to_string_f fd_test);
  print_newline ();

  let var_reg = Register.program_to_reg asml_prog [] in
  let modified_prog = Register.modify_program asml_prog var_reg in
  let armfile = open_out !output_file in
  output_string armfile (Asm.prog_to_asm modified_prog);
  close_out armfile;

  List.iter (fun (s1, s2) -> Printf.printf "(%s, %d) " s1 s2) var_reg;
  print_newline ()
*)

let print_ast l =
  print_newline ();
  print_newline ();
  print_string (Asml.to_string_f (AsmlParser.fundef AsmlLexer.token l));
  print_newline ();
  print_newline ()

let usage_msg = Printf.sprintf "usage: %s filenames\n" Sys.argv.(0)

let print_usage_and_exit () =
  Printf.eprintf "%s" usage_msg;
  exit 1

let () =
  let files = ref [] in
  Arg.parse speclist (fun s -> files := !files @ [ s ]) usage_msg;

  let () =
    match (!output_file, List.length !files) with
    | _, 0 -> print_usage_and_exit ()
    | None, _ -> ()
    | Some _, 1 -> ()
    | Some _, _ ->
        Printf.eprintf "Cannot use -o with more than one input file.\n";
        ignore (exit 1)
  in
  (* file_name * Syntax.t *)
  let asts = List.map (fun f -> (f, read_ast_from_file f)) !files in
  if !typecheck_only then
    List.iter
      (fun (_, ast) ->
        Typing.typed_ast ast |> Syntax.to_string |> print_string;
        print_newline ())
      asts
  else
    List.iter
      (fun (file_name, ast) ->
        let ast_knorm = Knorm.of_syntax ast in
        let ast_alpha = Alpha.convert ast_knorm [] in
        let ast_reduced_nested_lets = NestedLetReduction.reduction ast_alpha in
        let ast_closure_conversion =
          Closure.prog_of_knorm ast_reduced_nested_lets
        in
        let asml_prog = Asml.of_closure_prog ast_closure_conversion in
        ( if !disp_asml then
          let asml_fundef = Asml.prog_to_fd asml_prog in
          Printf.printf "%s\n" (Asml.to_string_f asml_fundef) );
        let var_reg = Register.program_to_reg asml_prog [] in
        let modified_prog = Register.modify_program asml_prog var_reg in
        let output_file_name =
          match !output_file with
          | None ->
              let file_name =
                Filename.remove_extension (Filename.basename file_name) ^ ".s"
              in
              Filename.quote file_name
          | Some s ->
              (* Only valid if there is only one input file
               * which should be checked before *)
              Filename.quote s
        in
        Printf.printf "output: %s\n" output_file_name;
        let output_file = open_out output_file_name in
        output_string output_file (Asm.prog_to_asm modified_prog);
        close_out output_file)
      asts
