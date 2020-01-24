let output_file = ref None

let set_output_file s = output_file := Some s

let disp_help = ref false

let compile_from_asml = ref false

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
    ("-from-asml", Arg.Set compile_from_asml, "Compile from ASML input file");
  ]

let read_ast_from_file f =
  let inchan = open_in f in
  try Lexing.from_channel inchan |> Parser.exp Lexer.token
  with e ->
    close_in inchan;
    raise e

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

let asml_prog_of_asml_file f =
  let inchan = open_in f in
  let empty_prog = Asml.Program ([], [], Ans Unit) in
  let fundef = AsmlParser.fundef AsmlLexer.token (Lexing.from_channel inchan) in
  Asml.fd_to_prog fundef empty_prog

let asml_prog_of_ml_file f =
  let ast = read_ast_from_file f in
  let ast = Typing.typed_ast ast in
  let ast_knorm = Knorm.of_syntax ast in
  let ast_alpha = Alpha.convert ast_knorm [] in
  let ast_beta = Beta.convert ast_alpha [] in
  let ast_inline_expansion = Inline.expansion ast_beta [] in
  let ast_reduced_nested_lets = NestedLetReduction.reduction ast_inline_expansion in
  let ast_constant_folding = Constant.folding ast_reduced_nested_lets [] in
  (* let ast_elimination = Elim.elimination ast_constant_folding in *)
  let ast_closure_conversion = Closure.prog_of_knorm ast_constant_folding in
  Asml.of_closure_prog ast_closure_conversion

let asml_prog_to_arm prog output_file_name =
  ( if !disp_asml then
      let asml_fundef = Asml.prog_to_fd prog in
      Printf.printf "%s\n" (Asml.to_string_f asml_fundef) );
  let var_reg = Register.program_to_reg prog in
  let modified_prog = Register.modify_program prog var_reg in
  let outchan = open_out output_file_name in
  output_string outchan (Asm.prog_to_asm modified_prog);
  close_out outchan

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
  if !typecheck_only then (
    if !compile_from_asml then (
      Printf.eprintf "Cannot typecheck an ASML file.\n";
      exit 1 );

    List.iter
      (fun ast ->
         Typing.typed_ast ast |> Syntax.to_string |> print_string;
         print_newline ())
      (List.map read_ast_from_file !files);
    exit 0 )
  else
    let asml_progs =
      match !compile_from_asml with
      | true -> List.map (fun f -> (f, asml_prog_of_asml_file f)) !files
      | false -> List.map (fun f -> (f, asml_prog_of_ml_file f)) !files
    in
    List.iter
      (fun (file_name, prog) ->
         let output_file_name =
           match !output_file with
           | None ->
             Filename.remove_extension (Filename.basename file_name) ^ ".s"
           | Some s ->
             (* Only valid if there is only one input file
              * which should be checked before *)
             s
         in
         asml_prog_to_arm prog output_file_name)
      asml_progs

(*
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
        let var_reg = Register.program_to_reg asml_prog in
        let modified_prog = Register.modify_program asml_prog var_reg in
        let output_file_name =
          match !output_file with
          | None ->
              Filename.remove_extension (Filename.basename file_name) ^ ".s"
          | Some s ->
              (* Only valid if there is only one input file
               * which should be checked before *)
              s
        in
        let output_file = open_out output_file_name in
        output_string output_file (Asm.prog_to_asm modified_prog);
        close_out output_file)
      asts
      *)
