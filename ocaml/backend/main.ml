open Asml
open Asm

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
  
  let chann = open_in "test.asml" in
  (* empty program with empty main body *)
  let empty_prog = Program ( [], [], Ans Unit ) in
  (* Parsing the test ASML file chann *)
  let fundef_test = Parser.fundef Lexer.token (Lexing.from_channel chann) in
  (* converting a fundef to a program *)
  let pg_test = fd_to_prog fundef_test empty_prog in
  (* associating each variable in main body of program with an offset from the
   * frame pointer *)
  let var_reg = Register.program_to_reg pg_test [] in
  (* replace each variable by its fp offset in program *)
  let modified_pg_test = (Register.modify_program pg_test var_reg) in
  (* let pg_from_fd_test = prog_to_fd pg_test in *)
  List.iter (fun (s1, s2) -> Printf.printf "(%s, %d) " s1 s2) var_reg;
  print_newline ();
  print_string (Asml.to_string_f (prog_to_fd modified_pg_test))
;
print_newline ();

print_string (prog_to_asm modified_pg_test var_reg);

let armfile = open_out "../../ARM/test.s" in
output_string armfile (prog_to_asm modified_pg_test var_reg);
close_out armfile;
