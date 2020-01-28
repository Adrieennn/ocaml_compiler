type counttype = Decr | Peak

val program_to_reg : Asml.prog -> (string * int) list

val modify_program : Asml.prog -> (string * int) list -> Asml.prog
