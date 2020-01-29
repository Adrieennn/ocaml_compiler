(** This module is in charge of transforming an [Asml.prog] into a running ARM
machine code. The only public function is [prog_to_asm] but other functions help
it translate it correctly. *)

val prog_to_asm : Asml.prog -> string
(** prog_to_asm [prog]: main function of the module that returns a string equal
to the ARMv7 version of the program. *)
