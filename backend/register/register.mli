(** The Register module takes care of creating a register table and modifying 
the [Asml.prog] before turning it to machine code. *)

(** counttype helps us set up a counter for register allocation. *)
type counttype =
  | Decr  (** Decrement the counter *)
  | Peak  (** Peak at the counter's value *)

val program_to_reg : Asml.prog -> (string * int) list
(** takes a program and returns a variable list. *)

val modify_program : Asml.prog -> (string * int) list -> Asml.prog
(** takes a program and its variable list. It returns a modified program where
variable accesses have been modified with their variable list offset. *)
