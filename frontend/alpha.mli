(**
This module is for alpha conversion, which makes variable names in
a program unique. 
*)

val convert : Knorm.t -> (Id.t * Id.t) list -> Knorm.t
(** convert is the main conversion function. It takes 2 variable: the
expression needs to be converted and the mapping environment. *)
