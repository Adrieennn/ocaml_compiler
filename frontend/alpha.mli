val convert : Knorm.t -> (Id.t * Id.t) list -> Knorm.t
(** convert is the main conversion function. It takes 2 variable: the
expression needs to be converted and the mapping environment. The goal
is to make every variable name unique. *)
