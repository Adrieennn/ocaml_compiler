(**
This module is for constant folding whitch replaces variables with their 
constant values if possible.
*)

val folding : Knorm.t -> (Id.t * Knorm.t) list -> Knorm.t
(**Constant folding on Knorm.t. It takes an expression and mapping, and returns a
  constant folded expression *)