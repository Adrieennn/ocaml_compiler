val expansion :
  Knorm.t -> (Id.t * ((Id.t * Type.t) list * Knorm.t)) list -> int -> Knorm.t

(**
	expansion is the main function responsable for inline expansion, it takes
	a expression, a list that maps a function name with it's list of arguments 
	and function body, and a interger for the max size of function to be 
	expanded in inline expansion. [(Id.t * ((Id.t * Type.t) list * Knorm.t))] 
	correspondes to the sturcture of Knorm.fundef.
*)
