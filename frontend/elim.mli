(**
	This module is reponsable for elimination of unnecessary definitions in the 
	to be compiled code. 
*)

(** elimination takes an expression as a parameter and returns a expression 
of Knorm.t *)
val elimination : Knorm.t -> Knorm.t
