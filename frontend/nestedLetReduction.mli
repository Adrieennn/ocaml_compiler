(**
This module is for reduction of nested let expressions which flattens the nested let in 
a program and mekes it closer to asml.
*)

val reduction : Knorm.t -> Knorm.t
(**
	reduction takes an expression of [Knorm.t] and reduce the nested let in 
	this expression. The return type is also [Knorm.t].
*)