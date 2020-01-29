(**
This moudle is for beta reduction which can reduce unecessary redrclarations.
*)

val convert : Knorm.t -> (Id.t * Id.t) list -> Knorm.t
(**
convert is the main conversion function. It takes 2 parameters: the 
expression needs to be converted and the mapping environment. The goal 
is to expand the aliasing of variables thus increase the efficiency of 
the compiler.*)
