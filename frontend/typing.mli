(** Type inference and subsequent type checking. *)

val typed_ast : Syntax.t -> Syntax.t
(** [typed_ast] applies type inference and unification and returns a Syntax.t
 * where type variables have been instantiated by their inferred types.
 * Warning: During this process the input AST is mutated! *)
