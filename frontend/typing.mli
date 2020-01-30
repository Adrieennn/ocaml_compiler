(** Type inference and subsequent type checking. *)

val typed_ast : Syntax.t -> Syntax.t
(** [typed_ast] applies type inference and unification and returns a Syntax.t
 * where type variables have been instantiated by their inferred types.
 * Warning: During this process the input AST is mutated! *)

val default_to_int : bool ref
(** Specifies whether types that could not be inferred should be defaulted to
 * Type.Int. Otherwise such uninstantiated variables will cause an error. *)
