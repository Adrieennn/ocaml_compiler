type t =
  | Unit
  | Int of int
  | Float of float
  | Add of Id.t * Id.t
  | Sub of Id.t * Id.t
  | FAdd of Id.t * Id.t
  | FSub of Id.t * Id.t
  | FMul of Id.t * Id.t
  | FDiv of Id.t * Id.t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | IfEq of (Id.t * Id.t) * t * t
  | IfLe of (Id.t * Id.t) * t * t
  | Tuple of Id.t list
  | LetTuple of (Id.t * Type.t) list * t * t
  | Array of Id.t * Id.t
  | Get of Id.t * Id.t
  | Put of Id.t * Id.t * Id.t
  | MkCls of (Id.t * Type.t) * (Id.l * (Id.t * Type.t) list) * t
  | AppCls of Id.t * Id.t list
  | AppDir of Id.l * Id.t list

type fundef = {
  name : Id.l * Type.t;
  args : (Id.t * Type.t) list;
  formal_fv : (Id.t * Type.t) list;
  body : t;
}

type prog = Prog of fundef list * t

val prog_of_knorm : Knorm.t -> prog

val to_string' : t -> string
