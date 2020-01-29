(**
  This module is for [k-normalisation] which turns the result of 
  expressions computations into variables and make the code more 
  assembly like.
*)
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
  | LetRec of fundef * t
  | Var of Id.t
  | IfEq of (Id.t * Id.t) * t * t
  (* cf. BLE branch if less than or equal *)
  | IfLe of (Id.t * Id.t) * t * t
  | App of Id.t * Id.t list
  | Tuple of Id.t list
  | LetTuple of (Id.t * Type.t) list * t * t
  | Array of Id.t * Id.t
  | Get of Id.t * Id.t
  | Put of Id.t * Id.t * Id.t

and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }

val of_syntax : Syntax.t -> t
(** of_syntax is the main function responsible for k-normalisation, it takes an
 expression of type Syntax.t and return a normalised expression of type Knorm.t *)

val to_string : t -> string
