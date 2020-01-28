type id_or_imm = Var of Id.t | Int of int

type t = Ans of exp | Let of (Id.t * Type.t) * exp * t

and exp =
  | Var of Id.t
  | Label of Id.t
  | Int of int
  | Unit
  | Add of Id.t * id_or_imm
  | Sub of Id.t * id_or_imm
  | Ld of Id.t * id_or_imm
  | St of Id.t * id_or_imm * Id.t
  | New of int
  | Neg of Id.t
  | FNeg of Id.t
  | FAdd of Id.t * Id.t
  | FSub of Id.t * Id.t
  | FMul of Id.t * Id.t
  | FDiv of Id.t * Id.t
  | IfEq of Id.t * id_or_imm * t * t
  | IfFEq of Id.t * Id.t * t * t
  | IfLEq of Id.t * id_or_imm * t * t
  | IfFLEq of Id.t * Id.t * t * t
  | CallCls of Id.t * Id.t list
  | CallDir of Id.l * Id.t list

type fu = { name : Id.t; args : Id.t list; body : t }

type fundef = Fu of fu * fundef | Fl of Id.t * float * fundef | Main of t

type prog = Program of (Id.t * float) list * fu list * t

val to_string : exp -> string

val to_string_t : t -> string

val to_string_f : fundef -> string

val fd_to_prog : fundef -> prog -> prog

val of_closure_prog : Closure.prog -> prog

val prog_to_fd : prog -> fundef
