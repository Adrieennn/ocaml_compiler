(** This module is here to lex, parse and construct the ASML tree for further
uses.*)

(** This type is required from the paper. When adding, subtracting, ... it can
be to either a register or an immediate (hence a Var or an Int). *)
type id_or_imm =
  | Var of Id.t  (** Represents a variable *)
  | Int of int  (** Represents an int *)

(** Type [t] is either a [Let] or an [exp] *)
type t =
  | Ans of exp  (** An expression *)
  | Let of (Id.t * Type.t) * exp * t
      (** A Let definition followed by other
  [t] *)

(** Type [exp] represents what expressions can be. *)
and exp =
  | Var of Id.t  (** variable, to be transformed to a register *)
  | Label of Id.t  (** function/float label *)
  | Int of int  (** integer (immediate) *)
  | Unit  (** translates to [nop] in ARM, does nothing *)
  | Add of Id.t * id_or_imm
      (** addition of a variable with an
  immediate/variable *)
  | Sub of Id.t * id_or_imm  (** subtraction *)
  | Ld of Id.t * id_or_imm
      (** Load a value represented by imm/var into a var *)
  | St of Id.t * id_or_imm * Id.t  (** Store *)
  | New of id_or_imm  (** Used for closures and tuples *)
  | Neg of Id.t  (** Negative of an integer *)
  | FNeg of Id.t  (** Negative of a floating point *)
  | FAdd of Id.t * Id.t  (** Float addition *)
  | FSub of Id.t * Id.t  (** Float subtraction *)
  | FMul of Id.t * Id.t  (** Float multiplication *)
  | FDiv of Id.t * Id.t  (** Float division *)
  | IfEq of Id.t * id_or_imm * t * t  (** If [=] for integers *)
  | IfFEq of Id.t * Id.t * t * t  (** If [=.] for floats *)
  | IfLEq of Id.t * id_or_imm * t * t  (** If [<=] for integers *)
  | IfGEq of Id.t * id_or_imm * t * t  (** If [>=] for integers *)
  | IfFLEq of Id.t * Id.t * t * t  (** If [<=.] for floats *)
  | CallCls of Id.t * Id.t list  (** Calling a closure *)
  | CallDir of Id.l * Id.t list  (** Calling a function *)

type fu = { name : Id.t; args : Id.t list; body : t }
(** Type [fu] represents a function definition. *)

(** Type [fundef] represents function definitions, float definitions and the
main body. *)
type fundef = Fu of fu * fundef | Fl of Id.t * float * fundef | Main of t

(** Type [prog] represents a program as a list of float, a list of function
definition and [t] (instructions). *)
type prog = Program of (Id.t * float) list * fu list * t

val to_string_e : exp -> string
(** Turns an [exp] to an ASML string. *)

val to_string_f : fundef -> string
(** Turns the function and float definitinos to an ASML string. *)

val to_string_p : prog -> string
(** Turns the [prog] comment to a string. *)

val fd_to_prog : fundef -> prog -> prog
(** From a [fundef] and an empty [prog], populates the [prog]. *)

val of_closure_prog : Closure.prog -> prog
(** Transforms a [Closure.prog] from the frontend to a backend [prog]. *)

val prog_to_fd : prog -> fundef
(** Turns an [Asml.prog] to [fundef] type. *)
