(* C(i) represents 13-bit immediates of SPARC *)
(*
type id_or_imm = V of Id.t | I of int

type t = Ans of exp | Let of (Id.t * Type.t) * exp * t

and exp =
  | Var of Id.t
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
  | IfFEq of Id.t * id_or_imm * t * t
  | IfGEq of Id.t * id_or_imm * t * t
  | IfLEq of Id.t * id_or_imm * t * t
  | IfFLEq of Id.t * id_or_imm * t * t
  | CallCls of Id.t * Id.t list
  | CallDir of Id.t * Id.t list

type fu = { name : Id.t; args : Id.t list; body : t }

type fundef = Fu of fu * fundef | Fl of Id.t * float * fundef | Main of t

type prog = Program of (Id.t * float) list * fu list * t

(*let floatTable =(*(Id.l * float) list ref = *) ref []*)
*)

let rec asmConvert_exp (expr : Closure.t) : Asml.exp =
  match expr with
  | Closure.Unit -> Asml.Unit  (*nop- no operation*)
  | Closure.Int i -> Asml.Int i
  | Closure.Float f -> failwith "to do"
  | Closure.Add (a1,a2) -> Asml.Add (a1,Var(a2))
  | Closure.Sub (a1,a2) -> Asml.Sub (a1,Var(a2))
  | Closure.FAdd (v1,v2) -> Asml.FAdd (v1,v2)
  | Closure.FSub (v1,v2) -> Asml.FSub (v1,v2)
  | Closure.FMul (v1,v2) -> Asml.FMul (v1,v2)
  | Closure.FDiv (v1,v2) -> Asml.FDiv (v1,v2)
  | Closure.Var x -> Var(x)
  | _ -> failwith "net there yet1"

and asmConvert_t (expr : Closure.t) : Asml.t =
  match expr with
  | Closure.Unit  (*nop- no operation*)
  | Closure.Int _
  | Closure.Float _
  | Closure.Add _
  | Closure.Sub _
  | Closure.FAdd _
  | Closure.FSub _
  | Closure.FMul _ 
  | Closure.FDiv _
  | Closure.Var _ as e -> Asml.Ans(asmConvert_exp e)
  | Closure.Let((id1, typ1), def1, (Closure.Let _ as l)) ->
    Let((id1, typ1), asmConvert_exp def1, asmConvert_t l)
  | Closure.Let ((id, typ), def, body) -> Asml.Let((id, typ), asmConvert_exp def, Asml.Ans(asmConvert_exp body))
(*
  | Closure.Let ((id, typ), def, body) ->
  | Closure.Var
  | Closure.IfEq
  (* what of GE mentioned in ASML file? *)
  | Closure.IfLe
  | Closure.Tuple
  | Closure.LetTuple
  | Closure.Array
  | Closure.Get
  | Closure.Put
  (* Closure.MkCls *)
  (* Closure.AppCls of *)
  | Closure.AppDir
  | _ -> failwith "net there yet2"
*)

(*

let rec infix_to_string to_s l op =
  match l with
  | [] -> ""
  | [ x ] -> to_s x
  | hd :: tl -> to_s hd ^ op ^ infix_to_string to_s tl op

let id_or_imm_to_string x = 
match x with
| V x -> x
| I i -> string_of_int i 

let rec to_string_exp exp =
  match exp with
  | Unit -> "()"
  | Int i -> string_of_int i
 (* | Ans exp -> to_string exp *)
  | Add (e1, e2) -> Printf.sprintf "(%s + %s)" e1 (id_or_imm_to_string e2)
  | Sub (e1, e2) -> Printf.sprintf "(%s - %s)" e1 (id_or_imm_to_string e2)
  | FAdd (e1, e2) -> Printf.sprintf "(%s +. %s)" e1 e2
  | FSub (e1, e2) -> Printf.sprintf "(%s -. %s)" e1 e2
  | FMul (e1, e2) -> Printf.sprintf "(%s *. %s)" e1 e2
  | FDiv (e1, e2) -> Printf.sprintf "(%s /. %s)" e1 e2


let rec to_string_t t =
  match t with
  | Ans exp ->  to_string_exp exp
  | Let ((id, _t), e1, e2) ->
      Printf.sprintf "(let %s = %s in %s)"  id (to_string_exp e1)
        (to_string_t e2)
  | _ -> failwith "not implemented"
*)