open Printf

type id_or_imm = Var of Id.t | Int of int

type t = Ans of exp | Let of (Id.t * Type.t) * exp * t

and exp =
  | Var of Id.t
  | Int of int
  | Unit (* TODO might not be needed *)
  | Add of Id.t * id_or_imm
  | Sub of Id.t * id_or_imm
  | Ld of Id.t * id_or_imm
  | St of Id.t * id_or_imm * Id.t
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

type fundef = { name : Id.t; args : Id.t list; body : t }

type prog = Program of (Id.t * float) list * fundef list * t


let rec t_to_reg t var_reg =
  match t with
  | Ans e -> []
  | Let ((variable, _), exp, t2) ->
      t_to_reg t2 ((variable, "fp") :: var_reg )

let program_to_reg pg var_reg =
  match pg with
  | Program ( lfu, lfl, t ) ->
    (* match lfl with
    | _ -> "float not implemented yet"
    match lfu with
    | _ -> "fun list not implemented yet" *)
    match t with
    | Let ((variable, _), exp, t) -> t_to_reg t var_reg
    | _ -> []


(*
type t = 
  | Unit
  | Bool of bool
  | Int of int
  | Float of float
  | Not of t
  | Neg of t
  | Add of t * t
  | Sub of t * t
  | FNeg of t
  | FAdd of t * t
  | FSub of t * t
  | FMul of t * t
  | FDiv of t * t
  | Eq of t * t
  | LE of t * t
  | If of t * t * t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | LetRec of fundef * t
  | App of t * t list
  | Tuple of t list
  | LetTuple of (Id.t * Type.t) list * t * t
  | Array of t * t
  | Get of t * t
  | Put of t * t * t
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }
*)

let rec infix_to_string (to_s : 'a -> string) (l : 'a list) (op : string) :
    string =
  match l with
  | [] -> ""
  | [ x ] -> to_s x
  | hd :: tl -> to_s hd ^ op ^ infix_to_string to_s tl op

let rec to_string_id_or_imm (i : id_or_imm) =
  match i with Int i -> string_of_int i | Var id -> Id.to_string id

let rec to_string exp =
  match exp with
  | Unit -> "()"
  | Int i -> string_of_int i
  | Neg e -> sprintf "(- %s)" (Id.to_string e)
  | Add (e1, e2) ->
      sprintf "(%s + %s)" (Id.to_string e1) (to_string_id_or_imm e2)
  | Sub (e1, e2) ->
      sprintf "(%s - %s)" (Id.to_string e1) (to_string_id_or_imm e2)
  | FNeg e -> sprintf "(-. %s)" (Id.to_string e)
  | FAdd (e1, e2) -> sprintf "(%s +. %s)" (Id.to_string e1) (Id.to_string e2)
  | FSub (e1, e2) -> sprintf "(%s -. %s)" (Id.to_string e1) (Id.to_string e2)
  | FMul (e1, e2) -> sprintf "(%s *. %s)" (Id.to_string e1) (Id.to_string e2)
  | FDiv (e1, e2) -> sprintf "(%s /. %s)" (Id.to_string e1) (Id.to_string e2)
  | IfEq (e1, e2, e3, e4) ->
      sprintf "(if %s = %s then %s else %s)" (Id.to_string e1)
        (to_string_id_or_imm e2) (to_string_t e3) (to_string_t e4)
  | IfFEq (e1, e2, e3, e4) ->
      sprintf "(if %s = %s then %s else %s)" (Id.to_string e1)
        (to_string_id_or_imm e2) (to_string_t e3) (to_string_t e4)
  | IfGEq (e1, e2, e3, e4) ->
      sprintf "(if %s >= %s then %s else %s)" (Id.to_string e1)
        (to_string_id_or_imm e2) (to_string_t e3) (to_string_t e4)
  | IfLEq (e1, e2, e3, e4) ->
      sprintf "(if %s <= %s then %s else %s)" (Id.to_string e1)
        (to_string_id_or_imm e2) (to_string_t e3) (to_string_t e4)
  | IfFLEq (e1, e2, e3, e4) ->
      sprintf "(if %s <= %s then %s else %s)" (Id.to_string e1)
        (to_string_id_or_imm e2) (to_string_t e3) (to_string_t e4)
  (* | Let ((id,t), e1, e2) -> 
          sprintf "(let %s = %s in %s)" (Id.to_string id) (to_string e1) (to_string e2)   
*)
  | Var id -> Id.to_string id
  | CallDir (e1, le2) ->
      sprintf "(%s %s)" (Id.to_string e1) (infix_to_string Id.to_string le2 " ")
  (*  | LetRec (fd, e) ->  
          sprintf "(let rec %s %s = %s in %s)" 
          (let (x, _) = fd.name in (Id.to_string x))
          (infix_to_string (fun (x,_) -> (Id.to_string x)) fd.args " ") 
          (to_string fd.body)
          (to_string e)
  | LetTuple (l, e1, e2)-> 
          sprintf "(let (%s) = %s in %s)" 
          (infix_to_string (fun (x, _) -> Id.to_string x) l ", ")
          (to_string e1)
          (to_string e2)
*)
  | Ld (e1, e2) -> sprintf "%s.(%s)" (Id.to_string e1) (to_string_id_or_imm e2)
  | St (e1, e2, e3) ->
      sprintf "(%s.(%s) <- %s)" (Id.to_string e1) (to_string_id_or_imm e2)
        (Id.to_string e3)
  (*  | Tuple(l) -> sprintf "(%s)" (infix_to_string to_string l ", ") 
  | Array(e1,e2) -> sprintf "(Array.create %s %s)" 
       (to_string e1) (to_string e2) 
*)
  | _ -> "not implemented"

and to_string_t t =
  match t with
  | Ans e -> sprintf "hi %s" (to_string e)
  | Let ((id, t), e1, e2) ->
      sprintf "(letting %s = %s in %s)" (Id.to_string id) (to_string e1)
        (to_string_t e2)


let program =
  Program
    ( [],
      [],
      Let
        ( ("x", Type.gentyp ()),
          Int 0,
          Let
            ( ("y", Type.gentyp ()),
              Int 1,
              Let (("z", Type.gentyp ()), Add ("x", Var "y"), Ans Unit) ) ) );;
let _ = List.iter ((fun (s1, s2) -> Printf.printf "(a, a)" s1 s2) program_to_reg program []);;


