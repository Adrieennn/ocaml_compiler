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

let rec t_to_reg t var_reg =
  match t with
  | Ans e -> var_reg
  | Let ((variable, _), exp, t2) -> t_to_reg t2 (var_reg @ [ (variable, "fp") ])

let program_to_reg pg var_reg =
  match pg with
  | Program (lfu, lfl, t) -> (
      (* match lfl with
    | _ -> "float not implemented yet"
    match lfu with
    | _ -> "fun list not implemented yet" *)
      match t with
      | Let ((variable, _), exp, t2) -> t_to_reg t var_reg
      | _ -> [] )

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
  | Var id -> Id.to_string id
  | CallDir (e1, le2) ->
      sprintf "(%s %s)" (Id.to_string e1) (infix_to_string Id.to_string le2 " ")
  | Ld (e1, e2) -> sprintf "%s.(%s)" (Id.to_string e1) (to_string_id_or_imm e2)
  | St (e1, e2, e3) ->
      sprintf "(%s.(%s) <- %s)" (Id.to_string e1) (to_string_id_or_imm e2)
        (Id.to_string e3)
  | _ -> "not implemented"

and to_string_t t =
  match t with
  | Ans e -> to_string e
  | Let ((id, t), e1, e2) ->
      sprintf "(let %s = %s in %s)" (Id.to_string id) (to_string e1)
        (to_string_t e2)

let rec to_string_f fd =
  match fd with
  | Main t -> sprintf "let _ = %s" (to_string_t t)
  | Fl (l, f, fd2) ->
      sprintf "(let %s = %s) %s" (Id.to_string l) (string_of_float f)
        (to_string_f fd2)
  | Fu (fn, fd2) ->
      sprintf "(let %s %s = %s in %s)"
        (let x = fn.name in
         Id.to_string x)
        (infix_to_string (fun x -> Id.to_string x) fn.args " ")
        (to_string_t fn.body) (to_string_f fd2)

let rec fd_to_prog fd prog =
  match prog with
  | Program (lfl, lfu, body) -> (
      match fd with
      | Main t -> Program (lfl, lfu, t)
      | Fl (l, f, fd2) -> fd_to_prog fd2 (Program (lfl @ [ (l, f) ], lfu, body))
      | Fu (fn, fd2) -> fd_to_prog fd2 (Program (lfl, lfu @ [ fn ], body)) )
