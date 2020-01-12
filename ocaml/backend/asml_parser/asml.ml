open Printf

type id_or_imm = Var of Id.t | Int of int

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

let rec infix_to_string (to_s : 'a -> string) (l : 'a list) (op : string) :
    string =
  match l with
  | [] -> ""
  | [ x ] -> to_s x
  | hd :: tl -> to_s hd ^ op ^ infix_to_string to_s tl op

let rec to_string_id_or_imm (i : id_or_imm) =
  match i with Int i -> string_of_int i | Var id -> Id.to_string id

(* to_string: returns a string out of an expresssion exp *)
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

(* to_string_t: match t to correct substring *)
and to_string_t t =
  match t with
  | Ans e -> to_string e
  | Let ((id, t), e1, e2) ->
      sprintf "(let %s = %s in %s)" (Id.to_string id) (to_string e1)
        (to_string_t e2)

(* to_string_f: fundef to string *)
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

(* fd_to_prog: changes a tree of fundef to a program *)
let rec fd_to_prog fd prog =
  match prog with
  | Program (lfl, lfu, body) -> (
      match fd with
      | Main t -> Program (lfl, lfu, t)
      | Fl (l, f, fd2) -> fd_to_prog fd2 (Program (lfl @ [ (l, f) ], lfu, body))
      | Fu (fn, fd2) -> fd_to_prog fd2 (Program (lfl, lfu @ [ fn ], body)) )

(* The three following functions transform a program to fundef *)
let rec prog_fl_to_fd lfl_reversed fd =
  match lfl_reversed with
  | [] -> fd
  | (label, flt) :: rest -> prog_fl_to_fd rest (Fl (label, flt, fd))

let rec prog_fu_to_fd lfu_reversed lfl_reversed fd =
  match lfu_reversed with
  | [] -> prog_fl_to_fd lfl_reversed fd
  | hd :: rest -> prog_fu_to_fd rest lfl_reversed (Fu (hd, fd))

let rec prog_to_fd prog =
  match prog with
  | Program (lfl, lfu, body) ->
      prog_fu_to_fd (List.rev lfu) (List.rev lfl) (Main body)
