type t =
  | Unit
  | Int of int
  | Add of Id.t * Id.t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t

and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }

let rec of_syntax exp_s =
  match exp_s with
  | Syntax.Unit -> Unit
  | Syntax.Int i -> Int i
  | Syntax.Add (e1, e2) ->
      let e1_id = Id.genid () in
      let e2_id = Id.genid () in
      Let
        ( (e1_id, Type.Int),
          of_syntax e1,
          Let ((e2_id, Type.Int), of_syntax e2, Add (e1_id, e2_id)) )
  | Syntax.Let ((id, typ), def, body) ->
      Let ((id, typ), of_syntax def, of_syntax body)
  | Syntax.Var id -> Var id
  | e ->
      Printf.eprintf "%s not implemented\n" (Syntax.to_string e);
      exit 0

let rec infix_to_string to_s l op =
  match l with
  | [] -> ""
  | [ x ] -> to_s x
  | hd :: tl -> to_s hd ^ op ^ infix_to_string to_s tl op

let rec to_string exp =
  match exp with
  | Unit -> "()"
  | Int i -> string_of_int i
  | Add (e1, e2) -> Printf.sprintf "(%s + %s)" e1 e2
  | Var id -> Id.to_string id
  | Let ((id, _t), e1, e2) ->
      Printf.sprintf "(let %s = %s in %s)" (Id.to_string id) (to_string e1)
        (to_string e2)
