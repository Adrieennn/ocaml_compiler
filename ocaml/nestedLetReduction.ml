(*Reduction of nested let expressions*)
let rec reduction expr =   (**)
     match expr with
     (*first case was copied from mincaml article*)
     | Knorm.Let(xt, e1, e2) ->
         let rec insert = function
           | Knorm.Let(yt, e3, e4) -> Let(yt, e3, insert e4)
           | Knorm.LetRec(fundefs, e) -> LetRec(fundefs, insert e)
           | Knorm.LetTuple(yts, z, e) -> LetTuple(yts, z, insert e)
           | e -> Let(xt, e, reduction e2) in (*write e as Knorm.e? *)
         insert (reduction e1)
     | Knorm.LetRec ({ name; args; body }, e) -> LetRec ({ name; args; body = reduction body }, reduction e)
     | Knorm.IfEq (x, y, e2, e3) -> IfEq (x, y, reduction e2, reduction e3)
     | Knorm.IfLe (x, y, e2, e3) -> IfLe (x, y, reduction e2, reduction e3)
     | Knorm.LetTuple (vars, def, body) -> LetTuple (vars, def, reduction body)
     |__ -> expr

let rec infix_to_string to_s l op =
  match l with
  | [] -> ""
  | [ x ] -> to_s x
  | hd :: tl -> to_s hd ^ op ^ infix_to_string to_s tl op

let rec to_string exp =
  match exp with
  | Unit -> "()"
  | Int i -> string_of_int i
  | Float f -> string_of_float f
  | Add (e1, e2) -> Printf.sprintf "(%s + %s)" e1 e2
  | Sub (e1, e2) -> Printf.sprintf "(%s - %s)" e1 e2
  | FAdd (e1, e2) -> Printf.sprintf "(%s +. %s)" e1 e2
  | FSub (e1, e2) -> Printf.sprintf "(%s -. %s)" e1 e2
  | FMul (e1, e2) -> Printf.sprintf "(%s *. %s)" e1 e2
  | FDiv (e1, e2) -> Printf.sprintf "(%s /. %s)" e1 e2
  | Var id -> Id.to_string id
  | Tuple l -> Printf.sprintf "(%s)" (infix_to_string Id.to_string l ", ")
  | App (e1, le2) ->
    Printf.sprintf "(%s %s)" (Id.to_string e1)
      (infix_to_string Id.to_string le2 " ")
  | Let ((id, _t), e1, e2) ->
    Printf.sprintf "(let %s = %s in %s)" (Id.to_string id) (to_string e1)
      (to_string e2)
  | LetRec (fd, e) ->
    Printf.sprintf "(let rec %s %s = %s in %s)"
      (let x, _ = fd.name in
       Id.to_string x)
      (infix_to_string (fun (x, _) -> Id.to_string x) fd.args " ")
      (to_string fd.body) (to_string e)
  | LetTuple (l, e1, e2) ->
    Printf.sprintf "(let (%s) = %s in %s)"
      (infix_to_string (fun (x, _) -> Id.to_string x) l ", ")
      (to_string e1) (to_string e2)
  | IfEq ((id1, id2), e1, e2) ->
    Printf.sprintf "(if %s  = %s then %s else %s)" (Id.to_string id1)
      (Id.to_string id2) (to_string e1) (to_string e2)
  | IfLe ((id1, id2), e1, e2) ->
    Printf.sprintf "(if %s  <= %s then %s else %s)" (Id.to_string id1)
      (Id.to_string id2) (to_string e1) (to_string e2)
  | Get (e1, e2) -> Printf.sprintf "%s.(%s)" (Id.to_string e1) (Id.to_string e2)
  | Put (e1, e2, e3) ->
    Printf.sprintf "(%s.(%s) <- %s)" (Id.to_string e1) (Id.to_string e2)
      (Id.to_string e3)
  | Array (e1, e2) ->
    Printf.sprintf "(Array.create %s %s)" (Id.to_string e1) (Id.to_string e2)
