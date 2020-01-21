let find mapping var = List.assoc_opt var mapping

let mem mapping var = match find mapping var with | None -> false | Some _ -> true

let add x_id y_id mapping = (x_id, y_id) :: mapping 




let threshold = 9

let rec size exp = 
  match exp with
  | (Knorm.Unit | Knorm.Int _ | Knorm.Float _ | Knorm.Tuple _ | Knorm.Array _ | Knorm.Get _ | Knorm.Put _ | Knorm.Var _   | Knorm.App _)  -> 1
  | Knorm.Add _ | Knorm.Sub _ | Knorm.FAdd _ | Knorm.FSub _ | Knorm.FMul _ | Knorm.FDiv _ -> 3 
  | Knorm.Let ((id, typ), def, body) -> 
      (size def) + (size body) +1
  | Knorm.LetRec ({ name = fun_id, fun_typ; args; body = fun_body }, let_body) ->
      (size fun_body) + (size let_body) + 1
  | Knorm.LetTuple (vars, def, body) ->
      (size def) + (size body) + 1
  | Knorm.IfEq ((v1, v2), e1, e2) ->
      (size e1) + (size e2) + 1
  | Knorm.IfLe ((v1, v2), e1, e2) ->
      (size e1) + (size e2) + 1
(*
let rec rec_fun id body = 
  match body with
  | Knorm.App (id, _) -> true
  | Knorm.Unit | Knorm.Int _ | Knorm.Float _ | Knorm.Tuple _ | Knorm.Array _ | Knorm.Get _ | Knorm.Put _ | Knorm.Var _ | Knorm.Add _ | Knorm.Sub _ | Knorm.FAdd _ | Knorm.FSub _ | Knorm.FMul _ | Knorm.FDiv _ -> false
  | Knorm.Let ((id, typ), def, lbody) -> 
      (rec_fun id def) and (rec_fun id lbody)
  | Knorm.LetRec ({ name = fun_id, fun_typ; args; body = fun_body }, let_body) ->
      (rec_fun id fun_body) and (rec_fun id let_body) and (rec_fun fun_id fun_body)
  | Knorm.LetTuple (vars, def, body) ->
      (size def) (size body)
  | Knorm.IfEq ((v1, v2), e1, e2) ->
      (size e1) + (size e2) + 1
  | Knorm.IfLe ((v1, v2), e1, e2) ->
      (size e1) + (size e2) + 1  
*)

let rec expansion exp map = 
  match exp with
  | Knorm.App (f, ys) -> 
    (match find map f with
    | None -> Knorm.App (f, ys)
    | Some (args, body) -> 
      let env' =
        List.fold_left2
        (fun env' (id, t) y -> add id y env')
        [] args ys in
      Alpha.convert body env'
    )
  | Knorm.Let ((id, typ), def, body) -> 
    Knorm.Let ((id, typ), expansion def map, expansion body map)
  | Knorm.LetRec ({ name = fun_id, fun_typ; args; body = fun_body }, let_body) -> 
    (*add to map the expanded version*)
    (*recursive? infinity recursion??*)
    (*supress or not supress, it's a question*)
    (*version non supress*)
    let expanded_fun_body = (expansion fun_body map) in 
      if ((size fun_body)<threshold) then 
          Knorm.LetRec({ name = fun_id, fun_typ; args; body = expanded_fun_body }, expansion let_body ((fun_id,(args,expanded_fun_body)) :: map))
      else
          Knorm.LetRec({ name = fun_id, fun_typ; args; body = expanded_fun_body }, expansion let_body map) 
  | Knorm.LetTuple (vars, defs, body) ->
      Knorm.LetTuple (vars, expansion defs map, expansion body map)
  | Knorm.IfEq ((v1, v2), e1, e2) -> 
      Knorm.IfEq ((v1, v2), expansion e1 map, expansion e2 map)
  | Knorm.IfLe ((v1, v2), e1, e2) -> 
      Knorm.IfLe ((v1, v2), expansion e1 map, expansion e2 map)
  | _ as c -> c