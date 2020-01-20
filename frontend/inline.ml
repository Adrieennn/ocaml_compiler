let find mapping var = List.assoc_opt var mapping

let mem mapping var = match find mapping var with | None -> false | Some _ -> true

let add x_id y_id mapping = (x_id, y_id) :: mapping 




let threshold = 3 

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


let rec expansion exp map = 
  match exp with
  | Knorm.App (f, ys) when mem map f -> 
    (* let (xs, e) = find map f in
    let env' =
      List.fold_left2
      (fun env' (id, t) y -> add id y env')
      [] xs ys in
    Alpha.convert e env' *)
    failwith "app in inline expansion net implemented yet"
  | Knorm.Let ((id, typ), def, body) -> failwith "let in inline expansion net implemented yet"

  | Knorm.LetRec ({ name = fun_id, fun_typ; args; body = fun_body }, let_body) -> failwith "letrec in inline expansion net implemented yet"

  | Knorm.LetTuple (vars, def, body) -> failwith "letrec in inline expansion net implemented yet"
      
  | Knorm.IfEq ((v1, v2), e1, e2) -> failwith "letrec in inline expansion net implemented yet"
      
  | Knorm.IfLe ((v1, v2), e1, e2) -> failwith "letrec in inline expansion net implemented yet"
      
  | _ as c -> c