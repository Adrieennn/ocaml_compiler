let find mapping var = List.assoc_opt var mapping

(** replace_name takes 2 parameters, the mapping of old variable names to new 
variable names and a variable name. If the variable name is not found in the 
mapping environment, it is considered external and the original variable name 
is returned. Otherwise it returns the new name it found in the mapping 
envirement.
*)
let replace_name mapping name =
  match find mapping name with None -> name | Some def_name -> def_name

(** new_name takes the same parameters as replace_name, it generates a new 
variable name using Id.genid when encountering a non-external variable.
*)
let new_name mapping name =
  match find mapping name with None -> name | Some _ -> name ^ Id.genid ()


let rec convert exp mapping =
  match exp with
  | (Knorm.Unit | Knorm.Int _ | Knorm.Float _) as c -> c
  | Knorm.Add (v1, v2) ->
      let c_v1 = replace_name mapping v1 in
      let c_v2 = replace_name mapping v2 in
      Knorm.Add (c_v1, c_v2)
  | Knorm.Sub (v1, v2) ->
      let c_v1 = replace_name mapping v1 in
      let c_v2 = replace_name mapping v2 in
      Knorm.Sub (c_v1, c_v2)
  | Knorm.FAdd (v1, v2) ->
      let c_v1 = replace_name mapping v1 in
      let c_v2 = replace_name mapping v2 in
      Knorm.FAdd (c_v1, c_v2)
  | Knorm.FSub (v1, v2) ->
      let c_v1 = replace_name mapping v1 in
      let c_v2 = replace_name mapping v2 in
      Knorm.FSub (c_v1, c_v2)
  | Knorm.FMul (v1, v2) ->
      let c_v1 = replace_name mapping v1 in
      let c_v2 = replace_name mapping v2 in
      Knorm.FMul (c_v1, c_v2)
  | Knorm.FDiv (v1, v2) ->
      let c_v1 = replace_name mapping v1 in
      let c_v2 = replace_name mapping v2 in
      Knorm.FDiv (c_v1, c_v2)
  | Knorm.Var x -> Knorm.Var (replace_name mapping x)

  | Knorm.Let ((id, typ), def, body) ->
      (*In the case of a Let expression, we use the updated mapping environment 
          to evaluate the let body*)
    let new_def = convert def mapping in 
    (match new_def with
    | Knorm.Var new_id -> convert body ((id, new_id) :: mapping)
    | _ -> Knorm.Let((id, typ), new_def, convert body mapping))

  | Knorm.LetRec ({ name = fun_id, fun_typ; args; body = fun_body }, let_body)
    -> failwith "beta conversion for LetRec is not implemented yet"

    (*need to think about how to map from one list to another*)
  | Knorm.LetTuple (vars, def, body) -> failwith "beta conversion for LetTuple is not implemented yet"

  | Knorm.App (f, args) ->
      let new_args = List.map (fun id -> replace_name mapping id) args in
      Knorm.App (f, new_args)

  | Knorm.IfEq ((v1, v2), e1, e2) ->
      let c_v1 = replace_name mapping v1 in
      let c_v2 = replace_name mapping v2 in
      Knorm.IfEq ((c_v1, c_v2), convert e1 mapping, convert e2 mapping)

  | Knorm.IfLe ((v1, v2), e1, e2) ->
      let c_v1 = replace_name mapping v1 in
      let c_v2 = replace_name mapping v2 in
      Knorm.IfLe ((c_v1, c_v2), convert e1 mapping, convert e2 mapping)

  | Knorm.Tuple(tups) ->
      let new_tups = List.map (fun tup -> (replace_name mapping tup)) tups in
      Knorm.Tuple(new_tups)

  | Knorm.Array (v1,v2) ->
      let c_v1 = replace_name mapping v1 in
      let c_v2 = replace_name mapping v2 in
      Knorm.Array (c_v1, c_v2)
  | Knorm.Get (v1,v2) ->
      let c_v1 = replace_name mapping v1 in
      let c_v2 = replace_name mapping v2 in
      Knorm.Get (c_v1, c_v2)
  | Knorm.Put (v1,v2,v3) ->
      let c_v1 = replace_name mapping v1 in
      let c_v2 = replace_name mapping v2 in
      let c_v3 = replace_name mapping v3 in
      Knorm.Put (c_v1, c_v2, c_v3)