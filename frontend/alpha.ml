let find mapping var = List.assoc_opt var mapping

(* TODO handle name clashes *)
let replace_name mapping name =
  match find mapping name with None -> name | Some new_name -> new_name

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
  | Knorm.Let ((id, typ), def, body) ->
      let new_id =
        match find mapping id with
        | None -> id
        | Some _ ->
            let suffix = Id.genid () in
            id ^ suffix
      in
      Knorm.Let
        ( (new_id, typ),
          convert def mapping,
          convert body ((id, new_id) :: mapping) )
  | Knorm.Var x -> Knorm.Var (replace_name mapping x)
  | Knorm.LetRec ({ name = fun_id, fun_typ; args; body = fun_body }, let_body)
    ->
      let new_args = List.map (fun (id, t) -> (new_name mapping id, t)) args in
      let old_arg_names = List.map (fun (id, _t) -> id) args in
      let new_arg_names = List.map (fun (id, _t) -> id) new_args in
      let new_arg_mappings =
        List.map2
          (fun old_id new_id -> (old_id, new_id))
          old_arg_names new_arg_names
      in

      let new_fun_id = new_name mapping fun_id in
      let new_fun_name = (new_fun_id, fun_typ) in

      let new_mapping = ((fun_id, new_fun_id) :: new_arg_mappings) @ mapping in
      Knorm.LetRec
        ( {
            name = new_fun_name;
            args = new_args;
            body = convert fun_body new_mapping;
          },
          convert let_body ((fun_id, new_fun_id) :: mapping) )
  | Knorm.App (f, args) ->
      let new_args = List.map (fun id -> replace_name mapping id) args in
      let new_f = replace_name mapping f in
      Knorm.App (new_f, new_args)
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
  | Knorm.LetTuple (vars, def, body) ->
      let new_vars = List.map (fun (id, t) -> (new_name mapping id, t)) args in
      let old_vars_names = List.map (fun (id, _t) -> id) vars in
      let new_vars_names = List.map (fun (id, _t) -> id) new_vars in
      let new_vars_mappings =
        List.map2
          (fun old_id new_id -> (old_id, new_id))
          old_vars_names new_vars_names
      in
      Knorm.LetTuple (vars = new_vars, def = convert def new_vars_mappings, body = convert body new_vars_mappings)
  | Knorm.Array (v1,v2) ->
      let c_v1 = replace_name mapping v1 in  (*Not sure *)
      let c_v2 = replace_name mapping v2 in
      Knorm.Array (c_v1, c_v2)
  | Knorm.Get (v1,v2) ->
      let c_v1 = replace_name mapping v1 in  (*Not sure *)
      let c_v2 = replace_name mapping v2 in
      Knorm.Get (c_v1, c_v2)
  | Knorm.Put (v1,v2,v3) ->
      let c_v1 = replace_name mapping v1 in  (*Not sure *)
      let c_v2 = replace_name mapping v2 in
      let c_v3 = replace_name mapping v3 in
      Knorm.Put (c_v1, c_v2, c_v3)
