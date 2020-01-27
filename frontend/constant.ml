(*Constant.ml - replace int/float/tuple expressions with their constants, if defined*)

(*Finds the associated definition of var in mapping.*)
let find_definition mapping var = List.assoc_opt var mapping

(*Checks if a variable definition is an integer constant*)
let rec get_int_opt mapping id =
  match find_definition mapping id with
  | Some (Knorm.Int i) -> Some i
  | Some (Knorm.Var id') -> get_int_opt mapping id'
  | _ -> None

(*Checks if a variable definition is a float constant*)
let rec get_float_opt mapping id =
  match find_definition mapping id with
  | Some (Knorm.Float d) -> Some d
  | Some (Knorm.Var id') -> get_float_opt mapping id'
  | _ -> None

let get_tuple_opt mapping id =
  match find_definition mapping id with
  | Some (Knorm.Tuple elements) -> Some elements
  | _ -> None

(*Checks if a variable definition is a tuple constant*)
let is_tuple def = match def with Knorm.Tuple tups -> true | _ -> false

(*Constant folding on Knorm.t. It takes an expression and mapping, and returns a constant folded expression *)
let rec folding (exp : Knorm.t) mapping =
  match exp with
  | ( Knorm.Unit | Knorm.Int _ | Knorm.Float _ | Knorm.App _ | Knorm.Tuple _
    | Knorm.Array _ | Knorm.Get _ | Knorm.Put _ ) as c ->
      c
  (*If both arguments are both int/float constants approriately, do the operation and return the constant value.
Otherwise return the original expression*)
  | Knorm.Add (v1, v2) -> (
      match (get_int_opt mapping v1, get_int_opt mapping v2) with
      | Some i1, Some i2 -> Knorm.Int (i1 + i2)
      | _ -> Knorm.Add (v1, v2) )
  | Knorm.Sub (v1, v2) -> (
      match (get_int_opt mapping v1, get_int_opt mapping v2) with
      | Some i1, Some i2 -> Knorm.Int (i1 - i2)
      | _ -> Knorm.Sub (v1, v2) )
  | Knorm.FAdd (v1, v2) -> (
      match (get_float_opt mapping v1, get_float_opt mapping v2) with
      | Some f1, Some f2 -> Knorm.Float (f1 +. f2)
      | _ -> Knorm.FAdd (v1, v2) )
  | Knorm.FSub (v1, v2) -> (
      match (get_float_opt mapping v1, get_float_opt mapping v2) with
      | Some f1, Some f2 -> Knorm.Float (f1 -. f2)
      | _ -> Knorm.FSub (v1, v2) )
  | Knorm.FMul (v1, v2) -> (
      match (get_float_opt mapping v1, get_float_opt mapping v2) with
      | Some f1, Some f2 -> Knorm.Float (f1 *. f2)
      | _ -> Knorm.FMul (v1, v2) )
  | Knorm.FDiv (v1, v2) -> (
      match (get_float_opt mapping v1, get_float_opt mapping v2) with
      | Some f1, Some f2 -> Knorm.Float (f1 /. f2)
      | _ -> Knorm.FDiv (v1, v2) )
  | Knorm.Let ((id, typ), def, body) ->
      (*Constant fold the variable definition, add the new var-def mapping to mapping and constant fold body with new mapping*)
      let new_def = folding def mapping in
      let new_mapping = (id, new_def) :: mapping in
      Knorm.Let ((id, typ), new_def, folding body new_mapping)
  | Knorm.Var x -> (
      (*if a variable is Int/Float/Tuple, then replace with constant. otherwise, return var*)
      match get_int_opt mapping x with
      | Some i -> Knorm.Int i
      | None -> (
          match get_float_opt mapping x with
          | Some f -> Knorm.Float f
          | None -> (
              match get_tuple_opt mapping x with
              | Some elements -> Knorm.Tuple elements
              | None -> Knorm.Var x ) ) )
  | Knorm.LetRec ({ name = fun_id, fun_Ttyp; args; body = fun_body }, let_body)
    ->
      let new_fun_body = folding fun_body mapping in
      let new_let_body = folding let_body mapping in
      Knorm.LetRec
        ({ name = (fun_id, fun_Ttyp); args; body = new_fun_body }, new_let_body)
  | Knorm.LetTuple (vars, def, body) -> (
      (*fold tuple definition. if the folded definition is a tuple, do further constant folding for the individual tuple
  elements. Otherwise, only constant fold body.*)
      let new_def = folding def mapping in
      (* let new_body = folding body mapping in *)
      match new_def with
      | Knorm.Tuple elements ->
          let var_ids = List.map (fun (id, t) -> id) vars in
          let new_mappings =
            List.map2 (fun id id' -> (id, Knorm.Var id')) var_ids elements
          in
          Knorm.LetTuple (vars, new_def, folding body (new_mappings @ mapping))
      | _ -> Knorm.LetTuple (vars, new_def, folding body mapping) )
  (*For If/else expressions, if v1 and v2 are both ints/floats do the mathematical evaluation.*)
  | Knorm.IfEq ((v1, v2), e1, e2) -> (
      match (get_int_opt mapping v1, get_int_opt mapping v2) with
      | Some i1, Some i2 ->
          if i1 = i2 then folding e1 mapping else folding e2 mapping
      | _ -> Knorm.IfEq ((v1, v2), folding e1 mapping, folding e2 mapping) )
  | Knorm.IfLe ((v1, v2), e1, e2) -> (
      match (get_int_opt mapping v1, get_int_opt mapping v2) with
      | Some i1, Some i2 ->
          if i1 <= i2 then folding e1 mapping else folding e2 mapping
      | _ -> Knorm.IfLe ((v1, v2), folding e1 mapping, folding e2 mapping) )
