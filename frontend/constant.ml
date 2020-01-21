(*Constant.ml - replace int/float/tuple expressions with their constants, if defined*)

(*Finds the associated definition of var in mapping.
If a variable definition is a constant (int/float/tuple), replace the variable with the constant*)
let replace_with_constant mapping var =
  match (List.assoc_opt var mapping) with
  | None -> var (*failwith "To do - failed search" *)
  | Some def ->(
      match def with
      | Knorm.Int i -> i
      | Knorm.Float d -> d
      | Knorm.Tuple tups -> tups
      | _ -> def
    )
(*Checks if a variable definition is an integer constant*)
let is_int v =
    match v with
    | Knorm.Int i -> true | _ -> false

(*Checks if a variable definition is a float constant*)
let is_float v =
    match v with
    | Knorm.Float d -> true | _ -> false

(*Checks if a variable definition is a tuple constant*)
let is_tuple v =
    match v with
    | Knorm.Tuple tups -> true | _ -> false

(*Constant folding on Knorm.t. It takes an expression and mapping, and returns a constant folded expression *)
let rec folding exp mapping =
  match exp with
  | (Knorm.Unit | Knorm.Int _ | Knorm.Float _ | Knorm.App _ | Knorm.Tuple _| Knorm.Array _ | Knorm.Get _ | Knorm.Put _) as c -> c
  | Knorm.Add (v1, v2) ->
      let c_v1 = replace_with_constant mapping v1 in
      let c_v2 = replace_with_constant mapping v2 in
      if ((is_int c_v1) && (is_int c_v2)) then Knorm.Int (c_v1 + c_v2)
      else Knorm.Add(c_v1, c_v2)
  | Knorm.Sub (v1, v2) ->
      let c_v1 = replace_with_constant mapping v1 in
      let c_v2 = replace_with_constant mapping v2 in
      if ((is_int c_v1) && (is_int c_v2)) then Knorm.Int (c_v1 - c_v2)
      else Knorm.Sub (c_v1, c_v2)
  | Knorm.FAdd (v1, v2) ->
      let c_v1 = replace_with_constant mapping v1 in
      let c_v2 = replace_with_constant mapping v2 in
      if ((is_float c_v1) && (is_float c_v2)) then Knorm.Float (c_v1 +. c_v2)
      else Knorm.FAdd (c_v1, c_v2)
  | Knorm.FSub (v1, v2) ->
      let c_v1 = replace_with_constant mapping v1 in
      let c_v2 = replace_with_constant mapping v2 in
      if ((is_float c_v1) && (is_float c_v2)) then Knorm.Float (c_v1 -. c_v2)
      else Knorm.FSub (c_v1, c_v2)
  | Knorm.FMul (v1, v2) ->
      let c_v1 = replace_with_constant mapping v1 in
      let c_v2 = replace_with_constant mapping v2 in
      if ((is_float c_v1) && (is_float c_v2)) then Knorm.Float (c_v1 *. c_v2)
      else Knorm.FMul (c_v1, c_v2)
  | Knorm.FDiv (v1, v2) ->
      let c_v1 = replace_with_constant mapping v1 in
      let c_v2 = replace_with_constant mapping v2 in
      if ((is_float c_v1) && (is_float c_v2)) then Knorm.Float (c_v1 /. c_v2)
      else Knorm.FDiv (c_v1, c_v2)
  | Knorm.Let ((id, typ), def, body) ->
      let new_def = folding def mapping in
      let new_mapping = (id, new_def) :: mapping in
      Knorm.Let ((id, typ), new_def, folding body new_mapping)
  | Knorm.Var x ->
      let c_x = replace_with_constant mapping x in
      if is_int c_x then Knorm.Int c_x
      else if is_float c_x then Knorm.Float c_x
      else if is_tuple c_x then Knorm.Tuple c_x
      else Knorm.Var c_x
  | Knorm.LetRec ({ name = fun_id, fun_Ttyp; args; body = fun_body }, let_body) ->
      let new_fun_body = folding fun_body mapping in
      let new_let_body = folding let_body mapping in
      Knorm.LetRec ({name; args; body = new_fun_body}, let_body = new_let_body)
  | Knorm.IfEq ((v1, v2), e1, e2) ->
      let c_v1 = replace_with_constant mapping v1 in
      let c_v2 = replace_with_constant mapping v2 in
      Knorm.IfEq ((c_v1, c_v2), folding e1 mapping, folding e2 mapping)
  | Knorm.IfLe ((v1, v2), e1, e2) ->
      let c_v1 = replace_with_constant mapping v1 in
      let c_v2 = replace_with_constant mapping v2 in
      Knorm.IfLe ((c_v1, c_v2), folding e1 mapping, folding e2 mapping)
  | Knorm.LetTuple (vars, def, body) ->
      (*if is_tuple new_def then
        failwith "To Do"
        let new_def = folding def mapping in
        let new_mapping =
        Knorm.LetTuple (vars, def = new_def, folding body new_mapping)
        else *)
      Knorm.LetTuple (vars, def, folding def body)
