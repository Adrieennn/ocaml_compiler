(*Constant.ml - replace int/float/tuple expressions with their constants, if defined*)

(*Finds the associated definition of var in mapping.*)
let find_definition mapping var =
  match (List.assoc_opt var mapping) with
  | None -> var (*failwith "To do - failed search" *)
  | Some def -> def

let find_int mapping var =
  let definition = find_definition mapping var in
  match definition with
  | Knorm.Int i -> i
  | _-> failwith "To do find_int"

let find_float mapping var =
  let definition = find_definition mapping var in
  match definition with
  | Knorm.Float d -> d
  | _-> failwith "To do find_float"

let find_tuple mapping var =
  let definition = find_definition mapping var in
  match definition with
  | Knorm.Tuple tups -> tups
  | _-> failwith "To do find_float"
  (*match (List.assoc_opt var mapping) with
  | None -> var (*failwith "To do - failed search" *)
  | Some def ->if is_int def then def *)

(*Checks if a variable definition is an integer constant*)
let is_int def =
    match def with
    | Knorm.Int i -> true | _ -> false

(*Checks if a variable definition is a float constant*)
let is_float def =
    match def with
    | Knorm.Float d -> true | _ -> false

(*Checks if a variable definition is a tuple constant*)
let is_tuple def =
    match def with
    | Knorm.Tuple tups -> true | _ -> false

(*Constant folding on Knorm.t. It takes an expression and mapping, and returns a constant folded expression *)
let rec folding (exp : Knorm.t) mapping =
  match exp with
  | (Knorm.Unit | Knorm.Int _ | Knorm.Float _ | Knorm.App _ | Knorm.Tuple _| Knorm.Array _ | Knorm.Get _ | Knorm.Put _) as c -> c
  (*If both arguments are both int/float constants approriately, do the operation and return the constant value*)
  | Knorm.Add (v1, v2) ->
      let c_v1 = find_int mapping (Knorm.Var v1) in
      let c_v2 = find_int mapping (Knorm.Var v2) in
      Knorm.Int (c_v1 + c_v2) (*
      let c_v1 = find_definition mapping (Knorm.Var v1) in
      let c_v2 = find_definition mapping (Knorm.Var v2) in
      if ((is_int c_v1) && (is_int c_v2)) then (Knorm.Int (c_v1 + c_v2))
      else (Knorm.Add(c_v1, c_v2)) *)
      (*if (is_int (find_definition mapping v1)) && (is_int (find_definition mapping v2))
        then Knorm.Int ((find_definition mapping v1) + (find_definition mapping v2)) *)
  (*else Knorm.Add(v1, v2)*)
  | Knorm.Sub (v1, v2) ->
      let c_v1 = find_int mapping (Knorm.Var v1) in
      let c_v2 = find_int mapping (Knorm.Var v2) in
      Knorm.Int (c_v1 - c_v2)
      (*let c_v1 = find_definition mapping v1 in
      let c_v2 = find_definition mapping v2 in
      if ((is_int c_v1) && (is_int c_v2)) then Knorm.Int (c_v1 - c_v2)
      else Knorm.Sub (Var c_v1, Var c_v2) *)
  | Knorm.FAdd (v1, v2) ->
      let c_v1 = find_float mapping (Knorm.Var v1) in
      let c_v2 = find_float mapping (Knorm.Var v2) in
      Knorm.Float (c_v1 +. c_v2)
  | Knorm.FSub (v1, v2) ->
      let c_v1 = find_float mapping (Knorm.Var v1) in
      let c_v2 = find_float mapping (Knorm.Var v2) in
      Knorm.Float (c_v1 -. c_v2)
  | Knorm.FMul (v1, v2) ->
      let c_v1 = find_float mapping (Knorm.Var v1) in
      let c_v2 = find_float mapping (Knorm.Var v2) in
      Knorm.Float (c_v1 *. c_v2)
  | Knorm.FDiv (v1, v2) ->
      let c_v1 = find_float mapping (Knorm.Var v1) in
      let c_v2 = find_float mapping (Knorm.Var v2) in
      Knorm.Float (c_v1 /. c_v2)
  | Knorm.Let ((id, typ), def, body) ->
    (*Constant fold the variable definition, add the new var-def mapping to mapping and constant fold body with new mapping*)
      let new_def = folding def mapping in
      let new_mapping = (Knorm.Var id, new_def) :: mapping in
      Knorm.Let ((id, typ), new_def, folding body new_mapping)
        (*  | Knorm.Var x ->
    let int_x = find_int mapping (Knorm.Var x) in
    let float_x = find_float mapping (Knorm.Var x) in
    let tuple_x = find_tuple mapping (Knorm.Var x) in
    if is_int (Knorm.t int_x) then Knorm.Int int_x
    else if is_float float_x then Knorm.Float float_x
            else if is_tuple tuple_x then Knorm.Tuple tuple_x *)
  | Knorm.LetRec ({ name = fun_id, fun_Ttyp; args; body = fun_body }, let_body) ->
    let new_fun_body = folding fun_body mapping in
    let new_let_body = folding let_body mapping in
    Knorm.LetRec ({name = fun_id, fun_Ttyp; args; body = new_fun_body}, new_let_body)
  | Knorm.LetTuple (vars, def, body) ->
    (*fold tuple definition. if the folded definition is a tuple, do further constant folding for the individual tuple
      elements. Otherwise, only constant fold body.*)
      let new_def = folding def mapping in
      let new_body = folding body mapping in
      if is_tuple new_def then
        (*Deconstruct tuple elements into lets and associate each var with its corresponding def
          List.fold_left2 (fun var v_def v_body -> Knorm.Let((var, None), v_def, v_body)) new_body vars new_def *)
        failwith "To do further tuple Deconstruction"
      else
        Knorm.LetTuple (vars, def, new_body)
  | _ ->  failwith "Not implemented"
(*
  | Knorm.Var x ->
    let c_x = find_definition mapping (Knorm.Var x) in
      if is_int c_x then Knorm.Int c_x
      else if is_float c_x then Knorm.Float c_x
      else if is_tuple c_x then Knorm.Tuple c_x
      else Knorm.Var c_x

  | Knorm.IfEq ((v1, v2), e1, e2) ->
      let c_v1 = find_definition mapping v1 in
      let c_v2 = find_definition mapping v2 in
      Knorm.IfEq ((c_v1, c_v2), folding e1 mapping, folding e2 mapping)
  | Knorm.IfLe ((v1, v2), e1, e2) ->
      let c_v1 = find_definition mapping v1 in
      let c_v2 = find_definition mapping v2 in
      Knorm.IfLe ((c_v1, c_v2), folding e1 mapping, folding e2 mapping)

*)
