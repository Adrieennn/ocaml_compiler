(*let find var mapping = List.assoc_opt var mapping*)

(*Finds the associated definition of var in mapping. If *)
let in_mapping mapping var =
  match (List.assoc_opt var mapping) with
  | None -> Knorm.Unit (*is it better to write "false"?*)
  | Some def -> def

(*If a variable definition is a constant (int/float), replace the variable with the constant*)
let replace_with_constant mapping var =
  let definition  = find mapping var in
  match definition with
  | Knorm.Int i -> i
  | Knorm.Float d -> d
  | Knorm.Tuple tups -> tups
  | _ -> false

(*Constant folding on Knorm.t. It takes an expression and mapping, and returns a constant folded expression *)
let rec folding exp mapping =
  match exp with
  | (Knorm.Unit | Knorm.Int _ | Knorm.Float _) as c -> c
  | Knorm.Add (v1, v2) ->
    let c_v1 = replace_with_constant mapping v1 in   (*Need to enforce constant is of type int*)
    let c_v2 = replace_with_constant mapping v2 in
    Knorm.Add (c_v1, c_v2)
    if c_v1 && c_v2  then Knorm.Int (c_v1 + c_v2)
  | Knorm.Sub (v1, v2) ->
    let c_v1 = replace_with_constant mapping v1 in
    let c_v2 = replace_with_constant mapping v2 in
    Knorm.Sub (c_v1, c_v2)
  | Knorm.FAdd (v1, v2) ->
    let c_v1 = replace_with_constant mapping v1 in
    let c_v2 = replace_with_constant mapping v2 in  (*Need to enforce constant is of type float?*)
    Knorm.FAdd (c_v1, c_v2)
  | Knorm.FSub (v1, v2) ->
    let c_v1 = replace_with_constant mapping v1 in
    let c_v2 = replace_with_constant mapping v2 in
    Knorm.FSub (c_v1, c_v2)
  | Knorm.FMul (v1, v2) ->
    let c_v1 = replace_with_constant mapping v1 in
    let c_v2 = replace_with_constant mapping v2 in
    Knorm.FMul (c_v1, c_v2)
  | Knorm.FDiv (v1, v2) ->
    let c_v1 = replace_with_constant mapping v1 in
    let c_v2 = replace_with_constant mapping v2 in
    Knorm.FDiv (c_v1, c_v2)
  | _ -> failwith "Not implemented"
