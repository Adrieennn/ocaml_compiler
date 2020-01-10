open Asml

let ref_counter x =
  let counter = ref (x-4) in
  fun () ->
    counter := !counter + 4;
    !counter

let rec t_to_reg t var_reg =
  match t with
  | Ans e -> var_reg
  | Let ((variable, _), exp, t2) -> t_to_reg t2 (var_reg @ [ (variable, 3) ])

let program_to_reg pg var_reg =
  match pg with
  | Program (lfu, lfl, t) -> (
      (* match lfl with
    | _ -> "float not implemented yet"
    match lfu with
    | _ -> "fun list not implemented yet" *)
      match t with
      | Let ((variable, _), exp, t2) -> t_to_reg t var_reg
      | _ -> [] )

