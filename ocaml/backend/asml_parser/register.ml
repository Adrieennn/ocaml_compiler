open Asml

let ref_counter x =
  let counter = ref x in
  fun () ->
    counter := !counter + 4;
    !counter

let rec t_to_reg t var_reg count =
  match t with
  | Ans e -> var_reg
  | Let ((variable, _), exp, t2) -> (
      let matched_value = List.assoc_opt variable var_reg in
      match matched_value with
      | None -> t_to_reg t2 (var_reg @ [ (variable, count ()) ]) count
      | Some a -> t_to_reg t2 var_reg count )

let program_to_reg pg var_reg =
  let count = ref_counter 0 in
  match pg with
  | Program (lfu, lfl, t) -> (
      (* match lfl with
    | _ -> "float not implemented yet"
    match lfu with
    | _ -> "fun list not implemented yet" *)
      match t with
      | Let ((variable, _), exp, t2) -> t_to_reg t var_reg count
      | _ -> [] )

let modify_variable variable var_reg =
  match List.assoc_opt variable var_reg with
  | Some a -> string_of_int a
  | None ->
      failwith ("Variable " ^ variable ^ " does not exist in association list.")

let rec modify_variable_list variable_list var_reg =
  match variable_list with
  | hd :: rest ->
      modify_variable hd var_reg :: modify_variable_list rest var_reg
  | [] -> variable_list

(* String.concat "" ["[fp, "; string_of_int (snd (List.find (fun s -> fst s
 * = variable) var_reg)); "]"] *)

let modify_exp exp var_reg =
  match exp with
  | Add (s1, s2) -> (
      match s2 with
      | Var v ->
          Add (modify_variable s1 var_reg, Var (modify_variable v var_reg))
      | _ -> Add (modify_variable s1 var_reg, s2) )
  | Sub (s1, s2) -> (
      match s2 with
      | Var v ->
          Sub (modify_variable s1 var_reg, Var (modify_variable v var_reg))
      | _ -> Sub (modify_variable s1 var_reg, s2) )
  | CallDir (label, args) -> CallDir (label, modify_variable_list args var_reg)
  | _ -> exp

let rec modify_t t var_reg =
  match t with
  | Let ((variable, typ), exp, t2) ->
      Let
        ( (modify_variable variable var_reg, typ),
          modify_exp exp var_reg,
          modify_t t2 var_reg )
  | Ans e -> Ans (modify_exp e var_reg)

let modify_program pg var_reg =
  match pg with Program (lfu, lfl, t) -> Program (lfu, lfl, modify_t t var_reg)
