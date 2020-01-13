open Asml

let ref_counter x =
  let counter = ref x in
  fun () ->
    counter := !counter + 4;
    !counter

let rec t_to_reg fn_name t var_reg count =
  match t with
  | Ans e -> var_reg
  | Let ((variable, _), exp, t2) -> (
      let matched_value = List.assoc_opt (fn_name ^ "." ^ variable) var_reg in
      match matched_value with
      | None ->
          t_to_reg fn_name t2
            (var_reg @ [ (fn_name ^ "." ^ variable, count ()) ])
            count
      | Some a -> t_to_reg fn_name t2 var_reg count )

let rec lfu_to_reg_rec lfu =
  match lfu with
  | hd :: rest ->
      (let new_count = ref_counter 0 in
       t_to_reg hd.name hd.body [] new_count)
      @ lfu_to_reg_rec rest
  | [] -> []

let program_to_reg pg var_reg =
  let count = ref_counter 0 in
  match pg with
  | Program (lfl, lfu, t) -> (
      lfu_to_reg_rec lfu
      @
      (* match lfl with
    | _ -> "float not implemented yet"
    match lfu with
    | _ -> "fun list not implemented yet" *)
      match t with
      | Let ((variable, _), exp, t2) -> t_to_reg "main" t var_reg count
      | _ -> [] )

let modify_variable fn_name variable var_reg =
  match List.assoc_opt (fn_name ^ "." ^ variable) var_reg with
  | Some a -> string_of_int a
  | None ->
      failwith
        ( "Variable " ^ fn_name ^ "." ^ variable
        ^ " does not exist in association list." )

let rec modify_variable_list fn_name variable_list var_reg =
  match variable_list with
  | hd :: rest ->
      modify_variable fn_name hd var_reg
      :: modify_variable_list fn_name rest var_reg
  | [] -> variable_list

(* String.concat "" ["[fp, "; string_of_int (snd (List.find (fun s -> fst s
 * = variable) var_reg)); "]"] *)

let modify_exp fn_name exp var_reg =
  match exp with
  | Add (s1, s2) -> (
      match s2 with
      | Var v ->
          Add
            ( modify_variable fn_name s1 var_reg,
              Var (modify_variable fn_name v var_reg) )
      | _ -> Add (modify_variable fn_name s1 var_reg, s2) )
  | Sub (s1, s2) -> (
      match s2 with
      | Var v ->
          Sub
            ( modify_variable fn_name s1 var_reg,
              Var (modify_variable fn_name v var_reg) )
      | _ -> Sub (modify_variable fn_name s1 var_reg, s2) )
  | CallDir (label, args) ->
      CallDir (label, modify_variable_list fn_name args var_reg)
  | _ -> exp

let rec modify_t fn_name t var_reg =
  match t with
  | Let ((variable, typ), exp, t2) ->
      Let
        ( (modify_variable fn_name variable var_reg, typ),
          modify_exp fn_name exp var_reg,
          modify_t fn_name t2 var_reg )
  | Ans e -> Ans (modify_exp fn_name e var_reg)

let modify_program pg var_reg =
  match pg with
  | Program (lfu, lfl, t) -> Program (lfu, lfl, modify_t "main" t var_reg)
