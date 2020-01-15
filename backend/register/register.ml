open Asml

type counttype = Decr | Peak

let ref_counter x =
  let counter = ref x in
  fun a ->
    match a with
    | Decr ->
        counter := !counter - 4;
        !counter
    | Peak -> !counter

(*
let ref_counter_pos x =
  let counter = ref x in
  fun () ->
    counter := !counter + 4;
    !counter
    *)

let rec exp_to_reg exp fn_name count =
  match exp with
  | IfEq (_, _, t1, t2) ->
      let count1 = ref_counter (count Peak) in
      let count2 = ref_counter (count Peak) in
      t_to_reg fn_name t1 [] count1 @ t_to_reg fn_name t2 [] count2
  | _ -> []

and t_to_reg fn_name t var_reg count =
  match t with
  | Ans e -> var_reg @ exp_to_reg e fn_name count
  | Let ((variable, _), exp, t2) -> (
      let var_reg_exp = exp_to_reg exp fn_name count in
      let matched_value = List.assoc_opt (fn_name ^ "." ^ variable) var_reg in
      match matched_value with
      | None ->
          t_to_reg fn_name t2
            ( var_reg
            @ [ (fn_name ^ "." ^ variable, count Decr) ]
            @ var_reg_exp )
            count
      | Some a -> t_to_reg fn_name t2 var_reg count @ var_reg_exp )

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

let rec modify_args_reg fn_name args var_reg count =
  match args with
  | var :: rest ->
      modify_args_reg fn_name rest
        (var_reg @ [ (fn_name ^ "." ^ var, count Decr) ])
        count
  | [] -> var_reg

let rec modify_fn_t fu var_reg =
  let len = List.length fu.args in
  let newcount = ref_counter ((len + 2) * 4) in
  let args_reg = modify_args_reg fu.name fu.args [] newcount in
  List.iter (fun (s1, s2) -> Printf.printf "(%s, %d)" s1 s2) args_reg;
  {
    name = fu.name;
    args = fu.args;
    body = modify_t fu.name fu.body (var_reg @ args_reg);
  }

let modify_program pg var_reg =
  match pg with
  | Program (lfl, lfu, t) ->
      Program
        ( lfl,
          List.map (fun s -> modify_fn_t s var_reg) lfu,
          modify_t "main" t var_reg )
