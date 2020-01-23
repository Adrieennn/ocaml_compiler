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

(* function that takes an expression and outputs an association list with
 * variable names and their corresponding offsets.
 * Needed for IF statements where new variables can be defined *)
let rec exp_to_reg exp fn_name count =
  match exp with
  | IfLEq (_, _, t1, t2) ->
      let count1 = ref_counter (count Peak) in
      let count2 = ref_counter (count Peak) in
      t_to_reg fn_name t1 [] count1 @ t_to_reg fn_name t2 [] count2
  | IfEq (_, _, t1, t2) ->
      let count1 = ref_counter (count Peak) in
      let count2 = ref_counter (count Peak) in
      t_to_reg fn_name t1 [] count1 @ t_to_reg fn_name t2 [] count2
  | _ -> []

(* function that takes a function body, an association list and appends
 * to the list the variable names in the body with their corresponding fp
 * offsets *)
and t_to_reg fn_name t var_reg count =
  match t with
  | Ans e -> var_reg @ exp_to_reg e fn_name count
  | Let ((variable, _), exp, t2) -> (
      let var_reg_exp = exp_to_reg exp fn_name count in
      let matched_value = List.assoc_opt (fn_name ^ "." ^ variable) var_reg in
      match matched_value with
      | None ->
          t_to_reg fn_name t2
            (var_reg @ [ (fn_name ^ "." ^ variable, count Decr) ] @ var_reg_exp)
            count
      | Some a ->
          let new_var_reg =
            List.map
              (fun (s1, s2) ->
                if s1 = fn_name ^ "." ^ variable then (s1, count Decr)
                else (s1, s2))
              var_reg
          in
          t_to_reg fn_name t2 new_var_reg count @ var_reg_exp )

(* function that takes a list of function definitions and outputs an
 * association list of variable names and their corresponding fp offsets *)
let rec lfu_to_reg_rec lfu =
  match lfu with
  | hd :: rest ->
      (let new_count = ref_counter 0 in
       t_to_reg hd.name hd.body [] new_count)
      @ lfu_to_reg_rec rest
  | [] -> []

(* function that takes a program and outputs an association list of variable
 * names and their corresponding fp offsets *)
let program_to_reg pg =
  let count = ref_counter 0 in
  match pg with
  | Program (lfl, lfu, t) -> (
      lfu_to_reg_rec lfu
      @
      match t with
      | Let ((variable, _), exp, t2) -> t_to_reg "main" t [] count
      | _ -> [] )

(* function that takes a variable name and an association list between variable
 * names and fp offsets; it modifies the variable name with its corresponding
 * fp offset from the list *)
let modify_variable fn_name variable var_reg =
  match List.assoc_opt (fn_name ^ "." ^ variable) var_reg with
  | Some a -> string_of_int a
  | None ->
      failwith
        ( "Variable " ^ fn_name ^ "." ^ variable
        ^ " does not exist in association list." )

(* function that takes a list of variables and an association list between variable
 * names and fp offsets; it modifies all variables in the list with their
 * corresponding fp offsets from the association list *)
let rec modify_variable_list fn_name variable_list var_reg =
  match variable_list with
  | hd :: rest ->
      modify_variable fn_name hd var_reg
      :: modify_variable_list fn_name rest var_reg
  | [] -> variable_list

(* function that takes an expression and an association list between variable
 * names and fp offsets; it modifies all occurrences of variables in the
 * expression with their corresponding fp offsets from the list *)
let rec modify_exp fn_name exp var_reg =
  match exp with
  | Var v ->
      if v.[0] = '_' then Label v else Var (modify_variable fn_name v var_reg)
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
  | FAdd (s1, s2) ->
      FAdd
        (modify_variable fn_name s1 var_reg, modify_variable fn_name s2 var_reg)
  | FSub (s1, s2) ->
      FSub
        (modify_variable fn_name s1 var_reg, modify_variable fn_name s2 var_reg)
  | FMul (s1, s2) ->
      FMul
        (modify_variable fn_name s1 var_reg, modify_variable fn_name s2 var_reg)
  | FDiv (s1, s2) ->
      FDiv
        (modify_variable fn_name s1 var_reg, modify_variable fn_name s2 var_reg)
  | CallDir (label, args) ->
      CallDir (label, modify_variable_list fn_name args var_reg)
  | CallCls (id, args) ->
      CallCls
        ( modify_variable fn_name id var_reg,
          modify_variable_list fn_name args var_reg )
  | IfLEq (s1, s2, t1, t2) -> (
      let mod_s1 = modify_variable fn_name s1 var_reg in
      let mod_t1 = modify_t fn_name t1 var_reg in
      let mod_t2 = modify_t fn_name t2 var_reg in
      match s2 with
      | Int i -> IfLEq (mod_s1, s2, mod_t1, mod_t2)
      | Var v ->
          IfLEq (mod_s1, Var (modify_variable fn_name v var_reg), mod_t1, mod_t2)
      )
  | IfEq (s1, s2, t1, t2) -> (
      let mod_s1 = modify_variable fn_name s1 var_reg in
      let mod_t1 = modify_t fn_name t1 var_reg in
      let mod_t2 = modify_t fn_name t2 var_reg in
      match s2 with
      | Int i -> IfEq (mod_s1, s2, mod_t1, mod_t2)
      | Var v ->
          IfEq (mod_s1, Var (modify_variable fn_name v var_reg), mod_t1, mod_t2)
      )
  | Ld (s1, s2) -> (
      match s2 with
      | Var v ->
          Ld
            ( modify_variable fn_name s1 var_reg,
              Var (modify_variable fn_name v var_reg) )
      | Int i -> Ld (modify_variable fn_name s1 var_reg, s2) )
  | St (s1, s2, s3) -> (
      match s2 with
      | Var v ->
          St
            ( modify_variable fn_name s1 var_reg,
              Var (modify_variable fn_name v var_reg),
              modify_variable fn_name s3 var_reg )
      | Int i ->
          St
            ( modify_variable fn_name s1 var_reg,
              s2,
              modify_variable fn_name s3 var_reg ) )
  | _ -> exp

(* function that takes a function body and an association list between variable
 * names and fp offsets; it modifies all occurrences of variables in the
 * function body with their corresponding fp offsets from the list *)
and modify_t fn_name t var_reg =
  match t with
  | Let ((variable, typ), exp, t2) ->
      Let
        ( (modify_variable fn_name variable var_reg, typ),
          modify_exp fn_name exp var_reg,
          modify_t fn_name t2 var_reg )
  | Ans e -> Ans (modify_exp fn_name e var_reg)

(* function that takes a list of arguments and an association list between variable
 * names and fp offsets; it modifies all occurrences of variables in the
 * list of arguments with their corresponding fp offsets from the list *)
let rec modify_args_reg fn_name args var_reg count =
  match args with
  | var :: rest ->
      modify_args_reg fn_name rest
        (var_reg @ [ (fn_name ^ "." ^ var, count Decr) ])
        count
  | [] -> var_reg

(* function that takes a function definition and an association list between variable
 * names and fp offsets; it modifies all occurrences of variables in the
 * function body and arguments with their corresponding fp offsets from the list *)
let rec modify_fn_t fu var_reg =
  let len = List.length fu.args in
  let newcount = ref_counter ((len + 3) * 4) in
  let args_reg = modify_args_reg fu.name fu.args [] newcount in
  {
    name = fu.name;
    args = fu.args;
    body =
      modify_t fu.name fu.body (var_reg @ args_reg @ [ (fu.name ^ ".%self", 8) ]);
  }

(* function that takes a program and an association list between variable
 * names and fp offsets; it modifies all occurrences of variables in the
 * program with their corresponding fp offsets from the list *)
let modify_program pg var_reg =
  match pg with
  | Program (lfl, lfu, t) ->
      Program
        ( lfl,
          List.map (fun s -> modify_fn_t s var_reg) lfu,
          modify_t "main" t var_reg )
