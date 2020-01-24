(*Closure converted types for expresions (t), top-level functions (fundef), and porgrams (prog)*)
type t =
  | Unit
  | Int of int
  | Float of float
  | Add of Id.t * Id.t
  | Sub of Id.t * Id.t
  | FAdd of Id.t * Id.t
  | FSub of Id.t * Id.t
  | FMul of Id.t * Id.t
  | FDiv of Id.t * Id.t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | IfEq of (Id.t * Id.t) * t * t
  | IfLe of (Id.t * Id.t) * t * t
  | Tuple of Id.t list
  | LetTuple of (Id.t * Type.t) list * t * t
  | Array of Id.t * Id.t
  | Get of Id.t * Id.t
  | Put of Id.t * Id.t * Id.t
  | MkCls of (Id.t * Type.t) * (Id.l * (Id.t * Type.t) list) * t
  | AppCls of Id.t * Id.t list
  | AppDir of Id.l * Id.t list

type fundef = {
  name : Id.l * Type.t;
  args : (Id.t * Type.t) list;
  formal_fv : (Id.t * Type.t) list;
  body : t;
}

type prog = Prog of fundef list * t

(*list of top-level functions*)
let top_level = ref []

(* searches for and return the value of var in set*)
let find set var = List.assoc_opt var set

(*Returns a list of elements that are in a but not in b. In this case, the free variables*)
let difference a b = List.filter (fun i -> not (List.mem i b)) a

(* copied verbatim from OCaml stdlib. Some team members do not have up-to-date ocamlc versions... *)
let my_filter_map f =
  let rec aux accu = function
    | [] -> List.rev accu
    | x :: l -> (
        match f x with None -> aux accu l | Some v -> aux (v :: accu) l )
  in
  aux []

(*find the FVs in an expression. It is required for deciding whether to use apply direct or apply closure*)
let rec find_fv expr bound_variables =
  let find_fv_list vars env =
    List.fold_left
      (fun acc var ->
        if not (List.mem var bound_variables) then var :: acc else acc)
      [] vars
  in
  match expr with
  | Unit -> []
  | Int i -> []
  | Float f -> []
  | Add (v1, v2)
  | Sub (v1, v2)
  | FAdd (v1, v2)
  | FSub (v1, v2)
  | FMul (v1, v2)
  | FDiv (v1, v2) ->
      find_fv_list [ v1; v2 ] bound_variables
  | Let ((id, typ), e1, e2) ->
      let bound_variables' = id :: bound_variables in
      find_fv e1 bound_variables @ find_fv e2 bound_variables'
  | Var x -> if List.mem x bound_variables then [] else [ x ]
  | IfEq ((v1, v2), e1, e2) | IfLe ((v1, v2), e1, e2) ->
      find_fv_list [ v1; v2 ] bound_variables
      @ find_fv e1 bound_variables @ find_fv e2 bound_variables
  | LetTuple (vars, def, body) ->
      let bound_variables' =
        List.map (fun (id, _t) -> id) vars @ bound_variables
      in
      find_fv def bound_variables @ find_fv body bound_variables'
  | Tuple tups -> find_fv_list tups bound_variables
  | Array (v1, v2) -> find_fv_list [ v1; v2 ] bound_variables
  | Get (v1, v2) -> find_fv_list [ v1; v2 ] bound_variables
  | Put (v1, v2, v3) -> find_fv_list [ v1; v2; v3 ] bound_variables
  | AppDir (_f, args) -> find_fv_list args bound_variables
  | AppCls (f, args) -> find_fv_list (f :: args) bound_variables
  | MkCls ((fun_id, _fun_typ), (fun_label, fvars), cont) ->
      let bound_variables' = fun_id :: bound_variables in
      let fvar_ids = List.map (fun (id, _typ) -> id) fvars in
      find_fv_list fvar_ids bound_variables @ find_fv cont bound_variables'

(*Convert Knorm.t to Closure.t*)
(*Added another argument var_env (variable environment) to function convert. This is to enable us
  to find the appropriate free variable (FV) when making closures*)
let rec convert exp known_fun var_env =
  match exp with
  (*if function f is part of known_fun(set of functions known to contain no FV), apply direct conversion. Otherwise, apply closure conversion*)
  | Knorm.App (f, args) -> (
      (*let fun_label = find known_fun f in
        match fun_label with
        | Some label -> AppDir (label, args)
          | None ->  AppCls(f, args) *)
      let f_is_known = List.mem f known_fun in
      match f_is_known with
      | true ->
          let fun_label = Id.label_of_id f in
          AppDir (fun_label, args)
      | false -> AppCls (f, args) )
  (*Function is initially assumed to contain no FV and added to known_fun + top_level. Then, the function body is converted first
    and the let_body after.*)
  | Knorm.LetRec
      ( { Knorm.name = fun_id, fun_typ; Knorm.args; Knorm.body = fun_body },
        let_body ) -> (
      let previous_top_level = !top_level in

      (*First step- function is initially assumed to contain no FV and added to known_fun + top_level. Then, the function body is converted*)
      let new_known_fun = fun_id :: known_fun in
      let new_var_env = (fun_id, fun_typ) :: var_env in
      let converted_fun_body = convert fun_body new_known_fun new_var_env in
      let fun_label = Id.label_of_id fun_id in
      top_level :=
        {
          name = (fun_label, fun_typ);
          args;
          formal_fv = [];
          body = converted_fun_body;
        }
        :: !top_level;

      (*Third step- if a function is returned as a value in let_body (irrespective of whether it contains FVs or not),
            represent it as a closure. Otherwise, do not create closure for it. *)
      let convert_let_body fvars =
        (*fvars = list of FVs*)

        (*check if function appears as a value in let_body by checking if it is part of let_body's list of FVs*)
        let rec fun_id_occurs_as_variable id = function
          | Knorm.Var id' -> id = id'
          | Knorm.Let ((id', _typ), e1, e2) ->
              fun_id_occurs_as_variable id e1
              || if id = id' then false else fun_id_occurs_as_variable id e2
          | Knorm.LetTuple (vars, def_body, let_body) ->
              fun_id_occurs_as_variable id def_body
              ||
              let var_ids = List.map (fun (id, _typ) -> id) vars in
              if List.mem id var_ids then false
              else fun_id_occurs_as_variable id let_body
          | Knorm.LetRec
              ({ Knorm.name = id', _typ; args; body = fun_body }, let_body)
            when id = id' ->
              false
          | Knorm.LetRec
              ({ Knorm.name = id', _typ; args; body = fun_body }, let_body) ->
              let arg_ids = List.map (fun (arg_id, _typ) -> arg_id) args in
              fun_id_occurs_as_variable id fun_body
              ||
              if List.mem id arg_ids then false
              else fun_id_occurs_as_variable id let_body
          | Knorm.IfEq ((_, _), e1, e2) | Knorm.IfLe ((_, _), e1, e2) ->
              fun_id_occurs_as_variable id e1 || fun_id_occurs_as_variable id e2
          | Knorm.App (_, args) -> List.mem id args
          | Knorm.Tuple elements -> List.mem id elements
          | Knorm.Array (_arr_size, array_init) -> id = array_init
          | Knorm.Get (_, _) -> false
          | Knorm.Put (_arr_name, _index, new_element) -> id = new_element
          | Knorm.Unit | Knorm.Int _ | Knorm.Float _ | Knorm.Add _
          | Knorm.Sub _ | Knorm.FAdd _ | Knorm.FSub _ | Knorm.FMul _
          | Knorm.FDiv _ ->
              false
        in

        let cls_rep_check = fun_id_occurs_as_variable fun_id let_body in
        if List.length fvars > 0 || cls_rep_check then (
          top_level := previous_top_level;

          let alpha_mappings =
            List.map (fun (id, typ) -> (id, Id.genid ())) fvars
          in
          let alpha_vars =
            List.map2
              (fun (_, new_id) (_, typ) -> (new_id, typ))
              alpha_mappings fvars
          in
          let cfbody =
            convert
              (Alpha.convert fun_body alpha_mappings)
              known_fun
              ((fun_id, fun_typ) :: (alpha_vars @ var_env))
          in
          let new_closure =
            {
              name = (fun_label, fun_typ);
              args;
              formal_fv = alpha_vars;
              body = cfbody;
            }
          in

          top_level := new_closure :: !top_level;

          let clbody =
            convert let_body known_fun ((fun_id, fun_typ) :: var_env)
          in
          MkCls ((fun_id, fun_typ), (fun_label, fvars), clbody) )
        else convert let_body new_known_fun new_var_env
        (*Format.eprintf "Function appears as a lable in let_body. No need for MkCls"*)
      in

      (*Second step - Check if fun_body truly doesn't have free variables*)
      let arg_ids = List.map (fun (id, _t) -> id) args in
      (*compare Fvs found in fun_body with function arguments- the difference is the FV list*)
      let bound_variables = fun_id :: arg_ids in
      let fv_ids = find_fv converted_fun_body bound_variables in
      match fv_ids with
      (*if function has no free variables, convert let_body*)
      | [] -> convert_let_body []
      | _ ->
          (*otherwise, restore known_funct and top_level to their previous state; reconvert fun_body; get actual FVs list  and then add funtion to top_level*)
          top_level := previous_top_level;
          let cfbody =
            convert fun_body known_fun (((fun_id, fun_typ) :: args) @ var_env)
          in

          (*list of FVs*)
          let fvs =
            my_filter_map
              (fun fv_id ->
                match find new_var_env fv_id with
                | None ->
                    Printf.eprintf
                      "Free variable %s is actually undefind in current \
                       environment, while processing %s.\n"
                      (Id.to_string fv_id) fun_id;
                    exit 1
                | Some typ -> Some (fv_id, typ))
              fv_ids
          in
          (*current mapping of FVs in var_env*)
          top_level :=
            {
              name = (fun_label, fun_typ);
              args;
              formal_fv = fvs;
              body = cfbody;
            }
            :: !top_level;
          convert_let_body fvs )
  (*For all if statements, convert the body - e1 and e2*)
  | Knorm.IfEq ((v1, v2), e1, e2) ->
      IfEq ((v1, v2), convert e1 known_fun var_env, convert e2 known_fun var_env)
  | Knorm.IfLe ((v1, v2), e1, e2) ->
      IfLe ((v1, v2), convert e1 known_fun var_env, convert e2 known_fun var_env)
  (*For LetTuple and Let variable declariation, convert definition and body*)
  | Knorm.LetTuple (var, def, body) ->
      LetTuple
        ( var,
          convert def known_fun var_env,
          convert body known_fun (var @ var_env) )
  | Knorm.Let ((id, typ), def, body) ->
      Let
        ( (id, typ),
          convert def known_fun var_env,
          convert body known_fun ((id, typ) :: var_env) )
  (*For all other expressions, return*)
  | Knorm.Unit -> Unit
  | Knorm.Int i -> Int i
  | Knorm.Float f ->
      let id = Id.genid () in
      Let((id, Type.Float), Float f, Var id)
  | Knorm.Add (v1, v2) -> Add (v1, v2)
  | Knorm.Sub (v1, v2) -> Sub (v1, v2)
  | Knorm.FAdd (v1, v2) -> FAdd (v1, v2)
  | Knorm.FSub (v1, v2) -> FSub (v1, v2)
  | Knorm.FMul (v1, v2) -> FMul (v1, v2)
  | Knorm.FDiv (v1, v2) -> FDiv (v1, v2)
  | Knorm.Var x -> Var x
  | Knorm.Array (v1, v2) -> Array (v1, v2)
  | Knorm.Tuple tups -> Tuple tups
  | Knorm.Get (v1, v2) -> Get (v1, v2)
  | Knorm.Put (v1, v2, v3) -> Put (v1, v2, v3)

(*wrapper function for convert function above
  let con exp = convert exp [ ("print_int", "min_caml_print_int") ] [] *)

(*Convert Closure.t to Closure.prog*)
let rec prog_of_knorm exp =
  (* If compiling multiple files, clean up previous fundefs *)
  top_level := [];

  let main_body =
    convert exp
      [
        "print_int";
        "print_newline";
        "sin";
        "cos";
        "sqrt";
        "abs_float";
        "int_of_float";
        "float_of_int";
        "truncate";
      ]
      []
  in
  Prog (!top_level, main_body)

let rec infix_to_string to_s l op =
  match l with
  | [] -> ""
  | [ x ] -> to_s x
  | hd :: tl -> to_s hd ^ op ^ infix_to_string to_s tl op

(*Convert Closure.t to string*)
let rec to_string' exp =
  match exp with
  | Unit -> "()"
  | Int i -> string_of_int i
  | Float f -> string_of_float f
  | Add (e1, e2) -> Printf.sprintf "(%s + %s)" e1 e2
  | Sub (e1, e2) -> Printf.sprintf "(%s - %s)" e1 e2
  | FAdd (e1, e2) -> Printf.sprintf "(%s +. %s)" e1 e2
  | FSub (e1, e2) -> Printf.sprintf "(%s -. %s)" e1 e2
  | FMul (e1, e2) -> Printf.sprintf "(%s *. %s)" e1 e2
  | FDiv (e1, e2) -> Printf.sprintf "(%s /. %s)" e1 e2
  | Var id -> Id.to_string id
  | Tuple l -> Printf.sprintf "(%s)" (infix_to_string Id.to_string l ", ")
  | AppDir (e1, le2) ->
      Printf.sprintf "(%s %s)" (Id.to_string e1)
        (infix_to_string Id.to_string le2 " ")
  | AppCls (e1, le2) ->
      Printf.sprintf "(%s %s)" (Id.to_string e1)
        (infix_to_string Id.to_string le2 " ")
  | MkCls ((id, _t), (label, vs), e2) ->
      Printf.sprintf "(let %s = %s %s in %s)" (Id.to_string id)
        (Id.to_string label)
        (infix_to_string (fun (x, _) -> Id.to_string x) vs " ")
        (to_string' e2)
  | Let ((id, _t), e1, e2) ->
      Printf.sprintf "(let %s = %s in %s)" (Id.to_string id) (to_string' e1)
        (to_string' e2)
  | LetTuple (l, e1, e2) ->
      Printf.sprintf "(let (%s) = %s in %s)"
        (infix_to_string (fun (x, _) -> Id.to_string x) l ", ")
        (to_string' e1) (to_string' e2)
  | IfEq ((id1, id2), e1, e2) ->
      Printf.sprintf "(if %s  = %s then %s else %s)" (Id.to_string id1)
        (Id.to_string id2) (to_string' e1) (to_string' e2)
  | IfLe ((id1, id2), e1, e2) ->
      Printf.sprintf "(if %s  <= %s then %s else %s)" (Id.to_string id1)
        (Id.to_string id2) (to_string' e1) (to_string' e2)
  | Get (e1, e2) -> Printf.sprintf "%s.(%s)" (Id.to_string e1) (Id.to_string e2)
  | Put (e1, e2, e3) ->
      Printf.sprintf "(%s.(%s) <- %s)" (Id.to_string e1) (Id.to_string e2)
        (Id.to_string e3)
  | Array (e1, e2) ->
      Printf.sprintf "(Array.create %s %s)" (Id.to_string e1) (Id.to_string e2)

(*Convert Closure.prog to string*)
let prog_to_string prog =
  let (Prog (fundefs, main_body)) = prog in

  let fundefs_string =
    List.map
      (fun { name = fun_label, _fun_typ; args; formal_fv; body } ->
        Printf.sprintf "Label: %s Arguments: %s Free variables: %s\n%s\n"
          fun_label
          (List.map (fun (arg_id, _arg_typ) -> arg_id) args |> String.concat " ")
          ( List.map (fun (fv_id, _fv_typ) -> fv_id) formal_fv
          |> String.concat " " )
          (to_string' body))
      fundefs
  in
  String.concat "\n" fundefs_string ^ "\nMain:\n" ^ to_string' main_body
