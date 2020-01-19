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
  | MkCls of (Id.t * Type.t) * (Id.l * (Id.t * Type.t option) list) * t  (*Added option but shouldn't*)
  | AppCls of Id.t * Id.t list
  | AppDir of Id.l * Id.t list

type fundef = {
  name : Id.l * Type.t;
  args : (Id.t * Type.t) list;
  formal_fv : (Id.t * Type.t option) list; (*Shouldn't but will add option. type t option*)
  body : t;
}

type prog = Prog of fundef list * t

(*list of top-level functions*)
let top_level = ref []

(* searches for and return the value of var in set*)
let find set var = List.assoc_opt var set

(*Returns a list of elements that are in a but not in b. In this case, the free variables*)
let difference a b = List.filter (fun i -> not (List.mem i b)) a

(*find the FVs in an expression. It is required for deciding whether to use apply direct or apply closure*)
let rec find_fv expr =
  match expr with
  | Unit -> []
  | Int i -> []
  | Float f -> []
  | Add (v1, v2) -> [v1; v2]
  | Sub (v1, v2) -> [v1; v2]
  | FAdd (v1, v2) -> [v1; v2]
  | FSub (v1, v2) -> [v1; v2]
  | FMul (v1, v2) -> [v1; v2]
  | FDiv (v1, v2) -> [v1; v2]
  | Let ((id, typ), e1, e2) -> (find_fv e1) @ (List.filter (fun id' -> not(id = id')) (find_fv e2))
  | Var x -> [x]
  | IfEq ((v1, v2), e1, e2) ->  [v1; v2] @ ((find_fv e1) @ (find_fv e2))
  | IfLe ((v1, v2), e1, e2) ->  [v1; v2] @ ((find_fv e1) @ (find_fv e2))
  | LetTuple (vars, def, body) ->
      ((find_fv def) @ (difference (find_fv body) (List.map (fun (id, _t) -> id) vars)))
  | Array (v1, v2) -> [v1; v2]
  | Tuple (tups) -> tups
  | Get (v1,v2) -> [v1; v2]
  | Put (v1, v2, v3) -> [v1; v2; v3]
  | MkCls ((idx, typ), (lbl, vars), e) -> List.filter (fun id' -> not(idx = id')) ((List.map (fun (id, _t) -> id) vars) @ (find_fv e)) (*remove idx*)
  | AppCls (f, args) -> f :: args
  | AppDir (_, args) -> args


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
            let fun_label = f ^ Id.genid () in
            AppDir (fun_label, args)
        | false -> AppCls(f, args)

      )
    (*Function is initially assumed to contain no FV and added to known_fun + top_level. Then, the function body is converted first
    and the let_body after.*)
    | Knorm.LetRec ({ Knorm.name = (fun_id, fun_typ); Knorm.args; Knorm.body = fun_body }, let_body)
      ->
        let previous_top_level = !top_level in

        (*First step- function is initially assumed to contain no FV and added to known_fun + top_level. Then, the function body is converted*)
        let new_known_fun = (fun_id :: known_fun) in
        let new_var_env = args @ ((fun_id, fun_typ) :: var_env) in
        let converted_fun_body = convert fun_body new_known_fun new_var_env in
        let fun_label = fun_id ^ Id.genid () in
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
        let convert_let_body fvars =  (*fvars = list of FVs*)
          let clbody = convert let_body new_known_fun new_var_env in  (*convert let body*)
          (*check if function appears as a value in let_body by checking if it is part of let_body's list of FVs*)
          let cls_rep_check = List.mem fun_id (find_fv clbody) in
          (match cls_rep_check with
           | true -> MkCls ((fun_id, fun_typ), (fun_label, fvars), clbody)
           | false -> failwith "To do- implement sth in failed cls_rep_check case")
            (*Format.eprintf "Function appears as a lable in let_body. No need for MkCls"*)
        in

        (*Second step - Check if fun_body truly doesn't have free variables*)
        let arg_ids = List.map (fun (id, _t) -> id) args in
        (*compare Fvs found in fun_body with function arguments- the difference is the FV list*)
        let diff_result = difference (find_fv converted_fun_body) arg_ids in
        ( match diff_result with
        (*if function has no free variables, convert let_body*)
        | [] -> convert_let_body []
        | hd :: tl ->
          (*otherwise, restore known_funct and top_level to their previous state; reconvert fun_body; get actual FVs list  and then add funtion to top_level*)
            top_level := previous_top_level;
            let cfbody = convert fun_body known_fun (args @ var_env) in
            let f_arg_ids =  List.map (fun (id, _t) -> id) ((fun_id, fun_typ) :: args) in
            let fv_ids = difference (find_fv cfbody) f_arg_ids in (*list of FVs*)
            let fvs = List.map (fun fv_id -> (fv_id, (find new_var_env fv_id))) fv_ids in (*current mapping of FVs in var_env*)
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
      LetTuple (var, convert def known_fun var_env, convert body known_fun (var @ var_env))
    | Knorm.Let ((id, typ), def, body) ->
      Let ((id, typ), convert def known_fun ((id, typ) :: var_env), convert body known_fun ((id, typ) :: var_env))
    (*For all other expressions, return*)
    | Knorm.Unit -> Unit
    | Knorm.Int i -> Int i
    | Knorm.Float f -> Float f
    | Knorm.Add (v1, v2) -> Add (v1, v2)
    | Knorm.Sub (v1, v2) -> Sub (v1, v2)
    | Knorm.FAdd (v1, v2) -> FAdd (v1, v2)
    | Knorm.FSub (v1, v2) -> FSub (v1, v2)
    | Knorm.FMul (v1, v2) -> FMul (v1, v2)
    | Knorm.FDiv (v1, v2) -> FDiv (v1, v2)
    | Knorm.Var x -> Var x
    | Knorm.Array (v1, v2) -> Array (v1, v2)
    | Knorm.Tuple (tups) -> Tuple (tups)
    | Knorm.Get (v1,v2) -> Get (v1, v2)
    | Knorm.Put (v1, v2, v3) -> Put (v1, v2, v3)

(*wrapper function for convert function above
  let con exp = convert exp [ ("print_int", "min_caml_print_int") ] [] *)

(*Convert Closure.t to Closure.prog*)
let rec prog_of_knorm exp =
  (* If compiling multiple files, clean up previous fundefs *)
  top_level := [];

  let main_body = convert exp [("print_int", "min_caml_print_int")] [] in
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

  let fun_labels =
    List.map
      (fun { name = fun_label, _; _ } -> Printf.sprintf "%s" fun_label)
      fundefs
  in
  let fun_labels_string = String.concat "\n" fun_labels in
  Printf.sprintf "Function labels:\n"
  ^ fun_labels_string ^ "\n" ^ to_string' main_body
