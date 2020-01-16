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
  | MkCls of (Id.t * Type.t) * (Id.l * (Id.t * Type.t) list) * t*)
  | AppCls of (Id.t * Type.t) * (Id.t * Type.t) list*)
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

(* function searches for and return the value of var in set*)
let find set var = List.assoc_opt var set

(*Convert Knorm.t to Closure.t*)
let rec convert (exp : Knorm.t) known_fun =
  match exp with
  (*if function f is part of known_fun(set of functions known to contain no free variables (FV)), apply direct conversion. Otherwise, apply closure conversion*)
  | Knorm.App (f, args) -> (
      let fun_label = find known_fun f in
      match fun_label with
      | Some label -> AppDir (label, args)
      | None ->  AppCls(f, args) (*failwith "Apply closure not implemented" )*)
  (*For this case, function is initially assumed to contain no FV and added to known_fun + top_level. Then, the function body is converted first
  and the let_body is converted after with the updated known_fun set*)
  | Knorm.LetRec ({ name = fun_id, fun_typ; args; body = fun_body }, let_body)
    ->
      let fun_label = fun_id ^ Id.genid () in
      top_level :=
        {
          name = (fun_label, fun_typ);
          args;
          formal_fv = [];
          body = convert fun_body ((fun_id, fun_label) :: known_fun);
        }
        :: !top_level;
      (*check if it has a fv, how?*) (*Maybe *)
      (*if no fv continue with line below*)
      convert let_body ((fun_id, fun_label) :: known_fun)
      (*otherwise remove fun_label from  known_fun and top level, convert e1 again*)
      (*last opt, if function is returned as a value (just id.t without args), make and apply closure*)
      (*otherwise, if f is called as a function (type id.L with args) only, apply direct
      that is if function f is not an id.t in e2*)
  (*For all if statements, convert the body - e1 and e2*)
  | Knorm.IfEq ((v1, v2), e1, e2) ->
      IfEq ((v1, v2), convert e1 known_fun, convert e2 known_fun)
  | Knorm.IfLe ((v1, v2), e1, e2) ->
      IfLe ((v1, v2), convert e1 known_fun, convert e2 known_fun)
  (*For LetTuple and Let variable declariation, convert definition and body*)
  | Knorm.LetTuple (var, def, body) ->
      LetTuple (var, convert def known_fun, convert body known_fun)
  | Knorm.Let ((id, typ), def, body) ->
    Let ((id, typ), convert def known_fun, convert body known_fun)
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

(*wrapper function for convert function above*)
let con exp = convert exp [ ("print_int", "min_caml_print_int") ]

(*Convert Closure.t to Closure.prog*)
let rec prog_of_knorm exp =
  (* If compiling multiple files, clean up previous fundefs *)
  top_level := [];

  let main_body = convert exp [ ("print_int", "min_caml_print_int") ] in
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
