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
  (* cf. BLE branch if less than or equal *)
  | IfLe of (Id.t * Id.t) * t * t
  | Tuple of Id.t list
  | LetTuple of (Id.t * Type.t) list * t * t
  | Array of Id.t * Id.t
  | Get of Id.t * Id.t
  | Put of Id.t * Id.t * Id.t
  (*| MkCls of (Id.t * Type.t) * (Id.l * (Id.t * Type.t) list) * t*)
  (*| AppCls of (Id.t * Type.t) * (Id.t * Type.t) list*)
  | AppDir of Id.l * Id.t list

type fundef = {
  name : Id.l * Type.t;
  args : (Id.t * Type.t) list;
  formal_fv : (Id.t * Type.t) list;
  body : t;
}

type prog = Prog of fundef list * t

let top_level = ref []

let find set var = List.assoc_opt var set

let rec convert (exp : Knorm.t) known_fun =
  match exp with
  | Knorm.App (f, args) -> (
      let fun_label = find known_fun f in
      match fun_label with
      | Some label -> AppDir (label, args)
      | None -> failwith "not there yet" )
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
      convert let_body ((fun_id, fun_label) :: known_fun)
  | Knorm.IfEq ((v1, v2), e1, e2) ->
      IfEq ((v1, v2), convert e1 known_fun, convert e2 known_fun)
  | Knorm.IfLe ((v1, v2), e1, e2) ->
      IfLe ((v1, v2), convert e1 known_fun, convert e2 known_fun)
  | Knorm.LetTuple (var, def, body) ->
      LetTuple (var, convert def known_fun, convert body known_fun)
  | Knorm.Unit -> Unit
  | Knorm.Int i -> Int i
  | Knorm.Float f -> Float f
  | Knorm.Add (v1, v2) -> Add (v1, v2)
  | Knorm.Sub (v1, v2) -> Sub (v1, v2)
  | Knorm.FAdd (v1, v2) -> FAdd (v1, v2)
  | Knorm.FSub (v1, v2) -> FSub (v1, v2)
  | Knorm.FMul (v1, v2) -> FMul (v1, v2)
  | Knorm.FDiv (v1, v2) -> FDiv (v1, v2)
  | Knorm.Let ((id, typ), def, body) ->
      Let ((id, typ), convert def known_fun, convert body known_fun)
  | Knorm.Var x -> Var x
  | _ -> failwith "not there yet"

let con exp = convert exp [ ("print_int", "min_caml_print_int") ]

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
