module TypingEnvironment : sig
  type t

  val default : unit -> t

  val add : Id.t * Type.t -> t -> t

  val find_opt : Id.t -> t -> Type.t option
end = struct
  type t = (Id.t * Type.t) list

  let empty () = []

  let add (x : Id.t * Type.t) (env : t) = x :: env

  let find_opt x env = List.assoc_opt x env

  let default () =
    empty () |> add ("print_int", Type.Fun ([ Type.Int ], Type.Unit))
end

module TypingEquation = struct
  type t = Type.t * Type.t

  let to_string eqn =
    let t1, t2 = eqn in
    Printf.sprintf "(%s, %s)" (Type.to_string t1) (Type.to_string t2)
end

let rec gen_equations env exp expected_type =
  match exp with
  | Syntax.Unit -> [ (Type.Unit, expected_type) ]
  | Syntax.Bool _ -> [ (Type.Bool, expected_type) ]
  | Syntax.Int _ -> [ (Type.Int, expected_type) ]
  | Syntax.Float _ -> [ (Type.Float, expected_type) ]
  | Syntax.Not e ->
      let eqs = gen_equations env e Type.Bool in
      (Type.Bool, expected_type) :: eqs
  | Syntax.Neg e ->
      let eqs = gen_equations env e Type.Int in
      (Type.Int, expected_type) :: eqs
  | Syntax.Add (e1, e2) ->
      let eqs1 = gen_equations env e1 Type.Int in
      let eqs2 = gen_equations env e2 Type.Int in
      ((Type.Int, expected_type) :: eqs1) @ eqs2
  | Syntax.Sub (e1, e2) ->
      let eqs1 = gen_equations env e1 Type.Int in
      let eqs2 = gen_equations env e2 Type.Int in
      ((Type.Int, expected_type) :: eqs1) @ eqs2
  | Syntax.FNeg e ->
      let eqs = gen_equations env e Type.Float in
      (Type.Float, expected_type) :: eqs
  | Syntax.FAdd (e1, e2) ->
      let eqs1 = gen_equations env e1 Type.Float in
      let eqs2 = gen_equations env e2 Type.Float in
      ((Type.Float, expected_type) :: eqs1) @ eqs2
  | Syntax.FSub (e1, e2) ->
      let eqs1 = gen_equations env e1 Type.Float in
      let eqs2 = gen_equations env e2 Type.Float in
      ((Type.Float, expected_type) :: eqs1) @ eqs2
  | Syntax.FMul (e1, e2) ->
      let eqs1 = gen_equations env e1 Type.Float in
      let eqs2 = gen_equations env e2 Type.Float in
      ((Type.Float, expected_type) :: eqs1) @ eqs2
  | Syntax.FDiv (e1, e2) ->
      let eqs1 = gen_equations env e1 Type.Float in
      let eqs2 = gen_equations env e2 Type.Float in
      ((Type.Float, expected_type) :: eqs1) @ eqs2
  | Syntax.Eq (e1, e2) ->
      let eqs1 = gen_equations env e1 Type.Int in
      let eqs2 = gen_equations env e2 Type.Int in
      ((Type.Bool, expected_type) :: eqs1) @ eqs2
  | Syntax.LE (e1, e2) ->
      let eqs1 = gen_equations env e1 Type.Int in
      let eqs2 = gen_equations env e2 Type.Int in
      eqs1 @ eqs2
  | Syntax.If (e1, e2, e3) ->
      let eqs1 = gen_equations env e1 Type.Bool in
      let eqs2 = gen_equations env e2 expected_type in
      let eqs3 = gen_equations env e3 expected_type in
      eqs1 @ eqs2 @ eqs3
  | Syntax.Let ((id, t), e1, e2) ->
      let eqs1 = gen_equations env e1 t in
      let eqs2 =
        gen_equations (TypingEnvironment.add (id, t) env) e2 expected_type
      in
      eqs1 @ eqs2
  | Syntax.Var id -> (
      match TypingEnvironment.find_opt id env with
      | None ->
          failwith "Undefined variable in environment"
          (* TODO better error message *)
      | Some typ -> [ (typ, expected_type) ] )
  | Syntax.App (e1, le2) ->
      let arg_typs = List.map (fun _ -> Type.Var (ref None)) le2 in
      let arg_eqs =
        List.map2 (fun e typ -> gen_equations env e typ) le2 arg_typs
        |> List.concat
      in

      let fun_typ = Type.Fun (arg_typs, expected_type) in
      let fun_eqs = gen_equations env e1 fun_typ in

      arg_eqs @ fun_eqs
  | Syntax.LetRec (fd, e) ->
      let { Syntax.name = fun_name; args = fun_args; body = fun_body } = fd in
      let fun_arg_typs = List.map (fun (_id, typ) -> typ) fun_args in

      (* XXX check folds (here and below) *)
      let fun_body_env =
        (* Add function arguments to environment of function body *)
        List.fold_right TypingEnvironment.add (List.rev fun_args) env
        (* Add function name to environment of function body for recursive calls *)
        |> TypingEnvironment.add fun_name
      in
      let fun_body_typ = Type.Var (ref None) in
      let fun_body_eqs = gen_equations fun_body_env fun_body fun_body_typ in

      let let_body_env = TypingEnvironment.add fun_name env in
      let let_body_eqs = gen_equations let_body_env e expected_type in

      let _fun_id, fun_typ = fun_name in
      let fun_typ_eq = (Type.Fun (fun_arg_typs, fun_body_typ), fun_typ) in

      (* XXX does the order matter? *)
      (fun_typ_eq :: fun_body_eqs) @ let_body_eqs
  | Syntax.LetTuple (l, e1, e2) ->
      let element_types = List.map (fun (_id, typ) -> typ) l in
      let eqs1 = gen_equations env e1 (Type.Tuple element_types) in
      let new_env = List.fold_right TypingEnvironment.add (List.rev l) env in
      let eqs2 = gen_equations new_env e2 expected_type in
      eqs1 @ eqs2
  | Syntax.Get (e1, e2) ->
      let eqs1 = gen_equations env e1 (Type.Array expected_type) in
      let eqs2 = gen_equations env e2 Type.Int in
      eqs1 @ eqs2
  | Syntax.Put (e1, e2, e3) ->
      let element_type = Type.Var (ref None) in
      let eqs1 = gen_equations env e1 (Type.Array element_type) in
      let eqs2 = gen_equations env e2 Type.Int in
      let eqs3 = gen_equations env e3 element_type in
      ((Type.Unit, expected_type) :: eqs1) @ eqs2 @ eqs3
  | Syntax.Tuple l ->
      let element_types = List.map (fun _ -> Type.Var (ref None)) l in
      let eqs =
        List.map2 (fun e typ -> gen_equations env e typ) l element_types
        |> List.concat
      in
      (Type.Tuple element_types, expected_type) :: eqs
  | Syntax.Array (e1, e2) ->
      let eqs1 = gen_equations env e1 Type.Int in
      let element_type = Type.Var (ref None) in
      let eqs2 = gen_equations env e2 element_type in
      ((Type.Array element_type, expected_type) :: eqs1) @ eqs2

(* Test whether type t1 is part of t2 *)
let rec occurs t1 t2 =
  match t2 with
  | Type.Unit | Type.Bool | Type.Int | Type.Float -> false
  | Type.Fun (args, ret) -> List.exists (fun t -> occurs t1 t) (ret :: args)
  | Type.Tuple elements -> List.exists (fun t -> occurs t1 t) elements
  | Type.Array t -> occurs t1 t
  | Type.Var t_ref -> (
      if (* NB (==) is physical equality, (=) structural equality *)
         t1 == t2
      then true
      else match !t_ref with None -> false | Some t -> occurs t1 t )

let occurs_check t1 t2 =
  if occurs t1 t2 then failwith "Recursive type detected. Aborting."

let rec unify equations =
  match equations with
  | [] -> ()
  | hd :: tl -> (
      match hd with
      (* | (t1, t2) when t1 = t2 -> () (* Equation is actually equal *) *)
      | Type.Unit, Type.Unit
      | Type.Bool, Type.Bool
      | Type.Int, Type.Int
      | Type.Float, Type.Float ->
          unify tl
      | Type.Fun (args1, ret1), Type.Fun (args2, ret2) ->
          let args1_len = List.length args1 in
          let args2_len = List.length args2 in
          if args1_len = args2_len then
            let arg_equations =
              List.map2 (fun typ1 typ2 -> (typ1, typ2)) args1 args2
            in
            let ret_equation = (ret1, ret2) in
            unify ((ret_equation :: arg_equations) @ tl)
          else failwith "Functions have different numbers of arguments."
      | Type.Tuple elts1, Type.Tuple elts2 ->
          let len1 = List.length elts1 in
          let len2 = List.length elts2 in
          if len1 = len2 then
            let match_tuple_types =
              List.map2 (fun typ1 typ2 -> (typ1, typ2)) elts1 elts2
            in
            unify (match_tuple_types @ tl)
          else failwith "Tuple lengths do not match."
      | Type.Array t1, Type.Array t2 -> unify ((t1, t2) :: tl)
      | (Type.Var v1 as t1), (Type.Var v2 as t2) -> (
          match (!v1, !v2) with
          | None, None -> unify tl
          | Some t, None ->
              occurs_check t2 t1;
              (* XXX What if t is again Type.Var *)
              v2 := Some t;
              unify tl
          | None, Some t ->
              occurs_check t1 t2;
              v1 := Some t;
              unify tl
          | Some t1, Some t2 -> unify ((t1, t2) :: tl) )
      | (Type.Var v as t1), t2 -> (
          occurs_check t1 t2;
          match !v with
          | None ->
              v := Some t2;
              unify tl
          | Some tv -> unify ((tv, t2) :: tl) )
      | t1, (Type.Var v as t2) -> (
          occurs_check t1 t2;
          match !v with
          | None ->
              v := Some t1;
              unify tl
          | Some tv -> unify ((tv, t1) :: tl) )
      | t1, t2 ->
          Printf.eprintf "Unification of %s and %s is impossible.\n"
            (Type.to_string t1) (Type.to_string t2);
          exit 0 )

let rec substitue_type_exn typ =
  let module T = Type in
  match typ with
  | T.Unit -> T.Unit
  | T.Bool -> T.Bool
  | T.Int -> T.Int
  | T.Float -> T.Float
  | T.Fun (args, ret) ->
      let new_ret = substitue_type_exn ret in
      let new_args = List.map substitue_type_exn args in
      T.Fun (new_args, new_ret)
  | T.Tuple elements -> T.Tuple (List.map substitue_type_exn elements)
  | T.Array t -> T.Array (substitue_type_exn t)
  | T.Var v -> (
      match !v with
      | None ->
          (* The paper suggests, arbitrarily, instantiating these variables to Type.Int *)
          failwith "Type variable is still undefined"
      (* XXX What if t is itself Type.Var? *)
      | Some t -> t )

let rec type_ast ast =
  let module S = Syntax in
  match ast with
  | S.Unit -> S.Unit
  | S.Bool _ as b -> b
  | S.Int _ as i -> i
  | S.Float _ as f -> f
  | S.Not e -> S.Not (type_ast e)
  | S.Neg e -> S.Neg (type_ast e)
  | S.Add (e1, e2) -> S.Add (type_ast e1, type_ast e2)
  | S.Sub (e1, e2) -> S.Sub (type_ast e1, type_ast e2)
  | S.FNeg e -> S.FNeg (type_ast e)
  | S.FAdd (e1, e2) -> S.FAdd (type_ast e1, type_ast e2)
  | S.FSub (e1, e2) -> S.FSub (type_ast e1, type_ast e2)
  | S.FMul (e1, e2) -> S.FMul (type_ast e1, type_ast e2)
  | S.FDiv (e1, e2) -> S.FDiv (type_ast e1, type_ast e2)
  | S.Eq (e1, e2) -> S.Eq (type_ast e1, type_ast e2)
  | S.LE (e1, e2) -> S.LE (type_ast e1, type_ast e2)
  | S.If (e1, e2, e3) -> S.If (type_ast e1, type_ast e2, type_ast e3)
  | S.Let ((id, typ), e1, e2) ->
      S.Let ((id, substitue_type_exn typ), type_ast e1, type_ast e2)
  | S.Var _ as v -> v
  | S.LetRec (fd, e) ->
      let { Syntax.name; args; body } = fd in
      let new_args =
        List.map (fun (id, t) -> (id, substitue_type_exn t)) args
      in
      let new_body = type_ast body in
      let id, fun_typ = name in
      S.LetRec
        ( {
            name = (id, substitue_type_exn fun_typ);
            args = new_args;
            body = new_body;
          },
          type_ast e )
  | S.App (e1, es) -> S.App (type_ast e1, List.map type_ast es)
  | S.Tuple es -> S.Tuple (List.map type_ast es)
  | S.LetTuple (elements, definition, body) ->
      let new_elements =
        List.map (fun (id, t) -> (id, substitue_type_exn t)) elements
      in
      S.LetTuple (new_elements, type_ast definition, type_ast body)
  | S.Array (e1, e2) -> S.Array (type_ast e1, type_ast e2)
  | S.Get (e1, e2) -> S.Get (type_ast e1, type_ast e2)
  | S.Put (e1, e2, e3) -> S.Put (type_ast e1, type_ast e2, type_ast e3)

let typed_ast ast =
  let type_equations =
    gen_equations (TypingEnvironment.default ()) ast Type.Unit
  in
  (* Mutates ast *)
  unify type_equations;
  type_ast ast
