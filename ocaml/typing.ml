module TypingEnvironment : sig
  type t

  val add : Id.t * Type.t -> t -> t

  val find_opt : Id.t -> t -> Type.t option
end = struct
  type t = (Id.t * Type.t) list

  let add (x : Id.t * Type.t) (env : t) = x :: env

  let find_opt x env = List.assoc_opt x env
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

      fun_eqs @ arg_eqs
  | Syntax.LetRec (fd, e) ->
      let { Syntax.name = fun_name; args = fun_args; body = fun_body } = fd in
      let fun_arg_typs = List.map (fun (_id, typ) -> typ) fun_args in

      (* XXX check folds (here and below) *)
      let fun_body_env =
        List.fold_right TypingEnvironment.add (List.rev fun_args) env
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
