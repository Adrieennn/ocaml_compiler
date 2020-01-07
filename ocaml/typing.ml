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
  | Syntax.Bool b -> [ (Type.Bool, expected_type) ]
  | Syntax.Int i -> [ (Type.Int, expected_type) ]
  | Syntax.Float f -> [ (Type.Float, expected_type) ]
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
  | Syntax.App (e1, le2) -> failwith "not implemented"
  | Syntax.LetRec (fd, e) -> failwith "not implemented"
  | Syntax.LetTuple (l, e1, e2) -> failwith "not implemented"
  | Syntax.Get (e1, e2) ->
      let eqs1 = gen_equations env e1 (Type.Array expected_type) in
      let eqs2 = gen_equations env e2 Type.Int in
      eqs1 @ eqs2
  | Syntax.Put (e1, e2, e3) -> failwith "not implemented"
  | Syntax.Tuple l -> failwith "not implemented"
  | Syntax.Array (e1, e2) -> failwith "not implemented"
