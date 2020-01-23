(* Typing environment mapping variables names to their types
 * The type t is opaque so that we can later move to a more efficient
 * implementation without breaking the API
 *)
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
    empty ()
    (* random selection of predefined functions so that we can type
     * more programs *)
    |> add ("print_int", Type.Fun ([ Type.Int ], Type.Unit))
    |> add ("print_newline", Type.Fun ([ Type.Unit ], Type.Unit))
    |> add ("sin", Type.Fun ([ Type.Float ], Type.Float))
    |> add ("cos", Type.Fun ([ Type.Float ], Type.Float))
    |> add ("sqrt", Type.Fun ([ Type.Float ], Type.Float))
    |> add ("abs_float", Type.Fun ([ Type.Float ], Type.Float))
    |> add ("int_of_float", Type.Fun ([ Type.Float ], Type.Int))
    |> add ("float_of_int", Type.Fun ([ Type.Int ], Type.Float))
    |> add ("truncate", Type.Fun ([ Type.Float ], Type.Int))
end

module TypingEquation = struct
  (* Not really used for now, but the idea is the same i.e. roughly
   * having one module per type *)
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
      (* Simple base cases: the expected type must match the base values *)
      let eqs = gen_equations env e Type.Bool in
      (Type.Bool, expected_type) :: eqs
  | Syntax.Neg e ->
      let eqs = gen_equations env e Type.Int in
      (Type.Int, expected_type) :: eqs
  | Syntax.Add (e1, e2) ->
      (* Add expects both of its arguments to type to Type.Int *)
      let eqs1 = gen_equations env e1 Type.Int in
      let eqs2 = gen_equations env e2 Type.Int in
      (* ... and returns an integer which must match the expected type
       * the same logic applies to the remaining operations *)
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
      (* The let definition must be equal to the variable's type *)
      let eqs1 = gen_equations env e1 t in
      let eqs2 =
        (* The let body must evaluate to the expected type *)
        gen_equations (TypingEnvironment.add (id, t) env) e2 expected_type
      in
      eqs1 @ eqs2
  | Syntax.Var id -> (
      match TypingEnvironment.find_opt id env with
      | None ->
          (* TODO "external environment" *)
          Printf.eprintf "Variable %s has not been bound.\n" (Id.to_string id);
          exit 1
      | Some typ -> [ (typ, expected_type) ] )
  | Syntax.App (e1, le2) ->
      let arg_typs = List.map (fun _ -> Type.Var (ref None)) le2 in
      let arg_eqs =
        (* The expresions provided as arguments must match the parameter types
         * of the applied function *)
        List.map2 (fun e typ -> gen_equations env e typ) le2 arg_typs
        |> List.concat
      in

      (* The function's return type must match the expected_type since it is applied *)
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
      (* The function body's type is not yet known *)
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
      (* make the defined variables available in the let body *)
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
      (* NB (==) is physical equality, (=) structural equality *)
      t1 == t2
      || match !t_ref with None -> false | Some t -> occurs t1 t )

let occurs_check t1 t2 =
  if occurs t1 t2 then failwith "Recursive type detected. Aborting."

let rec unify equations =
  match equations with
  | [] -> ()
  | hd :: tl -> (
      match hd with
      | Type.Unit, Type.Unit
      | Type.Bool, Type.Bool
      | Type.Int, Type.Int
      | Type.Float, Type.Float ->
          (* continue if both sides of the equation are the same
           * that is, the equations type correctly for now *)
          unify tl
      | Type.Fun (args1, ret1), Type.Fun (args2, ret2) ->
          let args1_len = List.length args1 in
          let args2_len = List.length args2 in
          if args1_len = args2_len then
            let arg_equations =
              List.map2 (fun typ1 typ2 -> (typ1, typ2)) args1 args2
            in
            let ret_equation = (ret1, ret2) in
            (* Two function types are equal if their parameter types and
             * return types match up *)
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
      | (Type.Var v1 as t1), t2 -> (
          match !v1 with
          | Some t ->
              if t == t2 then (* Prevent infinite loop *)
                unify tl
              else unify ((t, t2) :: tl)
          | None ->
              occurs_check t1 t2;
              v1 := Some t2;
              unify tl )
      | t1, (Type.Var v2 as t2) -> (
          match !v2 with
          | Some t -> if t == t1 then unify tl else unify ((t1, t) :: tl)
          | None ->
              occurs_check t2 t1;
              v2 := Some t1;
              unify tl )
      | t1, t2 ->
          Printf.eprintf "Unification of %s and %s is impossible.\n"
            (Type.to_string t1) (Type.to_string t2);
          exit 1 )

(* Instantiate (replace) type variables by their content *)
(* Currently fails if a type variable in the AST has not been resolved *)
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
      | Some t -> substitue_type_exn t )

(* Substitute type variables by their content and return the resulting AST *)
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

(* Main function of consumers of this module
 * takes an AST and returns a typed AST
 * NB: that it modifies the originally passed AST during the type
 *     inference process *)
let typed_ast ast =
  let type_equations =
    gen_equations (TypingEnvironment.default ()) ast Type.Unit
  in
  (* Mutates ast *)
  unify type_equations;
  type_ast ast
