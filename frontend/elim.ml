(* effect takes 2 parameters, an expression and a mutable list.
 * This function checks if the given expression has a side effect or not by
 * comparing the function names used in the expression to a list of function
 * that's know to have side effects. *)
let rec effect e =
  match e with
  | Knorm.Put _ -> true
  | Knorm.App (id, args) -> true (*StringSet.mem id !effectful_functions*)
  | Knorm.LetRec ({ name = fun_id, fun_typ; args; body = fun_body }, let_body)
    ->
      (* We must run `effect` on fun_body so that
       * the mutable list of effectful functions is updated. That is when
       * checking the `let_body`, we must detect those functions which were
       * defined inside of the current function definition too, including the
       * function being currently defined itself *)

      (* Even if a fun_body is effectful, the LetRec might not be if that
       * function is not actaully run inside the body. *)
      effect let_body
  | Knorm.Let (var, def, body) ->
      let b1 = effect def in
      let b2 = effect body in
      b1 || b2
  | Knorm.LetTuple (vars, def, body) ->
      let b1 = effect def in
      let b2 = effect body in
      b1 || b2
  | Knorm.IfEq ((v1, v2), e1, e2) | Knorm.IfLe ((v1, v2), e1, e2) ->
      let b1 = effect e1 in
      let b2 = effect e2 in
      b1 || b2
  | Knorm.Unit | Knorm.Int _ | Knorm.Float _ | Knorm.Tuple _ | Knorm.Array _
  | Knorm.Get _ | Knorm.Var _ | Knorm.Add _ | Knorm.Sub _ | Knorm.FAdd _
  | Knorm.FSub _ | Knorm.FMul _ | Knorm.FDiv _ ->
      false

(* not_occurs takes 2 paremeters, a variable name and an expression. This
 * function checks if the variable does not occur in the given expression. *)
let rec not_occurs (id : Id.t) (exp : Knorm.t) =
  match exp with
  | Knorm.Unit | Knorm.Int _ | Knorm.Float _ -> true
  | Knorm.Put (e1, e2, e3) -> id <> e1 && id <> e2 && id <> e3
  | Knorm.Var i -> i = id
  | Knorm.Add (v1, v2)
  | Knorm.Sub (v1, v2)
  | Knorm.FAdd (v1, v2)
  | Knorm.FSub (v1, v2)
  | Knorm.FMul (v1, v2)
  | Knorm.FDiv (v1, v2)
  | Knorm.Array (v1, v2) ->
      id <> v1 && id <> v2
  | Knorm.Tuple l -> List.for_all (fun id' -> id <> id') l
  | Knorm.Get (e1, e2) -> id <> e1 && id <> e2
  (* assume that after alpha conversion all variable name is different *)
  | Knorm.Let ((name, _t), e1, e2) -> not_occurs id e1 && not_occurs id e2
  | Knorm.LetRec ({ name; args; body }, let_body) ->
      not_occurs id body && not_occurs id let_body
  | Knorm.LetTuple (l, e1, e2) -> not_occurs id e1 && not_occurs id e2
  | Knorm.IfEq ((v1, v2), e1, e2) | Knorm.IfLe ((v1, v2), e1, e2) ->
      id <> v1 && id <> v2 && not_occurs id e1 && not_occurs id e2
  | Knorm.App (f, args) -> f <> id && List.for_all (fun id' -> id <> id') args

(* elim takes 2 parameters, an expression and a list. This function is the main
 * body of elimination, it checks every definition and determine if it's
 * unnecessary. If so the definition will be suppressed. *)
let rec elim exp =
  match exp with
  | Knorm.Let ((id, _t), e1, e2) ->
      if (not (effect e1)) && not_occurs id e2 then elim e2
      else Knorm.Let ((id, _t), elim e1, elim e2)
  | Knorm.LetRec
      ({ Knorm.name = fun_id, fun_typ; args; body = fun_body }, let_body) ->
      if (not (effect fun_body)) && not_occurs fun_id let_body then
        elim let_body
      else (
        (* It is necessary to add the function here to because we will otherwise
         * miss an effectful occurence of the outermost function since it will
         * not be added to the list of effectful functions by `not_occurs`. *)
        Knorm.LetRec
          ( { name = (fun_id, fun_typ); args; body = elim fun_body },
            elim let_body ) )
  | Knorm.LetTuple (vars, def, body) ->
      Knorm.LetTuple (vars, elim def, elim body)
  | Knorm.IfEq ((v1, v2), e1, e2) -> Knorm.IfEq ((v1, v2), elim e1, elim e2)
  | Knorm.IfLe ((v1, v2), e1, e2) -> Knorm.IfLe ((v1, v2), elim e1, elim e2)
  | ( Knorm.Unit | Knorm.Int _ | Knorm.Float _ | Knorm.Tuple _ | Knorm.Array _
    | Knorm.Get _ | Knorm.Var _ | Knorm.Add _ | Knorm.Sub _ | Knorm.FAdd _
    | Knorm.FSub _ | Knorm.FMul _ | Knorm.FDiv _ | Knorm.App _ | Knorm.Put _ )
    as non_definition ->
      non_definition

(* elimination takes an expression as a parameter.
 * This function uses elim defined previously to eliminate unnecessary
 * definitions. *)
let elimination exp =
  (* reset mutable state *)
  elim exp
