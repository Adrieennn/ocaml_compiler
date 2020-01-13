type t =
  | Unit
  | Int of int
  | Add of Id.t * Id.t
  | Sub of Id.t * Id.t
  | Let of (Id.t * Type.t) * t * t
  | LetRec of fundef * t
  | Var of Id.t

and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }

let rec of_syntax exp_s =
  match exp_s with
  | Syntax.Unit -> Unit
  | Syntax.Bool true -> Int 1
  | Syntax.Bool false -> Int 0
  | Syntax.Int i -> Int i
  | Syntax.Add (e1, e2) ->
      let e1_id = Id.genid () in
      let e2_id = Id.genid () in
      Let
        ( (e1_id, Type.Int),
          of_syntax e1,
          Let ((e2_id, Type.Int), of_syntax e2, Add (e1_id, e2_id)) )
  | Syntax.Sub (e1, e2) ->
      let e1_id = Id.genid () in
      let e2_id = Id.genid () in
      Let
        ( (e1_id, Type.Int),
          of_syntax e1,
          Let ((e2_id, Type.Int), of_syntax e2, Sub (e1_id, e2_id)) )
  | Syntax.Not e ->
      (* true: 1
       * false: 0
       * not true: 1 - 1 -> 0 = true
       * not false: 1 - 0 -> 1 = false *)
      let e_id = Id.genid () in
      let e_1_id = Id.genid () in
      Let
        ( (e_id, Type.Int),
          of_syntax e,
          Let ((e_1_id, Type.Int), Int 1, Sub (e_1_id, e_id)) )
  | Syntax.Neg e ->
      let e_id = Id.genid () in
      let e_0_id = Id.genid () in
      Let
        ( (e_id, Type.Int),
          of_syntax e,
          Let ((e_0_id, Type.Int), Int 0, Sub (e_0_id, e_id)) )
  | Syntax.Eq (e1, e2) ->
      (* Integers are equal if their difference is zero
       * Because we interpret zero as boolean false, the result is not'ed *)
      of_syntax (Syntax.Not (Syntax.Sub (e1, e2)))
  | Syntax.Let ((id, typ), def, body) ->
      Let ((id, typ), of_syntax def, of_syntax body)
  | Syntax.LetRec ({ Syntax.name; args; body }, e) ->
      LetRec ({ name; args; body = of_syntax body }, of_syntax e)
  | Syntax.Var id -> Var id
  | e ->
      Printf.eprintf "%s not implemented\n" (Syntax.to_string e);
      exit 0

let rec infix_to_string to_s l op =
  match l with
  | [] -> ""
  | [ x ] -> to_s x
  | hd :: tl -> to_s hd ^ op ^ infix_to_string to_s tl op

let rec to_string exp =
  match exp with
  | Unit -> "()"
  | Int i -> string_of_int i
  | Add (e1, e2) -> Printf.sprintf "(%s + %s)" e1 e2
  | Sub (e1, e2) -> Printf.sprintf "(%s - %s)" e1 e2
  | Var id -> Id.to_string id
  | Let ((id, _t), e1, e2) ->
      Printf.sprintf "(let %s = %s in %s)" (Id.to_string id) (to_string e1)
        (to_string e2)
  | LetRec (fd, e) ->
      Printf.sprintf "(let rec %s %s = %s in %s)"
        (let x, _ = fd.name in
         Id.to_string x)
        (infix_to_string (fun (x, _) -> Id.to_string x) fd.args " ")
        (to_string fd.body) (to_string e)
