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
  | LetRec of fundef * t
  | Var of Id.t
  (* Not used since Syntax.t only defines Syntax.Eq and Syntax.LE
   * | IfNe
   * | Ifgt
   * | IfGe
   * | IfLt
   * Also, MinCaml only has signed integers so unsigned comparisons are unnecessary
   *)
  (* cf. BEQ branch if equal *)
  | IfEq of (Id.t * Id.t) * t * t
  (* cf. BLE branch if less than or equal *)
  | IfLe of (Id.t * Id.t) * t * t
  | App of Id.t * Id.t list
  | Tuple of Id.t list
  | LetTuple of (Id.t * Type.t) list * t * t

and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }

let rec add_let exp body =
  let e_id = Id.genid () in
  Let ((e_id, Type.Var (ref None)), of_syntax exp, body e_id)

and of_syntax exp_s =
  match exp_s with
  | Syntax.Unit -> Unit
  | Syntax.Bool true -> Int 1
  | Syntax.Bool false -> Int 0
  | Syntax.Int i -> Int i
  | Syntax.Float f -> Float f
  | Syntax.Add (e1, e2) ->
      (*
      let e1_id = Id.genid () in
      let e2_id = Id.genid () in
      Let
        ( (e1_id, Type.Int),
          of_syntax e1,
          Let ((e2_id, Type.Int), of_syntax e2, Add (e1_id, e2_id)) )
          *)
      add_let e1 (fun e1_id -> add_let e2 (fun e2_id -> Add (e1_id, e2_id)))
  | Syntax.Sub (e1, e2) ->
      let e1_id = Id.genid () in
      let e2_id = Id.genid () in
      Let
        ( (e1_id, Type.Int),
          of_syntax e1,
          Let ((e2_id, Type.Int), of_syntax e2, Sub (e1_id, e2_id)) )
  | Syntax.FAdd (e1, e2) ->
      let e1_id = Id.genid () in
      let e2_id = Id.genid () in
      Let
        ( (e1_id, Type.Float),
          of_syntax e1,
          Let ((e2_id, Type.Float), of_syntax e2, FAdd (e1_id, e2_id)) )
  | Syntax.FSub (e1, e2) ->
      let e1_id = Id.genid () in
      let e2_id = Id.genid () in
      Let
        ( (e1_id, Type.Float),
          of_syntax e1,
          Let ((e2_id, Type.Float), of_syntax e2, FSub (e1_id, e2_id)) )
  | Syntax.FMul (e1, e2) ->
      let e1_id = Id.genid () in
      let e2_id = Id.genid () in
      Let
        ( (e1_id, Type.Float),
          of_syntax e1,
          Let ((e2_id, Type.Float), of_syntax e2, FMul (e1_id, e2_id)) )
  | Syntax.FDiv (e1, e2) ->
      let e1_id = Id.genid () in
      let e2_id = Id.genid () in
      Let
        ( (e1_id, Type.Float),
          of_syntax e1,
          Let ((e2_id, Type.Float), of_syntax e2, FDiv (e1_id, e2_id)) )
  | Syntax.FNeg e ->
      let e_id = Id.genid () in
      let e_0_id = Id.genid () in
      Let
        ( (e_id, Type.Float),
          of_syntax e,
          Let ((e_0_id, Type.Int), Float 0., FSub (e_0_id, e_id)) )
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
      let e1_id = Id.genid () in
      let e2_id = Id.genid () in
      Let
        ( (e1_id, Type.Int),
          of_syntax e1,
          Let
            ( (e2_id, Type.Int),
              of_syntax e2,
              IfEq ((e1_id, e2_id), Int 1, Int 0) ) )
  | Syntax.LE (e1, e2) ->
      let e1_id = Id.genid () in
      let e2_id = Id.genid () in
      Let
        ( (e1_id, Type.Int),
          of_syntax e1,
          Let
            ( (e2_id, Type.Int),
              of_syntax e2,
              IfLe ((e1_id, e2_id), Int 1, Int 0) ) )
  | Syntax.Let ((id, typ), def, body) ->
      Let ((id, typ), of_syntax def, of_syntax body)
  | Syntax.LetRec ({ Syntax.name; args; body }, e) ->
      LetRec ({ name; args; body = of_syntax body }, of_syntax e)
  | Syntax.LetTuple (vars, def, body) ->
      LetTuple (vars, of_syntax def, of_syntax body)
  | Syntax.Var id -> Var id
  | Syntax.If (e1, e2, e3) -> (
      match e1 with
      | Syntax.Eq (e1', e2') ->
          let e1'_id = Id.genid () in
          let e2'_id = Id.genid () in
          Let
            ( (e1'_id, Type.Int),
              of_syntax e1',
              Let
                ( (e2'_id, Type.Int),
                  of_syntax e2',
                  IfEq ((e1'_id, e2'_id), of_syntax e2, of_syntax e3) ) )
      | Syntax.LE (e1', e2') ->
          let e1'_id = Id.genid () in
          let e2'_id = Id.genid () in
          Let
            ( (e1'_id, Type.Int),
              of_syntax e1',
              Let
                ( (e2'_id, Type.Int),
                  of_syntax e2',
                  IfLe ((e1'_id, e2'_id), of_syntax e2, of_syntax e3) ) )
      | e ->
          let e_id = Id.genid () in
          let e_true = Id.genid () in
          Let
            ( (e_id, Type.Int),
              of_syntax e,
              Let
                ( (e_true, Type.Int),
                  Int 1,
                  IfEq ((e_id, e_true), of_syntax e2, of_syntax e3) ) ) )
  | Syntax.App (f, args) ->
      let l = ref [] in
      let f_id = Id.genid () in
      let final_body () =
        Let ((f_id, Type.Var (ref None)), of_syntax f, App (f_id, List.rev !l))
      in

      let rec build_lets_and_collect_arg_names = function
        | [] -> final_body ()
        | hd :: tl ->
            add_let hd (fun x ->
                l := x :: !l;
                build_lets_and_collect_arg_names tl)
      in
      build_lets_and_collect_arg_names args
  | Syntax.Tuple elements ->
      let l = ref [] in

      let rec build_lets_and_collect_arg_names = function
        | [] -> Tuple (List.rev !l)
        | hd :: tl ->
            add_let hd (fun x ->
                l := x :: !l;
                build_lets_and_collect_arg_names tl)
      in
      build_lets_and_collect_arg_names elements
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
  | Float f -> string_of_float f
  | Add (e1, e2) -> Printf.sprintf "(%s + %s)" e1 e2
  | Sub (e1, e2) -> Printf.sprintf "(%s - %s)" e1 e2
  | FAdd (e1, e2) -> Printf.sprintf "(%s +. %s)" e1 e2
  | FSub (e1, e2) -> Printf.sprintf "(%s -. %s)" e1 e2
  | FMul (e1, e2) -> Printf.sprintf "(%s *. %s)" e1 e2
  | FDiv (e1, e2) -> Printf.sprintf "(%s /. %s)" e1 e2
  | Var id -> Id.to_string id
  | Tuple l -> Printf.sprintf "(%s)" (infix_to_string Id.to_string l ", ")
  | App (e1, le2) ->
      Printf.sprintf "(%s %s)" (Id.to_string e1)
        (infix_to_string Id.to_string le2 " ")
  | Let ((id, _t), e1, e2) ->
      Printf.sprintf "(let %s = %s in %s)" (Id.to_string id) (to_string e1)
        (to_string e2)
  | LetRec (fd, e) ->
      Printf.sprintf "(let rec %s %s = %s in %s)"
        (let x, _ = fd.name in
         Id.to_string x)
        (infix_to_string (fun (x, _) -> Id.to_string x) fd.args " ")
        (to_string fd.body) (to_string e)
  | LetTuple (l, e1, e2) ->
      Printf.sprintf "(let (%s) = %s in %s)"
        (infix_to_string (fun (x, _) -> Id.to_string x) l ", ")
        (to_string e1) (to_string e2)
  | IfEq ((id1, id2), e1, e2) ->
      Printf.sprintf "(if %s  = %s then %s else %s)" (Id.to_string id1)
        (Id.to_string id2) (to_string e1) (to_string e2)
  | IfLe ((id1, id2), e1, e2) ->
      Printf.sprintf "(if %s  <= %s then %s else %s)" (Id.to_string id1)
        (Id.to_string id2) (to_string e1) (to_string e2)
