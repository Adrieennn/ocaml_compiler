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
  | Let of (Id.t * Type.t) * t * t (*correct def to id.t?*)
  | LetRec of fundef * t
  | Var of Id.t
  (* Not used since Syntax.t only defines Syntax.Eq and Syntax.LE
   * | IfNe
   * | Ifgt
   * | IfGe
   * | IfLt
   * Also, MinCaml only has signed integers so unsigned comparisons are
   * unnecessary
   *)
  (* cf. BEQ branch if equal *)
  | IfEq of (Id.t * Id.t) * t * t
  (* cf. BLE branch if less than or equal *)
  | IfLe of (Id.t * Id.t) * t * t
  | App of Id.t * Id.t list
  | Tuple of Id.t list
  | LetTuple of (Id.t * Type.t) list * t * t
  | Array of Id.t * Id.t
  | Get of Id.t * Id.t
  | Put of Id.t * Id.t * Id.t

and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }

(** We took inspiration from the paper and implemented the add_let function to
simplify the implementation of the main normalisation function.

add_let function takes an expression and the continuation of this expression as
parameters.

If the expression is not already a variable, add_let function return a let 
expression of Knorm.t with a newly generated variable, the k-normalised 
expression as definition and the continuation apply to the previous newly 
generated variable as the let body.*)
let rec add_let exp body =
  match exp with
  | Syntax.Var x -> body x
  | _ ->
      let e_id = Id.genid () in
      Let ((e_id, Type.Var (ref None)), of_syntax exp, body e_id)

(**of_syntax is the main function responsible for k-normalisation, it takes an
 expression of type Syntax.t and return a normalised expression of type
 Knorm.t*)
and of_syntax exp_s =
  match exp_s with
  | Syntax.Unit -> Unit
  | Syntax.Bool true -> Int 1
  | Syntax.Bool false -> Int 0
  | Syntax.Int i -> Int i
  | Syntax.Float f -> Float f
  | Syntax.Add (e1, e2) ->
      (* In the case of a binary operation with, we have to create 2 new
      variable to store the intermediate result, return a let expression and use
      the operation as the let body. As in
      
      let e1_id = Id.genid () in
      let e2_id = Id.genid () in
      Let
        ( (e1_id, Type.Int),
          of_syntax e1,
          Let ((e2_id, Type.Int), of_syntax e2, Add (e1_id, e2_id)) )
          
      However, it's much shorter to write using the add_let expression as in our
      final implementation
      *)
      add_let e1 (fun e1_id -> add_let e2 (fun e2_id -> Add (e1_id, e2_id)))
  | Syntax.Sub (e1, e2) ->
      add_let e1 (fun e1_id -> add_let e2 (fun e2_id -> Sub (e1_id, e2_id)))
  | Syntax.FAdd (e1, e2) ->
      add_let e1 (fun e1_id -> add_let e2 (fun e2_id -> FAdd (e1_id, e2_id)))
  | Syntax.FSub (e1, e2) ->
      add_let e1 (fun e1_id -> add_let e2 (fun e2_id -> FSub (e1_id, e2_id)))
  | Syntax.FMul (e1, e2) ->
      add_let e1 (fun e1_id -> add_let e2 (fun e2_id -> FMul (e1_id, e2_id)))
  | Syntax.FDiv (e1, e2) ->
      add_let e1 (fun e1_id -> add_let e2 (fun e2_id -> FDiv (e1_id, e2_id)))
  | Syntax.FNeg e ->
      (* In the case of a unary operation as here, our strategy is to transform 
      the expression into a binary operation. As in
      
      let e_id = Id.genid () in
      let e_0_id = Id.genid () in
      Let
        ( (e_id, Type.Float),
          of_syntax e,
          Let ((e_0_id, Type.Int), Float 0., FSub (e_0_id, e_id)) )

      We used add_let function in the final implementation.
      *)
      add_let (Syntax.Float 0.0) (fun e1_id ->
          add_let e (fun e2_id -> FSub (e1_id, e2_id)))
  | Syntax.Not e ->
      add_let (Syntax.Int 1) (fun e1_id ->
          add_let e (fun e2_id -> Sub (e1_id, e2_id)))
  | Syntax.Neg e ->
      add_let (Syntax.Int 0) (fun e1_id ->
          add_let e (fun e2_id -> Sub (e1_id, e2_id)))
  | Syntax.Eq (e1, e2) ->
      (* In the case of comparison, we treat it the same way as a binary
      operation except we use an IfEq expression to get the result of comparison
      in the end, thus transform all comparison to if expressions to make the
      code more assembly like. As in

      let e1_id = Id.genid () in
      let e2_id = Id.genid () in
      Let
        ( (e1_id, Type.Int),
          of_syntax e1,
          Let
            ( (e2_id, Type.Int),
              of_syntax e2,
              IfEq ((e1_id, e2_id), Int 1, Int 0) ) )
      
      We used add_let function in the final implementation.
      *)
      add_let e1 (fun e1_id ->
          add_let e2 (fun e2_id -> IfEq ((e1_id, e2_id), Int 1, Int 0)))
  | Syntax.LE (e1, e2) ->
      add_let e1 (fun e1_id ->
          add_let e2 (fun e2_id -> IfLe ((e1_id, e2_id), Int 1, Int 0)))
  (* In the case of a Let or LetRec expression we only have to normalise it's
   * definition and its body *)
  | Syntax.Let ((id, typ), def, body) ->
      Let ((id, typ), of_syntax def, of_syntax body)
  | Syntax.LetRec ({ Syntax.name; args; body }, e) ->
      LetRec ({ name; args; body = of_syntax body }, of_syntax e)
  | Syntax.LetTuple (vars, def, body) ->
      LetTuple (vars, of_syntax def, of_syntax body)
  | Syntax.Var id -> Var id
  | Syntax.If (e1, e2, e3) -> (
      match e1 with
      (* Here our goal is to transform a Syntax.If to a Knorm.IfEq or 
       * Knorm.IfLE according to the boolean expression. *)
      | Syntax.Eq (e1', e2') ->
          (*
          Implementation without using add_let
          
          let e1'_id = Id.genid () in
          let e2'_id = Id.genid () in
          Let
            ( (e1'_id, Type.Int),
              of_syntax e1',
              Let
                ( (e2'_id, Type.Int),
                  of_syntax e2',
                  IfEq ((e1'_id, e2'_id), of_syntax e2, of_syntax e3) ) )
          *)
          add_let e1' (fun e1'_id ->
              add_let e2' (fun e2'_id ->
                  IfEq ((e1'_id, e2'_id), of_syntax e2, of_syntax e3)))
      | Syntax.LE (e1', e2') ->
          add_let e1' (fun e1'_id ->
              add_let e2' (fun e2'_id ->
                  IfLe ((e1'_id, e2'_id), of_syntax e2, of_syntax e3)))
      | e ->
          (*
          In the case where the boolean expression does not match to Eq or LE, 
          we simply compare the result to boolean true and transform the If 
          expression to a IfEq expression.

          let e_id = Id.genid () in
          let e_true = Id.genid () in
          Let
            ( (e_id, Type.Int),
              of_syntax e,
              Let
                ( (e_true, Type.Int),
                  Int 1,
                  IfEq ((e_id, e_true), of_syntax e2, of_syntax e3) ) ) 

          We used add_let function in the final implementation.
          *)
          add_let e (fun e_id ->
              add_let (Syntax.Bool true) (fun e_true ->
                  IfEq ((e_id, e_true), of_syntax e2, of_syntax e3))) )
  | Syntax.App (f, args) ->
      (*Here since the arguments is a list, we have no other way except using 
      a pre-defined function like add_let. This is also why we implemented 
      add_let. *)
      let l = ref [] in
      let final_body () = add_let f (fun f_id -> App (f_id, List.rev !l)) in

      let rec build_lets_and_collect_arg_names = function
        | [] -> final_body ()
        | hd :: tl -> (
            match hd with
            | Syntax.Unit ->
                l := "()" :: !l;
                build_lets_and_collect_arg_names tl
            | _ ->
                add_let hd (fun x ->
                    l := x :: !l;
                    build_lets_and_collect_arg_names tl) )
      in
      build_lets_and_collect_arg_names args
  | Syntax.Tuple elements ->
      (*Here we used a similar strategy as we treated the argument list in App*)
      let l = ref [] in

      let rec build_lets_and_collect_arg_names = function
        | [] -> Tuple (List.rev !l)
        | hd :: tl ->
            add_let hd (fun x ->
                l := x :: !l;
                build_lets_and_collect_arg_names tl)
      in
      build_lets_and_collect_arg_names elements
  | Syntax.Array (e1, e2) ->
      add_let e1 (fun e1_id -> add_let e2 (fun e2_id -> Array (e1_id, e2_id)))
  | Syntax.Get (e1, e2) ->
      add_let e1 (fun e1_id -> add_let e2 (fun e2_id -> Get (e1_id, e2_id)))
  | Syntax.Put (e1, e2, e3) ->
      add_let e1 (fun e1_id ->
          add_let e2 (fun e2_id ->
              add_let e3 (fun e3_id -> Put (e1_id, e2_id, e3_id))))

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
  | Get (e1, e2) -> Printf.sprintf "%s.(%s)" (Id.to_string e1) (Id.to_string e2)
  | Put (e1, e2, e3) ->
      Printf.sprintf "(%s.(%s) <- %s)" (Id.to_string e1) (Id.to_string e2)
        (Id.to_string e3)
  | Array (e1, e2) ->
      Printf.sprintf "(Array.create %s %s)" (Id.to_string e1) (Id.to_string e2)
