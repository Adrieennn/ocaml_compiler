open Printf

type id_or_imm = Var of Id.t | Int of int

type t = Ans of exp | Let of (Id.t * Type.t) * exp * t

and exp =
  | Var of Id.t
  | Label of Id.t
  | Int of int
  | Unit
  | Add of Id.t * id_or_imm
  | Sub of Id.t * id_or_imm
  | Ld of Id.t * id_or_imm
  | St of Id.t * id_or_imm * Id.t
  | New of id_or_imm
  | Neg of Id.t
  | FNeg of Id.t
  | FAdd of Id.t * Id.t
  | FSub of Id.t * Id.t
  | FMul of Id.t * Id.t
  | FDiv of Id.t * Id.t
  | IfEq of Id.t * id_or_imm * t * t
  | IfFEq of Id.t * Id.t * t * t
  | IfLEq of Id.t * id_or_imm * t * t
  | IfGEq of Id.t * id_or_imm * t * t
  | IfFLEq of Id.t * Id.t * t * t
  | CallCls of Id.t * Id.t list
  | CallDir of Id.l * Id.t list

type fu = { name : Id.t; args : Id.t list; body : t }

type fundef = Fu of fu * fundef | Fl of Id.t * float * fundef | Main of t

type prog = Program of (Id.t * float) list * fu list * t

(* Maps Id.t to flaots, before including in Asml.prog the ids need to be
 * converted to the proper labels. This process relies on Id.label_of_id
 * being deterministic *)
let float_map = ref []

(* List of float variables to decide whether to create an int or float array *)
let float_ids = ref []

let rec add_let exp body =
  let id = Id.genid () in
  Let ((id, Type.Var (ref None)), exp, body id)

let rec closure_to_t = function
  | Closure.Float f -> (
      match List.find_opt (fun (l, f') -> f = f') !float_map with
      | Some (l, _f) ->
          let label_var = Id.genid () in
          Let ((label_var, Type.Int), Var l, Ans (Ld (label_var, Int 0)))
      | None ->
          let label = Id.label_of_id (Id.genid ()) in
          float_map := (label, f) :: !float_map;

          let label_var = Id.genid () in
          Let ((label_var, Type.Int), Var label, Ans (Ld (label_var, Int 0))) )
  | Closure.Tuple elements ->
      let num_elements = List.length elements in
      let tuple_id = Id.genid () in
      let rec load_elements counter = function
        | [] -> Ans (Var tuple_id)
        | hd :: tl ->
            Let
              ( (Id.genid (), Type.Var (ref None)),
                St (tuple_id, Int counter, hd),
                load_elements (counter + 1) tl )
      in
      Let
        ( (tuple_id, Type.Int),
          New (Int (num_elements * 4)),
          load_elements 0 elements )
  | Closure.LetTuple (vars, def, body) ->
      let rec insert = function
        | Let (id_and_typ', def', body') -> Let (id_and_typ', def', insert body')
        | Ans exp -> (
            match exp with
            | Var tuple_id ->
                let rec add_lets counter = function
                  | [] -> closure_to_t body
                  | hd :: tl ->
                      Let
                        ( hd,
                          Ld (tuple_id, Int counter),
                          add_lets (counter + 1) tl )
                in
                add_lets 0 vars
            | _ ->
                failwith
                  "LetTuple definition body did not evaluate to a variable" )
      in
      insert (closure_to_t def)
  | Closure.Let ((id, typ), Closure.Float f, body) -> (
      match List.find_opt (fun (l, f') -> f = f') !float_map with
      | Some (l, _f) ->
          float_ids := id :: !float_ids;

          let label_var = Id.genid () in
          Let
            ( (label_var, Type.Int),
              Var l,
              Let ((id, Type.Float), Ld (label_var, Int 0), closure_to_t body)
            )
      | None ->
          let label = Id.label_of_id (Id.genid ()) in
          float_map := (label, f) :: !float_map;
          float_ids := id :: !float_ids;

          let label_var = Id.genid () in
          Let
            ( (label_var, Type.Int),
              Var label,
              Let ((id, Type.Float), Ld (label_var, Int 0), closure_to_t body)
            ) )
  | Closure.Let (id_and_typ, def, body) ->
      (* adapted from NestedLetReduction.reduction
       * it has become necessary because handling floating point values]
       * with labels, variables and memory access causes us to introduce new
       * let-bindings at this low level.
       * Effectively, this function re-applies the nested-let reduction. *)
      let rec insert = function
        | Let (id_and_typ', def', body') -> Let (id_and_typ', def', insert body')
        | Ans exp -> Let (id_and_typ, exp, closure_to_t body)
      in
      insert (closure_to_t def)
  | Closure.MkCls ((cls_id, cls_typ), (fun_label, args), body) ->
      (* reserve space
       * first word is for the method of the closure
       * afterwards space is for the captured variables *)
      let word_size = 4 in
      let num_vars = List.length args in

      let rec arg_lets loc counter = function
        | [] -> closure_to_t body
        | hd :: tl ->
            add_let
              (St (loc, Int counter, hd))
              (fun _ -> arg_lets loc (counter + 1) tl)
      in
      let arg_ids = List.map (fun (id, _typ) -> id) args in
      Let
        ( (cls_id, cls_typ),
          (* Space for arguments plus (+ 1) the fun_label in the beginning *)
          New (Int ((num_vars + 1) * word_size)),
          add_let (Var fun_label) (fun fun_label_var ->
              add_let
                (St (cls_id, Int 0, fun_label_var))
                (fun _ -> arg_lets cls_id 1 arg_ids)) )
  | exp -> Ans (closure_to_exp exp)

and closure_to_exp = function
  | Closure.Unit -> Unit
  | Closure.Int i -> Int i
  | Closure.Float f ->
      failwith "Closure.Float to ASML is handled in closure_to_t instead"
  | Closure.Add (id1, id2) -> Add (id1, Var id2)
  | Closure.Sub (id1, id2) -> Sub (id1, Var id2)
  | Closure.FAdd (id1, id2) -> FAdd (id1, id2)
  | Closure.FSub (id1, id2) -> FSub (id1, id2)
  | Closure.FMul (id1, id2) -> FMul (id1, id2)
  | Closure.FDiv (id1, id2) -> FDiv (id1, id2)
  | Closure.Var id -> Var id
  | Closure.IfEq ((id1, id2), e1, e2) ->
      IfEq (id1, Var id2, closure_to_t e1, closure_to_t e2)
  | Closure.IfLe ((id1, id2), e1, e2) ->
      IfLEq (id1, Var id2, closure_to_t e1, closure_to_t e2)
  | Closure.Let ((id, _), _, _) ->
      Printf.eprintf "Closure.Let %s = ... cannot be translated to Asml.exp\n"
        id;
      exit 1
  | Closure.AppDir (fun_label, arg_ids) -> CallDir (fun_label, arg_ids)
  | Closure.AppCls (id, arg_ids) -> CallCls (id, arg_ids)
  | Closure.Array (size, init) -> (
      match List.mem init !float_ids with
      | true -> CallDir (Id.label_of_id "create_float_array", [ size; init ])
      | false -> CallDir (Id.label_of_id "create_array", [ size; init ]) )
  | Closure.Get (arr, index) -> Ld (arr, Var index)
  | Closure.Put (arr, index, value) -> St (arr, Var index, value)
  | e ->
      Printf.eprintf
        "Conversion from Closure's %s to Asml.exp is not implemented.\n"
        (Closure.to_string' e);
      exit 1

let fundef_of_closure_fundef fd =
  let { Closure.name = label, _typ; args; formal_fv; body } = fd in
  match formal_fv with
  | [] ->
      let arg_names = List.map (fun (arg_id, _arg_typ) -> arg_id) args in
      { name = label; args = arg_names; body = closure_to_t body }
  | _ ->
      let closure_body =
        let rec retrieve_environment counter = function
          | [] -> closure_to_t body
          | hd :: tl ->
              Let
                ( (hd, Type.Var (ref None)),
                  Ld ("%self", Int counter),
                  retrieve_environment (counter + 1) tl )
        in
        Let
          ( (Id.id_of_label label, Type.Var (ref None)),
            Var "%self",
            retrieve_environment 1 (List.map (fun (id, _typ) -> id) formal_fv)
          )
      in
      let arg_names = List.map (fun (arg_id, _arg_typ) -> arg_id) args in
      { name = label; args = arg_names; body = closure_body }

let of_closure_prog prog =
  float_map := [];
  float_ids := [];

  let (Closure.Prog (fundefs, main_body)) = prog in
  Program
    ( !float_map,
      List.map fundef_of_closure_fundef fundefs,
      closure_to_t main_body )

let rec infix_to_string (to_s : 'a -> string) (l : 'a list) (op : string) :
    string =
  match l with
  | [] -> ""
  | [ x ] -> to_s x
  | hd :: tl -> to_s hd ^ op ^ infix_to_string to_s tl op

let rec to_string_id_or_imm (i : id_or_imm) =
  match i with Int i -> string_of_int i | Var id -> Id.to_string id

(* to_string: returns a string out of an expresssion exp *)
let rec to_string_e exp =
  match exp with
  | Unit -> "nop"
  | Int i -> string_of_int i
  | Neg e -> sprintf "(neg %s)" (Id.to_string e)
  | New s -> sprintf "(new %s)" (to_string_id_or_imm s)
  | Add (e1, e2) ->
      sprintf "(add %s %s)" (Id.to_string e1) (to_string_id_or_imm e2)
  | Sub (e1, e2) ->
      sprintf "(sub %s %s)" (Id.to_string e1) (to_string_id_or_imm e2)
  | FNeg e -> sprintf "(fneg %s)" (Id.to_string e)
  | FAdd (e1, e2) -> sprintf "(fadd %s %s)" (Id.to_string e1) (Id.to_string e2)
  | FSub (e1, e2) -> sprintf "(fsub %s %s)" (Id.to_string e1) (Id.to_string e2)
  | FMul (e1, e2) -> sprintf "(fmul %s %s)" (Id.to_string e1) (Id.to_string e2)
  | FDiv (e1, e2) -> sprintf "(fdiv %s %s)" (Id.to_string e1) (Id.to_string e2)
  | IfEq (e1, e2, e3, e4) ->
      sprintf "(if %s = %s then\n%s\n  else\n%s)" (Id.to_string e1)
        (to_string_id_or_imm e2) (to_string_t e3) (to_string_t e4)
  | IfFEq (e1, e2, e3, e4) ->
      sprintf "(if %s = %s then\n%s\n  else\n%s)" (Id.to_string e1)
        (Id.to_string e2) (to_string_t e3) (to_string_t e4)
  | IfLEq (e1, e2, e3, e4) ->
      sprintf "(if %s <= %s then\n%s\n  else\n%s)" (Id.to_string e1)
        (to_string_id_or_imm e2) (to_string_t e3) (to_string_t e4)
  | IfGEq (e1, e2, e3, e4) ->
      sprintf "(if %s >= %s then\n%s\n  else\n%s)" (Id.to_string e1)
        (to_string_id_or_imm e2) (to_string_t e3) (to_string_t e4)
  | IfFLEq (e1, e2, e3, e4) ->
      sprintf "(if %s <=. %s then\n%s\n  else\n%s)" (Id.to_string e1)
        (Id.to_string e2) (to_string_t e3) (to_string_t e4)
  | Var id -> Id.to_string id
  | Label id -> Id.to_string id
  | CallDir (e1, le2) ->
      sprintf "(call %s %s)" (Id.to_string e1)
        (infix_to_string Id.to_string le2 " ")
  | CallCls (e1, le2) ->
      sprintf "(call_closure %s %s)" (Id.to_string e1)
        (infix_to_string Id.to_string le2 " ")
  | Ld (e1, e2) ->
      sprintf "(mem(%s + %s))" (Id.to_string e1) (to_string_id_or_imm e2)
  | St (e1, e2, e3) ->
      sprintf "(mem(%s + %s) <- %s)" (Id.to_string e1) (to_string_id_or_imm e2)
        (Id.to_string e3)

(* to_string_t: match t to correct substring *)
and to_string_t t =
  match t with
  | Ans e -> "  " ^ to_string_e e
  | Let ((id, t), e1, e2) ->
      sprintf "  (let %s = %s in\n%s)" (Id.to_string id) (to_string_e e1)
        (to_string_t e2)

(* to_string_f: fundef to string *)
let rec to_string_f fd =
  match fd with
  | Main t -> sprintf "let _ =\n%s" (to_string_t t)
  | Fl (l, f, fd2) ->
      sprintf "let %s = %s\n%s" (Id.to_string l) (string_of_float f)
        (to_string_f fd2)
  | Fu (fn, fd2) ->
      sprintf "let %s %s =\n%s\n\n%s"
        (let x = fn.name in
         Id.to_string x)
        (infix_to_string (fun x -> Id.to_string x) fn.args " ")
        (to_string_t fn.body) (to_string_f fd2)

(* fd_to_prog: changes a tree of fundef to a program *)
let rec fd_to_prog fd prog =
  match prog with
  | Program (lfl, lfu, body) -> (
      match fd with
      | Main t -> Program (lfl, lfu, t)
      | Fl (l, f, fd2) -> fd_to_prog fd2 (Program (lfl @ [ (l, f) ], lfu, body))
      | Fu (fn, fd2) -> fd_to_prog fd2 (Program (lfl, lfu @ [ fn ], body)) )

(* The three following functions transform a program to fundef *)
let rec prog_fl_to_fd lfl_reversed fd =
  match lfl_reversed with
  | [] -> fd
  | (label, flt) :: rest -> prog_fl_to_fd rest (Fl (label, flt, fd))

let rec prog_fu_to_fd lfu_reversed lfl_reversed fd =
  match lfu_reversed with
  | [] -> prog_fl_to_fd lfl_reversed fd
  | hd :: rest -> prog_fu_to_fd rest lfl_reversed (Fu (hd, fd))

let rec prog_to_fd prog =
  match prog with
  | Program (lfl, lfu, body) ->
      prog_fu_to_fd (List.rev lfu) (List.rev lfl) (Main body)

let to_string_p prog =
  let fundef = prog_to_fd prog in
  to_string_f fundef
