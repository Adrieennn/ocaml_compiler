open Printf

type t =
  | Unit
  | Bool of bool
  | Int of int
  | Float of float
  | Not of t
  | Neg of t
  | Add of t * t
  | Sub of t * t
  | FNeg of t
  | FAdd of t * t
  | FSub of t * t
  | FMul of t * t
  | FDiv of t * t
  | Eq of t * t
  | LE of t * t
  | If of t * t * t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | LetRec of fundef * t
  | App of t * t list
  | Tuple of t list
  | LetTuple of (Id.t * Type.t) list * t * t
  | Array of t * t
  | Get of t * t
  | Put of t * t * t

and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }

let rec infix_to_string (to_s : 'a -> string) (l : 'a list) (op : string) :
    string =
  match l with
  | [] -> ""
  | [ x ] -> to_s x
  | hd :: tl -> to_s hd ^ op ^ infix_to_string to_s tl op

let rec to_string exp =
  match exp with
  | Unit -> "()"
  | Bool b -> if b then "true" else "false"
  | Int i -> string_of_int i
  | Float f -> sprintf "%.2f" f
  | Not e -> sprintf "(not %s)" (to_string e)
  | Neg e -> sprintf "(- %s)" (to_string e)
  | Add (e1, e2) -> sprintf "(%s + %s)" (to_string e1) (to_string e2)
  | Sub (e1, e2) -> sprintf "(%s - %s)" (to_string e1) (to_string e2)
  | FNeg e -> sprintf "(-. %s)" (to_string e)
  | FAdd (e1, e2) -> sprintf "(%s +. %s)" (to_string e1) (to_string e2)
  | FSub (e1, e2) -> sprintf "(%s -. %s)" (to_string e1) (to_string e2)
  | FMul (e1, e2) -> sprintf "(%s *. %s)" (to_string e1) (to_string e2)
  | FDiv (e1, e2) -> sprintf "(%s /. %s)" (to_string e1) (to_string e2)
  | Eq (e1, e2) -> sprintf "(%s = %s)" (to_string e1) (to_string e2)
  | LE (e1, e2) -> sprintf "(%s <= %s)" (to_string e1) (to_string e2)
  | If (e1, e2, e3) ->
      sprintf "(if %s then %s else %s)" (to_string e1) (to_string e2)
        (to_string e3)
  | Let ((id, typ), e1, e2) -> (
      match typ with
      | Type.Var { contents = None } ->
          (* ignore undetermined type *)
          sprintf "(let %s = %s in %s)" (Id.to_string id) (to_string e1)
            (to_string e2)
      | Type.Var { contents = Some t } | t ->
          sprintf "(let (%s : %s) = %s in %s)" (Id.to_string id)
            (Type.to_string t) (to_string e1) (to_string e2) )
  | Var id -> Id.to_string id
  | App (e1, le2) ->
      sprintf "(%s %s)" (to_string e1) (infix_to_string to_string le2 " ")
  | LetRec (fd, e) ->
      let fun_name, fun_typ = fd.name in
      let ret_typ_string =
        match fun_typ with
        | Type.Fun (_args, ret) -> " : " ^ Type.to_string ret
        | Type.Var { contents = None } -> ""
        | Type.Var { contents = Some _ } ->
            failwith
              "Types were not substituted before calling Syntax.to_string"
        | t ->
            Printf.eprintf "Expected function received %s" (Type.to_string t);
            exit 0
      in
      (* Lack of space between %s%s for arguments/return type is intentional.
       * This way the ast prints pretty both when the type variable has been
       * resolved and when it hasn't. *)
      sprintf "(let rec %s %s%s = %s in %s)" (Id.to_string fun_name)
        (infix_to_string
           (fun (x, typ) ->
             if typ = Type.Var { contents = None } then Id.to_string x
             else
               Printf.sprintf "(%s : %s)" (Id.to_string x) (Type.to_string typ))
           fd.args " ")
        ret_typ_string (to_string fd.body) (to_string e)
  | LetTuple (l, e1, e2) ->
      sprintf "(let (%s) = %s in %s)"
        (infix_to_string
           (fun (x, typ) ->
             if typ = Type.Var { contents = None } then Id.to_string x
             else
               Printf.sprintf "(%s : %s)" (Id.to_string x) (Type.to_string typ))
           l ", ")
        (to_string e1) (to_string e2)
  | Get (e1, e2) -> sprintf "%s.(%s)" (to_string e1) (to_string e2)
  | Put (e1, e2, e3) ->
      sprintf "(%s.(%s) <- %s)" (to_string e1) (to_string e2) (to_string e3)
  | Tuple l -> sprintf "(%s)" (infix_to_string to_string l ", ")
  | Array (e1, e2) ->
      sprintf "(Array.create %s %s)" (to_string e1) (to_string e2)
