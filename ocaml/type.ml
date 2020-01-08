type t =
  | Unit
  | Bool
  | Int
  | Float
  | Fun of t list * t 
  | Tuple of t list
  | Array of t
  | Var of t option ref

let rec to_string t =
  match t with
  | Unit -> "Unit"
  | Bool -> "Bool"
  | Int -> "Int"
  | Float -> "Float"
  | Fun(arg_types, return_type) ->
      let arg_types = List.map to_string arg_types in
      let return_type = to_string return_type in
      String.concat " -> " (arg_types @ [return_type])
  | Tuple elements ->
      let beginning = "Tuple(" in
      let middle = List.map to_string elements in
      let ending = ")" in
      String.concat "" (beginning :: middle @ [ending])
  | Array t-> Printf.sprintf "Array %s" (to_string t)
  | Var opt ->
      match !opt with
      | None -> "Undefined"
      | Some(t) -> to_string t

let gentyp () = Var(ref None) 
