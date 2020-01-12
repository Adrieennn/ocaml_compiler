type t =
  | Var of t option ref

let gentyp () = Var(ref None) 
