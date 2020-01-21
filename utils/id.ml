type t = string

type l = string

let remove_label_undersc label = String.sub label 1 (String.length label - 1)

let to_string x = x

let predefined_function_labels = function
  | "print_int" -> Some "_min_caml_print_int"
  | "print_newline" -> Some "_min_caml_print_newline"
  | "int_of_float" -> Some "_min_caml_int_of_float"
  | "float_of_int" -> Some "_min_caml_float_of_int"
  (* TODO the other predefined functions *)
  | _ -> None

let label_of_id x =
  match predefined_function_labels x with None -> "_" ^ x | Some x -> x

let genid =
  let counter = ref (-1) in
  fun () ->
    incr counter;
    Printf.sprintf "t%d" !counter
