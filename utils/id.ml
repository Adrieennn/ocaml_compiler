type t = string

type l = string

let remove_label_undersc label = String.sub label 1 (String.length label - 1)

let to_string x = x

let predefined_function_labels = function
  | "print_int" -> Some "_min_caml_print_int"
  | "print_newline" -> Some "_min_caml_print_newline"
  | "sin" -> Some "_min_caml_sin"
  | "cos" -> Some "_min_caml_cos"
  | "sqrt" -> Some "_min_caml_sqrt"
  | "abs_float" -> Some "_min_caml_abs_float"
  | "int_of_float" -> Some "_min_caml_int_of_float"
  | "float_of_int" -> Some "_min_caml_float_of_int"
  | "truncate" -> Some "_min_caml_truncate"
  | "create_array" -> Some "_min_caml_create_array"
  | _ -> None

let label_of_id x =
  match predefined_function_labels x with None -> "_" ^ x | Some x -> x

let id_of_label l =
  let len = String.length l in
  match len with
  (* String.length only returns natural numbers (0 inclusive) *)
  | 0 | 1 -> failwith "Cannot convert invalid Id.label to Id.t"
  | _ ->
      let id = String.sub l 1 (len - 1) in
      assert (id.[0] <> '_');
      id

let genid =
  let counter = ref (-1) in
  fun () ->
    incr counter;
    Printf.sprintf "t%d" !counter
