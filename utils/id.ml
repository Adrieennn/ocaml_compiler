type t = string
type l = string

let remove_label_undersc label = String.sub label 1 (String.length label - 1)

let to_string x = x

let genid =
  let counter = ref (-1) in
  fun () ->
    incr counter;
    Printf.sprintf "%d" !counter
