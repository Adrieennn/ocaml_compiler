open Asml
open Register

(* MISCELLANEOUS FUNCTION SHOULD BE MOVED SOMEWHERE ELSE *)
let remove_label_undersc label = String.sub label 1 (String.length label - 1)

let rec args_to_asm args regnum =
  match args with
  | h :: r ->
      "ldr r" ^ string_of_int regnum ^ ", [r11, #-" ^ h ^ "]\n"
      ^ args_to_asm r (regnum + 1)
  | [] -> ""

let exp_to_asm exp reg =
  match exp with
  | Add (x, y) ->
      ( "ldr r4, [r11, #-" ^ x ^ "]\n"
      ^
      match y with
      | Int i -> "add r4, r4, #" ^ string_of_int i ^ "\n"
      | Var a -> "ldr r5, [r11, #-" ^ a ^ "]\n" ^ "add r4, r4, r5\n" )
      ^ "push {r4}\n"
  | Sub (x, y) ->
      ( "ldr r4, [r11, #-" ^ x ^ "]\n"
      ^
      match y with
      | Int i -> "sub r4, r4, #" ^ string_of_int i ^ "\n"
      | Var a -> "ldr r5, [r11, #-" ^ a ^ "]\n" ^ "sub r4, r4, r5\n" )
      ^ "push {r4}\n"
  | CallDir (label, args) ->
      args_to_asm args 0 ^ "bl " ^ remove_label_undersc label
      ^ "\n push {r0}\n"
  | _ -> "@ IGNORED FOR NOW"

let rec t_to_asm_rec body reg =
  match body with
  | Let ((id, _), e, t) ->
      ( match e with
      | Int i -> "mov r4, #" ^ string_of_int i ^ "\n push {r4}\n"
      | Var a -> {|
        "ldr r4, [r11, #-" ^ a ^ "]\n push {r4}\n"
      |}
      | _ -> exp_to_asm e reg )
      ^ t_to_asm_rec t reg
  | Ans exp -> exp_to_asm exp reg

(* "mov r0, r4\n bl min_caml_print_int" *)
let rec lfu_to_asm_rec lfu reg =
  match lfu with
  | fu::r -> "IGNORED" 
  | [] -> ""


let prog_to_asm prog reg =
  match prog with
  | Program (lfl, lfu, body) ->
      lfu_to_asm_rec lfu reg
      ^ {|  .global _start

_start:
mov r11, r13 @ move sp to fp
|}
      ^ t_to_asm_rec body reg ^ "\n"
