open Asml
open Register

(* MISCELLANEOUS FUNCTION SHOULD BE MOVED SOMEWHERE ELSE *)
let remove_label_undersc label = String.sub label 1 (String.length label - 1)

let exp_to_asm exp reg =
  match exp with
  | Add (x, y) -> (
      "ldr r4, [r11, #-" ^ x ^ "]\n"
      ^
      match y with
      | Int i -> "add r4, r4, #" ^ string_of_int i ^ "\n"
      | Var a -> "ldr r5, [r11, #-" ^ a ^ "]\n" ^ "add r4, r4, r5\n" )
  | Sub (x, y) -> (
      "ldr r4, [r11, #-" ^ x ^ "]\n"
      ^
      match y with
      | Int i -> "sub r4, r4, #" ^ string_of_int i ^ "\n"
      | Var a -> "ldr r5, [r11, #-" ^ a ^ "]\n" ^ "sub r4, r4, r5\n" )
  | CallDir (label, args) ->
      "ldr r0, [r11, #-" ^ List.hd args ^ "]\n" ^ "bl "
      ^ remove_label_undersc label ^ "\n"
  | _ -> "@ IGNORED FOR NOW"

let rec t_to_asm_rec body reg =
  match body with
  | Let ((id, _), e, t) ->
      ( match e with
      | Int i -> "mov r4, #" ^ string_of_int i ^ "\n push {r4}\n"
      | Var a -> {|
        "ldr r4, [r11, #-" ^ a ^ "]\n push {r4}\n"
      |}
      | _ -> exp_to_asm e reg ^ "push {r4}\n" )
      ^ t_to_asm_rec t reg
  | Ans exp -> exp_to_asm exp reg

(* "mov r0, r4\n bl min_caml_print_int" *)

let prog_to_asm prog reg =
  {|  .global _start

_start:
mov r11, r13 @ move sp to fp
|}
  ^ match prog with Program (lfl, lfu, body) -> t_to_asm_rec body reg ^ "\n"
