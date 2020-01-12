open Asml
open Register

let exp_to_asm exp reg =
  match exp with
  | Add (x, y) -> "ldr r4, [r11, #-" ^ x ^ "]\n" ^ (match y with
      | Int i -> "add r4, r4, #" ^ (string_of_int i) ^ "\n"
      | Var a -> "ldr r5, [r11, #-" ^ a ^ "]\n" ^ "add r4, r4, r5\n")
  | _ -> "IGNORED"

let rec t_to_asm_rec body reg =
  match body with
  | Let ((id, _), e, t) -> (match e with
      | Int i -> "mov r4, #" ^ (string_of_int i) ^ "\n push {r4}\n"
      | Var a -> {|
        "ldr r4, [r11, #-" ^ a ^ "]\n push {r4}\n"
      |}
      | _ -> exp_to_asm e reg ^ "push {r4}\n") ^ (t_to_asm_rec t reg)
  | Ans exp -> "mov r0, r4\n bl min_caml_print_int"

let prog_to_asm prog reg =
  {|
  .global _start

_start:
mov r11, r13
|}
  ^ match prog with Program (lfl, lfu, body) -> t_to_asm_rec body reg
