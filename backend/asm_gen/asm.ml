open Asml
open Register

(* MISCELLANEOUS FUNCTION SHOULD BE MOVED SOMEWHERE ELSE *)
let remove_label_undersc label = String.sub label 1 (String.length label - 1)

let rec args_to_asm_pred args regnum =
  match args with
  | h :: r ->
      "ldr r" ^ string_of_int regnum ^ ", [r11, #" ^ h ^ "]\n"
      ^ "push {r0}\n"
      ^ args_to_asm_pred r (regnum + 1)
  | [] -> ""

let rec args_to_asm args =
  match args with
  | h :: r ->
      "ldr r0, [r11, #" ^ h ^ "]\n"
      (* "ldr r" ^ string_of_int regnum ^ ", [r11, #" ^ h ^ "]\n" *)
      ^ "push {r0}\n"
      ^ args_to_asm r
  | [] -> "push {r11}\n"

let reset_sp args =
  let len = (List.length args + 1) * 4 in
  "add r13, r13, #" ^ string_of_int len ^ "\n"

let exp_to_asm exp reg =
  match exp with
  | Add (x, y) ->
      ( "ldr r4, [r11, #" ^ x ^ "]\n"
      ^
      match y with
      | Int i -> "add r0, r4, #" ^ string_of_int i ^ "\n"
      | Var a -> "ldr r5, [r11, #" ^ a ^ "]\n" ^ "add r0, r4, r5\n" )
      ^ "push {r0}\n"
  | Sub (x, y) ->
      ( "ldr r4, [r11, #" ^ x ^ "]\n"
      ^
      match y with
      | Int i -> "sub r0, r4, #" ^ string_of_int i ^ "\n"
      | Var a -> "ldr r5, [r11, #" ^ a ^ "]\n" ^ "sub r0, r4, r5\n" )
      ^ "push {r0}\n"
  | CallDir (label, args) ->
      if
        String.length label > 9
        && String.equal (String.sub label 0 9) "_min_caml"
      then
        args_to_asm_pred args 0 ^ "bl "
        ^ remove_label_undersc label ^ "\n"
        ^ "push {r0}\n"
      else
        args_to_asm args ^ "mov r11, r13 @ move sp to fp\n" ^ "bl "
        ^ remove_label_undersc label ^ "\n" ^ "mov r13, r11 @ move fp to sp\n"
        ^ "ldr r11, [r11]\n" ^ reset_sp args ^ "push {r0}\n"
  | _ -> "@ IGNORED FOR NOW"

let rec t_to_asm_rec body reg =
  match body with
  | Let ((id, _), e, t) ->
      ( match e with
      | Int i -> "mov r4, #" ^ string_of_int i ^ "\n push {r4}\n"
      | Var a -> {|
        "ldr r4, [r11, #" ^ a ^ "]\n push {r4}\n"
      |}
      | _ -> exp_to_asm e reg )
      ^ t_to_asm_rec t reg
  | Ans exp -> exp_to_asm exp reg

(* "mov r0, r4\n bl min_caml_print_int" *)
let rec lfu_to_asm_rec lfu reg =
  match lfu with
  | fu :: r ->
      "  .globl "
      ^ remove_label_undersc fu.name
      ^ "\n"
      ^ remove_label_undersc fu.name
      ^ ":\n" ^ t_to_asm_rec fu.body reg ^ "\n"
      ^ "mov r15, r14 @reset pc to lr\n" ^ lfu_to_asm_rec r reg
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
