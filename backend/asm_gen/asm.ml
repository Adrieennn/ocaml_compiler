open Asml
open Register

let label_counter x =
  let counter = ref x in
  fun () ->
    counter := !counter + 1;
    !counter

let if_count = label_counter 0

let move_integer register i =
  if i > 65535 then
    let i' = abs i in
    let i_low = i' land 65535 in
    let i_top = i' lsr 16 in
    "mov " ^ register ^ ", #" ^ string_of_int i_low ^ "\n" ^ "movt " ^ register
    ^ ", #" ^ string_of_int i_top ^ "\n"
    ^ if i < 0 then "rsb " ^ register ^ ", " ^ register ^ ", #0\n" else ""
  else "mov " ^ register ^ ", #" ^ string_of_int i ^ "\n"

(* args_to_asm_pred: NOT spilling args of predefined functions but store them in
 * registers *)
let rec args_to_asm_pred args regnum =
  match args with
  | h :: r ->
      "ldr r" ^ string_of_int regnum ^ ", [r11, #" ^ h ^ "]\n" ^ "push {r0}\n"
      ^ args_to_asm_pred r (regnum + 1)
  | [] -> ""

(* args_to_asm: spilling arguments of user-defined functions *)
let rec args_to_asm args =
  match args with
  | h :: r -> "ldr r0, [r11, #" ^ h ^ "]\n" ^ "push {r0}\n" ^ args_to_asm r
  | [] -> "add r13, r13, #-4 @ making space for lr\n" ^ "push {r11}\n"

(* reset_sp: move stack pointer back to before function call *)
let reset_sp args =
  let len = (List.length args + 2) * 4 in
  "add r13, r13, #" ^ string_of_int len ^ "\n"

(* exp_to_asm: match exp with corresponding assembly operations and store in r0 *)
let rec exp_to_asm exp =
  match exp with
  | Int i -> move_integer "r0" i
  | Var v -> "ldr r0, [r11, #" ^ v ^ "]\n"
  | Add (x, y) ->
      "ldr r4, [r11, #" ^ x ^ "]\n"
      ^ ( match y with
        | Int i -> move_integer "r5" i
        | Var a -> "ldr r5, [r11, #" ^ a ^ "]\n" )
      ^ "add r0, r4, r5\n"
  | Sub (x, y) ->
      "ldr r4, [r11, #" ^ x ^ "]\n"
      ^ ( match y with
        | Int i -> move_integer "r5" i
        | Var a -> "ldr r5, [r11, #" ^ a ^ "]\n" )
      ^ "sub r0, r4, r5\n"
  | CallDir (label, args) ->
      if
        String.length label > 9
        && String.equal (String.sub label 0 9) "_min_caml"
      then
        args_to_asm_pred args 0 ^ "bl " ^ Id.remove_label_undersc label ^ "\n"
      else
        args_to_asm args ^ "mov r11, r13 @ move sp to fp\n" ^ "bl "
        ^ Id.remove_label_undersc label
        ^ "\n" ^ "mov r13, r11 @ move fp to sp\n" ^ "ldr r11, [r11]\n"
        ^ reset_sp args
  | IfEq (s1, s2, t1, t2) ->
      let label_index = string_of_int (if_count ()) in
      ( "ldr r4, [r11, #" ^ s1 ^ "]\n"
      ^
      match s2 with
      | Int i -> move_integer "r5" i
      | Var v -> "ldr r5, [r11, #" ^ v ^ "]\n" )
      ^ "cmp r4, r5\n" ^ "beq ltrue" ^ label_index ^ "\n" ^ "b lfalse"
      ^ label_index ^ "\n" ^ "ltrue" ^ label_index ^ ":\n" ^ t_to_asm t1
      ^ "b lnext" ^ label_index ^ "\n" ^ "lfalse" ^ label_index ^ ":\n"
      ^ t_to_asm t2 ^ "lnext" ^ label_index ^ ":\n"
  | IfLEq (s1, s2, t1, t2) ->
      let label_index = string_of_int (if_count ()) in
      ( "ldr r4, [r11, #" ^ s1 ^ "]\n"
      ^
      match s2 with
      | Int i -> move_integer "r5" i
      | Var v -> "ldr r5, [r11, #" ^ v ^ "]\n" )
      ^ "cmp r4, r5\n" ^ "ble ltrue" ^ label_index ^ "\n" ^ "b lfalse"
      ^ label_index ^ "\n" ^ "ltrue" ^ label_index ^ ":\n" ^ t_to_asm t1
      ^ "b lnext" ^ label_index ^ "\n" ^ "lfalse" ^ label_index ^ ":\n"
      ^ t_to_asm t2 ^ "lnext" ^ label_index ^ ":\n"
  | e -> Printf.sprintf "%s IGNORED FOR NOW\n" (Asml.to_string e)

(* t_to_asm: transform let and exp to assembly *)
and t_to_asm body =
  match body with
  | Let ((id, _), e, t) ->
      ( match e with
      | Int i -> move_integer "r0" i
      | Var a -> "ldr r0, [r11, #" ^ a ^ "]\n"
      | _ -> exp_to_asm e )
      ^ "push {r0}\n" ^ t_to_asm t ^ "add r13, r13, #4\n"
  | Ans exp -> exp_to_asm exp

(* lfu_to_asm: for each function definition, generate assembly code *)
let rec lfu_to_asm lfu =
  match lfu with
  | fu :: r ->
      "  .globl "
      ^ Id.remove_label_undersc fu.name
      ^ "\n"
      ^ Id.remove_label_undersc fu.name
      ^ ":\n" ^ "str r14, [r11, #4] @ store lr on the stack\n"
      ^ t_to_asm fu.body ^ "\n"
      ^ "ldr r15, [r11, #4] @ load lr (fp + 4) into pc\n" ^ lfu_to_asm r
  | [] -> ""

(* prog_to_asm: main function called, transform prog into assembly *)
let prog_to_asm prog =
  match prog with
  | Program (lfl, lfu, body) ->
      lfu_to_asm lfu
      ^ {|  .global _start

_start:
mov r11, r13 @ move sp to fp
|}
      ^ t_to_asm body ^ "\n"
