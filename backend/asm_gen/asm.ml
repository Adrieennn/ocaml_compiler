open Asml
open Register

let tabs = "\t\t\t\t"

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
    tabs ^ "mov " ^ register ^ ", #" ^ string_of_int i_low ^ "\n" ^ tabs
    ^ "movt " ^ register ^ ", #" ^ string_of_int i_top ^ "\n"
    ^
    if i < 0 then tabs ^ "rsb " ^ register ^ ", " ^ register ^ ", #0\n" else ""
  else tabs ^ "mov " ^ register ^ ", #" ^ string_of_int i ^ "\n"

(* args_to_asm_pred: NOT spilling args of predefined functions but store them in
 * registers *)
let rec args_to_asm_pred args regnum =
  match args with
  | h :: r ->
      tabs ^ "ldr r" ^ string_of_int regnum ^ ", [fp, #" ^ h ^ "]\n"
      ^ args_to_asm_pred r (regnum + 1)
  | [] -> ""

(* args_to_asm: spilling arguments of user-defined functions *)
let rec args_to_asm args =
  match args with
  | h :: r ->
      tabs ^ "ldr r0, [fp, #" ^ h ^ "]\n" ^ tabs ^ "push {r0}\n" ^ args_to_asm r
  | [] ->
      tabs ^ "add sp, sp, #-8 @ making space for lr and %self\n" ^ tabs
      ^ "push {fp}\n"

let rec args_to_asm_closure args =
  match args with
  | h :: r ->
      tabs ^ "ldr r0, [fp, #" ^ h ^ "]\n" ^ tabs ^ "push {r0}\n"
      ^ args_to_asm_closure r
  | [] ->
      tabs ^ "add sp, sp, #-4 @ making space for lr\n" ^ tabs ^ "push {fp}\n"

(* reset_sp: move stack pointer back to before function call *)
let reset_sp args =
  let len = (List.length args + 3) * 4 in
  tabs ^ "add sp, sp, #" ^ string_of_int len ^ "\n"

(* exp_to_asm: matches exp with corresponding assembly operations and store them
 * in r0 *)
let rec exp_to_asm exp =
  match exp with
  | Unit -> ""
  | Int i -> move_integer "r0" i
  | Label l -> tabs ^ "adrl r0, " ^Id.remove_label_undersc l ^ "\n"
  | Var v -> tabs ^ "ldr r0, [fp, #" ^ v ^ "]\n"
  | Add (x, y) ->
      tabs ^ "ldr r4, [fp, #" ^ x ^ "]\n"
      ^ ( match y with
        | Int i -> move_integer "r5" i
        | Var a -> tabs ^ "ldr r5, [fp, #" ^ a ^ "]\n" )
      ^ tabs ^ "add r0, r4, r5\n"
  | Sub (x, y) ->
      tabs ^ "ldr r4, [fp, #" ^ x ^ "]\n"
      ^ ( match y with
        | Int i -> move_integer "r5" i
        | Var a -> tabs ^ "ldr r5, [fp, #" ^ a ^ "]\n" )
      ^ tabs ^ "sub r0, r4, r5\n"
  | CallDir (label, args) ->
      if
        String.length label > 9
        && String.equal (String.sub label 0 9) "_min_caml"
      then
        args_to_asm_pred args 0 ^ tabs ^ "bl "
        ^ Id.remove_label_undersc label
        ^ "\n"
      else
        args_to_asm args ^ tabs ^ "mov fp, sp @ move sp to fp\n" ^ "bl "
        ^ Id.remove_label_undersc label
        ^ "\n" ^ tabs ^ "mov sp, fp @ move fp to sp\n" ^ tabs ^ "ldr fp, [fp]\n"
        ^ reset_sp args
  | CallCls (label, args) ->
      args_to_asm_closure (args @ [ label ])
      ^ tabs ^ "mov fp, sp @ move sp to fp\n" ^ tabs ^ "ldr r0, [fp, #8]\n"
      ^ tabs ^ "ldr r0, [r0]\n" ^ tabs ^ "blx r0\n" ^ tabs
      ^ "mov sp, fp @ move fp to sp\n" ^ tabs ^ "ldr fp, [fp]\n" ^ reset_sp args
  | IfFEq (s1, s2, t1, t2) ->
      let label_index = string_of_int (if_count ()) in
      ( tabs ^ "ldr r4, [fp, #" ^ s1 ^ "]\n" ^ tabs ^ "ldr r5, [fp, #" ^ s2
      ^ "]\n" )
      ^ tabs ^ "vmov.f32 s0, r4\n" ^ tabs ^ "vmov.f32 s1, r5\n" ^ tabs
      ^ "vcmp.f32 s0, s1\n" ^ tabs
      ^ "vmrs APSR_nzcv, FPSCR @ Get the flags into APSR.\n" ^ tabs
      ^ "beq ltrue" ^ label_index ^ "\n" ^ tabs ^ "b lfalse" ^ label_index
      ^ "\n" ^ tabs ^ "ltrue" ^ label_index ^ ":\n" ^ tabs ^ t_to_asm t1 0
      ^ tabs ^ "b lnext" ^ label_index ^ "\n" ^ tabs ^ "lfalse" ^ label_index
      ^ ":\n" ^ t_to_asm t2 0 ^ tabs ^ "lnext" ^ label_index ^ ":\n"
  | IfFLEq (s1, s2, t1, t2) ->
      let label_index = string_of_int (if_count ()) in
      ( tabs ^ "ldr r4, [fp, #" ^ s1 ^ "]\n" ^ tabs ^ "ldr r5, [fp, #" ^ s2
      ^ "]\n" )
      ^ tabs ^ "vmov.f32 s0, r4\n" ^ tabs ^ "vmov.f32 s1, r5\n" ^ tabs
      ^ "vcmp.f32 s0, s1\n" ^ tabs
      ^ "vmrs APSR_nzcv, FPSCR @ Get the flags into APSR.\n" ^ tabs
      ^ "ble ltrue" ^ label_index ^ "\n" ^ tabs ^ "b lfalse" ^ label_index
      ^ "\n" ^ tabs ^ "ltrue" ^ label_index ^ ":\n" ^ t_to_asm t1 0 ^ tabs
      ^ "b lnext" ^ label_index ^ "\n" ^ tabs ^ "lfalse" ^ label_index ^ ":\n"
      ^ t_to_asm t2 0 ^ tabs ^ "lnext" ^ label_index ^ ":\n"
  | IfEq (s1, s2, t1, t2) ->
      let label_index = string_of_int (if_count ()) in
      ( tabs ^ "ldr r4, [fp, #" ^ s1 ^ "]\n"
      ^
      match s2 with
      | Int i -> move_integer "r5" i
      | Var v -> tabs ^ "ldr r5, [fp, #" ^ v ^ "]\n" )
      ^ tabs ^ "cmp r4, r5\n" ^ tabs ^ "beq ltrue" ^ label_index ^ "\n" ^ tabs
      ^ "b lfalse" ^ label_index ^ "\n" ^ tabs ^ "ltrue" ^ label_index ^ ":\n"
      ^ t_to_asm t1 0 ^ tabs ^ "b lnext" ^ label_index ^ "\n" ^ tabs ^ "lfalse"
      ^ label_index ^ ":\n" ^ t_to_asm t2 0 ^ tabs ^ "lnext" ^ label_index
      ^ ":\n"
  | IfLEq (s1, s2, t1, t2) ->
      let label_index = string_of_int (if_count ()) in
      ( tabs ^ "ldr r4, [fp, #" ^ s1 ^ "]\n"
      ^
      match s2 with
      | Int i -> move_integer "r5" i
      | Var v -> tabs ^ "ldr r5, [fp, #" ^ v ^ "]\n" )
      ^ tabs ^ "cmp r4, r5\n" ^ tabs ^ "ble ltrue" ^ label_index ^ "\n" ^ tabs
      ^ "b lfalse" ^ label_index ^ "\n" ^ tabs ^ "ltrue" ^ label_index ^ ":\n"
      ^ t_to_asm t1 0 ^ tabs ^ "b lnext" ^ label_index ^ "\n" ^ tabs ^ "lfalse"
      ^ label_index ^ ":\n" ^ t_to_asm t2 0 ^ tabs ^ "lnext" ^ label_index
      ^ ":\n"
  | Ld (s1, s2) ->
      tabs ^ "ldr r4, [fp, #" ^ s1 ^ "]\n"
      ^ ( match s2 with
        | Var v -> tabs ^ "ldr r5, [fp, #" ^ v ^ "]\n"
        | Int i -> move_integer "r5" i )
      ^ tabs ^ "lsl r5, r5, #2\n" ^ tabs ^ "ldr r0, [r4, r5]\n"
  | St (s1, s2, s3) ->
      tabs ^ "ldr r4, [fp, #" ^ s1 ^ "]\n"
      ^ ( match s2 with
        | Var v -> tabs ^ "ldr r5, [fp, #" ^ v ^ "]\n"
        | Int i -> move_integer "r5" i )
      ^ tabs ^ "ldr r6, [fp, #" ^ s3 ^ "]\n" ^ tabs ^ "lsl r5, r5, #2\n" ^ tabs
      ^ "str r6, [r4, r5]\n"
  | New i ->
      tabs ^ "mov r0, r12\n" ^ move_integer "r4" i ^ tabs ^ "add r12, r12, r4\n"
  | FAdd (s1, s2) ->
      tabs ^ "vldr s0, [fp, #" ^ s1 ^ "]\n" ^ tabs ^ "vldr s1, [fp, #" ^ s2
      ^ "]\n" ^ tabs ^ "vadd.f32 s0, s0, s1\n" ^ tabs ^ "vmov.f32 r0, s0\n"
  | FSub (s1, s2) ->
      tabs ^ "vldr s0, [fp, #" ^ s1 ^ "]\n" ^ tabs ^ "vldr s1, [fp, #" ^ s2
      ^ "]\n" ^ tabs ^ "vsub.f32 s0, s0, s1\n" ^ tabs ^ "vmov.f32 r0, s0\n"
  | FMul (s1, s2) ->
      tabs ^ "vldr s0, [fp, #" ^ s1 ^ "]\n" ^ tabs ^ "vldr s1, [fp, #" ^ s2
      ^ "]\n" ^ tabs ^ "vmul.f32 s0, s0, s1\n" ^ tabs ^ "vmov.32 r0, s0\n"
  | FDiv (s1, s2) ->
      tabs ^ "vldr s0, [fp, #" ^ s1 ^ "]\n" ^ tabs ^ "vldr s1, [fp, #" ^ s2
      ^ "]\n" ^ tabs ^ "vdiv.f32 s0, s0, s1\n" ^ tabs ^ "vmov.32 r0, s0\n"
  | e -> Printf.sprintf "%s IGNORED FOR NOW\n" (Asml.to_string e)

(* t_to_asm: transform let and exp to assembly *)
and t_to_asm body sp_reset =
  match body with
  | Let ((id, _), e, t) ->
      ( match e with
      | Int i -> move_integer "r0" i
      | Var a -> tabs ^ "ldr r0, [fp, #" ^ a ^ "]\n"
      | _ -> exp_to_asm e )
      ^ tabs ^ "push {r0}\n"
      ^ t_to_asm t (sp_reset + 4)
  | Ans exp ->
      exp_to_asm exp
      ^
      if sp_reset > 0 then
        tabs ^ "add sp, sp, #" ^ string_of_int sp_reset ^ "\n"
        (*move_integer "r10" sp_reset ^ "add sp, sp,r10\n"*)
      else ""

(* lfu_to_asm: for each function definition, generate assembly code *)
let rec lfu_to_asm lfu =
  match lfu with
  | fu :: r ->
      tabs ^ ".globl "
      ^ Id.remove_label_undersc fu.name
      ^ "\n"
      ^ Id.remove_label_undersc fu.name
      ^ ":\n" ^ tabs ^ "str lr, [fp, #4] @ store lr on the stack\n"
      ^ t_to_asm fu.body 0 ^ tabs
      ^ "ldr pc, [fp, #4] @ load lr (fp + 4) into pc\n\n" ^ lfu_to_asm r
  | [] -> ""

let rec lfl_to_asm lfl =
  match lfl with
  | (id, fl) :: r ->
      Id.remove_label_undersc id ^ ":\n" ^ tabs ^ ".word "
      ^ Int32.to_string (Int32.bits_of_float fl)
      ^ "\n" ^ lfl_to_asm r
  | [] -> ""

(* prog_to_asm: main function called, transform prog into assembly *)
let prog_to_asm prog =
  match prog with
  | Program (lfl, lfu, body) ->
      lfl_to_asm lfl ^ lfu_to_asm lfu ^ tabs ^ ".global _start\n" ^ "_start:\n"
      ^ tabs ^ "mov fp, sp @ move sp to fp\n" ^ tabs ^ "bl min_caml_mmap\n"
      ^ t_to_asm body 0 ^ "\n"
