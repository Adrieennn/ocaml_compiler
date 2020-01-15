  .global _start
_start:
  @ move sp to fp
  mov r11, r13
  mov r4, #3
  push {r4}
  mov r4, #5
  push {r4}
  ldr r4, [r11, #-4]
  ldr r5, [r11, #-8]
  add r4, r4, r5
  mov r0, r4 @ print_int reads in r0
  bl min_caml_print_int

