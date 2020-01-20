.text
  .global _start

_start:
mov r11, r13
bl min_caml_mmap

@ str r1, [r12, #0]
@ str r1, [r12, #8]
@ mov r0, r12
@ bl min_caml_print_int
@ bl min_caml_print_newline
@ ldr r0, [r12, #0]
@ bl min_caml_print_int
@ bl min_caml_print_newline
@ ldr r0, [r12, #4]
@ bl min_caml_print_int
@ bl min_caml_print_newline
@ ldr r0, [r12, #8]
@ bl min_caml_print_int
@ bl min_caml_print_newline
@ ldr r0, [r12, #12]
@ bl min_caml_print_int
@ bl min_caml_print_newline

mov r0, r12
bl min_caml_print_int
bl min_caml_print_newline

mov r0, #3
mov r1, #42

bl min_caml_create_array
push {r0}

ldr r0, [r11, #-4]
ldr r0, [r0]
bl min_caml_print_int
bl min_caml_print_newline

ldr r0, [r11, #-4]
ldr r0, [r0, #4]
bl min_caml_print_int
bl min_caml_print_newline

ldr r0, [r11, #-4]
ldr r0, [r0, #8]
bl min_caml_print_int
bl min_caml_print_newline

ldr r0, [r11, #-4]
ldr r0, [r0, #12]
bl min_caml_print_int
bl min_caml_print_newline
