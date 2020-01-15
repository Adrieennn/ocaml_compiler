	.data
x: .word 3
y: .word 2
  .text
	
  .global _start
_start:
  ldr    r4, addr_x @ r4 <- @x
  ldr    r5, [r4] @ r5 <- x
  ldr    r6, addr_y @ r6 <- @y
  ldr    r7, [r6] @ r7 <- y
  add r8, r5, r7
  mov r0, r8 @ print_int reads in r0
  bl min_caml_print_int

addr_x: .word x
addr_y: .word y
