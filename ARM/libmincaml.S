	.text
        .globl min_caml_exit
/* exit syscall */
min_caml_exit:
	mov	r0, #0
	mov	r7, #1  @ exit syscall
	swi	#0      @ perform syscall

  .globl min_caml_mmap
/* mmap syscall */
min_caml_mmap:
	stmfd	sp!, {lr}
  mov r0, #0
  mov r1, #4194304
  mov r2, #0x2
  mov r3, #0x22
  mov r4, #-1
  mov r5, #0
  mov r7, #192
  swi #0
  mov r12, r0 @r12 is the register used to store heap starting address
	ldmfd	sp!, {lr}
        bx	lr
        .size	min_caml_mmap, .-min_caml_mmap

/* create array function */
        .globl min_caml_abs_float
min_caml_abs_float:
	stmfd	sp!, {lr}
  vmov.f32 s0, r0
  vabs.f32 s0, s0
  vmov.f32 r0, s0
	ldmfd	sp!, {lr}
        bx	lr
        .size	min_caml_abs_float, .-min_caml_abs_float

  .globl min_caml_cos
/* cos utility function */
min_caml_cos:
	stmfd	sp!, {lr}
  vmov s0, r0             @ s0 = x
  vmov.f32 s1, #0.125     @ s1 = tolerance
  vmul.f32 s1, s1
  vmul.f32 s1, s1
  vmul.f32 s1, s1
  vmul.f32 s1, s1
  vmul.f32 s1, s1
  vmov.f32 s3, #1.0       @ s3 = factorial
  vsub.f32 s2, s3, s3     @ s2 = count = 0
  vmov.f32 s4, #30.0      @ s4 = old
  vmov.f32 s5, #1.0       @ s5 = current
  vmov.f32 s6, #1.0       @ s6 = new
  vmov.f32 s8, #-1.0      @ s8 = sign
  vmov.f32 s9, #1.0       @ s9 = 1
  coscheck:
    vsub.f32 s7, s4, s6   @ s7 = old - new
    vabs.f32 s7, s7       @ s7 = abs(old - new)
    vcmp.f32 s7, s1       @ compare with tolerance
    vmrs     APSR_nzcv, FPSCR    @ Get the flags into APSR.
    blt cosfalse            @ if abs(old - new) < tolerance then done
  costrue:
    vmul.f32 s5, s5, s0   @ current = current * x
    vmul.f32 s5, s5, s0   @ current = current * x
    vadd.f32 s2, s2, s9   @ count = count + 1
    vdiv.f32 s5, s5, s2   @ current = current / count
    vadd.f32 s2, s2, s9   @ count = count + 1
    vdiv.f32 s5, s5, s2   @ current = current / count
    vmul.f32 s5, s5, s8   @ current = current * -1
    vmov.f32 s4, s6       @ old = new
    vadd.f32 s6, s6, s5   @ new = new + current
    b coscheck
  cosfalse:
    vmov r0, s6
    ldmfd	sp!, {lr}
          bx	lr
          .size	min_caml_cos, .-min_caml_cos

  .globl min_caml_sin
/* sin utility function */
min_caml_sin:
	stmfd	sp!, {lr}
  vmov s0, r0             @ s0 = x
  vmov.f32 s1, #0.125     @ s1 = tolerance
  vmul.f32 s1, s1
  vmul.f32 s1, s1
  vmul.f32 s1, s1
  vmul.f32 s1, s1
  vmul.f32 s1, s1
  vmov.f32 s2, #1.0       @ s2 = count
  vmov.f32 s3, #1.0       @ s3 = factorial
  vmov.f32 s4, #30.0      @ s4 = old
  vmov.f32 s5, s0         @ s5 = current
  vmov.f32 s6, s0         @ s6 = new
  vmov.f32 s8, #-1.0      @ s8 = sign
  vmov.f32 s9, #1.0       @ s9 = 1
  sincheck:
    vsub.f32 s7, s4, s6   @ s7 = old - new
    vabs.f32 s7, s7       @ s7 = abs(old - new)
    vcmp.f32 s7, s1       @ compare with tolerance
    vmrs     APSR_nzcv, FPSCR    @ Get the flags into APSR.
    blt sinfalse            @ if abs(old - new) < tolerance then done
  sintrue:
    vmul.f32 s5, s5, s0   @ current = current * x
    vmul.f32 s5, s5, s0   @ current = current * x
    vadd.f32 s2, s2, s9   @ count = count + 1
    vdiv.f32 s5, s5, s2   @ current = current / count
    vadd.f32 s2, s2, s9   @ count = count + 1
    vdiv.f32 s5, s5, s2   @ current = current / count
    vmul.f32 s5, s5, s8   @ current = current * -1
    vmov.f32 s4, s6       @ old = new
    vadd.f32 s6, s6, s5   @ new = new + current
    b sincheck
  sinfalse:
    vmov r0, s6
    ldmfd	sp!, {lr}
          bx	lr
          .size	min_caml_sin, .-min_caml_sin

  .globl min_caml_sqrt
/* sqrt utility function */
min_caml_sqrt:
	stmfd	sp!, {lr}
  vmov s0, r0
  vsqrt.f32 s0, s0
  vmov r0, s0
	ldmfd	sp!, {lr}
        bx	lr
        .size	min_caml_sqrt, .-min_caml_sqrt

  .globl min_caml_float_of_int
/* float_of_int utility function */
min_caml_float_of_int:
	stmfd	sp!, {lr}
  vmov s0, r0
  vcvt.f32.s32 s0, s0
  vmov r0, s0 
	ldmfd	sp!, {lr}
        bx	lr
        .size	min_caml_float_of_int, .-min_caml_float_of_int

  .globl min_caml_int_of_float
/* int_of_float utility function */
min_caml_int_of_float:
	stmfd	sp!, {lr}
  vmov s0, r0
  vcvt.s32.f32 s0, s0
  vmov r0, s0 
	ldmfd	sp!, {lr}
        bx	lr
        .size	min_caml_int_of_float, .-min_caml_int_of_float

  .globl min_caml_truncate
/* truncate utility function */
min_caml_truncate:
  b min_caml_int_of_float

	.globl min_caml_print_newline
min_caml_print_newline:
	stmfd	sp!, {lr}
	mov	r0, #10
	bl	min_caml_print_char
	mov	r0, #0
	ldmfd	sp!, {lr}
        bx	lr
        .size	min_caml_print_newline, .-min_caml_print_newline

	.globl stringlength
stringlength:             @ note: r1 is untouched
	mov	r3, #0   @ number of char
        b	stringlength_first
stringlength_loop:
	add	r0, #1
	add	r3, #1
stringlength_first:
	ldrb	r2, [r0]  @ r2 <- current char
	cmp	r2, #0    @ r2 /= \0?
	bne	stringlength_loop	@ yes -> shift and again
        mov	r0, r3    @ return num of char
        bx	lr
        .size	stringlength, .-stringlength

	.globl min_caml_print_string
min_caml_print_string:
	stmfd	sp!, {lr}
        mov	r1, r0          @ save r0 in r1 untouched by stringlength
        bl	stringlength
        mov	r2, r0                   @ length of string
        mov	r0, #1                   @ on stdout
        mov	r7, #4                   @ write syscall
        swi	#0
        nop            @ why is that needed?
	ldmfd	sp!, {lr}
        bx	lr
        .size	min_caml_print_string, .-min_caml_print_string

	.data
hello_world_string:
	.asciz	"Hello world!\n"

	.text
	.globl min_caml_hello_world
min_caml_hello_world:
	stmfd	sp!, {lr}
	ldr	r0, =hello_world_string  @ the string
        bl	min_caml_print_string
	ldmfd	sp!, {lr}
        bx	lr
	.size min_caml_hello_world, .-min_caml_hello_world

	.globl min_caml_print_char
min_caml_print_char:
	sub	sp, #4   @ room for char
        strb	r0, [sp]
	mov	r0, #1 @ file descriptor
	mov	r1, sp
	mov	r2, #1
	mov	r7, #4 @ write syscall
	swi	#0      @ perform syscall
	nop
	add	sp, #4   @ get back room for char
        bx	lr
        .size	min_caml_print_char, .-min_caml_print_char
  
.val_10:
  .word 1203982336

  .globl min_caml_print_float
/* print float  function */
min_caml_print_float:
  stmfd sp!, {lr}
  vmov.f32 s2, r0
  bl min_caml_int_of_float
  bl min_caml_print_int
  mov r0, #46
  bl min_caml_print_char
  ldr r0, .val_10
  vmov.f32 s0, r0 
  vmul.f32 s2, s2, s0
  vmov r0, s2
  bl min_caml_int_of_float
  bl min_caml_print_int
  ldmfd sp!, {lr}
  bx lr

	.data
.string_for_int:
	.asciz "-2147483648"    @ 12 char are sufficient (for 32-bit ints, incl '\0')

@ Warning: does not work for -2147483648
	.text
	.globl	stringofint
stringofint:
	stmfd	sp!, {r4, r5}
	mov	r3, r0    @ the number
	ldr	r0, =.string_for_int  @ pointer to begining of buffer
        add	r0, #11     @ pointer to end of buffer (char '\0')
      @special case for -2147483648
	ldr	r2, .i2s_constants+4
        cmp	r3, r2
        moveq	r1, #56     @ char '8'
        subeq	r0, #1
        streq	r1, [r0]
        ldreq	r3, .i2s_constants+8
      @special case for 0
        cmp	r3, #0
        bne	.i2s_non_zero @ here number is zero
        moveq	r1, #48     @ char '0'
        subeq	r0, #1
        streq	r1, [r0]
        beq	.i2s_exit
	ldmfd	sp!, {lr}
.i2s_non_zero:
	movgt	r5, #43    @ char '+'
        movlt	r5, #45    @ char '-'
	rsblt	r3, r3, #0 @ get absolute number
	ldr	r4, .i2s_constants  @ load before loop
.i2s_positive:
      @ tricky stuff to divide r3 and get modulo 10
	smull	r2, r1, r4, r3
	mov	r2, r3, asr #31
	rsb	r2, r2, r1, asr #2
	add	r1, r2, r2, asl #2
	sub	r3, r3, r1, asl #1
	add	r3, r3, #48  @ add '0' to modulo
        sub	r0, #1
	strb	r3, [r0]
	mov	r3, r2       @ prepare result of division for next step
	cmp	r2, #0
        bne	.i2s_positive
        cmp	r5, #45    @ char '-'
        subeq	r0, #1
        streqb	r5, [r0]
.i2s_exit:
	ldmfd	sp!, {r4, r5}
        bx	lr
.i2s_constants:
	.word	1717986919
	.word	-2147483648
	.word	-214748364
	.size	stringofint, .-stringofint
        .align	2


        .globl min_caml_print_int
min_caml_print_int:
	stmfd	sp!, {lr}
	bl	stringofint
	bl	min_caml_print_string
	ldmfd	sp!, {lr}
        bx	lr
        .size min_caml_print_int, .-min_caml_print_int

/* create array function */
        .globl min_caml_create_array
min_caml_create_array:
	stmfd	sp!, {lr}
  mov r2, r0
  mov r0, r12
  @ r0 = return; r2 = size; r1 = init
        .globl ltrue_array
ltrue_array:
  cmp r2, #0
  ble lnext_array
  str r1, [r12]
  add r12, r12, #4
  sub r2, r2, #1
  bl  ltrue_array
        .globl lnext_array
lnext_array:
	ldmfd	sp!, {lr}
        bx	lr


/* create float array function */
        .globl min_caml_create_float_array
min_caml_create_float_array:
  b min_caml_create_array
/* stmfd	sp!, {lr}
  mov r2, r0
  mov r0, r12
  @ r0 = return; r2 = size; r1 = init
        .globl ltrue_float_array
ltrue_float_array:
  cmp r2, #0
  ble lnext_float_array
  str r1, [r12]
  add r12, r12, #4
  sub r2, r2, #1
  bl  ltrue_float_array
        .globl lnext_float_array
lnext_float_array:
	ldmfd	sp!, {lr}
        bx	lr
*/
