open Asml
open Register

let rec prog_to_asm prog reg =
{|
  .global _start

_start:
  bl min_caml_print_int
|}
