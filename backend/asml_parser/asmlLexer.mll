{
open AsmlParser
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']

rule token = parse
| space+
    { token lexbuf }
| "(*"
    { comment lexbuf; 
      token lexbuf }
| "()"
    { NIL }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| digit+ 
    { INT(int_of_string (Lexing.lexeme lexbuf)) }
| digit+ ('.' digit*)? (['e' 'E'] ['+' '-']? digit+)?
    { FLOAT(float_of_string (Lexing.lexeme lexbuf)) }
| '+'
    { PLUS }
| '='
    { EQUAL }
| "=."
    { FEQUAL }  
| "<="
    { LE }
| "<=."
    { FLE }
| ">="
    { GE }
| "if"
    { IF }
| "then"
    { THEN }
| "else"
    { ELSE }
| "let"
    { LET }
| "in"
    { IN }
| '.'
    { DOT }
| "neg"
    { NEG }
| "fneg"
    { FNEG }
| "mem"
    { MEM }
| "fmul"
    { FMUL }
| "fdiv"
    { FDIV }
| "fadd"
    { FADD }
| "fsub"
    { FSUB }
| "<-"
    { ASSIGN }
| "add"
    { ADD }
| "sub"
    { SUB }
| "call"
    { CALL }
| "new"
    { NEW }
| "nop"
    { NOP }
| "call_closure"
    { CALLCLO }
| "apply_closure"
    { APPCLO }
| '_'
    { UNDERSC }
| '_' lower (digit|lower|upper|'_')*
    { LABEL(Lexing.lexeme lexbuf) }
| lower (digit|lower|upper|'_')*
    { IDENT(Lexing.lexeme lexbuf) }
| "%self"
    { IDENT(Lexing.lexeme lexbuf) }
| eof
    { EOF }
| _
    { failwith
	(Printf.sprintf "you are stupid because unknown token %s near characters %d-%d"
	   (Lexing.lexeme lexbuf)
	   (Lexing.lexeme_start lexbuf)
	   (Lexing.lexeme_end lexbuf)) }
and comment = parse
| "*)"
    { () }
| "(*"
    { comment lexbuf;
      comment lexbuf }
| eof
    { Format.eprintf "warning: unterminated comment@." }
| _
    { comment lexbuf }
