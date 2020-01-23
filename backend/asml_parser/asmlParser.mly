%{
open Asml
let addtyp x = (x, Type.gentyp ())
%}

/* tokens */
%token <int> INT
%token <float> FLOAT
%token PLUS
%token EQUAL
%token FEQUAL
%token LE
%token FLE
%token IF
%token THEN
%token ELSE
%token <Id.t> IDENT
%token LET
%token IN
%token DOT
%token ASSIGN
%token LPAREN
%token RPAREN
%token EOF
%token NEG
%token FNEG
%token MEM
%token FMUL
%token FDIV
%token FADD
%token FSUB
%token ADD
%token SUB
%token CALL 
%token CALLCLO
%token NEW
%token NOP
%token <Id.t> LABEL
%token UNDERSC
%token NIL


%right prec_let
%right prec_if
%right ASSIGN
%left EQUAL FEQUAL LE FLE GE
%right prec_unary_minus
%left prec_app
%left DOT

%type <Asml.exp> exp
%type <Asml.t> asmt
%type <Asml.id_or_imm> ident_or_imm
%type <Asml.fundef> fundef
%start fundef

%%

ident_or_imm:
| INT
  { Int($1)}
| IDENT
  { Var($1)}

exp: /* expressions */
| NOP
  { Unit }
| LPAREN exp RPAREN
    { $2 }
| LPAREN RPAREN /* maybe useless */
    { Unit }
| INT
    { Int($1) }
| IDENT
    { Var($1) }
| LABEL
    { Label($1) }
| NEG IDENT
    { Neg($2) }
| FNEG IDENT
    { FNeg($2) }
| FADD IDENT IDENT
    { FAdd($2, $3) }
| FSUB IDENT IDENT
    { FSub($2, $3) }
| FMUL IDENT IDENT
    { FMul($2, $3) }
| FDIV IDENT IDENT
    { FDiv($2, $3) }
/*
| NEW ident_or_imm
    { Mem() } 
*/
| ADD IDENT ident_or_imm
    { Add($2, $3) }
| SUB IDENT ident_or_imm
    { Sub($2, $3) }
| MEM LPAREN IDENT PLUS ident_or_imm RPAREN
    { Ld($3, $5) }
| MEM LPAREN IDENT PLUS ident_or_imm RPAREN ASSIGN IDENT
    { St($3, $5, $8) }
| IF IDENT EQUAL ident_or_imm THEN asmt ELSE asmt
    %prec prec_if
    { IfEq($2, $4, $6, $8) }
| IF IDENT LE ident_or_imm THEN asmt ELSE asmt
    %prec prec_if
    { IfLEq($2, $4, $6, $8) }
| IF IDENT FEQUAL IDENT THEN asmt ELSE asmt
    %prec prec_if
    { IfFEq($2, $4, $6, $8) }
| IF IDENT FLE IDENT THEN asmt ELSE asmt
    %prec prec_if
    { IfFLEq($2, $4, $6, $8) }
| CALL LABEL formal_args
    %prec prec_app
    { CallDir($2, $3) }
| CALLCLO IDENT formal_args
    { CallCls($2, $3) }
| NEW INT
    { New($2) }
| error
    { failwith
	(Printf.sprintf "parse error near characters %d-%d"
	   (Parsing.symbol_start ())
	   (Parsing.symbol_end ())) }

asmt:
| LPAREN asmt RPAREN
    { $2 }
| LET IDENT EQUAL exp IN asmt
    %prec prec_let
    { Let(addtyp $2, $4, $6) }
| exp
    { Ans($1) }

fundef:
| LET UNDERSC EQUAL asmt
    { Main($4) }
| LET LABEL EQUAL FLOAT fundef
    { Fl($2, $4, $5) }
| LET LABEL formal_args EQUAL asmt fundef
    { Fu({name = $2; args = $3; body = $5}, $6) }

formal_args:
| IDENT formal_args
    { $1 :: $2 }
| IDENT
    { [$1] }
| NIL
    { [] }

actual_args:
| actual_args exp
    %prec prec_app
    { $1 @ [$2] }
| exp
    %prec prec_app
    { [$1] }

toplevel:
| fundef
    { $1 }

