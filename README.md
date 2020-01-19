## Organization of the project

ARM/     arm source example and compilation with libmincaml   
asml/    asml examples
doc/     all the documentation, start with index.hml
mincaml/ MinCaml examples
ocaml/   MinCaml parser in OCaml, if you do the project in OCaml
scripts/ put your test scripts and symbolic links there, and add this 
         directory to your path
tests/   put your tests there
tools/   asml intepreter (linux binary)

We recommend that you add scripts/ and the dir that contains mincamlc to your
PATH.


## Usage:

  `./mincamlc  filenames`

  -o          Outputs to file <file>
  -h          Display help
  -v          Display compiler's version
  -t          Only do typechecking
  -asml       Print asml
  -from-asml  Compile from ASML input file
  -help       Display this list of options
  --help      Display this list of options

## Testing

The whole compiler can be tested using `make test` which is going to run
all of our smaller test suites.

To run specific test suites, run:

`make test_typechecking`

`make test_asml_gen`

`make test_asm_gen`

`make test_asm_output`

## Specifications of the project

The project currently supports arithmetic expressions, function definitions 
assuming it is a pure function depending on its own arguments (but not
closures), calls to functions and if-then-else statements.
It also supports parsing ASML directly and thus generating assembly from it.

### Register Allocation

The compiler now always spills variables and parameters to the stack, except
for predefined functions, in which it uses registers r0-r3.
After ASML generation, each variable is assigned a negative offset from the
frame pointer.
Parameters to functions are assigned a positive offset.

### Typing

The compiler currently first generates all typing equations and resolves them
afterwards with the unification function. An occurs check is used to detect
cyclic types.

Differences from the paper: If after the type inference there are still some
unresolved type variables the current functions fail. The paper suggests to
arbitrarily replace those type variables by Int. The current approach makes it
easier to detect fault in our type inference implementation.

Furthermore we do not yet implement the external variable environment i.e. we
fail when we encounter unbound variables.

### Remark

User-defined functions should not start with `_min_caml_`.
