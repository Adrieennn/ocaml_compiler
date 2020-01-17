MAKEFLAGS += --no-print-directory
PROG=mincamlc
FAIL="$(KRED)FAILURE$(KNRM)"


all:
	ocamlbuild -lib unix main.byte
	mv main.byte $(PROG)

test: all test_typechecking test_asml_gen test_asm_gen

test_typechecking: all
	PROG=$(PROG) ./tests/typechecking/suite.sh

test_asml_gen: all
	PROG=$(PROG) ./tests/asml_gen/suite.sh

test_asm_gen: all
	PROG=$(PROG) ./tests/asm_gen/suite.sh

clean:
	rm -rf _build $(PROG) *.s *.asml
