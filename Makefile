MAKEFLAGS += --no-print-directory --silent
PROG=mincamlc
FAIL="$(KRED)FAILURE$(KNRM)"


all:
	ocamlbuild -lib unix main.byte
	mv main.byte $(PROG)

doc: all
	mkdir -p _build/doc
	ocamldoc -html -colorize-code  _build/main.ml _build/utils/*.ml \
		_build/backend/*/*.mli _build/frontend/*.ml -I _build/utils \
		-I _build/backend/asml_parser -I _build/backend/register \
		-I _build/backend/asm_gen	-I _build/frontend -d _build/doc
	echo -e "\e[42m\033[1mDocumentation ready at: _build/doc/index.html\033[0m"

test: clean all test_typechecking test_asml_gen test_asm_gen test_asm_output

test_typechecking: all
	PROG=$(PROG) ./tests/typechecking/suite.sh

test_asml_gen: all
	PROG=$(PROG) ./tests/suite_asml_gen.sh

test_asm_gen: all
	PROG=$(PROG) ./tests/suite_asm_gen.sh

test_asm_output: all
	PROG=$(PROG) ./tests/suite_asm_output.sh

clean:
	rm -rf _build $(PROG) *.s *.asml ARM/*.ml* *.output
