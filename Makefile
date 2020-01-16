PROG=mincamlc
ASML=-asml

all:
	ocamlbuild -lib unix main.byte
	mv main.byte $(PROG)

test: all test_typechecking test_asml_gen

test_typechecking: all
	PROG=$(PROG) ./tests/typechecking/suite.sh

test_asml_gen: all
	PROG="$(PROG) $(ASML)" ./tests/asml_gen/suite.sh

clean:
	rm -rf _build

cleanest: clean
	rm -f $(PROG)
