PROG=mincamlc

all: 
	ocamlbuild main.byte 
	mv main.byte $(PROG)

test: all test_typecheck 

test_typecheck: all
	PROG=$(PROG) ./tests/typechecking/suit.sh

clean:
	rm -rf _build

cleanest: clean
	rm -f $(PROG)
