
all: 
	ocamlbuild -lib unix main.byte 
	mv main.byte mincamlc

clean:
	rm -rf _build

cleanest: clean
	rm -f mincamlc
