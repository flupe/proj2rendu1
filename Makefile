all:
	ocamlbuild -yaccflag -v -lib unix -lib str src/main.native; mv main.native f2bdd

clean:
	ocamlbuild -clean

