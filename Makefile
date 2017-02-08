all:
	ocamlbuild -yaccflag -v -lib unix src/main.native; mv main.native f2bdd

clean:
	ocamlbuild -clean

