all: BDD

FILES = BDD.mli BDD.ml

BDD: $(FILES)
	ocamlc -c -i $(FILES)