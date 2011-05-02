all: BDDgui.exe

BDDgui.exe: 
	ocamlc -c BDD.ml
	ocamllex lexer.mll
	ocamlyacc parser.mly
	sed '1i\
    open BDD;;' parser.mli > parsercopy.mli
	mv parsercopy.mli parser.mli
	ocamlc BDD.cmo parser.mli
	ocamlc BDD.cmo lexer.ml
	ocamlc BDD.cmo parser.ml
	ocamlc BDD.cmo print.ml
	ocamlc -g -o BDDgui.exe BDD.cmo lexer.cmo parser.cmo print.cmo BDDgui.ml
	
clean: 
	rm -f BDDgui.exe camlprog* lexer.ml parser.ml parser.mli *.cmi *.cmo