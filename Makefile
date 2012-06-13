ELIOMC = eliomc
OCAMLC = ocamlc

all:
	$(ELIOMC) *.ml
	$(OCAMLC) -a _server/*.cmo -o _server/cumulus.cma	
