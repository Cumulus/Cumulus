ELIOMC = eliomc

all:
	$(ELIOMC) -a *.ml -o _server/cumulus.cma
