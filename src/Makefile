ELIOMC = eliomc
ELIOMOPT = eliomopt
OCAMLOPT = ocamlopt
RM = rm -f
CP = cp

DEST = ../_server

NAME_BYTE = $(DEST)/cumulus.cma
NAME_OPT = $(DEST)/cumulus.cmxs
NAME_STATIC_OPT = $(DEST)/cumulus.cmxa
MODULES = html.ml \
	  utils.ml \
	  feed.mli \
	  feed.ml \
	  user.mli \
	  user.ml \
	  feeds.mli \
	  feeds.ml \
	  users.mli \
	  users.ml \
	  services.ml \
	  templates.mli \
	  templates.ml \
	  main.ml

OBJ_BYTE := $(patsubst %.ml, $(DEST)/%.cmo, $(MODULES))
OBJ_BYTE := $(patsubst %.mli, $(DEST)/%.cmi, $(OBJ_BYTE))
OBJ_OPT := $(patsubst %.ml, $(DEST)/%.cmx, $(MODULES))
OBJ_OPT := $(patsubst %.mli, $(DEST)/%.cmi, $(OBJ_OPT))

all: byte opt

byte: $(NAME_BYTE)
opt: $(NAME_OPT)

$(NAME_BYTE): $(OBJ_BYTE)
	$(ELIOMC) -a $(filter %.cmo, $^) -o $@

$(NAME_OPT): $(NAME_STATIC_OPT)
	$(OCAMLOPT) -shared -linkall $< -o $@

$(NAME_STATIC_OPT): $(OBJ_OPT)
	$(ELIOMOPT) -a $(filter %.cmx, $^) -o $@

$(DEST)/%.cmx: %.ml
	$(ELIOMOPT) -dir $(DEST) $< -o $@

$(DEST)/%.cmo: %.ml
	$(ELIOMC) -dir $(DEST) $< -o $@

$(DEST)/%.cmi: %.mli
	$(ELIOMC) -dir $(DEST) $< -o $@

clean:
	$(RM) $(OBJ_BYTE) $(OBJ_OPT) $(NAME_BYTE) $(NAME_OPT)

install:
	$(CP) $(NAME_BYTE) $(NAME_OPT) $(NAME_STATIC_OPT) ../data/style.css /tmp
