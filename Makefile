ELIOMC = eliomc
ELIOMOPT = eliomopt
OCAMLOPT = ocamlopt
RM = rm -f
CP = cp

DEST = dest
DEST_SERVER = _server
DEST_CLIENT = _client

STATIC_DIR = /tmp

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
CSS_FILES = data/style.css

OBJ_BYTE := $(patsubst %.ml, $(DEST_SERVER)/%.cmo, $(MODULES))
OBJ_BYTE := $(patsubst %.mli, $(DEST_SERVER)/%.cmi, $(OBJ_BYTE))
OBJ_OPT := $(patsubst %.ml, $(DEST_SERVER)/%.cmx, $(MODULES))
OBJ_OPT := $(patsubst %.mli, $(DEST_SERVER)/%.cmi, $(OBJ_OPT))

all: byte opt

byte: $(NAME_BYTE)
opt: $(NAME_OPT)

$(NAME_BYTE): $(OBJ_BYTE)
	$(ELIOMC) -a $(filter %.cmo, $^) -o $@

$(NAME_OPT): $(NAME_STATIC_OPT)
	$(OCAMLOPT) -shared -linkall $< -o $@

$(NAME_STATIC_OPT): $(OBJ_OPT)
	$(ELIOMOPT) -a $(filter %.cmx, $^) -o $@

$(DEST_SERVER)/%.cmx: %.ml
	$(ELIOMOPT) -dir $(DEST_SERVER) $< -o $@

$(DEST_SERVER)/%.cmo: %.ml
	$(ELIOMC) -dir $(DEST_SERVER) $< -o $@

$(DEST_SERVER)/%.cmi: %.mli
	$(ELIOMC) -dir $(DEST_SERVER) $< -o $@

clean:
	$(RM) $(OBJ_BYTE) $(OBJ_OPT) $(NAME_BYTE) $(NAME_OPT)

install:
	$(CP) $(NAME_BYTE) $(NAME_OPT) $(NAME_STATIC_OPT) $(CSS_FILES) $(STATIC_DIR)
