ELIOMC = eliomc
RM = rm -f

DEST = _server

NAME = $(DEST)/cumulus.cma
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
	  templates.ml \
	  main.ml

OBJ := $(patsubst %.ml, $(DEST)/%.cmo, $(MODULES))
OBJ := $(patsubst %.mli, $(DEST)/%.cmi, $(OBJ))

all: $(OBJ_INTERFACES) $(NAME)

$(NAME): $(OBJ)
	$(ELIOMC) -a $(filter %.cmo, $^) -o $@

$(DEST)/%.cmo: %.ml
	$(ELIOMC) $< -o $@

$(DEST)/%.cmi: %.mli
	$(ELIOMC) $< -o $@

clean:
	$(RM) $(OBJ) $(NAME)
