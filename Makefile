ELIOMC = eliomc

SRC = utils.ml \
      feed.ml \
      feeds.ml \
      main.ml

all:
	$(ELIOMC) -a $(SRC) -o _server/cumulus.cma
