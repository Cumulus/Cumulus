## Sample Makefile for eliom application.

APP_NAME := dest/cumulus

## Packages required to build the server part of the application

SERVER_PACKAGES := macaque.syntax safepass

## Packages to be linked in the client part

CLIENT_PACKAGES :=

## Source files for the server part

SERVER_FILES := html.ml \
		utils.ml \
		db.mli \
		db.ml \
		services.ml \
		user.mli \
		user.ml \
		feed.mli \
		feed.ml \
		feeds.mli \
		feeds.ml \
		users.mli \
		users.ml \
		templates.mli \
		templates.ml \
		main.eliom

## Source files for the client part

CLIENT_FILES := main.eliom

## Required binaries

ELIOMC      := eliomc
ELIOMOPT    := eliomopt
ELIOMDEP    := eliomdep
JS_OF_ELIOM := js_of_eliom

## Where to put intermediate object files.
## - ELIOM_{SERVER,CLIENT}_DIR must be distinct
## - ELIOM_CLIENT_DIR mustn't be the local dir.
## - ELIOM_SERVER_DIR could be ".", but you need to
##   remove it from the "clean" rules...

export ELIOM_SERVER_DIR := _server
export ELIOM_CLIENT_DIR := _client
export ELIOM_TYPE_DIR   := .

#####################################

all: byte opt
byte:: ${APP_NAME}.cma ${APP_NAME}.js
opt:: ${APP_NAME}.cmxs ${APP_NAME}.js

#### Server side compilation #######

SERVER_INC  := ${addprefix -package ,${SERVER_PACKAGES}}

SERVER_OBJS := $(patsubst %.eliom,${ELIOM_SERVER_DIR}/%.cmo, ${SERVER_FILES})
SERVER_OBJS := $(patsubst %.mli,${ELIOM_SERVER_DIR}/%.cmi, ${SERVER_OBJS})
SERVER_OBJS := $(patsubst %.ml,${ELIOM_SERVER_DIR}/%.cmo, ${SERVER_OBJS})

${APP_NAME}.cma: ${SERVER_OBJS}
	${ELIOMC} -a -o $@ $(filter %.cmo, $^)
${APP_NAME}.cmxa: ${SERVER_OBJS:.cmo=.cmx}
	${ELIOMOPT} -a -o $@ $(filter %.cmx, $^)

${ELIOM_TYPE_DIR}/%.type_mli: %.eliom
	${ELIOMC} -infer ${SERVER_INC} $<

${ELIOM_SERVER_DIR}/%.cmi: %.mli
	${ELIOMC} -c ${SERVER_INC} $<

${ELIOM_SERVER_DIR}/%.cmo: %.ml
	${ELIOMC} -c ${SERVER_INC} $<
${ELIOM_SERVER_DIR}/%.cmo: %.eliom
	${ELIOMC} -c -noinfer ${SERVER_INC} $<

${ELIOM_SERVER_DIR}/%.cmx: %.ml
	${ELIOMOPT} -c ${SERVER_INC} $<
${ELIOM_SERVER_DIR}/%.cmx: %.eliom
	${ELIOMOPT} -c -noinfer ${SERVER_INC} $<

%.cmxs: %.cmxa
	$(ELIOMOPT) -shared -linkall -o $@ $<

##### Client side compilation ####

CLIENT_LIBS := ${addprefix -package ,${CLIENT_PACKAGES}}
CLIENT_INC  := ${addprefix -package ,${CLIENT_PACKAGES}}

CLIENT_OBJS := $(patsubst %.eliom,${ELIOM_CLIENT_DIR}/%.cmo, ${CLIENT_FILES})
CLIENT_OBJS := $(patsubst %.mli,${ELIOM_CLIENT_DIR}/%.cmi, ${CLIENT_OBJS})
CLIENT_OBJS := $(patsubst %.ml,${ELIOM_CLIENT_DIR}/%.cmo, ${CLIENT_OBJS})

${APP_NAME}.js: ${CLIENT_OBJS}
	${JS_OF_ELIOM} -o $@ ${CLIENT_LIBS} $(filter %.cmo, $^)

${ELIOM_CLIENT_DIR}/%.cmi: %.mli
	${JS_OF_ELIOM} -c ${CLIENT_INC} $<

${ELIOM_CLIENT_DIR}/%.cmo: %.eliom
	${JS_OF_ELIOM} -c ${CLIENT_INC} $<
${ELIOM_CLIENT_DIR}/%.cmo: %.ml
	${JS_OF_ELIOM} -c ${CLIENT_INC} $<

############

## Clean up

clean:
	-rm -f *.cm[ioax] *.cmxa *.cmxs *.o *.a *.annot
	-rm -f *.type_mli
	-rm -f ${APP_NAME}.js
	-rm -rf ${ELIOM_CLIENT_DIR} ${ELIOM_SERVER_DIR}

distclean:
	-rm -f *~ \#* .\#*

## Dependencies

depend: .depend
.depend: ${SERVER_FILES} ${CLIENT_FILES}
	$(ELIOMDEP) -server ${SERVER_INC} ${SERVER_FILES} > .depend
	$(ELIOMDEP) -client ${CLIENT_INC} ${CLIENT_FILES} >> .depend

## Warning: Dependencies towards *.eliom are not handled by eliomdep yet.

include .depend

## installation #########

STATICDIR      := /tmp

$(STATICDIR):
	mkdir -p $@

install: all $(STATICDIR)
	cp data/* $(STATICDIR)
	cp ${APP_NAME}.js $(STATICDIR)
