.SUFFIXES:	.erl .beam

BIN = ebin

.erl.beam:
	erlc -o ${BIN} -W $<

ERL = erl -boot start_clean

MODS = bin_to_hex socket_server squeezeling hexdump

all: compile tags

run: all
	${ERL} -pa ${BIN} -s squeezeling start

compile: ${MODS:%=%.beam} 

#$(EBIN)/%.beam: %.erl
#	erlc  -W -b beam -o $(BIN) $(EFLAGS) $<


tags:
	etags `find . -name \*.erl`

clean:
	rm -rf ${BIN}/*.beam erl_crash.dump TAGS
