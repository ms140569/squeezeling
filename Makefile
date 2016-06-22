.SUFFIXES:	.erl .beam

BIN = ebin
SRC = src

.erl.beam:
	erlc -o ${BIN} -W $<

ERL = erl -boot start_clean

MODS = bin_to_hex socket_server squeezeling hexdump

all: compile tags

shell: all
	${ERL} -pa ${BIN}

run: all
	${ERL} -pa ${BIN} -s squeezeling start

compile:
	erl -make

tags:
	etags `find . -name \*.erl`

clean:
	rm -rf ${BIN}/*.beam erl_crash.dump TAGS
