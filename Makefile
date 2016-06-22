.SUFFIXES:	.erl .beam

.erl.beam:
	erlc -W $<

ERL = erl -boot start_clean

MODS = bin_to_hex socket_server squeezeling hexdump

all: compile tags

run: all
	${ERL} -pa . -s squeezeling start

compile: ${MODS:%=%.beam} 

tags:
	etags `find . -name \*.erl`

clean:
	rm -rf *.beam erl_crash.dump TAGS
