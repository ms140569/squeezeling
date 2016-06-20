.SUFFIXES:	.erl .beam

.erl.beam:
	erlc -W $<

ERL = erl -boot start_clean

MODS = bin_to_hex socket_server squeezeling hexdump

all:	compile
	${ERL} -pa . -s squeezeling start

compile:	${MODS:%=%.beam}

clean:
	rm -rf *.beam erl_crash.dump
