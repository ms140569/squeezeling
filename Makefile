.SUFFIXES:	.erl .beam

.erl.beam:
	erlc -W $<

ERL = erl -boot start_clean

MODS = socket_server squeezeling

all:	compile
	${ERL} -pa . -s squeezeling start

compile:	${MODS:%=%.beam}

clean:
	rm -rf *.beam erl_crash.dump
