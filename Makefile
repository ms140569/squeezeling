.SUFFIXES:	.erl .beam

.erl.beam:
	erlc -W $<

ERL = erl -boot start_clean

MODS = echo_server main socket_server

all:	compile
	${ERL} -pa . -s echo_server start

compile:	${MODS:%=%.beam}

clean:
	rm -rf *.beam erl_crash.dump
