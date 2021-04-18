.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

.yrl.erl:
	erlc -W $<

ERL = erl -boot start_clean

MODS = search_funcs \
	sequential_search

all: compile

compile: ${MODS:%=%.beam}

clean:
	rm -fr *.beam erl_crash.dump
