.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

.yrl.erl:
	erlc -W $<

ERL = erl -boot start_clean

MODS = ford_fullkerson \
	ford_fullkerson_with_breadth_first \
	biporate_matching \
	augmenting_path \
	minimum_cost \
	transhipment \
	transport \
	allocation

all: compile

compile: ${MODS:%=%.beam}

clean:
	rm -fr *.beam erl_crash.dump
