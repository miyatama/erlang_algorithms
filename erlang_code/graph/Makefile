.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

.yrl.erl:
	erlc -W $<

ERL = erl -boot start_clean

MODS = brute_force \
			 depth_first_search \
			 breadth_first_search \
			 dijkstras_algorithm \
			 dijkstras_algorithm_dense_graph \
			 bellman_ford \
			 floyd_warshall \
			 prims_algorithm

all: compile

compile: ${MODS:%=%.beam}

clean:
	rm -fr *.beam erl_crash.dump
