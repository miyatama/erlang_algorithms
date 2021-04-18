.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

.yrl.erl:
	erlc -W $<

ERL = erl -boot start_clean

MODS = sort_funcs \
	bubble_sort \
	insertion_sort \
	selection_sort \
	heap_sort \
	quick_sort \
	bucket_sort \
	merge_sort \
	cocktail_shaker_sort	

all: compile

compile: ${MODS:%=%.beam}

clean:
	rm -fr *.beam erl_crash.dump
