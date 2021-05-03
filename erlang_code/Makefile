.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

.yrl.erl:
	erlc -W $<

ERL = erl -boot start_clean

all: compile

compile: ${MODS:%=%.beam} subdirs

subdirs:
	cd sort; make; cd ..
	cd search; make

clean:
	rm -fr *.beam erl_crash.dump
	cd sort; make clean; cd ..
	cd search; make clean
