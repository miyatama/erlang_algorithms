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
	cd search; make; cd ..
	cd graph; make; cd ..
	cd route_search; make; cd ..
	cd network_flow; make; cd ..
	cd computational_geometry; make

clean:
	rm -fr *.beam erl_crash.dump
	cd sort; make clean; cd ..
	cd search; make clean; cd ..
	cd graph; make clean; cd ..
	cd route_search; make clean; cd ..
	cd network_flow; make clean; cd ..
	cd computational_geometry; make clean
