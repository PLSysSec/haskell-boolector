deps/boolector-dist:
	-git submodule init
	-git submodule update
	-cd deps/boolector && ./contrib/setup-lingeling.sh && ./contrib/setup-btor2tools.sh && ./configure.sh --shared --prefix ../../boolector-dist
	-cd deps/boolector/build && make && make install

clean:
	-rm -rf deps/boolector-dist
