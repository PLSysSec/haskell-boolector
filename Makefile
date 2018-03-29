BTOR=boolector-2.4.1-with-lingeling-bbc
EXT=.tar.bz2

.PHONY: all
all: download setup build

.PHONY: download
download:
	-rm $(BTOR)$(EXT)
	wget http://fmv.jku.at/boolector/$(BTOR)$(EXT)
	-rm -rf $(BTOR)
	tar xvfj $(BTOR)$(EXT)
	-rm $(BTOR)$(EXT)
	ln -s $(BTOR) boolector

.PHONY: setup setup-lingeling setup-boolector
setup: download setup-lingeling setup-boolector

.ONESHELL:
setup-lingeling:
	cd $(BTOR)
	-rm -rf lingeling*
	tar xf archives/lingeling*.tar.gz
	mv lingeling* lingeling


.ONESHELL:
setup-boolector:
	cd $(BTOR)
	-rm -rf boolector*
	tar xf archives/boolector*.tar.gz
	mv boolector* boolector

.PHONY: build build-lingeling build-boolector
build: setup build-lingeling build-boolector

.ONESHELL:
build-lingeling:
	cd $(BTOR)/lingeling
	./configure.sh -fPIC 
	make

.ONESHELL:
build-boolector:
	cd $(BTOR)/boolector 
	./configure.sh -shared
	make

clean:
	-rm -rf $(BTOR)
	-rm $(BTOR)
	-rm boolector

install:
	-cp $(BTOR)/boolector/build/libboolector.a /usr/lib/
	-cp $(BTOR)/boolector/build/libboolector.so /usr/lib/
