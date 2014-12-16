#!/bin/bash

# get and unpack source

BTOR=boolector-2.0.1-with-lingeling-azd
EXT=.tar.bz2

rm -f $BTOR$EXT
wget http://fmv.jku.at/boolector/$BTOR$EXT

rm -rf $BTOR
tar xvfj $BTOR$EXT

# this is from the makefile of boolector
# but we need to add -fPIC / -shared :

pushd $BTOR
rm -rf lingeling*
tar xf archives/lingeling*.tar.gz
mv lingeling* lingeling
( cd lingeling && ./configure.sh -fPIC && make )
rm -rf boolector*
tar xf archives/boolector*.tar.gz
mv boolector* boolector
( cd boolector && ./configure -shared && make )
popd

# now we can build the Haskell binding

rm -f cbits
ln -s $BTOR cbits

cabal install -v
