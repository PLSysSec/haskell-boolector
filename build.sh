#!/bin/bash

cat LICENSE

echo "#################### download and unpack Boolector source"

# BTOR=boolector-2.0.6-with-lingeling-azd
BTOR=boolector-2.1.1-with-lingeling-b85
EXT=.tar.bz2

rm -f $BTOR$EXT
wget http://fmv.jku.at/boolector/$BTOR$EXT

rm -rf $BTOR
tar xvfj $BTOR$EXT

echo "#################### configure and compile Boolector"

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

echo "#################### you need to put cbits/boolector/libboolector.{a,so}"
echo "#################### in a place where your (dynamic) linker finds it."

echo "#################### prepare for building the Haskell binding"

rm -f cbits
ln -s $BTOR cbits

echo "#################### now type:  cabal install -v"

