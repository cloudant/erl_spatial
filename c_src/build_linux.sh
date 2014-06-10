#!/bin/bash
cd c_src

cd libspatialindex
./autogen.sh
./configure
make

cd ../geos-3.4.2
./configure
make

cd ../CsMap/Source
cp ../../Library.mak .
make -fLibrary.mak

cd ../Dictionaries
cp ../../Compiler.mak .
make -fCompiler.mak

rm -rf ../../../priv/CsDict
mkdir -p ../../../priv/CsDict

# simulate a key press
echo | ./CS_Comp . .
cp -R * ../../../priv/CsDict

cd ../../..
