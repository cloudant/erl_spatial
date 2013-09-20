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
cp ../../csEpsgStuff.cpp .
cp ../../Library.mak .
cp ../../csepsgstuff.h ../Include
make -fLibrary.mak

cd ../Dictionaries
make -fCompiler.mak
if ! [ -d "../../../priv/CsDict" ];
then
  mkdir ../../../priv/CsDict
fi

# simulate a key press
echo | ./CS_Comp . .
cp -R * ../../../priv/CsDict

cd ../../..




