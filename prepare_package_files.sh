#!/bin/bash

# pull libgeos, csmap and libspatialindex
cd c_src
if ! [ -d "geos-3.4.2" ];
then
  wget http://download.osgeo.org/geos/geos-3.4.2.tar.bz2
  tar -jxf geos-3.4.2.tar.bz2
fi
# both libspatialindex and CsMap will be using fixed version as of the next release
if ! [ -d "libspatialindex" ];
then
  git clone https://github.com/cloudant/libspatialindex.git
  cd libspatialindex; git checkout 1.0.0; cd ..
fi
if ! [ -d "CsMap" ];
then
  svn checkout http://svn.osgeo.org/metacrs/csmap/branches/14.01/CsMapDev CsMap
fi
cd ..
