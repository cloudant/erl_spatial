This is Erlang NIF binding to the libspatialindex C++ library using the C interface.

After installing

libspatialindex - http://libspatialindex.github.com/
geos - http://trac.osgeo.org/geos/


* Note *

To compile geos on a mac (this will be fixed in a GEOS future release)

./configure CC=/usr/bin/clang CXX=/usr/bin/clang++
make
sudo make install

run the following;

Run 'make' to build the binding, 'make test' to run the unit tests,
and 'make doc' will generate Edoc documentation.

