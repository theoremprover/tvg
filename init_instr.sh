#!/bin/bash

set -e

pushd /tvg

rm -rf build/*
rm -rf install/*
rm -rf gcc-4.7.4
unzip -qo gcc-4.7.4.zip

cd tvg

sed -i -e 's/_INSTR = False/_INSTR = True/g' /tvg/tvg/app/Main.hs
stack install --allow-different-user --ghc-options -O3 --force-dirty

cd /tvg/tvg/incs
cp data.c.start data.c; cp data.h.start data.h

rm -f CovStats.hs CovStats.hi CovStats_stub.h CovStats.o libdata.so data.o
echo "Compiling CovStats.hsc"
/root/.local/bin/hsc2hs CovStats.hsc

echo "Generate CovStats_stub.h"
stack --allow-different-user --stack-yaml /tvg/tvg/stack.yaml ghc -- CovStats.hs

cd /tvg/tvg
stack --allow-different-user --stack-yaml /tvg/tvg/stack.yaml ghc -- -shared -threaded -dynamic -DQUIET -fPIC -no-hs-main -I/tvg/tvg/incs /tvg/tvg/incs/data.c /tvg/tvg/incs/CovStats.hs -o /tvg/tvg/incs/libdata.so -lHSrts_thr-ghc8.4.3 -lffi

rm cov.dat

cd /tvg/build
../gcc-4.7.4/configure --disable-checking --enable-languages=c --disable-multiarch --disable-multilib --enable-shared --enable-threads=posix --program-suffix=-instr --with-gmp=/usr --with-mpc=/usr/lib --with-mpfr=/usr/lib --without-included-gettext --with-system-zlib --with-tune=generic --prefix=/tvg/install/gcc-4.7.4 --disable-bootstrap --disable-build-with-cxx

make

sed -i -e 's/_INSTR = True/_INSTR = False/g' /tvg/tvg/app/Main.hs
stack install --allow-different-user --ghc-options -O3 --force-dirty

make install

sed -i -e 's/_INSTR = False/_INSTR = True/g' /tvg/tvg/app/Main.hs
stack install --allow-different-user --ghc-options -O3 --force-dirty

popd
