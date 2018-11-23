#!/bin/bash

set -e

pushd /tvg

rm -rf build/*
rm -rf install/*
rm -rf gcc-4.7.4
unzip -qo gcc-4.7.4.zip
chmod -R 777 gcc-4.7.4

source /tvg/tvg/make_tvg.sh

rm -f cov.dat

cd /tvg/build
../gcc-4.7.4/configure --disable-checking --enable-languages=c --disable-multiarch --disable-multilib --enable-shared --enable-threads=posix --program-suffix=-instr --with-gmp=/usr --with-mpc=/usr/lib --with-mpfr=/usr/lib --without-included-gettext --with-system-zlib --with-tune=generic --prefix=/tvg/install/gcc-4.7.4 --disable-bootstrap --disable-build-with-cxx

make

cd /tvg/tvg
sed -i -e 's/_INSTR = True/_INSTR = False/g' /tvg/tvg/app/Main.hs
stack install --allow-different-user --ghc-options -O3 --force-dirty

cd /tvg/build
make install

cd /tvg/tvg
sed -i -e 's/_INSTR = False/_INSTR = True/g' /tvg/tvg/app/Main.hs
stack install --allow-different-user --ghc-options -O3 --force-dirty

stack --allow-different-user --stack-yaml /tvg/tvg/stack.yaml ghc -- -shared -threaded -dynamic -fPIC -no-hs-main -I/tvg/tvg/incs /tvg/tvg/incs/data.c /tvg/tvg/incs/CovStats.hs -o /tvg/tvg/incs/libdata.so -lHSrts_thr-ghc8.4.3 -lffi

popd
