#!/bin/bash

pushd /tvg/tvg

set -e

source init_env.sh

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
stack --allow-different-user --stack-yaml /tvg/tvg/stack.yaml ghc -- -shared -threaded -dynamic -fPIC -no-hs-main -I/tvg/tvg/incs /tvg/tvg/incs/data.c /tvg/tvg/incs/CovStats.hs -o /tvg/tvg/incs/libdata.so -lHSrts_thr-ghc8.4.3 -lffi

rm -f cov.dat

popd
