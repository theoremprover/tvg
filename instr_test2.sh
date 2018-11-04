#!/bin/bash

set -e

pushd /tvg/tvg

rm -f cov.dat
/root/.local/bin/tvg-exe /tvg test2.c -o test2

set +e
./test2
set -e

/root/.local/bin/covreport-exe cov.dat

popd
