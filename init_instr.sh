set -e

rm -rf /tvg/build/*
rm -rf /tvg/install/*

export CC="/root/.local/bin/tvg-exe /tvg"
export CFLAGS="-w -I/usr/include/i386-linux-gnu"
export LDFLAGS="-L/usr/lib/i386-linux-gnu"
export LD_LIBRARY_PATH=/tvg/tvg/incs
export LIBRARY_PATH=/usr/lib/i386-linux-gnu

pushd

cd /tvg/tvg

stack install --allow-different-user --ghc-options -O3 --force-dirty

cd /tvg/tvg/incs
cp data.c.start data.c; cp data.h.start data.h
hsc2hs CovStats.hsc && ghc -fPIC -dynamic -c CovStats.hs
ghc -shared -dynamic -fPIC -no-hs-main -DQUIET -I. data.c CovStats.o -o libdata.so -optl-Wl,-rpath,/usr/lib/ghc/ -lHSrts_thr-ghc7.10.3 -optl-Wl,-L/usr/lib/ghc/binar_3uXFWMoAGBg0xKP9MHKRwi -lHSbinary-0.7.5.0-3uXFWMoAGBg0xKP9MHKRwi-ghc7.10.3 -optl-Wl,-rpath,/usr/lib/ghc/binar_3uXFWMoAGBg0xKP9MHKRwi/ -optl-Wl,-L/usr/lib/ghc/direc_0hFG6ZxK1nk4zsyOqbNHfm -lHSdirectory-1.2.2.0-0hFG6ZxK1nk4zsyOqbNHfm-ghc7.10.3 -optl-Wl,-rpath,/usr/lib/ghc/direc_0hFG6ZxK1nk4zsyOqbNHfm

cd /tvg/build
../gcc-4.7.4/configure --disable-checking --enable-languages=c --disable-multiarch --disable-multilib --enable-shared --enable-threads=posix --program-suffix=-instr --with-gmp=/usr --with-mpc=/usr/lib --with-mpfr=/usr/lib --without-included-gettext --with-system-zlib --with-tune=generic --prefix=/tvg/install/gcc-4.7.4 --disable-bootstrap --disable-build-with-cxx

make

sed -i -e 's/_INSTR = True/_INSTR = False/g' /tvg/tvg/app/Main.hs
stack install --allow-different-user --ghc-options -O3 --force-dirty

make install

sed -i -e 's/_INSTR = False/_INSTR = True/g' /tvg/tvg/app/Main.hs

popd
