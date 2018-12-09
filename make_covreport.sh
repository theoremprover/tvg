source init_env.sh

stack install --allow-different-user --ghc-options -O3 --force-dirty
#cp incs/data.c.start incs/data.c; cp incs/data.h.start incs/data.h
#stack --allow-different-user --stack-yaml /tvg/tvg/stack.yaml ghc -- -shared -threaded -dynamic -fPIC -no-hs-main -I/tvg/tvg/incs /tvg/tvg/incs/data.c /tvg/tvg/incs/CovStats.hs -o /tvg/tvg/incs/libdata.so -lHSrts_thr-ghc8.4.3 -lffi
#/root/.local/bin/tvg-exe /tvg test2.c -o test2
#rm -rf cov.dat
#stack --allow-different-user --stack-yaml /tvg/tvg/stack.yaml ghc -- -shared -threaded -dynamic -fPIC -no-hs-main -I/tvg/tvg/incs /tvg/tvg/incs/data.c /tvg/tvg/incs/CovStats.hs -o /tvg/tvg/incs/libdata.so -lHSrts_thr-ghc8.4.3 -lffi
#./test2
/root/.local/bin/covreport-exe cov1.dat 2>covreport.out
