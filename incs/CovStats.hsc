{-# LANGUAGE ForeignFunctionInterface,ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module CovStats where

import Control.Applicative ((<$>), (<*>))
import Foreign
import Foreign.C
import Text.Printf
import Control.Monad

#include "data.h"

#if __GLASGOW_HASKELL__ < 800
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#endif

{-
hsc2hs CovStats.hsc && ghc -fPIC -c CovStats.hs
ghc -shared -dynamic -fPIC -no-hs-main -I. data.c CovStats.o -o libdata.so -optl-Wl,-rpath,/usr/lib/ghc/ -lHSrts_thr-ghc7.10.3
For testing:
ghc -fPIC -no-hs-main -I. CovStats.o foreigntest.c -o foreigntest
-}


{-
typedef struct {
	long line, column;
	long cnt; }
	COUNTER;
-}
data Counter = Counter { lineC :: Int32, columnC :: Int32, cntC :: Int32 } deriving Show
instance Storable Counter where
	alignment _ = #{alignment COUNTER}
	sizeOf _    = #{size COUNTER}
	peek ptr    = Counter <$>
		#{peek COUNTER, line} ptr <*>
		#{peek COUNTER, column} ptr <*>
		#{peek COUNTER, cnt} ptr
	poke ptr (Counter line col cnt) = do
		#{poke COUNTER, line} ptr line
		#{poke COUNTER, column} ptr col
		#{poke COUNTER, cnt} ptr cnt

{-
typedef struct {
	char sourcefilename[256];
	char output_filename[256];
	long num_counters;
	COUNTER counters[];
} SRCFILE;
-}
data SrcFile = SrcFile { sourceFilenameS :: String, outputFilenameS :: String, countersS :: [Counter] } deriving Show
instance Storable SrcFile where
	alignment _ = #{alignment SRCFILE}
	sizeOf _    = #{size SRCFILE}
	peek ptr    = SrcFile <$>
		peekCString (#{ptr SRCFILE, sourcefilename} ptr) <*>
		peekCString (#{ptr SRCFILE, output_filename} ptr) <*>
		do
			num_counters <- #{peek SRCFILE, num_counters} ptr
			peekArray num_counters $ #{ptr SRCFILE, counters} ptr
	poke ptr (SrcFile sourcefn outputfn cnts) = do
		withCStringLen (take (#{const MAX_FILENAME_LEN}) sourcefn) $ uncurry (copyArray (#{ptr SRCFILE, sourcefilename} ptr))
		withCStringLen (take (#{const MAX_FILENAME_LEN}) outputfn) $ uncurry (copyArray (#{ptr SRCFILE, output_filename} ptr))
		#{poke SRCFILE, num_counters} ptr (length cnts)
		pokeArray (#{ptr SRCFILE, counters} ptr) cnts

foreign export ccall show_stats :: Ptr (Ptr SrcFile) -> Int -> IO ()
show_stats ptr_ptr_srcfiles num_srcfiles = do
	ptrs_srcfiles <- peekArray num_srcfiles ptr_ptr_srcfiles
	srcfiles <- mapM peek ptrs_srcfiles
	covs <- forM srcfiles $ \ (SrcFile sourcefn outputfn counters) -> do
		let
			n_cov :: Int = sum $ map (min 1 . fromIntegral . cntC) counters
			n_stmts :: Int = length counters
		putStrLn $ printf "%6i of %6i statements, %5.1f %% coverage in %s (compiled into %s)" n_cov n_stmts (cov_pct n_cov n_stmts) sourcefn outputfn
		return (n_cov,n_stmts)
	let (n_covs,n_stmtss) = unzip covs
	putStrLn $ printf "Overall coverage: %5.1f %%" (cov_pct (sum n_covs) (sum n_stmtss))
	where
	cov_pct :: (Int,Int) -> Float
	cov_pct (n_cov,n_stmts) = if n_stmts<=0 then 100.0 else 100.0* (fromIntegral n_cov) / (fromIntegral n_stmts)