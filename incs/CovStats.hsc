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

ghc -shared -fPIC -no-hs-main -I. CovStats.o data.c -o libdata.so

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
data SrcFile = SrcFile { sourceFilenameS :: CString, outputFilenameS :: CString, countersS :: [Counter] }
instance Storable SrcFile where
	alignment _ = #{alignment SRCFILE}
	sizeOf _    = #{size SRCFILE}
	peek ptr    = SrcFile <$>
		#{peek SRCFILE, sourcefilename} ptr <*>
		#{peek SRCFILE, output_filename} ptr <*> do
			num_counters <- #{peek SRCFILE, num_counters} ptr
			peekArray num_counters $ #{ptr SRCFILE, counters} ptr
	poke ptr (SrcFile sourcefn outputfn cnts) = do
		#{poke SRCFILE, sourcefilename} ptr sourcefn
		#{poke SRCFILE, output_filename} ptr outputfn
		#{poke SRCFILE, num_counters} ptr (length cnts)
		pokeArray (#{ptr SRCFILE, counters} ptr) cnts


foreign export ccall show_stats :: Ptr SrcFile -> Int -> IO ()
show_stats ptr_srcfiles num_srcfiles = do
	srcfiles :: [SrcFile] <- peekArray num_srcfiles ptr_srcfiles
	forM_ srcfiles $ \ (SrcFile csourcefn coutputfn counters) -> do
		sourcefn <- peekCString csourcefn
		outputfn <- peekCString coutputfn
		let
			n_cov :: Int = sum $ map (max 1 . fromIntegral . cntC) counters
			n_stmts :: Int = length counters
			cov_pct :: Float = if n_stmts<=0 then 100.0 else 100.0* (fromIntegral n_cov) / (fromIntegral n_stmts)
		putStrLn $ printf "%6i of %6i statements, %5.1f %% coverage in %s (compiled into %s)" n_cov n_stmts cov_pct sourcefn outputfn
