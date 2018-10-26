{-# LANGUAGE ForeignFunctionInterface,ScopedTypeVariables,DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module CovStats where

import Prelude hiding (readFile)
import System.IO.Strict (readFile)
import Control.Applicative ((<$>), (<*>))
import Foreign
import Foreign.C
import Text.Printf
import Control.Monad
import Data.Binary
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.List
import System.Directory

#include "data.h"


{-
typedef struct {
	long line, column;
	long cnt; }
	COUNTER;
-}
data Counter = Counter { lineC :: Int, columnC :: Int, cntC :: Int } deriving (Show,Generic)
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
instance Binary Counter

{-
typedef struct {
	char sourcefilename[256];
	char output_filename[256];
	long num_counters;
	COUNTER counters[];
} SRCFILE;
-}
data SrcFile = SrcFile { sourceFilenameS :: String, outputFilenameS :: String, countersS :: [Counter] } deriving (Show,Generic)
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
instance Binary SrcFile

type Coverage = [SrcFile]

foreign export ccall show_stats :: Int -> CString -> Ptr (Ptr SrcFile) -> Int -> IO ()
show_stats quiet ccovfilename ptr_ptr_srcfiles num_srcfiles = do
	cov_filename <- peekCString ccovfilename
	ptrs_srcfiles <- peekArray num_srcfiles ptr_ptr_srcfiles
	srcfiles <- mapM peek ptrs_srcfiles
	new_coverage <- accumulateCoverage cov_filename srcfiles
	unless (quiet==0) $ showCoverage new_coverage

showCoverage srcfiles = do
	covs <- forM srcfiles $ \ (SrcFile sourcefn outputfn counters) -> do
		let
			n_cov :: Int = sum $ map (min 1 . fromIntegral . cntC) counters
			n_stmts :: Int = length counters
		putStrLn $ printf "%6i of %6i statements, %5.1f %% coverage in %s (compiled into %s)" n_cov n_stmts (cov_pct n_cov n_stmts) sourcefn outputfn
		return (n_cov,n_stmts)
	let (n_covs,n_stmtss) = unzip covs
	putStrLn $ printf "Overall coverage: %5.1f %%" (cov_pct (sum n_covs) (sum n_stmtss))	
	where
	cov_pct :: Int -> Int -> Float
	cov_pct n_cov n_stmts = if n_stmts<=0 then 100.0 else 100.0* (fromIntegral n_cov) / (fromIntegral n_stmts)

writeCoverage :: String -> Coverage -> IO ()
writeCoverage cov_filename srcfiles = BSL.writeFile cov_filename $ encode srcfiles

readCoverage :: String -> IO Coverage
readCoverage cov_filename = do
	s <- readFile cov_filename
	return $ decode $ BSL.pack s

accumulateCoverage cov_filename srcfiles = do
	cov_exists <- doesFileExist cov_filename
	new_coverage <- case cov_exists of
		False -> return srcfiles 
		True -> do
			prev_srcfiles <- readCoverage cov_filename
			return $ map merge_srcfiles (zip prev_srcfiles srcfiles)
			where
			merge_srcfiles (prev_srcfile,srcfile) = prev_srcfile { countersS = map merge_counters (zip (countersS prev_srcfile) (countersS srcfile)) }
			merge_counters (cnt1,cnt2) = cnt1 { cntC = cntC cnt1 + cntC cnt2 }
	writeCoverage cov_filename new_coverage
	return new_coverage
