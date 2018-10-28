{-# LANGUAGE DeriveGeneric,ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module CovDefs where

import Prelude hiding (readFile)
import System.IO.Strict
import Text.Printf
import Control.Monad
import Data.Binary
import GHC.Generics (Generic)
import Data.List
import System.Directory

{-
typedef struct {
	long line, column, len;
	long cnt; }
	COUNTER;
-}
data Counter = Counter { lineC :: Int, columnC :: Int, lenC :: Int, cntC :: Int } deriving (Show,Generic)
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
instance Binary SrcFile

type Coverage = [SrcFile]

srcfile_covs :: SrcFile -> (Int,Int,Float,String)
srcfile_covs (SrcFile sourcefn outputfn counters) = (n_cov,n_stmts,cov_pct n_cov n_stmts,line) where
	n_cov :: Int   = sum $ map (min 1 . fromIntegral . cntC) counters
	n_stmts :: Int = length counters
	line :: String = printf "%6i of %6i statements, %5.1f %% coverage in %s (compiled into %s)" n_cov n_stmts (cov_pct n_cov n_stmts) sourcefn outputfn

cov_pct :: (Integral a,Integral b) => a -> b -> Float
cov_pct n_cov n_stmts = if n_stmts==0 then 100.0 else 100.0 * (fromIntegral n_cov) / (fromIntegral n_stmts)

showCoverage :: Coverage -> String
showCoverage srcfiles = unlines $ out_lines ++ [ printf "Overall coverage: %5.1f %%" (cov_pct (sum n_covs) (sum n_stmtss)) ]
	where
	(n_covs,n_stmtss,_,out_lines) = unzip4 $ map srcfile_covs srcfiles

writeCoverage :: String -> Coverage -> IO ()
writeCoverage cov_filename srcfiles = encodeFile cov_filename srcfiles

readCoverage :: String -> IO Coverage
readCoverage = decodeFile

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
