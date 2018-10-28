{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module CovStats where

import Control.Applicative ((<$>), (<*>))
import Foreign
import Foreign.C
--import Text.Printf
import Control.Monad
import GHC.Generics (Generic)
--import Data.List
--import System.Directory

import CovDefs


#include "data.h"


instance Storable Counter where
	alignment _ = #{alignment COUNTER}
	sizeOf _    = #{size COUNTER}
	peek ptr    = Counter <$>
		#{peek COUNTER, line} ptr <*>
		#{peek COUNTER, column} ptr <*>
		#{peek COUNTER, length} ptr <*>
		#{peek COUNTER, cnt} ptr
	poke ptr (Counter line col len cnt) = do
		#{poke COUNTER, line} ptr line
		#{poke COUNTER, column} ptr col
		#{poke COUNTER, length} ptr len
		#{poke COUNTER, cnt} ptr cnt

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

foreign export ccall show_stats :: Int -> CString -> Ptr (Ptr SrcFile) -> Int -> IO ()
show_stats quiet ccovfilename ptr_ptr_srcfiles num_srcfiles = do
	cov_filename <- peekCString ccovfilename
	ptrs_srcfiles <- peekArray num_srcfiles ptr_ptr_srcfiles
	srcfiles <- mapM peek ptrs_srcfiles
	unless (quiet==0) $ do
		new_cov <- accumulateCoverage cov_filename srcfiles
		let cov_txt = showCoverage new_cov
		writeFile (cov_filename ++ ".txt") cov_txt
		putStrLn cov_txt
