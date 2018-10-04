{-# LANGUAGE ForeignFunctionInterface,DeriveGeneric #-}

module CovStats where

import Control.Applicative ((<$>), (<*>))
import Foreign.Storable
import Foreign
import Foreign.C
import Foreign.Marshal.Array

{-
typedef struct {
	long line, column;
	long cnt; }
	COUNTER;
-}
data Counter = Counter { lineC :: Int64, columnC :: Int64, cntC :: Int64 }
instance Storable Counter where
	alignment _ = #{alignment COUNTER}
	sizeOf _    = #{size COUNTER}
	peek ptr    = Counter <$>
		#{peek COUNTER, line) ptr <*>
		#{peek COUNTER, column) ptr <*>
		#{peek COUNTER, cnt) ptr
	poke ptr (Counter line col cnt) = do
		#{poke COUNTER, line) ptr line
		#{poke COUNTER, col) ptr col
		#{poke COUNTER, cnt) ptr cnt

{-
typedef struct {
	char sourcefilename[250];
	long num_counters;
	COUNTER counters[];
} SRCFILE;
-}
data SrcFile = SrcFile { sourcefilenameS :: String, countersS :: [Counter] }

{-
http://blog.plowtech.net/posts/ffi-tutorial-part-1.html

https://wiki.haskell.org/Foreign_Function_Interface

hsc2hs
ghc -c CovStats.hs
ghc -shared -fPIC -no-hs-main -I. CovStats.o data.c -o libdata.so
-}

foreign export ccall show_stats :: Counter -> IO ()
show_stats counter = do
	print counter
	return ()
