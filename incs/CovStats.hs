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
	alignment _ = 8
	sizeOf _    = 3*8
	peek ptr    = Counter <$> peekByteOff ptr 0 <*> peekByteOff ptr 8 <*> peekByteOff ptr 16
	poke ptr (Counter line col cnt) = pokeByteOff ptr 0 line >>= pokeByteOff ptr 8 col >>= pokeByteOff ptr 16 cnt

{-
typedef struct {
	char sourcefilename[250];
	long num_counters;
	COUNTER counters[];
} SRCFILE;
-}
data SrcFile = SrcFile { sourcefilenameS :: String, countersS :: [Counter] }
instance Storable Counter where
	alignment _ = 8
	sizeOf srcfile = 250 + 8 + length (countersS srcfile) * 3 * 8
	peek ptr = do
		srcfilename <- peekCAString ptr
		num_cnts :: Int64 <- peekByteOff ptr 250
		cnts <- peekArray (fromIntegral num_cnts) ptr
		return $ SrcFile srcfilename cnts
	poke ptr (SrcFile srcfilename cnts) = do
		withCAString srcfilename (pokeByteOff ptr 0)
		pokeByteOff ptr 250 (fromIntegral (length cnts) :: Int64)
		pokeArray (ptr `plusPtr` (250+8) cnts

{-
http://blog.plowtech.net/posts/ffi-tutorial-part-1.html

https://wiki.haskell.org/Foreign_Function_Interface

ghc -c CovStats.hs

ghc -shared -fPIC -no-hs-main -I. CovStats.o data.c -o libdata.so
-}

foreign export ccall show_stats :: Int -> IO Char
show_stats i = do
	putStrLn $ "Haskell: " ++ show i
	return 'v'
