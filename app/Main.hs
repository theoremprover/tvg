{-# OPTIONS_GHC -fno-warn-tabs #-}

module Main where

import System.Process
import System.Environment
import System.IO
import System.Exit
import Data.List
import Control.Monad
import System.Directory

{-
export CC="stack exec --allow-different-user --stack-yaml /tvg/tvg/stack.yaml -- tvg-exe"

root@robert-VirtualBox:/tvg/build# ../gcc-4.7.4/configure --disable-checking --enable-languages=c --enable-multiarch --enable-shared --enable-threads=posix --program-suffix=4.7 --with-gmp=/usr/local/lib --with-mpc=/usr/lib --with-mpfr=/usr/lib --without-included-gettext --with-system-zlib --with-tune=generic --prefix=/tvg/install/gcc-4.7.4 --disable-bootstrap --disable-build-with-cxx
make -j4
-}

printLog msg = do
	appendFile "/tvg/calls.log" (msg++"\n")
	putStrLn $ "######## LOG ######## " ++ msg

main = do
	args <- getArgs
	let cmd = "gcc"
	printLog $ cmd ++ " " ++ intercalate " " args
	
	forM args handleArg >>= rawSystem cmd >>= exitWith

handleArg arg = do
	case ".c" `isSuffixOf` arg of
		False -> return arg
		True -> do
			fileexists <- doesFileExist arg
			case fileexists of
				False -> do
					printLog $ "STRANGE: " ++ arg ++ " does not exist!"
					return arg
				True -> do
					printLog $ "Found source file " ++ arg
					handleSrcFile arg

handleSrcFile arg = do
	return arg
