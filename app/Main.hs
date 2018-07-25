{-# OPTIONS_GHC -fno-warn-tabs #-}

module Main where

import System.Process
import System.Environment
import System.IO
import System.Exit
import Data.List
import Control.Monad
import System.Directory
import Language.C
import Language.C.System.GCC
import Text.PrettyPrint

{-
export CC="stack exec --allow-different-user --stack-yaml /tvg/tvg/stack.yaml -- tvg-exe"

root@robert-VirtualBox:/tvg/build# ../gcc-4.7.4/configure --disable-checking --enable-languages=c --enable-multiarch --enable-shared --enable-threads=posix --program-suffix=4.7 --with-gmp=/usr/local/lib --with-mpc=/usr/lib --with-mpfr=/usr/lib --without-included-gettext --with-system-zlib --with-tune=generic --prefix=/tvg/install/gcc-4.7.4 --disable-bootstrap --disable-build-with-cxx
make -j4
-}

logFileName = "/tvg/calls.log"

printLog msg = do
	appendFile logFileName (msg++"\n")
	putStrLn $ "######## LOG ######## " ++ msg

main = do
	args <- getArgs
	let cmd = "gcc"
	printLog $ cmd ++ " " ++ intercalate " " args
	
	let preprocess_args = filter (\ arg -> or (map (`isPrefixOf` arg) ["-I","-D"])) args
	forM args (handleArg preprocess_args) >>= rawSystem cmd >>= exitWith

handleArg preprocess_args arg = do
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
					handleSrcFile preprocess_args arg

handleSrcFile preprocess_args arg = do
	printLog $ "preprocess_args = " ++ show preprocess_args
	parse_result <- parseCFile (newGCC "gcc") Nothing preprocess_args arg
	case parse_result of
		Left parse_err -> do
			let errtxt = "HASKELL parseCFile: " ++ show parse_err
			printLog $ "=============== " ++ errtxt
			--error errtxt
		Right ast -> do
			printLog $ arg ++ " LOC=" ++ show (length $ lines $ render $ pretty ast)
	return arg
