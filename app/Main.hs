{-# OPTIONS_GHC -fno-warn-tabs #-}

module Main where

import System.Process
import System.Environment
import System.IO
import System.Exit
import Data.List
import Control.Monad

{-
export CC="stack exec --allow-different-user --stack-yaml /tvg/tvg/stack.yaml -- tvg-exe"

root@robert-VirtualBox:/tvg/build# ../gcc-4.7.4/configure --disable-checking --enable-languages=c --enable-multiarch --enable-shared --enable-threads=posix --program-suffix=4.7 --with-gmp=/usr/local/lib --with-mpc=/usr/lib --with-mpfr=/usr/lib --without-included-gettext --with-system-zlib --with-tune=generic --prefix=/tvg/install/gcc-4.7.4 --disable-bootstrap --disable-build-with-cxx
make -j4
-}

main = do
	args <- getArgs
	let cmd = "gcc"
	let outputstr = cmd ++ " " ++ intercalate " " args ++ "\n"
	appendFile "/tvg/calls.log" outputstr
	putStrLn $ "####################### " ++ outputstr

	args' <- forM_ args handleSrcFile
		
	exitcode <- rawSystem cmd args'
	exitWith exitcode

handleSrcFile arg = do
	