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
import System.FilePath
import Data.Generics

{-
export CC="stack exec --allow-different-user --stack-yaml /tvg/tvg/stack.yaml -- tvg-exe"

root@robert-VirtualBox:/tvg/build#
../gcc-4.7.4/configure --disable-checking --enable-languages=c --enable-multiarch --enable-shared --enable-threads=posix --program-suffix=-instr --with-gmp=/usr/local/lib --with-mpc=/usr/lib --with-mpfr=/usr/lib --without-included-gettext --with-system-zlib --with-tune=generic --prefix=/tvg/install/gcc-4.7.4 --disable-bootstrap --disable-build-with-cxx
make -j4
-}

logFileName = "calls.log"

printLog msg = do
	appendFile logFileName (msg++"\n")
	putStrLn $ "######## LOG ######## " ++ msg

main = do
	args <- getArgs
	printLog $ intercalate " " args
	
	let preprocess_args = filter (\ arg -> any (`isPrefixOf` arg) ["-I","-D"]) args
	args' <- forM args (handleArg preprocess_args)
	exitcode <- rawSystem "gcc" args'
	exitWith exitcode

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
			error errtxt
		Right ast -> do
			let
				src' = render $ pretty $ processAST ast
				(filename,extension) = splitExtension arg
				name' = filename ++ "_instrumented" ++ extension
			writeFile (filename ++ ".ast") (show ast)
			printLog $ arg ++ ": LOC=" ++ show (length $ lines src')
			writeFile name' src'
			return name'

processAST :: CTranslUnit -> CTranslUnit
processAST = everywhere (mkT processStat)

processStat :: CStatement -> CStatement
processStat (CCompound labels blockitems nodeinfo) = CCompound labels (concatMap mkInstr blockitems) nodeinfo
processStat other = other

mkInstr :: CBlockItem -> [CBlockItem]
--instrumentStmt blockitem@(CExpr (Just (CAssign op expr1 expr2 ni2)) ni) = [CExpr (Just ) ni,blockitem]
mkInstr x = [x]