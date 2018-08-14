{-# OPTIONS_GHC -fno-warn-tabs #-}

module Main where

import System.Process
import System.IO
import System.Environment
import System.Exit
import Data.List
import Control.Monad
import System.Directory
import Language.C
import Language.C.Data.Ident
import Language.C.System.GCC
import Text.PrettyPrint
import System.FilePath
import Data.Generics

import ShowAST

{-
export CC="stack exec --allow-different-user --stack-yaml /tvg/tvg/stack.yaml -- tvg-exe"
export CC="/root/.local/bin/tvg-exe"
export CFLAGS="-w"
export LIBRARY_PATH=/usr/lib/x86_64-linux-gnu:$LIBRARY_PATH
export LD_LIBRARY_PATH=/tvg/tvg/incs:$LD_LIBRARY_PATH

root@robert-VirtualBox:/tvg/build#
../gcc-4.7.4/configure --disable-checking --enable-languages=c --disable-multiarch --disable-multilib --enable-shared --enable-threads=posix --program-suffix=-instr --with-gmp=/usr/local/lib --with-mpc=/usr/lib --with-mpfr=/usr/lib --without-included-gettext --with-system-zlib --with-tune=generic --prefix=/tvg/install/gcc-4.7.4 --disable-bootstrap --disable-build-with-cxx

make

GCC-Executable in
/tvg/install/gcc-4.7.4/bin/gcc4.7

stack exec install
-}

logFileName = "/tvg/build/calls.log"
instrumentedMarker = "typedef int INSTRUMENTED_ALREADY;"

include_tvg = "#include <tvg.h>"
tvg_path = "/tvg/tvg/incs"
traceFileName = "trace.txt"

printLog msg = do
--	appendFile logFileName (msg++"\n")
--	putStrLn $ "######## LOG ######## " ++ msg
	return ()

main = do
	args <- getArgs
	printLog $ intercalate " " args
	
	let args' = ["-I"++tvg_path] ++ args

	let preprocess_args = filter (\ arg -> any (`isPrefixOf` arg) ["-I","-D"]) args'
	forM_ args (handleArg preprocess_args)
	exitcode <- rawSystem "gcc" $ ["-L"++tvg_path] ++ args ++ ["-ltvg"]
	exitWith exitcode

handleArg preprocess_args arg = when (".c" `isSuffixOf` arg) $ do
	printLog $ "Found source file " ++ arg
	handleSrcFile preprocess_args arg

handleSrcFile preprocess_args name = do
	cfile <- readFile name
	let filename = takeFileName name
{- SHOW ORIGINAL AST
	Right ast <- parseCFile (newGCC "gcc") Nothing preprocess_args name
	writeFile (name++".ast") $ showDataTree ast
-}
	when (not (instrumentedMarker `isInfixOf` cfile) && not ("conftest.c" `isInfixOf` name)) $ do
		printLog $ "Instrumenting " ++ name ++ "..."
		writeFile name $ include_tvg ++ "\n" ++ instrumentedMarker ++ "\n" ++ cfile

		printLog $ "Preprocessing " ++ name ++ " with args: " ++ intercalate " " preprocess_args
		parse_result <- parseCFile (newGCC "gcc") Nothing preprocess_args name
		case parse_result of
			Left parse_err -> do
				let errtxt = "HASKELL parseCFile: " ++ show parse_err
				printLog $ "=============== " ++ errtxt
				error errtxt
			Right ast -> do
				writeFile name $ render $ pretty $ processAST ast
	return ()

processAST :: CTranslUnit -> CTranslUnit
processAST = everywhere (mkT instrumentMain) . everywhere (mkT instrCompound) . everywhere (mkT instrStmt)

instrAssign :: CCompound 
{-
instrAssign :: CStat -> CStat
instrAssign cstat@(CExpr (Just (CAssign _ _ _ ni)) _) = CCompound [] [ instrExpr ni, CBlockStmt cstat ] ni
instrAssign x = x
-}

instrExpr :: NodeInfo -> CBlockItem
instrExpr nodeinfo = CBlockStmt (CExpr (Just (CCall (CVar (builtinIdent "mytrace") undefNode) args undefNode)) undefNode)
	where
	str = show $ posOfNode nodeinfo
	args = [CConst (CStrConst (cString $ "TRACE: " ++ str ++ "\n") undefNode)]

instrumentMain :: CFunDef -> CFunDef
instrumentMain (CFunDef declspecs declr@(CDeclr (Just (Ident name _ _)) _ _ _ _) decls cstat ni) | name=="main" =
	CFunDef declspecs declr decls (CCompound [] [callopentrace,CBlockStmt cstat] undefNode) ni where
	callopentrace = CBlockStmt (CExpr (Just (CCall (CVar (builtinIdent "opentrace") undefNode) args undefNode)) undefNode)
	args = [CConst (CStrConst (cString $ tvg_path ++ "/" ++ traceFileName) undefNode)]
instrumentMain x = x

