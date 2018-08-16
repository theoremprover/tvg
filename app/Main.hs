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
configure ENVVARS:
export CC="/root/.local/bin/tvg-exe"
export CFLAGS="-w"

export LIBRARY_PATH=/usr/lib/x86_64-linux-gnu:/usr/lib/i386-linux-gnu:$LIBRARY_PATH
export LD_LIBRARY_PATH=/tvg/tvg/incs:$LD_LIBRARY_PATH

root@robert-VirtualBox:/tvg/build#
../gcc-4.7.4/configure --disable-checking --enable-languages=c --disable-multiarch --disable-multilib --enable-shared --enable-threads=posix --program-suffix=-instr --with-gmp=/usr/local/lib --with-mpc=/usr/lib --with-mpfr=/usr/lib --without-included-gettext --with-system-zlib --with-tune=generic --prefix=/tvg/install/gcc-4.7.4 --disable-bootstrap --disable-build-with-cxx

make

GCC-Executable in
/tvg/install/gcc-4.7.4/bin/gcc4.7

stack exec install
-}

logFileName = "/tvg/build/calls.log"

-- The instrumentedMarker has to survive the preprocessor, which a typedef does (other than a comment,e.g.)
--instrumentedMarker = "typedef int INSTRUMENTED_ALREADY;"

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
	restore_files <- forM args (handleArg preprocess_args)
	exitcode <- rawSystem "gcc" $ ["-L"++tvg_path] ++ args ++ ["-ltvg"]
	sequence_ restore_files
	exitWith exitcode

handleArg preprocess_args arg = when (".c" `isSuffixOf` arg) $ do
	printLog $ "Found source file " ++ arg
	handleSrcFile preprocess_args arg

handleSrcFile preprocess_args name = do
	let bak_name = name ++ ".preinstr"
	renameFile name bak_name

{-
--	SHOW ORIGINAL AST
	Right ast <- parseCFile (newGCC "gcc") Nothing preprocess_args name
	writeFile (name++".ast") $ showDataTree ast
-}

	cfile <- readFile bak_name
	case "conftest.c" `isInfixOf` name of
		True -> return $ return ()
		False -> do
			printLog $ "Instrumenting " ++ name ++ "..."
			writeFile name $ include_tvg ++ "\n" ++ cfile

			printLog $ "Preprocessing " ++ name ++ " with args: " ++ intercalate " " preprocess_args
			parseresult <- parseCFile (newGCC "gcc") Nothing preprocess_args name
			case parseresult of
				Left errmsg -> error $ show errmsg
				Right ast -> do
					writeFile name $ render $ pretty $ processAST ast
					return $ removeFile name >> renameFile bak_name name

processAST :: CTranslUnit -> CTranslUnit
processAST = everywhere (mkT instrumentMain) . everywhere (mkT instrAssign)

instrAssign :: CStat -> CStat
instrAssign cstat@(CExpr (Just (CAssign _ _ _ ni)) _) = CCompound [] [ instrexpr ni, CBlockStmt cstat ] ni
	where
	instrexpr :: NodeInfo -> CBlockItem
	instrexpr nodeinfo = CBlockStmt (CExpr (Just (CCall (CVar (builtinIdent "mytrace") undefNode) args undefNode)) undefNode)
		where
		str = show $ posOfNode nodeinfo
		args = [CConst (CStrConst (cString $ "TRACE: " ++ str ++ "\n") undefNode)]
instrAssign x = x

instrumentMain :: CTranslUnit -> CTranslUnit
instrumentMain = everywhere (mkT insertopen)
	where
	insertopen :: CFunDef -> CFunDef
	insertopen (CFunDef declspecs declr@(CDeclr (Just (Ident name _ _)) _ _ _ _) decls cstat ni) | name=="main" =
		CFunDef declspecs declr decls (CCompound [] [callopentrace,CBlockStmt (insertbeforereturns cstat),callclosetrace] undefNode) ni where
	insertopen x = x

	callopentrace = CBlockStmt (CExpr (Just (CCall (CVar (builtinIdent "opentrace") undefNode) opentrace_args undefNode)) undefNode)
	opentrace_args = [CConst (CStrConst (cString $ tvg_path ++ "/" ++ traceFileName) undefNode)]

	callclosetrace = CBlockStmt (CExpr (Just (CCall (CVar (builtinIdent "closetrace") undefNode) [] undefNode)) undefNode)

	insertbeforereturns = everywhere (mkT insertclose)

	insertclose :: CStat -> CStat
	insertclose cret@(CReturn _ _) = CCompound [] [callclosetrace,CBlockStmt cret] undefNode
	insertclose x = x
