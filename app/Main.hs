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
export CFLAGS="-w -I/usr/include/i386-linux-gnu"

export LD_LIBRARY_PATH=/tvg/tvg/incs:$LD_LIBRARY_PATH

root@robert-VirtualBox:/tvg/build#
../gcc-4.7.4/configure --disable-checking --enable-languages=c --disable-multiarch --disable-multilib --enable-shared --enable-threads=posix --program-suffix=-instr --with-gmp=/usr --with-mpc=/usr/lib --with-mpfr=/usr/lib --without-included-gettext --with-system-zlib --with-tune=generic --prefix=/tvg/install/gcc-4.7.4 --disable-bootstrap --disable-build-with-cxx

export LIBRARY_PATH=/usr/lib/i386-linux-gnu:$LIBRARY_PATH
make

make install

GCC-Executable in
/tvg/install/gcc-4.7.4/bin/gcc4.7

stack install --allow-different-user
-}

logFileName = "/tvg/build/calls.log"

include_tvg = "#include <tvg.h>"
tvg_path = "/tvg/tvg/incs"
traceFileName = "trace.txt"

main = do
	args <- getArgs
	
	let args' = ["-I"++tvg_path] ++ args

	let preprocess_args = filter (\ arg -> any (`isPrefixOf` arg) ["-I","-D"]) args'
	restore_files <- forM args (handleArg preprocess_args)
	exitcode <- rawSystem "gcc" $ ["-L"++tvg_path] ++ args ++ ["-ltvg"]
	sequence_ restore_files
	exitWith exitcode

handleArg preprocess_args arg | ".c" `isSuffixOf` arg && takeFileName arg /= "conftest.c" = do
	handleSrcFile preprocess_args arg
handleArg _ _ = return $ return ()

handleSrcFile preprocess_args name = do
	let bak_name = name ++ ".preinstr"
	renameFile name bak_name

{-
--	OUTPUT ORIGINAL AST
	Right ast <- parseCFile (newGCC "gcc") Nothing preprocess_args name
	writeFile (name++".ast") $ showDataTree ast
-}

	cfile <- readFile bak_name
	writeFile name $ include_tvg ++ "\n" ++ cfile

	parseresult <- parseCFile (newGCC "gcc") Nothing preprocess_args name
	case parseresult of
		Left errmsg -> error $ show errmsg
		Right ast -> do
			writeFile name $ render $ pretty $ processAST ast
			writeFile (tvg_path ++ "/instrs/" ++ takeFileName name) $ render $ pretty $ processAST ast
			return $ removeFile name >> renameFile bak_name name

processAST :: CTranslUnit -> CTranslUnit
processAST = everywhere (mkT instrumentMain) . everywhere (mkT instrumentStmt)

instrumentStmt :: CStat -> CStat
instrumentStmt cstat = CCompound [] [ instr (nodeInfo cstat), CBlockStmt cstat ] undefNode
	where
	instr :: NodeInfo -> CBlockItem
	instr nodeinfo = CBlockStmt $ CExpr (Just $ CCall (CVar (builtinIdent "mytrace") undefNode) args undefNode) undefNode
		where
		pos = posOfNode nodeinfo
		str = posFile pos ++ show (posRow pos,posColumn pos)
		args = [CConst (CStrConst (cString $ str ++ "\n") undefNode)]

instrumentMain :: CTranslUnit -> CTranslUnit
instrumentMain = everywhere (mkT insertopen)
	where
	insertopen :: CFunDef -> CFunDef
	insertopen (CFunDef declspecs declr@(CDeclr (Just (Ident "main" _ _)) _ _ _ _) decls cstat ni) =
		CFunDef declspecs declr decls (CCompound [] [callopentrace,CBlockStmt (insertbeforereturns cstat),callclosetrace] undefNode) ni
	insertopen x = x
	callopentrace = CBlockStmt (CExpr (Just (CCall (CVar (builtinIdent "opentrace") undefNode) opentrace_args undefNode)) undefNode)
	opentrace_args = [CConst (CStrConst (cString $ tvg_path ++ "/" ++ traceFileName) undefNode)]

	callclosetrace = CBlockStmt (CExpr (Just (CCall (CVar (builtinIdent "closetrace") undefNode) [] undefNode)) undefNode)

	insertbeforereturns = everywhere (mkT insertclose)
	insertclose :: CStat -> CStat
	insertclose cret@(CReturn (Just ret_expr) _) = CCompound [] [
		CBlockDecl $ CDecl [CTypeSpec $ CIntType undefNode]
			[ ( Just $ CDeclr (Just my_ret) [] Nothing [] undefNode, Just $ CInitExpr ret_expr undefNode, Nothing ) ] undefNode,
		callclosetrace,
		CBlockStmt $ CReturn (Just $ CVar my_ret undefNode) undefNode
		] undefNode where
		my_ret = internalIdent "my_ret"
	insertclose x = x
