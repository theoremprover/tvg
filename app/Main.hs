{-# OPTIONS_GHC -fno-warn-tabs #-}

module Main where

import Prelude hiding (readFile)
import System.IO.Strict

import System.Process
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
import Control.Monad.Trans.State.Strict
import Data.Char
import Text.Printf

import ShowAST
import ASTLenses

{-
configure ENVVARS:
export CC="/root/.local/bin/tvg-exe /tvg/tvg"
export CFLAGS="-w -I/usr/include/i386-linux-gnu"

export LD_LIBRARY_PATH=/tvg/tvg/incs:$LD_LIBRARY_PATH
export LIBRARY_PATH=

root@robert-VirtualBox:/tvg/build#
../gcc-4.7.4/configure --disable-checking --enable-languages=c --disable-multiarch --disable-multilib --enable-shared --enable-threads=posix --program-suffix=-instr --with-gmp=/usr --with-mpc=/usr/lib --with-mpfr=/usr/lib --without-included-gettext --with-system-zlib --with-tune=generic --prefix=/tvg/install/gcc-4.7.4 --disable-bootstrap --disable-build-with-cxx

export LIBRARY_PATH=/usr/lib/i386-linux-gnu:$LIBRARY_PATH

cp /tvg/tvg/incs/data.c.start /tvg/tvg/incs/data.c
cp /tvg/tvg/incs/data.h.start /tvg/tvg/incs/data.h

stack install --allow-different-user

make

make install

GCC-Executable in
/tvg/install/gcc-4.7.4/bin/gcc4.7

gcc -shared -fPIC -Iincs incs/data.c -o incs/libdata.so

-}

_OUTPUT_AST = True
_INIT_DATA = True

gccExe = "gcc-4.7"

main = do
	tvg_path:args <- getArgs
	let incs_path = tvg_path </> "incs"

	when _INIT_DATA $ do
--		copyFile "test2.c.orig" "test2.c"
		copyFile (incs_path </> "data.c.start") (incs_path </> "data.c") 
		copyFile (incs_path </> "data.h.start") (incs_path </> "data.h") 

	let args' = ["-I" ++ incs_path] ++ args
	let preprocess_args = filter (\ arg -> any (`isPrefixOf` arg) ["-I","-D"]) args'
	restore_files <- forM args (handleArg preprocess_args incs_path)
	exitcode <- rawSystem gccExe $ ["-L"++incs_path] ++ args ++ ["-ldata"]
	sequence_ restore_files
	exitWith exitcode

handleArg preprocess_args incs_path arg | ".c" `isSuffixOf` arg && takeFileName arg /= "conftest.c" = do
	handleSrcFile preprocess_args incs_path arg
handleArg _ _ _ = return $ return ()

wait msg = do
	putStrLn $ msg ++ " Press [RETURN]"
	getLine

handleSrcFile preprocess_args incs_path name = do
	when _OUTPUT_AST $ do
--		OUTPUT ORIGINAL AST
		Right ast <- parseCFile (newGCC gccExe) Nothing preprocess_args name
		writeFile (name++".ast") $ showDataTree ast

	let bak_name = name ++ ".preinstr"
	renameFile name bak_name

	let filenameid = map (\ c -> if isAlphaNum c then c else '_') name
	let varname = "src_" ++ filenameid
	let tracefunname = "trace_" ++ filenameid

	appendFile (incs_path </> "data.h") $ printf "void %s(int);\n" tracefunname

	cfile <- readFile bak_name
	writeFile name $ "#include <data.h>\n\n" ++ cfile

	parseresult <- parseCFile (newGCC "gcc") Nothing preprocess_args name
	mb_err <- case parseresult of
		Left errmsg -> return $ Just $ show errmsg
		Right ast -> do
			(ast',InstrS _ _ locs) <- runStateT (processASTM ast) $ InstrS name tracefunname []
			writeFile name $ render $ pretty ast'

			insertInFile (incs_path </> "data.c") "/*INSERT_SRCFILE_HERE*/" $
				printf "SRCFILE %s = { %s, %i, {\n" varname (show name) (length locs) ++
				intercalate ",\n" (map (\ (l,c) -> printf "{ %li,%li,0 }" l c) locs) ++
				"\n} };\n" ++
				printf "void %s(int i) { %s.counters[i].cnt++; }\n" tracefunname varname 

			insertInFile (incs_path </> "data.c") "/*INSERT_SRCPTR_HERE*/" $
				printf ",\n&%s" varname

			(exitcode,stdout,stderr) <- readProcessWithExitCode gccExe
				["-shared", "-fPIC", "-I"++incs_path, incs_path </> "data.c", "-o", incs_path </> "libdata.so" ] ""
			case exitcode of
				ExitSuccess   -> return Nothing
				ExitFailure _ -> return $ Just $ "Compile data.c failed:\n" ++ stdout ++ stderr

	case mb_err of
		Nothing -> do
			copyFile name (name++".instr")
			return $ removeFile name >> renameFile bak_name name
		Just errmsg -> do
			removeFile name >> renameFile bak_name name
			error errmsg

insertInFile filename pos text = do
	f <- readFile filename
	unless (text `isInfixOf` f) $ writeFile filename $ insert_at pos text f
	where
	insert_at pos _ [] = error $ "did not find " ++ pos
	insert_at pos text rest | pos `isPrefixOf` rest = text ++ rest
	insert_at pos text (r:rest) = r : insert_at pos text rest

data InstrS = InstrS { fileNameS :: String, traceFunNameS :: String, locationsS :: [(Int,Int)] }
type InstrM a = StateT InstrS IO a

processASTM :: CTranslUnit -> InstrM CTranslUnit
processASTM ast = do
	ast' <- everywhereM (mkM instrumentStmt) ast
	return $ everywhere (mkT instrumentMain) ast'

instrumentStmt :: CStat -> InstrM CStat
instrumentStmt cstat = do
	InstrS _ tracefunname locs <- get
	modify $ \ s -> s { locationsS = locationsS s ++ [(line,col)] }
	let index = length locs
	return $ CCompound [] [ instr tracefunname index, CBlockStmt cstat ] undefNode
	where
	pos = posOfNode $ nodeInfo cstat
	(line,col) = (posRow pos,posColumn pos)
	str = posFile pos ++ show (posRow pos,posColumn pos)
	instr tracefunname index = CBlockStmt $ CExpr (Just $ CCall (CVar (builtinIdent tracefunname) undefNode)
		[CConst $ CIntConst (cInteger $ fromIntegral index) undefNode] undefNode) undefNode

instrumentMain :: CTranslUnit -> CTranslUnit
instrumentMain = everywhere (mkT insertopen)
	where
	insertopen :: CFunDef -> CFunDef
	insertopen (CFunDef declspecs declr@(CDeclr (Just (Ident "main" _ _)) _ _ _ _) decls cstat ni) =
		CFunDef declspecs declr decls (CCompound [] [callopentrace,CBlockStmt (insertbeforereturns cstat),callclosetrace] undefNode) ni
	insertopen x = x
	callopentrace = CBlockStmt (CExpr (Just (CCall (CVar (builtinIdent "opentrace") undefNode) opentrace_args undefNode)) undefNode)
	opentrace_args = []

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
