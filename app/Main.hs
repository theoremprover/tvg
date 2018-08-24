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
import Control.Monad.Trans.State.Strict
import Data.Char
import Text.Printf

import ShowAST

{-
configure ENVVARS:
export CC="/root/.local/bin/tvg-exe"
export CFLAGS="-w -I/usr/include/i386-linux-gnu"

export LD_LIBRARY_PATH=/tvg/tvg/incs:$LD_LIBRARY_PATH
export LIBRARY_PATH=

echo "#include <tvg.h>" > /tvg/tvg/incs/data.c

root@robert-VirtualBox:/tvg/build#
../gcc-4.7.4/configure --disable-checking --enable-languages=c --disable-multiarch --disable-multilib --enable-shared --enable-threads=posix --program-suffix=-instr --with-gmp=/usr --with-mpc=/usr/lib --with-mpfr=/usr/lib --without-included-gettext --with-system-zlib --with-tune=generic --prefix=/tvg/install/gcc-4.7.4 --disable-bootstrap --disable-build-with-cxx

export LIBRARY_PATH=/usr/lib/i386-linux-gnu:$LIBRARY_PATH

cp /tvg/tvg/incs/data.c.start /tvg/tvg/incs/data.c
cp /tvg/tvg/incs/data.h.start /tvg/tvg/incs/data.h

make

make install

GCC-Executable in
/tvg/install/gcc-4.7.4/bin/gcc4.7

stack install --allow-different-user
-}

main = do
	tvg_path:args <- getArgs
	let incs_path = tvg_path </> "incs"
	let args' = ["-I" ++ incs_path] ++ args
	let preprocess_args = filter (\ arg -> any (`isPrefixOf` arg) ["-I","-D"]) args'
	restore_files <- forM args (handleArg preprocess_args)
	exitcode <- rawSystem "gcc" $ ["-L"++incs_path] ++ args ++ ["-ltvg"]
	sequence_ restore_files
	exitWith exitcode

handleArg preprocess_args arg | ".c" `isSuffixOf` arg && takeFileName arg /= "conftest.c" = do
	handleSrcFile preprocess_args arg
handleArg _ _ = return $ return ()

handleSrcFile preprocess_args name = do
{-
--	OUTPUT ORIGINAL AST
	Right ast <- parseCFile (newGCC "gcc") Nothing preprocess_args name
	writeFile (name++".ast") $ showDataTree ast
-}

	let bak_name = name ++ ".preinstr"
	renameFile name bak_name

	cfile <- readFile bak_name
	writeFile name $ "#include <tvg.h>\n#include <data.h>\n" ++ "\n" ++ cfile

	parseresult <- parseCFile (newGCC "gcc") Nothing preprocess_args name
	case parseresult of
		Left errmsg -> error $ show errmsg
		Right ast -> do
			let filenameid = take 31 $ "src_" ++ map (\ c -> if isAlphaNum c then c else '_') name
			(ast',InstrS _ _ locs) <- runStateT (processASTM ast) $ InstrS name filenameid []
			writeFile name $ render $ pretty ast'

			appendFile (incs_path </> "data.h") $
				printf "extern SRCFILE %s;\n" filenameid
			appendFile (incs_path </> "data.c") $
				printf "SRCFILE %s = { %s, {\n" filenameid (show name) ++
				intercalate ",\n" (map (\ (l,c) -> printf "{ %li,%li,0 }" l c) locs) ++
				"\n} };\n\n"

			copyFile name (incs_path </> "instrs" </> takeFileName name)
			return $ removeFile name >> renameFile bak_name name

{-
SRCFILE src_abc_def_xyz_c = { "abc/def/xyz.c", {
{ 34,3,0 },
{ 35,4,0 }
} };

src_abc_def_xyz_c.counters[45].cnt++;
-}

data InstrS = InstrS { fileNameS :: String, fileNameIdS :: String, locationsS :: [(Int,Int)] }
type InstrM a = StateT InstrS IO a

processASTM :: CTranslUnit -> InstrM CTranslUnit
processASTM ast = do
	ast' <- everywhereM (mkM instrumentStmt) ast
	return $ everywhere (mkT instrumentMain) ast'

instrumentStmt :: CStat -> InstrM CStat
instrumentStmt cstat = do
	filenameid <- gets fileNameIdS
	modify $ \ s -> s { locationsS = locationsS s ++ [(line,col)] }
	locs <- gets locationsS
	let index = length locs
	return $ CCompound [] [ instr filenameid index, CBlockStmt cstat ] undefNode
	where
	pos = posOfNode $ nodeInfo cstat
	(line,col) = (posRow pos,posColumn pos)
	str = posFile pos ++ show (posRow pos,posColumn pos)
	instr filenameid index = CBlockStmt $ CExpr (Just $ CUnary CPostIncOp (incexpr filenameid index) undefNode) undefNode
	incexpr filenameid index = CMember (CIndex
		( CMember (CVar (internalIdent filenameid) undefNode) (builtinIdent "counters") False undefNode)
		(CConst $ CIntConst (cInteger $ fromIntegral index) undefNode) undefNode)
		(builtinIdent "cnt") False undefNode

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
