{-# LANGUAGE RecordWildCards #-}
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
--import Data.Data.Lens
--import Control.Lens.Traversal
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class (liftIO)
import Data.Char
import Text.Printf

import Text.Regex
import Text.Regex.TDFA

import Language.C.System.Preprocess
import Language.C.Data.InputStream

import ShowAST
--import ASTLenses

{-
Profiling:
stack install [...] --profile
stack exec -- [...] +RTS -h
tvg.exe +RTS -h -RTS [...]
-}

{-
configure ENVVARS:
export CC="/root/.local/bin/tvg-exe /tvg/tvg"
export CFLAGS="-w -I/usr/include/i386-linux-gnu"

Versuche:
export LDFLAGS="-L/usr/lib/i386-linux-gnu"
 
export LD_LIBRARY_PATH=/tvg/tvg/incs:$LD_LIBRARY_PATH
export LIBRARY_PATH=

root@robert-VirtualBox:/tvg/build#
../gcc-4.7.4/configure --disable-checking --enable-languages=c --disable-multiarch --disable-multilib --enable-shared --enable-threads=posix --program-suffix=-instr --with-gmp=/usr --with-mpc=/usr/lib --with-mpfr=/usr/lib --without-included-gettext --with-system-zlib --with-tune=generic --prefix=/tvg/install/gcc-4.7.4 --disable-bootstrap --disable-build-with-cxx

cp /tvg/tvg/incs/data.c.start /tvg/tvg/incs/data.c
cp /tvg/tvg/incs/data.h.start /tvg/tvg/incs/data.h

stack install --allow-different-user

make

Nach "Cannot find crti.o":
export LIBRARY_PATH=/usr/lib/i386-linux-gnu:$LIBRARY_PATH

make install

GCC-Executable in
/tvg/install/gcc-4.7.4/bin/gcc4.7

gcc -shared -fPIC -Iincs incs/data.c -o incs/libdata.so

-}

_OUTPUT_AST = False
_INIT_DATA = False
_WRITE_PREPROCESSED = False
_KEEP_INSTRUMENTED = True
_PROGRESS_OUTPUT = False
_DEBUG_OUTPUT = False

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
	when _DEBUG_OUTPUT $ putStrLn $ gccExe ++ " " ++ intercalate " " (["-L"++incs_path] ++ args ++ ["-ldata"])
	exitcode <- rawSystem gccExe $ ["-L"++incs_path] ++ args ++ ["-ldata"]
	sequence_ restore_files
	exitWith exitcode

handleArg preprocess_args incs_path arg | ".c" `isSuffixOf` arg && takeFileName arg /= "conftest.c" = do
	handleSrcFile preprocess_args incs_path arg
handleArg _ _ _ = return $ return ()

handleSrcFile preprocess_args incs_path name = do
	when _OUTPUT_AST $ do
		mb_ast <- parseCFile (newGCC gccExe) Nothing preprocess_args name
		case mb_ast of
			Left err -> error $ show err
			Right ast -> writeFile (name++".ast") $ showDataTree ast

	when _WRITE_PREPROCESSED $ do
		Right inputstream <- runPreprocessor (newGCC gccExe) (rawCppArgs preprocess_args name)
		writeFile (name++".i") $ inputStreamToString inputstream

	let bak_name = name ++ ".preinstr"
	renameFile name bak_name

	let filenameid = map (\ c -> if isAlphaNum c then c else '_') name
	let varname = "src_" ++ filenameid
	let tracefunname = "trace_" ++ filenameid

	-- Prepend "#include <data.h>" to instrumented file
	cfile <- readFile bak_name
	writeFile name $ "#include <data.h>\n\n" ++ cfile

	-- The trace_function declaration text
	let tracefundecl = printf "void %s(int i)" tracefunname

	-- Check if this source file has been processed already
	data_h <- readFile (incs_path </> "data.h")
	let already_processed = tracefundecl `isInfixOf` data_h
	unless already_processed $ do
		-- Insert trace_function declaration
		insertBeforeMarker (incs_path </> "data.h") "/*INSERT_HERE*/" $ tracefundecl ++ ";\n"

		-- Add definition of trace_function to data.c
		let fundecl = printf "%s { %s.counters[i+1].cnt++; }" tracefundecl varname
		insertBeforeMarker (incs_path </> "data.c") "/*INSERT_SRCFILE_HERE*/" $
			printf "SRCFILE %s = { %s, /*%s_NUM_LOCS*/0/*%s_NUM_LOCS*/, {\n{0,0,0}/*DUMMY*//*%s_LOCATIONS*/\n} };\n" varname (show name) filenameid filenameid filenameid ++
			fundecl ++ "\n\n"

	parseresult <- parseCFile (newGCC gccExe) Nothing preprocess_args name
	mb_err <- case parseresult of
		Left errmsg -> return $ Just $ show errmsg
		Right ast -> do
			let instr_filename = name++".instr"
			writeFile instr_filename ""

			-- Start with correct index of already_processed
			startindex <- case already_processed of
				False -> return 0
				True -> do
					f <- readFile (incs_path </> "data.c")
					let Just [is] = matchRegex (mkRegex $ printf "/[*]%s_NUM_LOCS[*]/([0-9]+)/[*]%s_NUM_LOCS[*]/" filenameid filenameid) f
					return $ read is

			InstrS{..} <- execStateT (processASTM ast) $ InstrS filenameid startindex incs_path name varname tracefunname instr_filename []
			copyFile instr_filename name
			when (not _KEEP_INSTRUMENTED) $ removeFile instr_filename

			replaceInBracket (incs_path </> "data.c") (printf "/*%s_NUM_LOCS*/" filenameid) (show numLocsS)

			unless already_processed $ do
				let srcptr = printf ",\n&%s " varname
				insertBeforeMarker (incs_path </> "data.c") "/*INSERT_SRCPTR_HERE*/" srcptr

			(exitcode,stdout,stderr) <- readProcessWithExitCode gccExe
				["-shared", "-fPIC", "-DQUIET", "-I"++incs_path, incs_path </> "data.c", "-o", incs_path </> "libdata.so" ] ""
			case exitcode of
				ExitSuccess   -> return Nothing
				ExitFailure _ -> return $ Just $ "Compile data.c failed:\n" ++ stdout ++ stderr

	case mb_err of
		Nothing -> return $ removeFile name >> renameFile bak_name name
		Just errmsg -> do
			removeFile name >> renameFile bak_name name
			error errmsg

escapeRegex "" = ""
escapeRegex ('*':r) = "[*]" ++ escapeRegex r
escapeRegex ('.':r) = "\\." ++ escapeRegex r
escapeRegex ('{':r) = "\\{" ++ escapeRegex r
escapeRegex ('}':r) = "\\}" ++ escapeRegex r
escapeRegex (c:r) = c : escapeRegex r

replaceInFile filename marker text = do
	f <- readFile filename
	writeFile filename $ subRegex (mkRegex $ escapeRegex marker) f text

replaceInBracket filename bracket text = do
	f <- readFile filename
	writeFile filename $ subRegex (mkRegex $ escapeRegex bracket ++ ".*" ++ escapeRegex bracket) f (bracket ++ text ++ bracket)

insertBeforeMarker filename marker text = do
	f <- readFile filename
	writeFile filename $ subRegex (mkRegex $ escapeRegex marker) f (text++marker)

data InstrS = InstrS {
	fileNameIdS :: String, numLocsS :: Int, incsPathS :: String, fileNameS :: String, varNameS :: String,
	traceFunNameS :: String, instrFileNameS :: String, locationsS :: [(Int,Int)] } deriving Show
type InstrM a = StateT InstrS IO a

processASTM :: CTranslUnit -> InstrM ()
processASTM (CTranslUnit extdecls _) = mapM_ instrumentExtDecl extdecls

locString filenameid (l,c) = printf "{ %li,%li,0 } /* %s */" l c filenameid

instrumentExtDecl :: CExtDecl -> InstrM CExtDecl
instrumentExtDecl ast = do
	incspath <- gets incsPathS
	modify $ \ s -> s { locationsS = [] }
	data_c <- liftIO $ readFile (incspath </> "data.c")
	ast' <- everywhereM (mkM (instrumentStmt data_c)) ast

	let instr_ast =
		everywhere (mkT elimInStatExprs) $
		everywhere (mkT insertbeforeexits) $
		everywhere (mkT insertopen)
		ast'
	instr_filename <- gets instrFileNameS
	liftIO $ appendFile instr_filename $ (render $ pretty instr_ast) ++ "\n"

	InstrS{..} <- get
	let new_locs = filter (not . (`isInfixOf` data_c) . locString fileNameIdS) locationsS
	liftIO $ insertBeforeMarker (incsPathS </> "data.c") (printf "/*%s_LOCATIONS*/" fileNameIdS) $
		concatMap (\ (i,loc) -> (",\n" ++ locString fileNameIdS loc ++ " /*" ++ show i ++ "*/")) (zip [numLocsS..] new_locs)

	modify $ \ s -> s { numLocsS = numLocsS + length new_locs }

	return instr_ast

instrumentStmt :: String -> CStat -> InstrM CStat
instrumentStmt data_c cstat = do
	InstrS{..} <- get
	let locstring = locString fileNameIdS loc
	index <- case locstring `isInfixOf` data_c of
		False -> do
			modify $ \ s -> s { locationsS = loc : locationsS }
			return $ numLocsS + length locationsS
		True -> do
			let regex = escapeRegex (locstring ++ " /*") ++ "([0-9]+)" ++ escapeRegex "*/"
			let Just [is] = matchRegex (mkRegex regex) data_c
			return $ read is
	let ret = CCompound [] [ instr traceFunNameS index, CBlockStmt cstat ] undefNode
	when _PROGRESS_OUTPUT $ liftIO $ putStr $ printf "locs= %8i\r" (length locationsS)
	return ret
	where
	pos = posOfNode $ nodeInfo cstat
	loc = (posRow pos,posColumn pos)
	str = posFile pos ++ show (posRow pos,posColumn pos)
	instr tracefunname index = CBlockStmt $ CExpr (Just $ CCall (CVar (builtinIdent tracefunname) undefNode)
		[CConst $ CIntConst (cInteger $ fromIntegral index) undefNode] undefNode) undefNode

elimInStatExprs :: CExpr -> CExpr
elimInStatExprs (CStatExpr stat nodeinfo) = CStatExpr (everywhere (mkT elimNestedCompounds) stat) nodeinfo
elimInStatExprs x = x

elimNestedCompounds :: CStat -> CStat
elimNestedCompounds (CCompound ids cbis nodeinfo) = CCompound ids (concatMap flatten cbis) nodeinfo where
	flatten (CBlockStmt (CCompound [] cbis2 _)) = cbis2
	flatten x = [x]
elimNestedCompounds x = x

insertopen :: CFunDef -> CFunDef
insertopen (CFunDef declspecs declr@(CDeclr (Just (Ident "main" _ _)) _ _ _ _) decls cstat ni) =
	CFunDef declspecs declr decls (CCompound [] [callopentrace,CBlockStmt (insertbeforereturns cstat),callclosetrace] undefNode) ni
insertopen x = x

callopentrace = CBlockStmt (CExpr (Just (CCall (CVar (builtinIdent "opentrace") undefNode) opentrace_args undefNode)) undefNode)
opentrace_args = []

callclosetrace = CBlockStmt (CExpr (Just (CCall (CVar (builtinIdent "closetrace") undefNode) [] undefNode)) undefNode)

my_ret = internalIdent "my_ret"

insertbeforereturns = everywhere (mkT insertclose) where
	insertclose :: CStat -> CStat
	insertclose (CReturn (Just ret_expr) _) = create_compound ret_expr $
		CReturn (Just $ CVar my_ret undefNode) undefNode
	insertclose x = x

insertbeforeexits :: CStat -> CStat
insertbeforeexits (CExpr (Just (CCall fun@(CVar (Ident "exit" _ _) _) [ret_expr] _)) _) = create_compound ret_expr $
	CExpr (Just $ CCall fun [CVar my_ret undefNode] undefNode) undefNode
insertbeforeexits x = x

create_compound ret_expr ret_stmt = CCompound [] [
	CBlockDecl $ CDecl [CTypeSpec $ CIntType undefNode]
		[ ( Just $ CDeclr (Just my_ret) [] Nothing [] undefNode, Just $ CInitExpr ret_expr undefNode, Nothing ) ] undefNode,
	callclosetrace,
	CBlockStmt ret_stmt
	] undefNode
