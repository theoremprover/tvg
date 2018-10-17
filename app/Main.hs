{-# LANGUAGE RecordWildCards,LambdaCase,OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Main where

import Prelude hiding (readFile)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
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
import Control.Monad.IO.Class (liftIO)
import Data.Char
import Text.Printf
import Data.Time.Clock

import Language.C.System.Preprocess
import Language.C.Data.InputStream

import ShowAST

{-
Profiling:
stack install [...] --profile
stack exec -- [...] +RTS -h
tvg.exe +RTS -h -RTS [...]
-}

{-
stack install --allow-different-user --ghc-options -O3 --force-dirty

/root/.local/bin/tvg-exe /tvg test2.c -o test2

cp incs/data.h.start incs/data.h; cp incs/data.c.start incs/data.c
export CC="/root/.local/bin/tvg-exe /tvg"
export CFLAGS="-w -I/usr/include/i386-linux-gnu"
export LDFLAGS="-L/usr/lib/i386-linux-gnu"
export LD_LIBRARY_PATH=/tvg/tvg/incs:$LD_LIBRARY_PATH
export LIBRARY_PATH=/usr/lib/i386-linux-gnu:$LIBRARY_PATH

root@robert-VirtualBox:/tvg/build#
../gcc-4.7.4/configure --disable-checking --enable-languages=c --disable-multiarch --disable-multilib --enable-shared --enable-threads=posix --program-suffix=-instr --with-gmp=/usr --with-mpc=/usr/lib --with-mpfr=/usr/lib --without-included-gettext --with-system-zlib --with-tune=generic --prefix=/tvg/install/gcc-4.7.4 --disable-bootstrap --disable-build-with-cxx

cp /tvg/tvg/incs/data.c.start /tvg/tvg/incs/data.c; cp /tvg/tvg/incs/data.h.start /tvg/tvg/incs/data.h

make

_INSTR = False setzen in Main.hs
stack install --allow-different-user --ghc-options -O3 --force-dirty

make install

-}

_OUTPUT_AST = False
_INIT_DATA = False
_WRITE_PREPROCESSED = False
_KEEP_INSTRUMENTED = True
_PROGRESS_OUTPUT = False
_DEBUG_OUTPUT = False
_INSTR = True

gccExe = "gcc-4.7"
covFilename = "cov.dat"

main = do
	tvg_path:args <- getArgs
	let incs_path = tvg_path </> "tvg" </> "incs"

	restore_files <- case _INSTR of
		False -> return [return ()]
		True -> do
			-- set coverage filename in data.c (if it is not set yet)
			replaceInFile (incs_path </> "data.c") "\"/*COV_FILENAME*/\"" (show $ tvg_path </> "tvg" </> covFilename)

			when _INIT_DATA $ do
		--		copyFile "test2.c.orig" "test2.c"
				copyFile (incs_path </> "data.c.start") (incs_path </> "data.c") 
				copyFile (incs_path </> "data.h.start") (incs_path </> "data.h") 

			let args' = ["-I" ++ incs_path] ++ args
			let preprocess_args = filter (\ arg -> any (`isPrefixOf` arg) ["-I","-D"]) args'
			let o_arg = fmap head $ searcharg "-o" args
			forM args (handleArg o_arg preprocess_args tvg_path incs_path)

	when _DEBUG_OUTPUT $ putStrLn "Doing the compile..." 
	exitcode <- rawSystem gccExe $ ["-L"++incs_path,"-I"++incs_path] ++ args ++ ["-ldata"]

	sequence_ restore_files
	exitWith exitcode

	where
	searcharg _ [] = Nothing
	searcharg findarg (arg:arglist) | findarg==arg = Just arglist
	searcharg findarg (arg:arglist) = searcharg findarg arglist

handleArg o_arg preprocess_args tvg_path incs_path arg | ".c" `isSuffixOf` arg && takeFileName arg /= "conftest.c" = do
	handleSrcFile o_arg preprocess_args tvg_path incs_path arg
handleArg _ _ _ _ _ = return $ return ()

handleSrcFile o_arg preprocess_args tvg_path incs_path name = do
	when _OUTPUT_AST $ do
		mb_ast <- parseCFile (newGCC gccExe) Nothing preprocess_args name
		case mb_ast of
			Left err -> error $ show err
			Right ast -> writeFile (name++".ast") $ showDataTree ast

	when _WRITE_PREPROCESSED $ do
		Right inputstream <- runPreprocessor (newGCC gccExe) (rawCppArgs preprocess_args name)
		writeFile (name++".i") $ inputStreamToString inputstream

	-- Make file pre-instrumentation backup 
	let bak_name = name ++ ".preinstr"
	copyFile name bak_name

	abs_filename <- makeAbsolute name
	output_filename <- case o_arg of
		Nothing    -> return "<none>"
		Just ofile -> makeAbsolute ofile

	let filenameid = to_id $ maybe "" id o_arg ++ "__" ++ name
	let varname = "src_" ++ filenameid
	let tracefunname = "trace_" ++ filenameid

	-- Add the trace function declaration to data.h
	let tracefundecl = printf "void %s(int i)" tracefunname
	insertBeforeMarker (incs_path </> "data.h") "/*INSERT_HERE*/" $ tracefundecl ++ ";\n"

	-- Add definition of trace_function to data.c
	let fundecl = printf "%s { %s.counters[i+1].cnt++; }" tracefundecl varname
	insertBeforeMarker (incs_path </> "data.c") "/*INSERT_SRCFILE_HERE*/" $
		printf "SRCFILE %s = { %s, %s, /*NUM_LOCS*/, {\n{0,0,0}/*DUMMY*//*LOCATIONS*/\n} };\n" varname (show abs_filename) (show output_filename) ++
		fundecl ++ "\n\n"

	parseresult <- parseCFile (newGCC gccExe) Nothing preprocess_args name
	mb_err <- case parseresult of
		Left errmsg -> return $ Just $ show errmsg
		Right ast -> do
			let instr_filename = name++".instr"
			writeFile instr_filename "#include <data.h>\n\n"

			InstrS{..} <- execStateT (processASTM ast) $ InstrS filenameid 0 tvg_path incs_path varname tracefunname abs_filename instr_filename []
			copyFile instr_filename name
			when (not _KEEP_INSTRUMENTED) $ removeFile instr_filename

			replaceInFile (incs_path </> "data.c") "/*NUM_LOCS*/" (show numLocsS)

			let srcptr = printf ",\n&%s " varname
			insertBeforeMarker (incs_path </> "data.c") "/*INSERT_SRCPTR_HERE*/" srcptr

			-- Delete LOCATIONS marker (for next source file)
			replaceInFile (incsPathS </> "data.c") "/*LOCATIONS*/" ""

			when _DEBUG_OUTPUT $ putStrLn "Compiling data.c with stack..."
			(exitcode,stdout,stderr) <- readProcessWithExitCode "stack" [ "--allow-different-user", "--stack-yaml", tvg_path</>"tvg"</>"stack.yaml", "ghc", "--", "-shared", "-threaded", "-dynamic", "-DQUIET", "-fPIC", "-no-hs-main",
				"-I"++incs_path, incs_path</>"data.c", incs_path</>"CovStats.hs", "-o", incs_path</>"libdata.so",
				"-lHSrts_thr-ghc8.4.3", "-lffi" ] ""
				
			case exitcode of
				ExitSuccess   -> return Nothing
				ExitFailure _ -> return $ Just $ "Compile data.c failed:\n" ++ stdout ++ stderr

	case mb_err of
		Nothing -> return $ do
			removeFile name
			-- This renameFile will set the source file's date to newer than the output file(s), so...
			renameFile bak_name name
			-- ... we set the date to "now" for make to behave correctly when resuming compilation etc.
			now <- getCurrentTime
			setModificationTime name (addUTCTime ((-1)*nominalDay) now)
		Just errmsg -> do
			removeFile name >> renameFile bak_name name
			error errmsg

to_id str = map (\ c -> if isAlphaNum c then c else '_') str

replaceInFile :: String -> String -> String -> IO ()
replaceInFile filename marker text = do
	f <- TIO.readFile filename
	TIO.writeFile filename $ T.replace (T.pack marker) (T.pack text) f

insertBeforeMarker filename marker text = replaceInFile filename marker (text ++ marker)

data InstrS = InstrS {
	fileNameIdS :: String, numLocsS :: Int, tvgPathS :: String, incsPathS :: String, varNameS :: String,
	traceFunNameS :: String, absFileNameS :: String, instrFileNameS :: String, locationsS :: [(Int,Int)] } deriving Show
type InstrM a = StateT InstrS IO a

processASTM :: CTranslUnit -> InstrM ()
processASTM (CTranslUnit extdecls _) = mapM_ instrumentExtDecl extdecls

locString (l,c) = printf "{ %li,%li,0 }" l c

instrumentExtDecl :: CExtDecl -> InstrM CExtDecl
instrumentExtDecl ast = do
{-
	-- Check if the ExtDecl's source file is somwhere under the tvg root.
	-- Instrument the file if yes (i.e. do not instrument system library source files)
	tvg_path <- gets tvgPathS
	tvg_abspath <- liftIO $ canonicalizePath tvg_path
--}
	let Just fn = fileOfNode ast
	src_abspath <- liftIO $ makeAbsolute fn
	absfilename <- gets absFileNameS
	ast' <- case absfilename == src_abspath of
		True -> do
			when _PROGRESS_OUTPUT $ liftIO $ putStrLn $ "instrumentExtDecl at " ++ show (posOfNode $ nodeInfo ast)
			incspath <- gets incsPathS
			modify $ \ s -> s { locationsS = [] }

			ast' <- everywhereM (mkM instrumentStmt) ast
			let instr_ast =
				everywhere (mkT elimInStatExprs) $
				everywhere (mkT insertbeforeexits) $
				everywhere (mkT insertopen)
				ast'
			InstrS{..} <- get
			liftIO $ insertBeforeMarker (incsPathS </> "data.c") "/*LOCATIONS*/" $
				concatMap (\ loc -> ",\n" ++ locString loc) locationsS

			modify $ \ s -> s { numLocsS = numLocsS + length locationsS, locationsS = [] }
			return instr_ast
		False -> return ast

	instr_filename <- gets instrFileNameS
	let filecomment = printf "/* %s */\n" (show (fileOfNode ast))
	liftIO $ appendFile instr_filename $ filecomment ++ (render $ pretty ast') ++ "\n"

	return ast'

instrumentStmt :: CStat -> InstrM CStat
instrumentStmt cstat = do
	InstrS{..} <- get

	modify $ \ s -> s { locationsS = loc : locationsS }

	let ret = CCompound [] [ instr traceFunNameS (numLocsS + length locationsS), CBlockStmt cstat ] undefNode
--	when (_PROGRESS_OUTPUT && mod (length locationsS) 100 == 0)  $ liftIO $ putStrLn $ printf "locs= %8i" (length locationsS)
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
