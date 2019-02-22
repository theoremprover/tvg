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
import Language.C.Data.Node
import Language.C.System.GCC
import Text.PrettyPrint
import System.FilePath
import Data.Generics
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class (liftIO)
import Data.Char
import Text.Printf
import Data.Time.Clock
import Data.Maybe

import Language.C.System.Preprocess
import Language.C.Data.InputStream

import ShowAST

_OUTPUT_AST = False
_INIT_DATA = False
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

handleArg o_arg preprocess_args tvg_path incs_path arg | ".c" `isSuffixOf` arg && takeFileName arg `notElem` ["conftest.c","dummy.c"] = do
	handleSrcFile o_arg preprocess_args tvg_path incs_path arg
handleArg _ _ _ _ _ = return $ return ()

handleSrcFile o_arg preprocess_args tvg_path incs_path name = do
	-- Make file pre-instrumentation backup 
	let bak_name = name ++ ".preinstr"
	copyFile name bak_name

{-
	Right inputstream <- runPreprocessor (newGCC gccExe) (rawCppArgs preprocess_args name)
	let
		delete_hashlines = unlines . filter (not . ("#" `isPrefixOf`)) . lines
		preprocessed_src = delete_hashlines $ inputStreamToString inputstream
	length preprocessed_src `seq` (writeFile name preprocessed_src)
	copyFile name (replaceExtension name "i")
-}

	mb_ast <- parseCFile (newGCC gccExe) Nothing preprocess_args name
	case mb_ast of
		Left err -> error $ show err
		Right ast -> do
			when _OUTPUT_AST $ writeFile (name++".ast") $ showDataTree ast

			abs_filename <- canonicalizePath name
			output_filename <- case o_arg of
				Nothing    -> return "<none>"
				Just ofile -> canonicalizePath ofile

			let filenameid = map (\ c -> if isAlphaNum c then c else '_') $
				maybe "" id o_arg ++ "__" ++ name
			let varname = "src_" ++ filenameid
			let tracefunname = "trace_" ++ filenameid

			-- Add the trace function declaration to data.h
			let tracefundecl = printf "void %s(int i)" tracefunname
			insertBeforeMarker (incs_path </> "data.h") "/*INSERT_HERE*/" $ tracefundecl ++ ";\n"

			-- Add definition of trace_function to data.c
			let fundecl = printf "%s { %s.counters[i].cnt++; }" tracefundecl varname
			insertBeforeMarker (incs_path </> "data.c") "/*INSERT_SRCFILE_HERE*/" $
				printf "SRCFILE %s = { %s, %s, /*NUM_LOCS*/, {\n{0,0,0,0}/*DUMMY*//*LOCATIONS*/\n} };\n" varname (show abs_filename) (show output_filename) ++
				fundecl ++ "\n\n"

			let instr_filename = name++".instr"
			writeFile instr_filename "#include <data.h>\n\n"

			InstrS{..} <- execStateT (processASTM ast) $ InstrS filenameid 0 tvg_path incs_path varname tracefunname abs_filename instr_filename []
			copyFile instr_filename name

			replaceInFile (incs_path </> "data.c") "/*NUM_LOCS*/" (show $ numLocsS + 1)

			let srcptr = printf ",\n&%s " varname
			insertBeforeMarker (incs_path </> "data.c") "/*INSERT_SRCPTR_HERE*/" srcptr

			-- Delete LOCATIONS marker (for next source file)
			replaceInFile (incsPathS </> "data.c") "/*LOCATIONS*/" ""

			when _DEBUG_OUTPUT $ putStrLn "Compiling data.c with stack..."
			(exitcode,stdout,stderr) <- readProcessWithExitCode "stack" [ "--allow-different-user", "--stack-yaml", tvg_path</>"tvg"</>"stack.yaml", "ghc", "--", "-shared", "-threaded", "-dynamic", "-optc-DQUIET", "-fPIC", "-no-hs-main",
				"-I"++incs_path, incs_path</>"data.c", incs_path</>"CovStats.hs", "-o", incs_path</>"libdata.so",
				"-lHSrts_thr-ghc8.4.3", "-lffi" ] ""

			case exitcode of
				ExitSuccess -> return $ do
					removeFile name
					-- This renameFile will set the source file's date to newer than the output file(s), so...
					renameFile bak_name name
					-- ... we set the date to "now" for make to behave correctly when resuming compilation etc.
					now <- getCurrentTime
					setModificationTime name (addUTCTime ((-1)*nominalDay) now)
				ExitFailure _ -> do
					removeFile name >> renameFile bak_name name
					error $ "Compile data.c failed:\n" ++ stdout ++ stderr

replaceInFile :: String -> String -> String -> IO ()
replaceInFile filename marker text = do
	f <- TIO.readFile filename
	TIO.writeFile filename $ T.replace (T.pack marker) (T.pack text) f

insertBeforeMarker filename marker text = replaceInFile filename marker (text ++ marker)

data InstrS = InstrS {
	fileNameIdS :: String, numLocsS :: Int, tvgPathS :: String, incsPathS :: String, varNameS :: String,
	traceFunNameS :: String, absFileNameS :: String, instrFileNameS :: String, locationsS :: [(Int,Int,Int)] } deriving Show
type InstrM a = StateT InstrS IO a

processASTM :: CTranslUnit -> InstrM ()
processASTM (CTranslUnit extdecls _) = mapM_ instrumentExtDecl extdecls

locString (l,c,len) = printf "{ %li,%li,%li,0 }" l c len

instrumentExtDecl :: CExtDecl -> InstrM CExtDecl
instrumentExtDecl ast = do
{-
	-- Check if the ExtDecl's source file is somwhere under the tvg root.
	-- Instrument the file if yes (i.e. do not instrument system library source files)
	tvg_path <- gets tvgPathS
	tvg_abspath <- liftIO $ canonicalizePath tvg_path
--}
	let Just fn = fileOfNode ast
	src_abspath <- liftIO $ canonicalizePath fn
	absfilename <- gets absFileNameS
	ast' <- case absfilename == src_abspath of
		True -> do
			when _PROGRESS_OUTPUT $ liftIO $ putStrLn $ "instrumentExtDecl at " ++ show (posOfNode $ nodeInfo ast)
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
instrumentStmt cstat = case cstat of
	CCompound _ _ _ -> return cstat
	CCase _ _ _     -> return cstat
	CCases _ _ _ _  -> return cstat
	CDefault _ _    -> return cstat
	CLabel _ _ _ _  -> return cstat
	_ -> do
		InstrS{..} <- get
		modify $ \ s -> s { locationsS = locationsS ++ [loc] }
		return $ CCompound [] [ instr traceFunNameS (numLocsS + length locationsS + 1), CBlockStmt cstat ] undefNode
		where
		pos = posOfNode $ nodeInfo cstat
		loc = (posRow pos,posColumn pos,maybe 1 id (lengthOfNode (nodeInfo cstat)))
		str = posFile pos ++ show (posRow pos,posColumn pos)
		instr tracefunname index = call_stmt (builtinIdent tracefunname) [CConst $ CIntConst (cInteger $ fromIntegral index) undefNode]

elimInStatExprs :: CExpr -> CExpr
elimInStatExprs (CStatExpr stat nodeinfo) = CStatExpr (everywhere (mkT elimNestedCompounds) stat) nodeinfo
elimInStatExprs x = x

elimNestedCompounds :: CStat -> CStat
elimNestedCompounds (CCompound ids cbis nodeinfo) = CCompound ids (concatMap flatten cbis) nodeinfo where
	flatten (CBlockStmt (CCompound [] cbis2 _)) = cbis2
	flatten x = [x]
elimNestedCompounds x = x

-- Insert call to "opentrace" at the beginning of "main" function
insertopen :: CFunDef -> CFunDef
insertopen (CFunDef declspecs declr@(CDeclr (Just (Ident "main" _ _)) _ _ _ _) decls cstat ni) =
	CFunDef declspecs declr decls (CCompound [] [callopentrace,CBlockStmt (insertbeforereturns cstat),callclosetrace] undefNode) ni
insertopen x = x

call_stmt ident args = CBlockStmt $ CExpr (Just (CCall (CVar ident undefNode) args undefNode)) undefNode
callopentrace = call_stmt (builtinIdent "opentrace") []
callclosetrace = call_stmt (builtinIdent "closetrace") []

my_ret = internalIdent "my_ret"

-- Insert call to "closetrace" before each return statement
-- (evaluating return's argument beforehand by assigning it to the fresh "my_ret" variable)
insertbeforereturns = everywhere (mkT insertclose) where
	insertclose :: CStat -> CStat
	insertclose (CReturn (Just ret_expr) _) = create_compound ret_expr $
		CReturn (Just $ CVar my_ret undefNode) undefNode
	insertclose x = x

-- Do the same like in "insertbeforereturns" before calls to "exit".
insertbeforeexits :: CStat -> CStat
insertbeforeexits (CExpr (Just (CCall fun@(CVar (Ident "exit" _ _) _) [ret_expr] _)) _) = create_compound ret_expr $
	CExpr (Just $ CCall fun [CVar my_ret undefNode] undefNode) undefNode
insertbeforeexits x = x

-- Create C compound assigning "my_ret" to a given expression, calling closetrace, and finishing with a given statement. 
create_compound ret_expr ret_stmt = CCompound [] [
	CBlockDecl $ CDecl [CTypeSpec $ CIntType undefNode]
		[ ( Just $ CDeclr (Just my_ret) [] Nothing [] undefNode, Just $ CInitExpr ret_expr undefNode, Nothing ) ] undefNode,
	callclosetrace,
	CBlockStmt ret_stmt
	] undefNode
