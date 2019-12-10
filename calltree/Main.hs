{-# LANGUAGE RecordWildCards,LambdaCase,OverloadedStrings,TupleSections,ScopedTypeVariables,StandaloneDeriving,FlexibleInstances,TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

{-
stack build :calltree-exe && stack exec calltree-exe -- "gcc" <SRCDIR>

_rtld_local._dl_rtld_lock_recursive
-}

module Main where

import System.Environment
import System.Directory
import Data.List
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Extra
import Language.C
import Language.C.Data.Ident
import Language.C.System.GCC
import Language.C.Pretty
import Data.Generics
import Control.Monad.Trans.State.Strict
import Data.Graph
import System.FilePath
import Data.Maybe
import System.IO
import Text.PrettyPrint
import Data.Time.Clock

import CDSL
import DataTree


outputDir = "calltree_out"
strangeCalls = "strangecalls.txt"
logFile = "log.txt"
writeASTFiles = True

myError :: (MonadIO m) => forall a . String -> m a
myError txt = do
	printLog txt
	error txt

printLog :: (MonadIO m) => String -> m ()
printLog txt = liftIO $ do
--	putStrLn txt
	appendFile (outputDir </> logFile) (txt++"\n")

main = do
	args <- getArgs
	maini args

maini [] = do
	putStrLn "USAGE (in tvg dir): stack exec :calltree-exe -- <GCC-EXE> {<SRCFILEDIR>|<OPT>}*"

maini (gccexe:args) = do
	hSetBuffering stdout NoBuffering
	
	removePathForcibly outputDir
	createDirectory outputDir
	getCurrentTime >>= return.(++"\n\n").show >>= writeFile (outputDir </> strangeCalls)
	getCurrentTime >>= return.(++"\n\n").show >>= writeFile (outputDir </> logFile)
	listDirectory outputDir >>= mapM_ print
	
	let (preprocess_args,other_args) = partition (\ arg -> any (`isPrefixOf` arg) ["-I","-D"]) args
	pot_filess <- forM other_args getsrcfiles
	let pot_files =  [ "ifiles/tlsdesc.i" ] --nub $ concat pot_filess  
	let num_srcfiles = length pot_files
	printLog $ "Found " ++ show num_srcfiles ++ " source files."
--	mapM_ print pot_files
	callss <- forM (zip [1..] pot_files) $ handleSrcFile gccexe preprocess_args num_srcfiles
	let calls = concat callss
	
	let (callgraph,vertex2node,key2vertex) = graphFromEdges $ map (\(funname,mb_defsrcfile,calledfunnames) -> ((funname,mb_defsrcfile),funname,calledfunnames)) calls

	writeFile "graph.dot" $ unlines $ nub $ map (drawedge vertex2node) (edges callgraph)

	calledfunctions_hull <- forM functions $ \ rootfunname -> do
		case key2vertex rootfunname of
			Nothing -> do
				printLog $ "\n??? Function " ++ rootfunname ++ " not found in source files!"
				writeFile (outputDir </> (rootfunname ++ "_NOT_FOUND")) ""
				return []
			Just rootvertex -> do
				forM (reachable callgraph rootvertex) $ \ vertex -> do
					let
						calledfuns = map (\ v -> let (node,key,_) = vertex2node v in (node,key)) $ reachable callgraph vertex
						((sourcefun,mb_sourcefile),_,_) = vertex2node vertex
					createDirectoryIfMissing True (outputDir </> rootfunname)
					writeFile (outputDir </> rootfunname </> sourcefun <.> "csv") $ printCSV $ ["FUNCTIONS CALLED DIRECTLY OR INDIRECTLY","FUNCTION DEFINED IN","DIRECTLY CALLED BY"] :
						[ [f, maybe "<NOWHERE>" id mb_srcfile, concat $ intersperse ","
							[ caller | (callerv,calledv) <- edges callgraph, Just calledv == key2vertex fk, let ((caller,_),_,_) = vertex2node callerv ] ] | ((f,mb_srcfile),fk) <- calledfuns ]
					return (sourcefun,map (fst.fst) calledfuns \\ [sourcefun])

	-- JSON output shall also contain indirectly called functions
	let functionhull = nub $ concat calledfunctions_hull
	json_liness :: [[String]] <- forM functionhull $ \ (funname,calledfuns) -> return ( map ("    "++) $ [ show funname ++ ": {", "  \"calltree\": [" ] ++ map ("    "++) (interspersef (++",") id (map show calledfuns)) ++ [ "  ]", "}" ] )
	writeFile (outputDir </> "calltree.json") $ unlines $ [
		"{",
		"  \"info\": {" ] ++
		concat (interspersef (interspersef id (++",")) id json_liness) ++ [
		"  }",
		"}" ]

	where
	drawedge vertex2node (callerv,calledv) = let
		((callername,_),_,_) = vertex2node callerv
		((calledname,_),_,_) = vertex2node calledv in
		callername ++ " -> " ++ calledname ++ ";"

	-- apply f to each but the last element in a list
	interspersef :: (a -> a) -> (a -> a) -> [a] -> [a]
	interspersef _ _ [] = []
	interspersef f g l = map f (take (length l - 1) l) ++ [ g $ last l ]

getsrcfiles :: String -> IO [String]
getsrcfiles name = do
	isdir <- doesDirectoryExist name
	case isdir of
		False -> case any (`isSuffixOf` name) [".i.c",".c.i",".h",".i"] of
			True -> return [name]
			False -> return []
		True -> do
			names <- getDirectoryContents name
			filess <- mapM getsrcfiles $ map (name </>) $ filter (not . (`elem` [".",".."])) names
			return $ concat filess

removeellipses path = do
	srcfiles <- getsrcfiles path
	forM_ srcfiles $ \ filename -> do
		f <- readFile filename
		let ls = lines f
		print $ length ls
		writeFile filename $ unlines $ ls

functions = [ "dl_main","_dl_start","_dl_start_final" ]

printCSV ls = unlines $ map (concat . (intersperse ";")) ls

type AnalyzeState = [Ident]
newAnalyzeState = []
type AnalyzeS m a = StateT AnalyzeState m a

addNamesS :: (MonadIO m) => [Ident] -> AnalyzeS m ()
addNamesS names = modify (names++)

-- Returns a list of (funname,srcfile,[calledfunname1,...])
handleSrcFile gccexe preprocess_args num_srcfiles (i,srcfilename) = do
	printLog $ "\n=============================================================="
	printLog $ "FILENAME = " ++ srcfilename ++ "\n"
	putStrLn $ "[ " ++ show (div (i*100) num_srcfiles) ++ "% ] Handling SrcFile " ++ show i ++ " of " ++ show num_srcfiles ++ ": " ++ srcfilename
--	mb_ast <- parseCFile (newGCC gccexe) Nothing preprocess_args srcfilename
	mb_ast <- parseCFilePre srcfilename
	ctranslunit <- case mb_ast of
		Left err -> error $ show err
		Right ast -> return ast
	when writeASTFiles $ liftIO $ writeFile (outputDir </> (takeFileName srcfilename) <.> ".ast.html") $ genericToHTMLString ctranslunit
	execStateT (everywhereM (mkM (searchFunDefs ctranslunit)) ctranslunit) []

-- For all function definitons in the current source file:
searchFunDefs :: CTranslUnit -> CFunDef -> StateT [(String,Maybe FilePath,[String])] IO CFunDef
searchFunDefs ctranslunit cfundef@(CFunDef _ (CDeclr (Just (Ident funname _ _)) _ _ _ _) _ stmt ni) = do
	calledfunidents <- execStateT (everywhereM (mkM $ searchFunCalls cfundef ctranslunit) stmt) newAnalyzeState
	modify ((funname,fileOfNode ni,map identToString $ nub calledfunidents) : )
	return cfundef

isEqualExpr :: CExpr -> CExpr -> Bool
isEqualExpr (CVar ident1 _) (CVar ident2 _) = ident1==ident2
isEqualExpr (CMember lexpr1 member_ident1 isptr1 _) (CMember lexpr2 member_ident2 isptr2 _) =
	isEqualExpr lexpr1 lexpr2 && member_ident1==member_ident2 && isptr1==isptr2
isEqualExpr expr1 expr2 = False

-- Returns all called functions
searchFunCalls :: (MonadIO m) => CFunDef -> CTranslUnit -> CExpr -> StateT AnalyzeState m CExpr
searchFunCalls fundef0@(CFunDef _ (CDeclr (Just (Ident funname _ _)) _ _ _ _) _ _ _) ctranslunit ccall@(CCall _ _ ni) = do
	liftIO $ putStrLn $ "In function " ++ funname ++ ": " ++ (render.pretty) ccall
	printLog $ "\n====== searchFunCalls ==========="
	printLog $ "current function = " ++ funname
	printLog $ "Call at " ++ show ni ++ ": " ++ (render.pretty) ccall
	idents <- chaseFun (fundef0,ccall)
	addNamesS idents
	return ccall

	where
	
	chaseFun :: (MonadIO m) => (CFunDef,CExpr) -> StateT AnalyzeState m [Ident]
	chaseFun (fundef,CCall funname_expr _ _) = do
		printLog $ "chaseFun " ++ (render.pretty) funname_expr
		case funname_expr of

-- (<cast_type> expr)(..)
			CCast _ expr _ -> do
				printLog $ "Ignoring cast..."
				chaseFun (fundef,expr)

-- (*<var>)(..)
			CUnary CIndOp expr _ -> do
				printLog $ "Found *<expr>, but ignoring it: " ++ show ni ++ " " ++ (render.pretty) expr
				chaseFun (fundef,expr)

-- <expr>
			expr -> do
				printLog $ "Chasing value " ++ (render.pretty) expr
				chaseValue [ getFunDefIdent fundef ] (fundef,expr)

	chaseFun (fundef,CVar ident _) = do
		printLog $ "chaseFun found variable " ++ (render.pretty) ident
		return [ident]

	chaseFun (fundef,expr) = error $ "chaseFun " ++ (render.pretty) expr ++ " not implemented"

	chaseValue :: (MonadIO m) => [Ident] -> (CFunDef,CExpr) -> StateT AnalyzeState m [Ident]

-- occurs check
	chaseValue occuredfunctions (CFunDef _ (CDeclr (Just fun_ident) _ _ _ _) _ _ _,_) |
		fun_ident `elem` occuredfunctions = do
			printLog $ "chaseValue occurs check: " ++ (render.pretty) fun_ident ++ " is in " ++ show (map identToString occuredfunctions)
			return []

-- <varname>
	chaseValue occuredfunctions (fundef@(CFunDef _ (CDeclr (Just fun_ident@(Ident funname _ _)) ((CFunDeclr (Right (argdecls,_)) _ _):_) _ _ _) _ _ _),cvar@(CVar ident@(Ident name _ _) _)) = do
		printLog $ "chaseValue ( _ , " ++ (render.pretty) cvar ++ " )"
		let
			argident_is_ident (_,CDecl _ [(Just (CDeclr (Just argident) _ _ _ _),_,_)] _) = argident==ident
			argident_is_ident _ = False
			filtered = filter argident_is_ident (zip [0..] argdecls)
		case filtered of
			[(argnum,_)] -> do
				printLog $ name ++ " is arg nr. " ++ show argnum ++ " in function " ++ funname
				printLog $ "Found these calls calling " ++ show funname ++ ":"
				funss1 <- forM ((findAll funCallInFunDef) ctranslunit) $ \ (fundef',call'@(CCall callexpr args _)) -> do
					calledidents <- chaseFun (fundef',callexpr)
					funss <- forM (filter (==fun_ident) calledidents) $ \ calledident -> do
						let call_source_funident = getFunDefIdent fundef
						printLog $ "#### " ++ (render.pretty) call' ++ " in " ++ (render.pretty) call_source_funident
						chaseValue (call_source_funident:occuredfunctions) (fundef',args!!argnum)
					return $ concat funss
				return $ concat funss1
			[] -> do
				printLog $ show name ++ " is not an argument of the current function."
				printLog $ "Searching for a function declaration for " ++ show name ++ " ..."
				case declFunName ident ctranslunit of
					(found_ident@(Ident found_name _ ni)):_ -> do
						printLog $ "Found function declaration for " ++ show name ++ " at " ++ show ni
						return [ found_ident ]
					[] -> do
						printLog $ "No function declaration found."
						printLog $ "Searching for assignments to " ++ show name ++ " ..."
						case varAssignmentWithFunDef (isEqualExpr cvar) ctranslunit of
							[] -> do
								printLog $ "No assignment found to " ++ show name ++ ", hence assuming it is a built-in function."
								return [ ident ]
							asss -> concatMapM (chaseValue occuredfunctions) asss
{-
	chaseValue (fundef,cmember@(CMember obj_expr member_ident isptr _)) = do
		printLog $ "Found CMember: " ++ (render.pretty) cmember
-}

-- <expr>
	chaseValue occuredfunctions (_,expr) = do
		printLog $ "chaseValue ( _ , " ++ (render.pretty) expr ++ " )"
		case varAssignmentWithFunDef (isEqualExpr expr) ctranslunit of
			[] -> do
				printLog $ (render.pretty) expr ++ " is assigned nowhere, hence it must have been declared external, so it is called from outside => out of scope"
				return []
			asss -> do
				concatMapM (chaseValue occuredfunctions) asss

searchFunCalls _ _ cexpr = return cexpr
