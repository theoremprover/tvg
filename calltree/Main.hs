{-# LANGUAGE RecordWildCards,LambdaCase,OverloadedStrings,ScopedTypeVariables,StandaloneDeriving,FlexibleInstances,TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

{-
stack build :calltree-exe && stack exec calltree-exe -- "gcc" <SRCDIR>
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

outputDir = "calltree_out"
strangeCalls = "strangecalls.txt"
logFile = "log.txt"

myError :: (MonadIO m) => forall a . String -> m a
myError txt = do
	printLog txt
	error txt

printLog :: (MonadIO m) => String -> m ()
printLog txt = liftIO $ do
	putStrLn txt
	appendFile (outputDir </> logFile) (txt++"\n")

main = do
	args <- getArgs
	maini args

maini [] = do
	putStrLn "USAGE (in tvg dir): stack exec :calltree-exe -- <GCC-EXE> {<SRCFILEDIR>|<OPT>}*"

maini (gccexe:args) = do
	removePathForcibly outputDir
	createDirectory outputDir
	getCurrentTime >>= return.show >>= writeFile (outputDir </> strangeCalls)
	getCurrentTime >>= return.show >>= writeFile (outputDir </> logFile)
	listDirectory outputDir >>= mapM_ print
	
	let (preprocess_args,other_args) = partition (\ arg -> any (`isPrefixOf` arg) ["-I","-D"]) args
	pot_filess <- forM other_args getsrcfiles
	let pot_files = concat pot_filess
	mapM_ print pot_files
	calls <- handleSrcFiles gccexe preprocess_args pot_files

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

-- Returns a list of (funname,srcfile,[calledfunname1,...])
handleSrcFiles gccexe preprocess_args srcfilenames = do
	extdeclss <- forM srcfilenames $ \ srcfilename -> do
		putStrLn $ "handling SrcFile " ++ srcfilename
	--	mb_ast <- parseCFile (newGCC gccexe) Nothing preprocess_args srcfilename
		mb_ast <- parseCFilePre srcfilename
		case mb_ast of
			Left err -> error $ show err
			Right (CTranslUnit extdecls _) -> return extdecls
	let ctranslunit = CTranslUnit (concat extdeclss) undefNode
	execStateT (everywhereM (mkM (searchFunDefs ctranslunit)) ctranslunit) []

-- For all function definitons in the current source file:
searchFunDefs :: CTranslUnit -> CFunDef -> StateT [(String,Maybe FilePath,[String])] IO CFunDef
searchFunDefs ctranslunit cfundef@(CFunDef _ (CDeclr (Just (Ident funname _ _)) _ _ _ _) _ stmt ni) = do
	calledfunnames <- execStateT (everywhereM (mkM $ searchFunCalls cfundef ctranslunit) stmt) []
--	liftIO $ print calledfunnames
	modify ((funname,fileOfNode ni,nub calledfunnames) : )
	return cfundef

isEqualExpr :: CExpr -> CExpr -> Bool
isEqualExpr (CVar ident1 _) (CVar ident2 _) = ident1==ident2
isEqualExpr (CMember lexpr1 member_ident1 isptr1 _) (CMember lexpr2 member_ident2 isptr2 _) =
	isEqualExpr lexpr1 lexpr2 && member_ident1==member_ident2 && isptr1==isptr2
isEqualExpr expr1 expr2 = False

-- Returns all called functions
searchFunCalls :: (MonadIO m) => CFunDef -> CTranslUnit -> CExpr -> StateT [String] m CExpr
searchFunCalls fundef0@(CFunDef _ (CDeclr (Just (Ident funname _ _)) _ _ _ _) _ _ _) ctranslunit ccall@(CCall funname_expr _ ni) = do
	liftIO $ do
		printLog $ "\n====== searchFunCalls ==========="
		printLog $ "current function = " ++ funname
		printLog $ "Call at " ++ show ni ++ ": " ++ (render.pretty) ccall
	names <- chaseFun funname_expr
	modify (names++)
	return ccall

	where
	
	chaseFun :: (MonadIO m) => CExpr -> StateT [String] m [String]
	chaseFun funname_expr = do
		liftIO $ printLog $ "chaseFun " ++ (render.pretty) funname_expr
		case funname_expr of

-- (<cast_type> expr)(..)
			CCast _ expr _ -> do
				liftIO $ printLog $ "Ignoring cast..."
				chaseFun expr

-- (*<var>)(..)
			CUnary CIndOp expr _ -> do
				printLog $ "Found *<expr>, but ignoring it: " ++ show ni ++ " " ++ (render.pretty) expr
				chaseFun expr

-- <expr>
			expr -> do
				printLog $ "Chasing value " ++ (render.pretty) expr
				chaseValue (fundef0,expr)

	chaseValue :: (MonadIO m) => (CFunDef,CExpr) -> StateT [String] m [String]

-- <varname>
	chaseValue (fundef@(CFunDef _ (CDeclr (Just fun_ident@(Ident funname _ _)) _ _ _ _) argdecls _ _),cvar@(CVar ident@(Ident name _ _) _)) = do
		printLog $ "chaseValue ( _ , " ++ (render.pretty) cvar ++ " )"
		let
			argident_is_ident (_,CDecl _ [(Just (CDeclr (Just argident) _ _ _ _),_,_)] _) = argident==ident
			argident_is_ident _ = False
		case filter argident_is_ident (zip [0..] argdecls) of
			[(argnum,_)] -> do
				printLog $ name ++ " is arg nr. " ++ show argnum ++ " in function " ++ funname
				concatMapM chaseValue $ map (\ cargs -> (fundef,cargs!!argnum)) $
					(findAll funCall >>> filterPred calledfunname_is_ident >>> getCallArgs) ctranslunit
					where
					calledfunname_is_ident (CCall (CVar i _) _ _) = i==fun_ident
					calledfunname_is_ident _ = False
			[] -> do
				printLog $ show name ++ " is not an argument of the current function."
				printLog $ "Searching for a function declaration for " ++ show name ++ " ..."
				case declFunName ident ctranslunit of
					(Ident found_name _ ni):_ -> do
						printLog $ "Found function declaration for " ++ show name ++ " at " ++ show ni
						return [ found_name ]
					[] -> do
						printLog $ "No function declaration found."
						printLog $ "Searching for assignments to " ++ show name ++ " ..."
						case varAssignmentWithFunDef (isEqualExpr cvar) ctranslunit of
							[] -> do
								printLog $ "No assignment found to " ++ show name ++ ", hence assuming it is a built-in function."
								return [ name ]
							asss -> concatMapM chaseValue asss
{-
	chaseValue (fundef,cmember@(CMember obj_expr member_ident isptr _)) = do
		printLog $ "Found CMember: " ++ (render.pretty) cmember
-}

-- <expr>
	chaseValue (_,expr) = do
		printLog $ "chaseValue ( _ , " ++ (render.pretty) expr ++ " )"
		case varAssignmentWithFunDef (isEqualExpr expr) ctranslunit of
			[] -> do
				printLog $ (render.pretty) expr ++ " is assigned nowhere, hence it must have been declared external, so it is called from outside => out of scope"
				return []
			asss -> do
				concatMapM chaseValue asss

searchFunCalls _ _ cexpr = return cexpr




