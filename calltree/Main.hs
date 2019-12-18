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
strangeCalls = outputDir </> "strangecalls.txt"
logFile = outputDir </> "log.txt"
writeASTFiles = True

myError :: (MonadIO m) => forall a . String -> m a
myError txt = do
	printLog txt
	error txt

showPositionAndPretty :: (CNode a,Pretty a) => a -> String
showPositionAndPretty a = show (posOfNode $ nodeInfo a) ++ ": " ++ (render.pretty) a

printLog :: (MonadIO m) => String -> m ()
printLog txt = liftIO $ do
--	putStrLn txt
	appendFile logFile (txt++"\n")

main = do
	args <- getArgs
	maini args

maini [] = do
	putStrLn "USAGE (in tvg dir): stack exec :calltree-exe -- <GCC-EXE> {<SRCFILEDIR>|<OPT>}*"

maini (gccexe:args) = do
	hSetBuffering stdout NoBuffering
	
--	removePathForcibly outputDir
--	createDirectory outputDir
	getCurrentTime >>= return.(++"\n\n").show >>= writeFile strangeCalls
	getCurrentTime >>= return.(++"\n\n").show >>= writeFile logFile
	
	let (preprocess_args,other_args) = partition (\ arg -> any (`isPrefixOf` arg) ["-I","-D"]) args
	pot_filess <- forM other_args getsrcfiles
	let pot_files =  nub $ concat pot_filess  
--	let pot_files =  [ "ifiles/tlsdesc.i" ]
	let num_srcfiles = length pot_files
	printLog $ "Found " ++ show num_srcfiles ++ " source files."
--	mapM_ print pot_files
	callss <- forM (zip [1..] pot_files) $ handleSrcFile gccexe preprocess_args num_srcfiles
	let calls = concat callss
	
	let (callgraph,vertex2node,key2vertex) = graphFromEdges $ map (\(funname,mb_defsrcfile,calledfunnames) -> ((funname,mb_defsrcfile),funname,calledfunnames)) calls

	writeFile (outputDir </> "graph.dot") $ unlines $ nub $ map (drawedge vertex2node) (edges callgraph)

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
searchFunCalls fundef0@(CFunDef _ (CDeclr (Just (Ident funname _ _)) _ _ _ _) _ _ _) ctranslunit ccall@(CCall funexpr _ ni) = do
	liftIO $ putStrLn $ "In function " ++ funname ++ ": " ++ (render.pretty) ccall
	printLog $ "\n====== searchFunCalls ==========="
	printLog $ "current function = " ++ funname
	printLog $ "Call at " ++ showPositionAndPretty ccall
	idents <- chaseFun [] (fundef0,funexpr)
	addNamesS idents
	return ccall

	where

	isFormalParam :: CFunDef -> Ident -> Maybe Int
	isFormalParam cfundef ident = case filter argident_is_ident (zip [0..] argdecls) of
		[(argnum,_)] -> Just argnum
		_ -> Nothing
		where
		CFunDef _ (CDeclr (Just funident) ((CFunDeclr (Right (argdecls,_)) _ _):_) _ _ _) _ _ _ = cfundef
		argident_is_ident (_,CDecl _ [(Just (CDeclr (Just argident) _ _ _ _),_,_)] _) = argident==ident
		argident_is_ident _ = False

	chaseFun :: (MonadIO m) => [Ident] -> (CFunDef,CExpr) -> StateT AnalyzeState m [Ident]
	
	chaseFun occured_funs (fundef,expr) | getFunDefIdent fundef `elem` occured_funs = do
		printLog $ "chaseFun " ++ (show $ map (render.pretty) occured_funs) ++ " " ++ (render.pretty) expr
		printLog $ "OCCURS CHECK, cutting off."
		return []

	chaseFun occured_funs (fundef,funname_expr) = do
		printLog $ "chaseFun " ++ (show $ map (render.pretty) occured_funs) ++ " " ++ (render.pretty) funname_expr
		let currentfunident = getFunDefIdent fundef
		let currentfunname = (render.pretty) currentfunident
		case funname_expr of

-- (<cast_type> expr)(..)
			CCast _ expr _ -> do
				printLog $ "Ignoring cast..."
				chaseFun occured_funs (fundef,expr)

-- (*<var>)(..)
			CUnary CIndOp expr _ -> do
				printLog $ "Found *<expr>, but ignoring it: " ++ showPositionAndPretty expr
				chaseFun occured_funs (fundef,expr)

-- <expr>
			cvar@(CVar ident _) -> do
				printLog $ "Found CVar " ++ (render.pretty) ident
				case isFormalParam fundef ident of
				-- is formal param
					Just argnum -> do
						printLog $ (render.pretty) ident ++ " is arg with index " ++ show argnum ++ " of function " ++ currentfunname
						concatForM (funCallInFunDef ctranslunit) $ \ (fundef',call'@(CCall callexpr args _)) -> do
							case callexpr of
								CVar src_fun_ident _ | currentfunident==src_fun_ident -> do
									printLog $ "Resolved call to " ++ currentfunname ++ " at " ++ showPositionAndPretty call'
									case args!!argnum of
										CVar target_ident _ ->do
											printLog $ "Returning call arg CVar: " ++ showPositionAndPretty target_ident
											return [ target_ident ]
										arg -> do
											let msg = "STRANGE: Not in CVar form: " ++ showPositionAndPretty arg
											printLog $ msg
											liftIO $ appendFile strangeCalls $ msg ++ "\n"
											return []
								_ -> return []

					-- is no param...
					Nothing -> do
						let ident_name = (render.pretty) ident
						printLog $ ident_name++ " is no formal parameter, searching for declaration"
						case declName ident ctranslunit of
							(found_ident@(Ident found_name _ ni):_) -> do
								printLog $ "Found function declaration for " ++ ident_name ++ " at " ++ show ni
								return [ found_ident ]
							[] -> do
								printLog $ "No declaration of " ++ ident_name ++ " found, hence assuming it is a built-in function"
								return [ ident ]

			cmember@(CMember (CVar structident _) member_ident isptr _) -> do
				let struct_ident_name = (render.pretty) structident
				printLog $ "Found CMember(CVar) ptr=" ++ show isptr ++ " at " ++ showPositionAndPretty cmember
				printLog $ "Returning member " ++ (render.pretty) member_ident
				return [ member_ident ]
{-
				case isFormalParam fundef structident of
					Nothing -> do
						let ident_name = (render.pretty) structident
						printLog $ ident_name ++ " is no formal parameter, searching for declaration"
						case declName structident ctranslunit of
							(found_ident@(Ident found_name _ _):_) -> do
								printLog $ "Found declaration for " ++ struct_ident_name ++ ": " ++ showPositionAndPretty found_ident
								case varAssignment (isCVarWithIdent structident) ctranslunit of
									[] -> do
										printLog $ "No assignment to " ++ found_name ++ " found, so it must have been defined externally. Returning the member " ++ (render.pretty) member_ident
										return [ member_ident ]
									_ -> myError "Found assignments, but not implemented yet." 
							[] -> do
								myError $ "No declaration of " ++ struct_ident_name ++ " found!"
					Just argnum -> do
						printLog $ (render.pretty) structident ++ " is arg with index " ++ show argnum ++ " of function " ++ currentfunname
						printLog $ "Found these calls calling " ++ currentfunname ++ ":"
						funss1 <- forM (funCallInFunDef ctranslunit) $ \ (fundef',call'@(CCall callexpr args _)) -> do
							funidents <- chaseFun (currentfunident:occured_funs) (fundef',call')
							forM (filter (==currentfunident) funidents) $ \ caller_ident -> do
								let call_source_funident = getFunDefIdent fundef'
								printLog $ "CALL: " ++ (render.pretty) call' ++ " in " ++ (render.pretty) call_source_funident
								return caller_ident
						return $ concat funss1
-}
			expr -> strangecall expr

		where

		strangecall expr = do
			printLog $ "Strange FunCall: " ++ (render.pretty) expr
			liftIO $ appendFile strangeCalls $ showPositionAndPretty expr ++ "\n"
			return []
		
searchFunCalls _ _ cexpr = return cexpr
