{-# LANGUAGE RecordWildCards,LambdaCase,OverloadedStrings,ScopedTypeVariables #-}
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

myError txt = do
	printLog txt
	error txt

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
	mapM_ print (concat pot_filess)
	callss <- forM (concat pot_filess) (handleSrcFile gccexe preprocess_args)

	let (callgraph,vertex2node,key2vertex) = graphFromEdges $ map (\(funname,mb_defsrcfile,calledfunnames) -> ((funname,mb_defsrcfile),funname,calledfunnames)) (concat callss)

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
handleSrcFile gccexe preprocess_args srcfilename = do
	putStrLn $ "handling SrcFile " ++ srcfilename
	mb_ast <- parseCFile (newGCC gccexe) Nothing preprocess_args srcfilename
--	mb_ast <- parseCFilePre srcfilename
	case mb_ast of
		Left err -> error $ show err
		Right ctranslunit -> execStateT (everywhereM (mkM (searchFunDefs ctranslunit)) ctranslunit) []

-- For all function definitons in the current source file:
searchFunDefs :: CTranslUnit -> CFunDef -> StateT [(String,Maybe FilePath,[String])] IO CFunDef
searchFunDefs ctranslunit cfundef@(CFunDef _ (CDeclr (Just fun_ident@(Ident funname _ _)) (CFunDeclr (Right (argdecls,_)) _ _ : _) _ _ _) _ stmt ni) = do
	calledfunnames <- execStateT (everywhereM (mkM $ searchFunCalls argdecls fun_ident ctranslunit) stmt) []
--	liftIO $ print calledfunnames
	modify ((funname,fileOfNode ni,nub calledfunnames) : )
	return cfundef

-- Returns all called functions
searchFunCalls :: (MonadIO m) => [CDecl] -> Ident -> CTranslUnit -> CExpr -> StateT [String] m CExpr
searchFunCalls argdecls fun_ident ctranslunit ccall@(CCall funname_expr _ ni) = do
	liftIO $ do
		printLog $ "\n====== searchFunCalls ==========="
		printLog $ "current function = " ++ (render.pretty) fun_ident
		printLog $ "Call at " ++ show ni ++ ": " ++ (render.pretty) ccall
	names <- chaseFun funname_expr
	modify (names++)
	return ccall

	where
	
	chaseFun :: (MonadIO m) => CExpr -> StateT [String] m [String]
	chaseFun funname_expr = do
		liftIO $ printLog $ "chaseFun " ++ (render.pretty) funname_expr
		case funname_expr of
-- ((<cast_type> expr)(..)
			CCast _ expr _ -> do
				liftIO $ printLog $ "Ignoring cast..."
				chaseFun expr
	
-- (*<var>)(..)
			CUnary CIndOp expr _ -> do
				printLog $ "Found *<expr>, but ignoring it: " ++ show ni ++ " " ++ (render.pretty) expr
				chaseFun expr
	
-- <var>(..)
--			CVar ident _ | ident==fun_ident -> return []  -- Recursive call occurence check
			CVar ident@(Ident name _ _) _ -> do
				printLog $ "Found <ident>: " ++ show ni ++ " " ++ (render.pretty) ident
				let
					argident_is_ident (_,CDecl _ [(Just (CDeclr (Just argident) _ _ _ _),_,_)] _) = argident==ident
					argident_is_ident _ = False
				case filter argident_is_ident (zip [0..] argdecls) of
					[(argnum,_)] -> do
						printLog $ name ++ " is arg nr. " ++ show argnum
						concatMapM chaseFun $ map (!!argnum) $
							(findAll funCall >>> filterPred calledfunname_is_ident >>> getCallArgs) ctranslunit
							where
							calledfunname_is_ident (CCall (CVar i _) _ _) = i==fun_ident
							calledfunname_is_ident _ = False
					[] -> do
						printLog $ name ++ " is not an argument of the current function, hence plain function call"
						return [ name ]

{-
-- (<struct_ident>.<member_ident>)(..)
			CMember (CVar struct_ident _) member_ident False _ -> do
				case varAssignment struct_ident ctranslunit of
					[] -> do
						printLog $ (render.pretty) struct_ident ++ " is assigned nowhere, hence it must have been declared external, so it is called from outside => out of scope"
						return ()
					asss -> forM_ asss $ \ ass -> case ass of
						CVar ident ni -> do
							printLog $ (render.pretty) struct_ident ++ " is assigned at " ++ show ni ++ " as " ++ (render.pretty) ass
							case defFunName ident ctranslunit of
								[] -> myError $ "Identifier " ++ (render.pretty) ident ++ " not found as a function name"
								[ Ident name _ ni ] -> do
									liftIO $ putStrLn $ "Found (a.b)(...) type call: " ++ show ni ++ ", called function is " ++ show name
									modify (name:)
								_ -> myError $ "Identifier " ++ (render.pretty) ident ++ " found more than once as a function name"
						assexpr -> myError $ "assexpr " ++ (render.pretty) assexpr ++ " not implemented"
				return ccall
-}
-- other calls
			fne -> do
				let msg = show (posOfNode ni) ++ ": " ++ (render $ pretty ccall) ++ "\n"
				liftIO $ do
					printLog $ "searchFunCallsM: Strange funname_expr: " ++ msg
					appendFile (outputDir </> "strangecalls.txt") msg
				return []

searchFunCalls _ _ _ cexpr = return cexpr




