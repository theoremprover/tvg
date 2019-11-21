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

main = do
	args <- getArgs
	maini args

maini [] = do
	putStrLn "USAGE (in tvg dir): stack exec :calltree-exe -- <GCC-EXE> {<SRCFILEDIR>|<OPT>}*"

maini (gccexe:args) = do
	let (preprocess_args,other_args) = partition (\ arg -> any (`isPrefixOf` arg) ["-I","-D"]) args
	pot_filess <- forM other_args getsrcfiles
	mapM_ print (concat pot_filess)
	callss <- forM (concat pot_filess) (handleSrcFile gccexe preprocess_args)
--	forM_ (concat callss) print

	let (callgraph,vertex2node,key2vertex) = graphFromEdges $ map (\(funname,mb_defsrcfile,calledfunnames) -> ((funname,mb_defsrcfile),funname,calledfunnames)) (concat callss)

	writeFile "graph.dot" $ unlines $ nub $ map (drawedge vertex2node) (edges callgraph)

	removePathForcibly "calltree_out"
	createDirectory "calltree_out"
	calledfunctions_hull <- forM functions $ \ rootfunname -> do
		case key2vertex rootfunname of
			Nothing -> do
				putStrLn $ "??? Function " ++ rootfunname ++ " not found in source files!"
				writeFile ("calltree_out" </> (rootfunname ++ "_NOT_FOUND")) ""
				return []
			Just rootvertex -> do
				forM (reachable callgraph rootvertex) $ \ vertex -> do
					let
						calledfuns = map (\ v -> let (node,key,_) = vertex2node v in (node,key)) $ reachable callgraph vertex
						((sourcefun,mb_sourcefile),_,_) = vertex2node vertex
					createDirectoryIfMissing True ("calltree_out" </> rootfunname)
					writeFile ("calltree_out" </> rootfunname </> sourcefun <.> "csv") $ printCSV $ ["FUNCTIONS CALLED DIRECTLY OR INDIRECTLY","FUNCTION DEFINED IN","DIRECTLY CALLED BY"] :
						[ [f, maybe "<NOWHERE>" id mb_srcfile, concat $ intersperse ","
							[ caller | (callerv,calledv) <- edges callgraph, Just calledv == key2vertex fk, let ((caller,_),_,_) = vertex2node callerv ] ] | ((f,mb_srcfile),fk) <- calledfuns ]
					return (sourcefun,map (fst.fst) calledfuns \\ [sourcefun])

	-- JSON output shall also contain indirectly called functions
	let functionhull = nub $ concat calledfunctions_hull
	json_liness :: [[String]] <- forM functionhull $ \ (funname,calledfuns) -> return ( map ("    "++) $ [ show funname ++ ": {", "  \"calltree\": [" ] ++ map ("    "++) (interspersef (++",") id (map show calledfuns)) ++ [ "  ]", "}" ] )
	writeFile ("calltree_out" </> "calltree.json") $ unlines $ [
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

functions = [ "f" ]
	--"dl_main","dl_start","dl_start_final" ]

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
searchFunDefs ctranslunit cfundef@(CFunDef _ (CDeclr (Just (Ident funname _ _)) (CFunDeclr (Right (argdecls,_)) _ _ : _) _ _ _) _ stmt ni) = do
	calledfunnames <- execStateT (everywhereM (mkM $ searchFunCalls funname argdecls ctranslunit) stmt) []
--	liftIO $ print calledfunnames
	modify ((funname,fileOfNode ni,nub calledfunnames) : )
	return cfundef

-- Returns all directly called functions
searchFunCalls :: (MonadIO m) => String -> [CDecl] -> CTranslUnit -> CExpr -> StateT [String] m CExpr
searchFunCalls currentfunname argdecls ctranslunit ccall@(CCall funname_expr callargs ni) = case funname_expr of
	CVar (Ident name _ _) _ -> do
		modify (name : )
		return ccall
	CUnary CIndOp (CVar (Ident name _ _) _) _ -> do
		liftIO $ do
			putStrLn $ "ccall = " ++ (render.pretty) ccall
			putStrLn $ "currentfunname = " ++ currentfunname
			putStrLn $ "Ident name = " ++ name
			putStrLn $ "argdecls = "
			forM_ argdecls print
		let
			[(argnum,_)] = filter (\ (_,CDecl _ [(Just (CDeclr (Just (Ident argname _ _)) _ _ _ _),_,_)] _) -> argname==name) (zip [0..] argdecls)
			calls :: [CExpr]
			calls = everything (++) (mkQ [] searchfundefs) ctranslunit
			searchfundefs :: CFunDef -> [CExpr]
			searchfundefs (CFunDef _ (CDeclr (Just (Ident funname _ _)) _ _ _ _) _ stmt _) = everything (++) (mkQ [] searchfuncalls) stmt
			searchfundefs _ = []
			searchfuncalls :: CExpr -> [CExpr]
			searchfuncalls (CCall (CVar (Ident calledfunname _ _) _) args _) | calledfunname==currentfunname = [ args!!argnum ]
			searchfuncalls _ = []
			names = map (\ (CVar (Ident fn _ _) _) -> fn) calls
		modify (names++)
		liftIO $ putStrLn $ "Found funptr calls: " ++ show ni ++ " " ++ show names
		return ccall
	fne -> do
		let msg = show (posOfNode ni) ++ ": " ++ (render $ pretty ccall) ++ "\n"
		liftIO $ do
			putStr $ "searchFunCallsM: Strange funname_expr: " ++ msg
			appendFile ("calltree_out" </> "strangecalls.txt") msg 
		return ccall

searchFunCalls _ _ _ cexpr = return cexpr

