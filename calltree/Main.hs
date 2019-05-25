{-# LANGUAGE RecordWildCards,LambdaCase,OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

{-
stack build :calltree-exe && stack exec calltree-exe -- "main" calltreetest.c calltreetest2.c calltree.h
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
import Data.Generics
import Control.Monad.Trans.State.Strict
import Data.Graph
import System.FilePath
import Data.Maybe
import System.IO

main = do
	args <- getArgs
	maini args

maini [] = putStrLn "USAGE (in tvg dir): stack exec :calltree-exe -- <GCC-EXE> <ROOT_FUN_NAME> {<SRCFILE>|<OPT>}*"

maini (gccexe:args) = do
	let (preprocess_args,other_args) = partition (\ arg -> any (`isPrefixOf` arg) ["-I","-D"]) args
	pot_filess <- forM other_args getsrcfiles
--	print pot_filess
	callss <- forM (concat pot_filess) (handleSrcFile gccexe preprocess_args)
--	forM_ (concat callss) print
	let (callgraph,vertex2node,key2vertex) = graphFromEdges $ map (\(funname,mb_defsrcfile,calledfunnames) -> ((funname,mb_defsrcfile),funname,calledfunnames)) (concat callss)

	writeFile "graph.dot" $ unlines $ nub $ map (drawedge vertex2node) (edges callgraph)

	forM_ functions $ \ rootfunname -> do
		case key2vertex rootfunname of
			Nothing -> do
				putStrLn $ "??? Function " ++ rootfunname ++ " not found in source files!"
				writeFile ("calltree_out" </> (rootfunname ++ "_NOT_FOUND")) ""
			Just rootvertex -> do
				forM_ (reachable callgraph rootvertex) $ \ vertex -> do
					let
						calledfuns = map (\ v -> let (node,key,_) = vertex2node v in (node,key)) $ reachable callgraph vertex
						((sourcefun,mb_sourcefile),_,_) = vertex2node vertex
					createDirectoryIfMissing True ("calltree_out" </> rootfunname)
					writeFile ("calltree_out" </> rootfunname </> sourcefun <.> "csv") $ printCSV $ ["FUNCTIONS CALLED DIRECTLY OR INDIRECTLY","FUNCTION DEFINED IN","DIRECTLY CALLED BY"] :
						[ [f, maybe "<NOWHERE>" id mb_srcfile, concat $ intersperse ","
							[ caller | (callerv,calledv) <- edges callgraph, Just calledv == key2vertex fk, let ((caller,_),_,_) = vertex2node callerv ] ] | ((f,mb_srcfile),fk) <- calledfuns ]

	where
	getsrcfiles :: String -> IO [String]
	getsrcfiles name = do
		isdir <- doesDirectoryExist name
		case isdir of
			False -> case any (`isSuffixOf` name) [".c",".h"] of
				True -> return [name]
				False -> return []
			True -> do
				names <- getDirectoryContents name
				filess <- mapM getsrcfiles names
				return $ concat filess

	drawedge vertex2node (callerv,calledv) = let
		((callername,_),_,_) = vertex2node callerv
		((calledname,_),_,_) = vertex2node calledv in
		callername ++ " -> " ++ calledname ++ ";"

printCSV ls = unlines $ map (concat . (intersperse ";")) ls

-- Returns a list of (funname,srcfile,[calledfunname1,...])
handleSrcFile gccexe preprocess_args srcfilename = do
	putStrLn $ "handling SrcFile " ++ srcfilename
	mb_ast <- parseCFile (newGCC gccexe) Nothing preprocess_args srcfilename
	case mb_ast of
		Left err -> error $ show err
		Right ctranslunit -> execStateT (everywhereM (mkM searchFunDefs) ctranslunit) []
			
-- For all function definitons in the current source file:
searchFunDefs :: CFunDef -> StateT [(String,Maybe FilePath,[String])] IO CFunDef
searchFunDefs cfundef@(CFunDef _ (CDeclr (Just (Ident funname _ _)) _ _ _ _) _ stmt ni) = do
	calledfunnames <- execStateT (everywhereM (mkM searchFunCalls) stmt) []
	modify ((funname,fileOfNode ni,nub calledfunnames) : )
	return cfundef

-- Returns all directly called functions
searchFunCalls :: (MonadIO m) => CExpr -> StateT [String] m CExpr
searchFunCalls ccall@(CCall funname_expr _ ni) = case funname_expr of
	CVar (Ident name _ _) _ -> do
		modify (name : )
		return ccall
	fne -> do
		liftIO $ putStrLn $ "searchFunCallsM: Strange funname_expr " ++ show fne
		return ccall

searchFunCalls cexpr = return cexpr

removeellipse filename = do
	f <- readFile filename
	let ls = lines f
	print $ length ls
	writeFile filename $ unlines $ filter (\ l -> not ("static void __tg_promote(...)" `isInfixOf` l) && not (" __attribute__((__overloadable__, __unavailable__));" `isInfixOf` l)) ls

functions = [ "abs","acos","acosf","acosh","acoshf","asin","asinf","asinh","asinhf","atan","atan2","atan2f",
	"atanf","atanh","atanhf","cbrt","cbrtf","ceil","ceilf","cos","cosf","cosh","coshf","exp","expm1f","exp2",
	"exp2f","expm1","expf","fabs","fabsf","fdim","fdimf","floor","floorf","fmaf","fmax","fmaxf","fmin","fminf",
	"fmod","fmodf","hypot","hypotf","ilogb","ilogbf","imaxabs","imaxdiv","ldexp","ldexpf","ldiv","llabs","lldiv",
	"log","log10","log10f","log1p","log1pf","log2","log2f","logb","logbf","logf","lrint","lrintf","lround",
	"lroundf","modf","modff","nextafter","nextafterf","pow","powf","remainder","remainderf","rint","rintf",
	"round","roundf","scalbln","scalblnf","scalbn","scalbnf","sin","sinf","sinh","sinhf","sqrt","sqrtf",
	"srand","tan","tanf","tanh","tanhf","trunc","truncf" ]