{-# LANGUAGE RecordWildCards,LambdaCase,OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

{-
stack build :calltree-exe && stack exec calltree-exe -- calltreetest.c calltreetest2.c calltree.h
-}

module Main where

import System.Environment
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

gccExe = "gcc"

main = do
	args <- getArgs
	maini args

maini (rootfunname:args) = do
	let preprocess_args = filter (\ arg -> any (`isPrefixOf` arg) ["-I","-D"]) args
	callss <- forM (filter (".c" `isSuffixOf`) args) (handleSrcFile preprocess_args)
	let (callgraph,vertex2node,key2vertex) = graphFromEdges $ map (\(funname,mb_defsrcfile,calledfunnames) -> ((funname,mb_defsrcfile),funname,calledfunnames)) (concat callss)
	forM_ (reachable callgraph $ fromJust (key2vertex rootfunname)) $ \ vertex -> do
		let
			calledfuns = map (\ v -> let (node,key,_) = vertex2node v in (node,key)) $ reachable callgraph vertex
			((sourcefun,mb_sourcefile),_,_) = vertex2node vertex
		writeFile ("calltree_out" </> sourcefun <.> "csv") $ printCSV $ ["FUNCTIONS CALLED DIRECTLY OR INDIRECTLY","FUNCTION DEFINED IN","DIRECTLY CALLED BY"] :
			[ [f, maybe "<NOWHERE>" id mb_srcfile, concat $ intersperse ","
				[ caller | (callerv,calledv) <- edges callgraph, Just calledv == key2vertex fk, let ((caller,_),_,_) = vertex2node callerv ] ] | ((f,mb_srcfile),fk) <- calledfuns ]

printCSV ls = unlines $ map (concat . (intersperse ";")) ls

-- Returns a list of (funname,srcfile,[calledfunname1,...])
handleSrcFile preprocess_args srcfilename = do
	mb_ast <- parseCFile (newGCC gccExe) Nothing preprocess_args srcfilename
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
	fne -> error $ "searchFunCallsM: Strange funname_expr " ++ show fne
searchFunCalls cexpr = return cexpr
