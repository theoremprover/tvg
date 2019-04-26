{-# LANGUAGE RecordWildCards,LambdaCase,OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

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

gccExe = "/usr/bin/gcc"

main = do
	args <- getArgs
	maini args

maini args = do
	let preprocess_args = filter (\ arg -> any (`isPrefixOf` arg) ["-I","-D"]) args
	forM (filter (".c" `isSuffixOf`) args) (handleSrcFile preprocess_args)

handleSrcFile preprocess_args name = do
	mb_ast <- parseCFile (newGCC gccExe) Nothing preprocess_args name
	case mb_ast of
		Left err -> error $ show err
		Right ctranslunit -> do
			calls <- execStateT (everywhereM (mkM (searchFunDefs name)) ctranslunit) []
			forM_ calls print
			return calls

data CallsInFun = CallsInFun String String [String] deriving Show

searchFunDefs :: String -> CFunDef -> StateT [CallsInFun] IO CFunDef
searchFunDefs sourcefilename cfundef@(CFunDef _ (CDeclr (Just (Ident funname _ _)) _ _ _ _) _ stmt ni) = do
	liftIO $ print funname
	calledfunnames <- execStateT (everywhereM (mkM searchFunCalls) stmt) []
	modify (CallsInFun sourcefilename funname (nub calledfunnames) : )
	return cfundef

searchFunCalls :: (MonadIO m) => CExpr -> StateT [String] m CExpr
searchFunCalls ccall@(CCall funname_expr _ ni) = case funname_expr of
	CVar (Ident name _ _) _ -> do
		modify (name : )
		return ccall
	fne -> error $ "searchFunCallsM: Strange funname_expr " ++ show fne
searchFunCalls cexpr = return cexpr
