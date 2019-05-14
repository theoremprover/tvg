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
import Language.C.Analysis.AstAnalysis
import Language.C.Analysis.TravMonad
import Language.C.Analysis.SemRep
import Data.Generics
import Control.Monad.Trans.State.Strict

import System.FilePath
import Text.Printf
import qualified Data.Map.Strict as Map
import Text.PrettyPrint

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
			rets <- execStateT (everywhereM (mkM search) ctranslunit) []
			forM_ rets print
			return rets

search :: CDeclaration NodeInfo -> StateT [NodeInfo] IO (CDeclaration NodeInfo)
search cdecl@(CDecl declspecs triples ni) = do
	forM_ triples $ \ (Just (CDeclr (Just ident) deriveddecls _ attrs ni),_,_) -> do
		forM_ deriveddecls $ \ (CArrDeclr _ (CArrSize isstatic arrsizeexpr) _) -> do
			case arrsizeexpr of
				CConst _ -> return ()
				_ -> modify (ni:)
	return cdecl
		

