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

import DataTree
import CLenses

{--
stack build :analyzer-exe
stack exec analyzer-exe -- test.c
stack build :prevent-exe && stack exec prevent-exe -- prevent/test.c
--}

gccExe = "/usr/bin/gcc"

main = do
	args <- getArgs
	maini args

maini args = do
	let preprocess_args = filter (\ arg -> any (`isPrefixOf` arg) ["-I","-D"]) args
	forM (filter (".c" `isSuffixOf`) args) (handleSrcFile preprocess_args)

handleSrcFile preprocess_args srcfilename = do
	mb_ast <- parseCFile (newGCC gccExe) Nothing preprocess_args srcfilename
	case mb_ast of
		Left err -> error $ show err
		Right ctranslunit -> do
			writeFile (srcfilename++".ast.html") $ genericToHTMLString ctranslunit
			rets <- execStateT (everywhereM (mkM (search srcfilename)) ctranslunit) []
			forM_ rets print
			return rets

search :: String -> CDeclaration NodeInfo -> StateT [(NodeInfo,String)] IO (CDeclaration NodeInfo)
search srcfilename cdecl@(CDecl declspecs triples ni) = do
	forM_ triples $ \case
		(Just (CDeclr (Just ident) deriveddecls _ attrs _),_,_) -> do
			forM_ deriveddecls $ \case
				(CArrDeclr _ (CArrSize _ arrsizeexpr) _) -> do
					case arrsizeexpr of
						CConst _ -> return ()
						_ | fileOfNode ni == Just srcfilename -> do
							modify ((ni,(render.pretty) cdecl):)
						_ -> return ()
				_ -> return ()
		_ -> return ()
	return cdecl
	where
	isconst (CTypeQual (CConstQual _)) = True
	isconst _ = False
