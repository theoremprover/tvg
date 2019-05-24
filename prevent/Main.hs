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

import Control.Lens
import CLenses

import DataTree

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
--			let rets search ctranslunit
--			forM_ rets print
--			return rets

--search :: CTranslationUnit NodeInfo -> [String]
--search ctranslunit = map (render.pretty) $ toListOf $ ctranslunit
