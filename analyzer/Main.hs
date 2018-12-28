{-# OPTIONS_GHC -fno-warn-tabs #-}

module Main where

import Prelude hiding (readFile)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import System.IO.Strict

import System.Environment
import Language.C
import Language.C.System.GCC
import Text.PrettyPrint
import System.FilePath
import ShowAST

{--
stack build :analyzer-exe
stack exec analyzer-exe -- test.c
--}

gccExe = "gcc-4.7"

main = do
	filename:[] <- getArgs

	mb_ast <- parseCFile (newGCC gccExe) Nothing [] filename
	case mb_ast of
		Left err -> error $ show err
		Right ast -> do
			writeFile (replaceExtension filename "ast") $ showDataTree ast
