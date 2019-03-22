{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad
import System.Environment
import Language.C
import Language.C.Data.Ident
import Language.C.System.GCC
import System.FilePath
import Text.Printf

import DataTree

{--
stack build :analyzer-exe
stack exec analyzer-exe -- test.c
stack build :analyzer-exe && stack exec analyzer-exe
--}

gccExe = "gcc-4.7"

main = do
	filename:functionname:[] <- return ["test.c","f"] --getArgs

	mb_ast <- parseCFile (newGCC gccExe) Nothing [] filename
	case mb_ast of
		Left err -> error $ show err
		Right translunit@(CTranslUnit extdecls _) -> do
			writeFile (filename++".ast.html") $ genericToHTMLString translunit
			forM_ extdecls $ \case
				CFDefExt (fundef@(CFunDef _ (CDeclr (Just (Ident name _ _)) _ _ _ _) _ _ _)) | name==functionname -> do
					genTestVectors fundef
				_ -> return ()

data XXX = GT

genTestVectors (CFunDef _ _ args body nodeinfo) = do
	putStrLn "NOT IMPLEMENTED"
