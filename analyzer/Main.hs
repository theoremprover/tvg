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
			let fundefs = concatMap ( \case 
				CFDefExt fundef@(CFunDef _ (CDeclr (Just (Ident name _ _)) _ _ _ _) _ _ _) -> [(name,fundef)]
				_ -> [] ) extdecls
			print $ genCovVectors fundefs functionname

--data XXX = 

genCovVectors fundefs funname = case lookup funname fundefs of
	Nothing     -> error $ "Function " ++ funname ++ " not (uniquely) found"
	Just fundef -> funname
