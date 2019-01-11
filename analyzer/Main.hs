{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad
import System.Environment
import Language.C
import Language.C.Data.Ident
import Language.C.System.GCC
import System.FilePath

import ShowAST

{--
stack build :analyzer-exe
stack exec analyzer-exe -- test.c
--}

gccExe = "gcc-4.7"

main = do
	filename:functionname:[] <- getArgs

	mb_ast <- parseCFile (newGCC gccExe) Nothing [] filename
	case mb_ast of
		Left err -> error $ show err
		Right (CTranslUnit extdecls _) -> do
			forM_ extdecls $ \case
				CFDefExt (fundef@(CFunDef _ (CDeclr (Just (Ident name _ _)) _ _ _ _) _ _ _)) | name==functionname -> do
					writeFile (functionname++".ast") $ showDataTree fundef
				_ -> return ()
