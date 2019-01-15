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
					analyzeFunction fundef
				_ -> return ()

analyzeFunction (CFunDef _ _ args body nodeinfo) = analyzeStmt body

analyzeStmt stmt = case stmt of
	(CCompound _ blockitems _) -> analyzeBlockItems (reverse blockitems)
	(CReturn mb_expr _) -> putStrLn $ printf "return %s" (show mb_expr)

analyzeBlockItems [] = return ()
analyzeBlockItems (blockitem:rest) = do
	case blockitem of
		CBlockDecl (CDecl [CTypeSpec (CIntType _)] [(
			Just (CDeclr (Just (Ident name _ _)) [] Nothing [] _),
			Just (CInitExpr expr _),
			Nothing )] _) -> do
				putStrLn $ printf "int %s = %s" name (show expr)
		CBlockStmt stmt -> do
			analyzeStmt stmt
	analyzeBlockItems rest
