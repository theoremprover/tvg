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

analyzeFunction (CFunDef _ _ args body nodeinfo) = error "NOT IMPLEMENTED" --analyzeStmt (8,[]) body

{-
analyzeStmt intended_val_env stmt = case stmt of
	(CCompound _ blockitems _) -> analyzeBlockItems intended_val_env (reverse blockitems)
	(CReturn (Just expr) _) -> do
		putStrLn $ printf "return %s" (show expr)
		inverseExpr intended_val_env expr

analyzeBlockItems (_,env) [] = return env
analyzeBlockItems intended_val_env (blockitem:rest) = do
	intended_val_env' <- case blockitem of
		CBlockDecl (CDecl [CTypeSpec (CIntType _)] [(
			Just (CDeclr (Just (Ident name _ _)) [] Nothing [] _),
			Just (CInitExpr expr _),
			Nothing )] _) -> do
				putStrLn $ printf "int %s = %s" name (show expr)
				inverseExpr intended_val_env expr
		CBlockStmt stmt -> do
			analyzeStmt intended_val_env stmt
	analyzeBlockItems intended_val_env' rest

inverseExpr (intended_val,env) = error "x"
-}