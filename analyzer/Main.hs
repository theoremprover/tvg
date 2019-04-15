{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad.Trans.State.Strict
import System.Environment
import Language.C
import Language.C.Data.Ident
import Language.C.Analysis.AstAnalysis
import Language.C.Analysis.TravMonad
import Language.C.Analysis.SemRep
import Language.C.System.GCC
import System.FilePath
import Text.Printf
import qualified Data.Map.Strict as Map

import DataTree

{--
stack build :analyzer-exe
stack exec analyzer-exe -- test.c
stack build :analyzer-exe && stack exec analyzer-exe
--}

gccExe = "gcc-4.7"

data CovVecState = CovVecState {
	allDefs :: Map.Map Ident IdentDecl
	}
	
type CovVecM a = StateT CovVecState IO a

main = do
	filename:funname:[] <- return ["test.c","f"] --getArgs

	mb_ast <- parseCFile (newGCC gccExe) Nothing [] filename
	case mb_ast of
		Left err -> error $ show err
		Right translunit@(CTranslUnit extdecls _) -> do
			writeFile (filename++".ast.html") $ genericToHTMLString translunit
			let Right (GlobalDecls globobjs _ _,[]) = runTrav_ $ analyseAST translunit
			res <- evalStateT (genCovVectorsM (builtinIdent funname) []) $ CovVecState globobjs
			print res

data Constraint = Or [Constraint] | And [Constraint] | ConstraintExpr :<= ConstraintExpr
	deriving Show

data ConstraintExpr =
	IntValue Int |
	BinaryOp CBinaryOp ConstraintExpr ConstraintExpr |
	FunCall Ident [ConstraintExpr] |
	Var Ident
	deriving Show

lookupFunM :: Ident -> CovVecM FunDef
lookupFunM ident = do
	map <- gets allDefs
	case Map.lookup ident map of
		Just (FunctionDef fundef) -> return fundef
		_ -> error $ "Function " ++ (show ident) ++ " not found"

genCovVectorsM :: Ident -> [Constraint] -> CovVecM NodeInfo
genCovVectorsM funident constraints = do
	FunDef (VarDecl (VarName ident _) declattrs (FunctionType (FunType ret_type paramdecls False) _)) stmt ni <- lookupFunM funident
	traces <- traceStmtM stmt
	return traces

data TraceStep = Assignment Ident ConstraintExpr
	deriving Show

traceStmtM :: Stmt -> CovVecM [[Constraint]]
traceStmtM (CExpr expr _) = case expr of
	CAssign CAssignOp (CVar ident _) cexpr _ -> Assignment ident (toConstraintExpr cexpr)
	CCall (CVar funname _) args _ ->
	unknown -> error $ "traceStmtM CExpr: " ++ show unknown ++ " not implemented yet"
traceStmtM (CCompound _ cbis _) = 
traceStmtM (CIf cond_expr then_stmt mb_else_stmt _) =
traceStmtM (CReturn (Just ret_expr) _) =
traceStmtM stmt = error $ "traceStmtM: " ++ show stmt ++ " not implemented yet"

toConstraintExpr (CBinary binop op1 op2 _) = BinaryOp binop (toConstraintExpr op1) (toConstraintExpr op2)
toConstraintExpr (CCall fun args _) = FunCall 