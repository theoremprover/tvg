{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad.Trans.State.Strict
import Control.Monad
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
import Text.PrettyPrint
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
			res <- evalStateT (genCovVectorsM (builtinIdent funname)) $ CovVecState globobjs
			forM_ res $ \ trace -> do
				print (map (render.pretty) trace)

data Constraint = Or [Constraint] | And [Constraint] | Expr :<= Expr
	deriving Show

lookupFunM :: Ident -> CovVecM FunDef
lookupFunM ident = do
	map <- gets allDefs
	case Map.lookup ident map of
		Just (FunctionDef fundef) -> return fundef
		_ -> error $ "Function " ++ (show ident) ++ " not found"

genCovVectorsM :: Ident -> CovVecM [[Stmt]]
genCovVectorsM funident = getFunStmtsM funident >>= tracesStmtM []

getFunStmtsM :: Ident -> CovVecM [Stmt]
getFunStmtsM funident = do
	FunDef (VarDecl (VarName ident _) declattrs (FunctionType (FunType ret_type paramdecls False) _)) stmt ni <- lookupFunM funident
	return [stmt]

type Trace = [Stmt]
type Traces = [Trace]

infixl 6 >>> 
(>>>) :: CovVecM Traces -> CovVecM Traces -> CovVecM Traces
earlier_m >>> later_m = do
	earlier <- earlier_m
	later <- later_m
	return $ [ l ++ e | e <- earlier, l <- later ]

infixl 5 |||
(|||) :: CovVecM Traces -> CovVecM Traces -> CovVecM Traces
alternative1_m ||| alternative2_m = do
	alternative1 <- alternative1_m
	alternative2 <- alternative2_m
	return $ alternative1 ++ alternative2

emptyTraces = [[]]

tracesStmtM :: Trace -> Trace -> CovVecM Traces

tracesStmtM tracepath ((stmt@(CExpr (Just expr) _)) : rest) = do
	tracesExprM tracepath expr >>> tracesStmtM [] rest

tracesStmtM tracepath (CCompound _ cbis _ : rest) = tracesStmtM tracepath (concatMap to_stmt cbis ++ rest)
	where
	to_stmt (CBlockStmt cstmt) = [cstmt]
	to_stmt (CBlockDecl (CDecl _ triples _)) = concatMap triple_to_stmt triples
	triple_to_stmt (_,Nothing,Nothing) = []
	triple_to_stmt (Just (CDeclr (Just ident) _ _ _ _),Just (CInitExpr init_expr _),Nothing) =
		[ CExpr (Just $ CAssign CAssignOp (CVar ident undefNode) init_expr undefNode) undefNode ]
	triple_to_stmt err = error $ "triple_to_stmt: " ++ show err ++ " not implemented yet"

tracesStmtM tracepath (CIf cond_expr then_stmt mb_else_stmt _ : rest) = then_m  |||  else_m
	where
	cond_stmt = CExpr (Just cond_expr) undefNode
	then_m = tracesStmtM (cond_stmt : tracepath) (then_stmt:rest)
	else_m = case mb_else_stmt of
		Just else_stmt -> tracesStmtM (cond_stmt : tracepath) (else_stmt:rest)
		Nothing -> return emptyTraces

tracesStmtM tracepath (cret@(CReturn (Just ret_expr) _) : _) =
	tracesExprM tracepath ret_expr >>> return [[cret]]

tracesStmtM tracepath [] = return [tracepath]

tracesStmtM _ (stmt:_) = error $ "traceStmtM: " ++ show stmt ++ " not implemented yet"


tracesExprM :: [Stmt] -> Expr -> CovVecM [[Stmt]]

tracesExprM tracepath cassign@(CAssign assign_op (CVar ident _) assigned_expr _) = do
	tracesExprM tracepath assigned_expr >>> return [[ CExpr (Just cassign) undefNode ]]

tracesExprM tracepath (CCall (CVar funident _) args _ ) = do
	getFunStmtsM funident >>= tracesStmtM tracepath
tracesExprM tracepath (CBinary binop expr1 expr2 _) =
	tracesExprM tracepath expr1 >>> tracesExprM [] expr2
tracesExprM tracepath (CVar _ _) = return [tracepath]
tracesExprM tracepath (CConst _) = return [tracepath]
tracesExprM _ unknown =error $ "tracesExprM: " ++ show unknown ++ " not implemented yet"