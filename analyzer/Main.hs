{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE LambdaCase,TypeSynonymInstances,FlexibleInstances #-}

module Main where

import Control.Monad.Trans.State.Strict
import Control.Monad
import Control.Monad.Trans.Class (lift)
import System.Environment
import Control.Applicative hiding (empty)
import Control.Monad.IO.Class (liftIO)
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
import Data.Generics

import DataTree

{--
stack build :analyzer-exe
stack exec analyzer-exe -- test.c
stack build :analyzer-exe && stack exec analyzer-exe
--}

gcc = newGCC "gcc"

data CovVecState = CovVecState {
	allDefs :: Map.Map Ident IdentDecl
	}

type CovVecM = StateT CovVecState IO

main = do
	args <- getArgs
	let (filename,funname) = case args of
		[] -> ("test.c","f")
		filename:funname:[] -> (filename,funname)

	mb_ast <- parseCFile gcc Nothing [] filename
	case mb_ast of
		Left err -> error $ show err
		Right translunit@(CTranslUnit extdecls _) -> do
			writeFile (filename++".ast.html") $ genericToHTMLString translunit
			let Right (GlobalDecls globobjs _ _,[]) = runTrav_ $ analyseAST translunit
			res <- evalStateT (funCovVectorsM (builtinIdent funname)) $ CovVecState globobjs
			lss <- forM res $ \ (trace,constraints) -> do
				return $ [
					"TRACE:" ] ++
					map (render.pretty) trace ++
					[ "CONSTRAINTS:",
					show $ map (render.pretty) constraints,
					"------" ]
			writeFile (filename <.> "traces") (unlines $ concat lss)

type Constraint = CExpr

lookupFunM :: Ident -> CovVecM FunDef
lookupFunM ident = do
	map <- gets allDefs
	case Map.lookup ident map of
		Just (FunctionDef fundef) -> return fundef
		_ -> error $ "Function " ++ (show ident) ++ " not found"

type AnalysisResult = [(Trace,[Constraint])]

funCovVectorsM :: Ident -> CovVecM AnalysisResult
funCovVectorsM funident = do
	FunDef (VarDecl _ _ _) stmt _ <- lookupFunM funident
	tracesStmtM [] [stmt] >>= mapM (aggregateConstraintsM [])

data TraceElem = TraceAssign Ident CAssignOp CExpr | TraceDecision CExpr | TraceReturn (Maybe CExpr)
	deriving Show
instance Pretty TraceElem where
	pretty (TraceAssign ident op expr) = pretty ident <+> text ":=" <+> pretty expr
	pretty (TraceDecision expr) = pretty expr
	pretty (TraceReturn mb_expr) = text "return" <+> maybe Text.PrettyPrint.empty pretty mb_expr

type Trace = [TraceElem]

{-
infixl 6 >>> 
(>>>) :: CovVecM [Trace] -> CovVecM [Trace] -> CovVecM Traces
earlier_m >>> later_m = do
	earlier <- earlier_m
	later <- later_m
	return $ [ l++e | e <- earlier, l <- later ]
-}
infixl 5 |||
(|||) :: CovVecM [a] -> CovVecM [a] -> CovVecM [a]
alternative1_m ||| alternative2_m = do
	alternative1 <- alternative1_m
	alternative2 <- alternative2_m
	return $ alternative1 ++ alternative2

tracesStmtM :: Trace -> [CStat] -> CovVecM [[TraceElem]]

tracesStmtM traceelems (stmt:rest) | containsfuncalls = do
	(stmt',calls_stmts) <- runStateT (everywhereM (mkM searchfuncalls) stmt) []
	liftIO $ forM_ calls_stmts (putStrLn.render.pretty)
	tracesStmtM traceelems (calls_stmts ++ (stmt':rest))

	where

	containsfuncalls = everything (||) (mkQ False isfuncall) stmt where
		isfuncall :: CExpr -> Bool
		isfuncall (CCall _ _ _) = True
		isfuncall _ = False
	
	searchfuncalls :: CExpr -> StateT [CStat] CovVecM CExpr
	searchfuncalls (CCall (CVar funident _) args call_ni) = do
		FunDef (VarDecl _ _ (FunctionType (FunType _ paramdecls False) _)) body _ <- lift $ lookupFunM funident
		stmts <- forM (zip paramdecls args) $ \ (ParamDecl (VarDecl (VarName ident_decl Nothing) _ _) _,arg) -> do
			return $ CExpr (Just $ CAssign CAssignOp (CVar ident_decl undefNode) arg undefNode) undefNode
		let
			new_ident = internalIdent (identToString funident ++ "_ret_" ++ show (posOffset (posOfNode call_ni)))
			ret_var = CVar new_ident undefNode
			body' = (mkT replace_return) body
			replace_return :: CStat -> CStat
			replace_return (CReturn Nothing _) = CCompound [] [] undefNode
			replace_return (CReturn (Just ret_expr) _) = CExpr (Just $ CAssign CAssignOp ret_var ret_expr undefNode) undefNode
			replace_return stmt = stmt
		modify (++(stmts++[body']))
		return ret_var
	searchfuncalls expr = return expr

tracesStmtM traceelems ((stmt@(CExpr (Just (CAssign assign_op (CVar ident _) assigned_expr _)) _)) : rest) =
	tracesStmtM (TraceAssign ident assign_op assigned_expr : traceelems) rest

tracesStmtM traceelems (CCompound _ cbis _ : rest) = tracesStmtM traceelems (concatMap to_stmt cbis ++ rest)
	where
	to_stmt (CBlockStmt cstmt) = [cstmt]
	to_stmt (CBlockDecl (CDecl _ triples _)) = concatMap triple_to_stmt triples
	triple_to_stmt (_,Nothing,Nothing) = []
	triple_to_stmt (Just (CDeclr (Just ident) _ _ _ _),mb_initexpr,Nothing) = do
		case mb_initexpr of
			Nothing -> []
			Just (CInitExpr init_expr _) -> [ CExpr (Just $ CAssign CAssignOp (CVar ident undefNode) init_expr undefNode) undefNode ]
	triple_to_stmt err = error $ "triple_to_stmt: " ++ show err ++ " not implemented yet"

tracesStmtM traceelems (CIf cond_expr then_stmt mb_else_stmt _ : rest) = then_m ||| else_m
	where
	then_m = tracesStmtM (TraceDecision cond_expr : traceelems) (then_stmt:rest)
	else_m = tracesStmtM (TraceDecision (CUnary CNegOp cond_expr undefNode) : traceelems) $ case mb_else_stmt of
		Just else_stmt -> else_stmt:rest
		Nothing -> rest

tracesStmtM traceelems (CReturn mb_ret_expr _ : _) = return [ TraceReturn mb_ret_expr : traceelems ]
tracesStmtM traceelems [] = return [ TraceReturn Nothing : traceelems ]
tracesStmtM _ (stmt:_) = error $ "traceStmtM: " ++ show stmt ++ " not implemented yet"

-- Takes an (already computed) list of Constraints and contracts it to one without definitions,
-- so that a solver could solve it
aggregateConstraintsM :: [Constraint] -> Trace -> CovVecM (Trace,[Constraint])
aggregateConstraintsM initial_constraints trace = return (trace,aggregateconstraints initial_constraints trace)
	where
	aggregateconstraints :: [Constraint] -> Trace -> [Constraint]
	aggregateconstraints constraints (TraceReturn _ : traceelems) = aggregateconstraints constraints traceelems
	aggregateconstraints constraints (TraceDecision cond_expr : traceelems) = aggregateconstraints (cond_expr:constraints) traceelems
	aggregateconstraints constraints (TraceAssign ident CAssignOp assigned_expr : traceelems) =
		aggregateconstraints (map (substituteVarInExpr ident assigned_expr) constraints) traceelems
	aggregateconstraints constraints [] = constraints

substituteVarInExpr ident subexpr cconst@(CConst _) = cconst
substituteVarInExpr ident subexpr (CUnary unaryop expr ni) = CUnary unaryop (substituteVarInExpr ident subexpr expr) ni
substituteVarInExpr ident subexpr (CBinary binop expr1 expr2 ni) = CBinary binop (substituteVarInExpr ident subexpr expr1) (substituteVarInExpr ident subexpr expr2) ni
substituteVarInExpr ident subexpr (CCall fun args ni) = CCall (substituteVarInExpr ident subexpr fun) (map (substituteVarInExpr ident subexpr) args) ni
substituteVarInExpr ident subexpr (CVar vident ni) | ident==vident = subexpr
substituteVarInExpr _ _ cvar@(CVar _ _) = cvar
substituteVarInExpr _ _ expr = error $ "substituteVarInExpr for " ++  show expr ++ " not implemented"
