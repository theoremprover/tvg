{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE LambdaCase,TypeSynonymInstances,FlexibleInstances #-}

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
			forM_ res $ \ (cs,l) -> do
				print $ map (render.pretty) cs
				print $ map (render.pretty) l
				print "------"

type Constraint = CExpr
{-
data Constraint = Or [Constraint] | And [Constraint] | Expr :<= Expr | Ident := Expr
	deriving Show
-}

lookupFunM :: Ident -> CovVecM FunDef
lookupFunM ident = do
	map <- gets allDefs
	case Map.lookup ident map of
		Just (FunctionDef fundef) -> return fundef
		_ -> error $ "Function " ++ (show ident) ++ " not found"

type AnalysisResult = [(Trace,[Constraint])]

-- Returns the body of a function definition
getFunStmtsM :: Ident -> CovVecM [Stmt]
getFunStmtsM funident = do
	FunDef (VarDecl (VarName ident _) declattrs (FunctionType (FunType ret_type paramdecls False) _)) stmt ni <- lookupFunM funident
	return [stmt]

genCovVectorsM :: Ident -> CovVecM AnalysisResult
genCovVectorsM funident = getFunStmtsM funident >>= tracesStmtM []

data TraceElem = TraceAssign Ident CAssignOp CExpr | TraceDecision CExpr | TraceReturn (Maybe CExpr)
	deriving Show
instance Pretty TraceElem where
	pretty (TraceAssign ident op expr) = pretty ident <+> text ":=" <+> pretty expr
	pretty (TraceDecision expr) = pretty expr
	pretty (TraceReturn mb_expr) = text "return" <+> maybe empty pretty mb_expr

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

tracesStmtM :: Trace -> [Stmt] -> CovVecM AnalysisResult

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

tracesStmtM traceelems (CReturn mb_ret_expr _ : _) = do
	let traceelems' = TraceReturn mb_ret_expr : traceelems
	constraints <- aggregateCovM [] traceelems'
	return [(traceelems',constraints)]
tracesStmtM traceelems [] = do
	let traceelems'= TraceReturn Nothing : traceelems
	constraints <- aggregateCovM [] traceelems'
	return [(traceelems',constraints)]

tracesStmtM _ (stmt:_) = error $ "traceStmtM: " ++ show stmt ++ " not implemented yet"

aggregateCovM :: [Constraint] -> Trace -> CovVecM [Constraint]
aggregateCovM constraints (TraceReturn _ : traceelems) = aggregateCovM constraints traceelems
aggregateCovM constraints (TraceDecision cond_expr : traceelems) = aggregateCovM (cond_expr:constraints) traceelems
aggregateCovM constraints (TraceAssign ident CAssignOp assigned_expr : traceelems) =
	aggregateCovM (map (substituteInExpr ident assigned_expr) constraints) traceelems
aggregateCovM constraints [] = return $ map (searchFunCall id) constraints

substituteInExpr ident subexpr (CConst c) = CConst c
substituteInExpr ident subexpr (CUnary unaryop expr ni) = CUnary unaryop (substituteInExpr ident subexpr expr) ni
substituteInExpr ident subexpr (CBinary binop expr1 expr2 ni) = CBinary binop (substituteInExpr ident subexpr expr1) (substituteInExpr ident subexpr expr2) ni
substituteInExpr ident subexpr (CCall fun args ni) = CCall (substituteInExpr ident subexpr fun) (map (substituteInExpr ident subexpr) args) ni
substituteInExpr ident subexpr (CVar vident ni) | ident==vident = subexpr
substituteInExpr _ _ (CVar vident ni) = CVar vident ni

-- !(2 * g(x) > 5)
-- 
{-
searchFunCall _ (CConst _) = []
searchFunCall _ (CVar _ _) = []
searchFunCall f (CCall fun args ni) = []
searchFunCall f (CUnary CNegOp expr ni) = searchFunCall ((\ x -> CUnary CNegOp x undefNode) . f) expr
searchFunCall f (CBinary CMulOp expr1 expr2 ni) =
	searchFunCall (f . (\ x -> CBinary CDivOp x expr2)) expr1 ++
	searchFunCall (f . (\ x -> CBinary CDivOp x expr1)) expr2
-}

{-
tracesExprM :: [TraceElem] -> Expr -> CovVecM [[TraceElem]]

tracesExprM traceelems cassign@(CAssign assign_op (CVar ident _) assigned_expr _) = do
	tracesExprM traceelems assigned_expr >>> return [[ TraceStmt $ CExpr (Just cassign) undefNode ]]

tracesExprM traceelems (CCall (CVar funident _) args _ ) = do
	getFunStmtsM funident >>= tracesStmtM traceelems
tracesExprM traceelems (CBinary binop expr1 expr2 _) =
	tracesExprM traceelems expr1 >>> tracesExprM [] expr2
tracesExprM traceelems (CVar _ _) = return [traceelems]
tracesExprM traceelems (CConst _) = return [traceelems]
tracesExprM _ unknown =error $ "tracesExprM: " ++ show unknown ++ " not implemented yet"
-}