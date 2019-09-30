{-# OPTIONS_GHC -fno-warn-tabs -Wno-unrecognised-pragmas #-}
{--# LANGUAGE LambdaCase,TypeSynonymInstances,FlexibleInstances #-}
{-# HLINT ignore "Use list literal pattern" #-}
{-# HLINT ignore "Redundant do" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Redundant $" #-}
{-# HLINT ignore "Eta reduce" #-}

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

-- haskellzinc

{--
stack build :analyzer-exe
stack exec analyzer-exe -- test.c
stack build :analyzer-exe && stack exec analyzer-exe
--}

gcc = newGCC "gcc"

data CovVecState = CovVecState {
	allDefs :: Map.Map Ident IdentDecl,
	newNameIndex :: Int
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
			res <- evalStateT (funCovVectorsM (builtinIdent funname)) $ CovVecState globobjs 1
			lss <- forM res $ \ (trace{-,constraints-}) -> do
				return $
					[ "TRACE:" ] ++
					map (render.pretty) trace ++
					[ "CONSTRAINTS:",
--					show $ map (render.pretty) constraints,
					"","------","" ]
			writeFile (filename <.> "traces") (unlines $ concat lss)

type Constraint = CExpr

lookupFunM :: Ident -> CovVecM FunDef
lookupFunM ident = do
	map <- gets allDefs
	case Map.lookup ident map of
		Just (FunctionDef fundef) -> return fundef
		_ -> error $ "Function " ++ (show ident) ++ " not found"

funCovVectorsM :: Ident -> CovVecM [Trace]
funCovVectorsM funident = do
	FunDef (VarDecl _ _ _) stmt _ <- lookupFunM funident
	tracesStmtM True ExpandCalls [] [] [[stmt]] -- >>= mapM (aggregateConstraintsM []) >>= mapM expandFunCallsM

getNewIdent :: String -> CovVecM Ident
getNewIdent name_prefix = do
	new_var_num <- gets newNameIndex
	modify $ \ s -> s { newNameIndex = newNameIndex s + 1 }
	return $ internalIdent (name_prefix ++ "$" ++ show new_var_num)

data TraceElem = TraceAssign Ident CAssignOp CExpr | TraceCondition CExpr
	deriving Show
instance Pretty TraceElem where
	pretty (TraceAssign ident op expr) = pretty ident <+> text ":=" <+> pretty expr
	pretty (TraceCondition expr) = text "Condition:" <+> pretty expr

type Trace = [TraceElem]
type AnalysisResult = [(Trace,[Constraint])]
type FunList = [Ident]
data StmtStage = ExpandCalls | NoCallsLeft deriving (Eq,Show)

{-
containsNoFunctionCalls :: CExpr -> Bool
containsNoFunctionCalls expr = everything (&&) (mkQ True searchfuncall) expr where
	searchfuncall :: CExpr -> Bool
	searchfuncall (CCall (CVar (Ident funname _ _) _) _ _) = False
	searchfuncall expr = True
-}

tracesStmtM :: Bool -> StmtStage -> [Ident] -> Trace -> [[CStat]] -> CovVecM [Trace]

tracesStmtM True stmtstage funidents traceelems rest = do
	liftIO $ do
		putStrLn "--------------"
		putStrLn $ "tracesStmtM " ++ show stmtstage
		putStrLn $ "    funidents = " ++ show (map (render.pretty) funidents)
		putStrLn $ "    traceelems = " ++ show (map (render.pretty) (take 5 traceelems)) ++ "..."
		putStrLn $ "    next = " ++ case rest of
			((r:_):_) -> "[ [ " ++ (render.pretty) r ++ ".. ], .. ]"
			_ -> show rest
	tracesStmtM False stmtstage funidents traceelems rest

tracesStmtM False _ funidents traceelems ((CCompound _ cbis _ : rest):rx) = tracesStmtM True ExpandCalls funidents traceelems ((concatMap to_stmt cbis ++ rest):rx)
	where
	to_stmt (CBlockStmt cstmt) = [cstmt]
	to_stmt (CBlockDecl (CDecl _ triples _)) = concatMap triple_to_stmt triples
	triple_to_stmt (_,Nothing,Nothing) = []
	triple_to_stmt (Just (CDeclr (Just ident) _ _ _ _),mb_initexpr,Nothing) = do
		case mb_initexpr of
			Nothing -> []
			Just (CInitExpr init_expr _) -> [ CExpr (Just $ CAssign CAssignOp (CVar ident undefNode) init_expr undefNode) undefNode ]
	triple_to_stmt err = error $ "triple_to_stmt: " ++ show err ++ " not implemented yet"

tracesStmtM False NoCallsLeft funidents traceelems ((CIf cond_expr then_stmt mb_else_stmt if_ni : rest):rx)  = do
	cond_ident <- getNewIdent "cond"
	let
		cond_var = CVar cond_ident undefNode
		cond_stmt = CExpr (Just $ CAssign CAssignOp cond_var cond_expr undefNode) undefNode
	then_m <- tracesStmtM True ExpandCalls funidents (TraceCondition cond_var : traceelems) ((cond_stmt : then_stmt : rest):rx)
	else_m <- tracesStmtM True ExpandCalls funidents (TraceCondition (CUnary CNegOp cond_var undefNode) : traceelems) ((case mb_else_stmt of
		Just else_stmt -> cond_stmt : else_stmt : rest
		Nothing -> cond_stmt : rest ) : rx )
	return $ then_m ++ else_m
tracesStmtM False ExpandCalls funidents traceelems ((CIf cond_expr then_stmt mb_else_stmt if_ni : rest):rx) = do
	(cond_expr',(funcall_stmts,called_funidents)) <- expandFunCallsM cond_expr
	tracesStmtM True NoCallsLeft (called_funidents++funidents) traceelems $ (funcall_stmts : (CIf cond_expr' then_stmt mb_else_stmt if_ni : rest) : rx)

tracesStmtM False NoCallsLeft funidents traceelems (((stmt@(CExpr (Just (CAssign assign_op (CVar ident _) assigned_expr _)) _)) : rest) : rx) = do
	tracesStmtM True ExpandCalls funidents (TraceAssign ident assign_op assigned_expr : traceelems) (rest:rx)

tracesStmtM False ExpandCalls funidents traceelems (((CExpr (Just (CAssign assign_op cvar assigned_expr _)) _) : rest ) : rx) = do
	(assigned_expr',(funcall_stmts,called_funidents)) <- expandFunCallsM assigned_expr
	tracesStmtM True NoCallsLeft (called_funidents++funidents) traceelems $
		((funcall_stmts ++ (CExpr (Just (CAssign assign_op cvar assigned_expr' undefNode)) undefNode) : rest) : rx)

tracesStmtM False NoCallsLeft funidents traceelems ((CReturn mb_ret_val _ : _):rx) = do
	tracesStmtM True ExpandCalls funidents traceelems $ case mb_ret_val of
		Nothing -> []:rx
		Just ret_val -> case funidents of
			[] -> []:rx
			(funident:_) -> ( CExpr (Just (CAssign CAssignOp (CVar funident undefNode) ret_val undefNode)) undefNode : head rx ) : tail rx
tracesStmtM False ExpandCalls funidents traceelems ((CReturn mb_ret_val _ : _):rx) = do
	(mb_ret_val',(funcall_stmts,called_funidents)) <- case mb_ret_val of
		Nothing -> return (Nothing,([],[]))
		Just ret_val -> do
			(v,ss) <- expandFunCallsM ret_val
			return (Just v,ss)
	tracesStmtM True NoCallsLeft (called_funidents++funidents) traceelems (funcall_stmts : [CReturn mb_ret_val' undefNode] : rx)

tracesStmtM False st funidents traceelems ([]:rx) | st `elem` [ExpandCalls,NoCallsLeft] = case funidents of
	[] -> case rx of
		[] -> return [traceelems]
		_ -> error "something wrong: funidents=[] and rx!=[]"
	(_:funidents) -> tracesStmtM True ExpandCalls funidents traceelems rx

tracesStmtM False _ [] traceelems [] = return [ traceelems ]
tracesStmtM False _ [] traceelems [[]] = return [ traceelems ]
tracesStmtM False _ funidents _ [] = error $ "funidents not empty when reaching end of stmts!"
tracesStmtM False _ funidents _ [[]] = error $ "funidents not empty when reaching end of stmts!"

tracesStmtM False _ _ _ ((stmt:_):_) = error $ "traceStmtM: " ++ show stmt ++ " not implemented yet"

tracesStmtM tracing stmtstage funidents traceelems rss = do
	liftIO $ do
		putStrLn "--- ERROR ---"
		putStrLn $ "tracesStmtM "
		putStrLn $ "    tracing = " ++ show tracing
		putStrLn $ "    stmtstage = " ++ show stmtstage
		putStrLn $ "    funidents = " ++ show (map (render.pretty) funidents)
		putStrLn $ "    traceelems = " ++ show (map (render.pretty) (take 5 traceelems)) ++ "..."
		putStrLn $ "    next = " ++ show (map (map (render.pretty)) rss)
	error "Non-exhaustive patterns in tracesStmtM"

-- Expands all function calls in expr and returns a statement handling all function calls, and the modified expression
expandFunCallsM :: CExpr -> CovVecM (CExpr,([CStat],[Ident]))
expandFunCallsM expr = runStateT (everywhereM (mkM searchfuncalls) expr) ([],[])
	where
	-- take an expression, expand function calls,
	-- return the substituted expression and a list of statements in the monad's state
	searchfuncalls :: CExpr -> StateT ([CStat],[Ident]) CovVecM CExpr
	searchfuncalls ccall@(CCall (CVar funident@(Ident funname _ _) _) args call_ni) = do
		FunDef (VarDecl _ _ (FunctionType (FunType _ paramdecls False) _)) body _ <- lift $ lookupFunM funident
		fun_val_ident <- lift $ getNewIdent funname
		
		param_assignments <- forM (zip args paramdecls) $ \ (arg,ParamDecl (VarDecl (VarName (formal_param@(Ident n _ _)) _) _ _) _) -> do
			return $ CExpr (Just $ CAssign CAssignOp (CVar formal_param undefNode) arg undefNode) undefNode
		let body_with_arg_assignments = CCompound [] (map CBlockStmt (param_assignments++[body])) undefNode
		
		-- search all locally declared variables (including the formal params, see above)
		let
			decl_idents = everything (++) (mkQ [] searchdecl) body_with_arg_assignments
			searchdecl :: CDeclr -> [Ident]
			searchdecl (CDeclr (Just ident) _ _ _ _) = [ ident ]
			searchdecl _ = []

		-- create new identifiers
		old_new_idents <- forM decl_idents $ \ ident@(Ident n _ _) -> do
			ident' <- lift $ getNewIdent (n ++ "_" ++ funname)
			return (ident,ident')
		
		-- Substitute identifiers in function body
		let
			subst_var b (old,new) = substituteIdentInStmt old new b
			body' = foldl subst_var body_with_arg_assignments old_new_idents
		
		modify $ \ (stats,funidents) -> (stats++[body'],funidents++[fun_val_ident])
		return $ CVar fun_val_ident call_ni
	searchfuncalls expr = return expr

substituteIdentInStmt :: Ident -> Ident -> Stmt -> Stmt
substituteIdentInStmt (Ident s i _) new_ident stmt = everywhere (mkT substident) stmt where
	substident :: Ident -> Ident
	substident (Ident s2 i2 _) | s2==s && i2==i = new_ident
	substident ident = ident
