{-# OPTIONS_GHC -fno-warn-tabs -Wno-unrecognised-pragmas #-}
{-# LANGUAGE TupleSections #-}
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
import Data.List

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
			case runTrav_ $ analyseAST translunit of
				Left errs -> forM_ errs print
				Right (GlobalDecls globobjs _ _,soft_errors) -> do
					forM_ soft_errors print
					res <- evalStateT (funCovVectorsM (builtinIdent funname)) $ CovVecState globobjs 1
					lss <- forM res $ \ (trace,constraints,solution) -> do
						return $
							[ "TRACE:" ] ++
							map (render.pretty) trace ++
							[ "","CONSTRAINTS:" ] ++
							map (render.pretty) constraints ++
							[ "","SOLUTION:" ] ++
							[ show solution ] ++
							[ "","------","" ]
					writeFile (filename <.> "traces") (unlines $ concat lss)

type Constraint = CExpr

lookupFunM :: Ident -> CovVecM FunDef
lookupFunM ident = do
	map <- gets allDefs
	case Map.lookup ident map of
		Just (FunctionDef fundef) -> return fundef
		_ -> error $ "Function " ++ (show ident) ++ " not found"

type Env = Map.Map String Int

funCovVectorsM :: Ident -> CovVecM [(Trace,[Constraint],Maybe Env)]
funCovVectorsM funident = do
	FunDef (VarDecl _ _ _) stmt _ <- lookupFunM funident
	tracesStmtM True ExpandCalls [] [] [[stmt]] >>= mapM aggregateconstraintsM
	where
	filtercond (TraceAssign (Ident n _ _) CAssignOp expr) | "cond$" `isPrefixOf` n = [expr]
	filtercond _ = []
	aggregateconstraintsM :: Trace -> CovVecM (Trace,[Constraint],Maybe Env)
	aggregateconstraintsM tr = do
		contrace <- aggregateConstraintsM [] tr
		let filtered_constraints = concatMap filtercond contrace
		solution <- solveConstraintsM filtered_constraints
		return (tr,filtered_constraints,solution)

getNewIdent :: String -> CovVecM Ident
getNewIdent name_prefix = do
	new_var_num <- gets newNameIndex
	modify $ \ s -> s { newNameIndex = newNameIndex s + 1 }
	return $ internalIdent (name_prefix ++ "$" ++ show new_var_num)

data TraceElem = TraceAssign Ident CAssignOp CExpr -- | TraceCondition CExpr
	deriving Show
instance Pretty TraceElem where
	pretty (TraceAssign ident op expr) = pretty ident <+> text ":=" <+> pretty expr
--	pretty (TraceCondition expr) = text "Condition:" <+> pretty expr

type Trace = [TraceElem]
type FunList = [Ident]
data StmtStage = ExpandCalls | NoCallsLeft deriving (Eq,Show)

tracesStmtM :: Bool -> StmtStage -> [Ident] -> Trace -> [[CStat]] -> CovVecM [Trace]

tracesStmtM True stmtstage funidents traceelems rest = do
	liftIO $ do
		putStrLn "--------------"
		putStrLn $ "tracesStmtM " ++ show stmtstage
		putStrLn $ "    funidents = " ++ show (map (render.pretty) funidents)
		putStrLn $ "    traceelems = " ++ show (map (render.pretty) (take 5 traceelems)) ++ "..."
		putStrLn $ "    next = [ "
		forM_ rest $ \ stmts -> do
			putStrLn "    ["
			forM_ stmts (putStrLn . (("        "++).render.pretty))
			putStrLn "    ],"
		putStrLn "  ]"
	tracesStmtM False stmtstage funidents traceelems rest


-- {} Compound ---

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

-- IF -----------

tracesStmtM False NoCallsLeft funidents traceelems ((CIf cond_expr then_stmt mb_else_stmt if_ni : rest):rx)  = do
	cond_ident <- getNewIdent "cond"
	let
		cond_var = CVar cond_ident undefNode
		cond_stmt_then = CExpr (Just $ CAssign CAssignOp cond_var cond_expr undefNode) undefNode
		cond_stmt_else = CExpr (Just $ CAssign CAssignOp cond_var (CUnary CNegOp cond_expr undefNode) undefNode) undefNode
	then_m <- tracesStmtM True ExpandCalls funidents traceelems ((cond_stmt_then : then_stmt : rest):rx)
	else_m <- tracesStmtM True ExpandCalls funidents traceelems $
		( cond_stmt_else : ( maybe [] (:[]) mb_else_stmt ++ rest ) ) : rx
	return $ then_m ++ else_m

tracesStmtM False ExpandCalls funidents traceelems ((CIf cond_expr then_stmt mb_else_stmt if_ni : rest):rx) = do
	(cond_expr',(funcall_stmts,called_funidents)) <- expandFunCallsM cond_expr
	tracesStmtM True NoCallsLeft (called_funidents++funidents) traceelems $
		(if null funcall_stmts then [] else [funcall_stmts]) ++
		((CIf cond_expr' then_stmt mb_else_stmt if_ni : rest) : rx)

-- CExpr Nothing -------

tracesStmtM False _ funidents traceelems (((CExpr Nothing _) : rest) : rx) =
	tracesStmtM True ExpandCalls funidents traceelems (rest:rx)

-- CExpr ASSIGNMENT --------

tracesStmtM False NoCallsLeft funidents traceelems (((CExpr (Just (CAssign assign_op (CVar ident _) assigned_expr _)) _) : rest) : rx) = do
	tracesStmtM True ExpandCalls funidents (TraceAssign ident assign_op assigned_expr : traceelems) (rest:rx)

tracesStmtM False ExpandCalls funidents traceelems (((CExpr (Just (CAssign assign_op cvar assigned_expr _)) _) : rest ) : rx) = do
	(assigned_expr',(funcall_stmts,called_funidents)) <- expandFunCallsM assigned_expr
	tracesStmtM True NoCallsLeft (called_funidents++funidents) traceelems $
		(if null funcall_stmts then [] else [funcall_stmts]) ++
		((CExpr (Just (CAssign assign_op cvar assigned_expr' undefNode)) undefNode : rest) : rx)


-- RETURN ------------

tracesStmtM False NoCallsLeft funidents traceelems ((CReturn mb_ret_val _ : _):rx) = do
	tracesStmtM True ExpandCalls funidents traceelems $ case mb_ret_val of
		Nothing -> []:rx
		Just ret_val -> case funidents of
			[] -> []:rx
			(funident:_) -> [ CExpr (Just (CAssign CAssignOp (CVar funident undefNode) ret_val undefNode)) undefNode ] : rx

tracesStmtM False ExpandCalls funidents traceelems ((CReturn mb_ret_val _ : _):rx) = do
	(mb_ret_val',(funcall_stmts,called_funidents)) <- case mb_ret_val of
		Nothing -> return (Nothing,([],[]))
		Just ret_val -> do
			(v,ss) <- expandFunCallsM ret_val
			return (Just v,ss)
	tracesStmtM True NoCallsLeft (called_funidents++funidents) traceelems $
		(if null funcall_stmts then [] else [funcall_stmts]) ++
		([CReturn mb_ret_val' undefNode] : rx)


-- pop funident when reaching [] in head of statements to be processed --------

tracesStmtM False st funidents traceelems ([]:rx) | st `elem` [ExpandCalls,NoCallsLeft] = case funidents of
	[] -> case rx of
		[] -> return [traceelems]
		_ -> error "something wrong: funidents=[] and rx!=[]"
	(_:funidents) -> tracesStmtM True ExpandCalls funidents traceelems rx


-- end trace ---------

tracesStmtM False _ [] traceelems [] = return [ traceelems ]
tracesStmtM False _ funidents _ [] = error $ "funidents not empty when reaching end of stmts!"

tracesStmtM False _ _ _ ((stmt:_):_) = error $ "traceStmtM: " ++ show stmt ++ " not implemented yet"


-- catch all -----

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

translateSemRepType :: Type -> CTypeSpecifier NodeInfo
translateSemRepType (DirectType (TyIntegral TyInt) _ _) = CIntType undefNode
translateSemRepType t = error $ "translateSemRepType " ++ (render.pretty) t ++ " not implemented yet"

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
		
		param_assignments <- forM (zip args paramdecls) $ \ (arg,ParamDecl (VarDecl (VarName formal_param _) _ typ) _) -> do
			return $ CBlockDecl $ CDecl [ CTypeSpec (translateSemRepType typ) ]
				[(Just $ CDeclr (Just formal_param) [] Nothing [] undefNode,Just $ CInitExpr arg undefNode,Nothing)]
				undefNode
		let body_with_arg_assignments = CCompound [] (param_assignments ++ [CBlockStmt body]) undefNode
		
--		liftIO $ writeFile ("body_" ++ funname ++ ".html") $ genericToHTMLString body_with_arg_assignments
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
		
		liftIO $ do
			putStrLn $ "old_new_idents =\n"
			forM_ old_new_idents $ \ (old,new) ->
				putStrLn $ "old=" ++ (render.pretty) old ++ ", new=" ++ (render.pretty) new

		-- Substitute identifiers in function body
		let
			subst_var b (old,new) = substituteIdentInStmt old new b
			body' = foldl subst_var body_with_arg_assignments old_new_idents

		liftIO $ putStrLn $ "body' =\n" ++ (render.pretty) body'
		modify $ \ (stats,funidents) -> (stats++[body'],funidents++[fun_val_ident])
		return $ CVar fun_val_ident call_ni
	searchfuncalls expr = return expr

eqIdent :: Ident -> Ident -> Bool
eqIdent (Ident s1 i1 _) (Ident s2 i2 _) = s1==s2 && i1==i2

substituteIdentInStmt :: Ident -> Ident -> Stmt -> Stmt
substituteIdentInStmt ident1 new_ident stmt = everywhere (mkT substident) stmt where
	substident :: Ident -> Ident
	substident ident2 | ident1 `eqIdent` ident2 = new_ident
	substident ident2 = ident2

substituteVarInTraceElem :: Ident -> CExpr -> TraceElem -> TraceElem
substituteVarInTraceElem ident expr (TraceAssign ident2 assignop expr2) =
	TraceAssign ident2 assignop (everywhere (mkT substvar) expr2) where
	substvar :: CExpr -> CExpr
	substvar (CVar varid ni) | varid `eqIdent` ident = expr
	substvar e = e

aggregateConstraintsM :: Trace -> Trace -> CovVecM Trace
aggregateConstraintsM traceelems [] = return traceelems
aggregateConstraintsM traceelems (traceelem@(TraceAssign ident assignop expr) : rest) = do
	case assignop of
		CAssignOp -> aggregateConstraintsM (traceelem : map (substituteVarInTraceElem ident expr) traceelems) rest
		_ -> error $ "aggregateConstraintsM: " ++ show assignop ++ " not implemented yet"

epsilon = 1e-12 :: Double

type ConstraintWeightFun = Map.Map String Int -> Double
	
solveConstraintsM :: [Constraint] -> CovVecM (Maybe Env)
solveConstraintsM constraints = do
	let
		weightfun_expr = foldl plus zero (map (square.to_term) constraints) where
			zero = CConst $ CIntConst (cInteger 0) undefNode
			plus x y = CBinary CAddOp x y undefNode
			square x = CBinary CMulOp x x undefNode
			to_term :: CExpr -> CExpr
			to_term (CUnary unaryop expr1 _) = case unaryop of
					CNegOp -> 1/v1
					_      -> error $ "to_fun_top: unaryop " ++ show unaryop ++ " not implemented yet."
			to_term (CBinary binop expr1 expr2 _) = \ xs -> let
				v1 = (to_fun expr1) xs
				v2 = (to_fun expr2) xs
				in case binop of
					CEqOp  -> v1-v2
					CLeqOp -> if v1>v2 then v1-v2 else 0
					CGeqOp -> if v1<v2 then v2-v1 else 0
					CLeOp  -> if v1>=v2 then abs (v1-v2) + epsilon else 0
					CGrOp  -> if v1<=v2 then abs (v2-v1) + epsilon else 0
					_      -> error $ "to_fun_top: binop " ++ show binop ++ " not implemented yet."

		all_vars = nub $ concatMap (everything (++) (mkQ [] searchvar)) constraints where
			searchvar :: CExpr -> [String]
			searchvar (CVar (Ident name _ _) _) = [name]
			searchvar _ = []
		
		return $ Just $ Map.singleton (render.pretty $ weightfun_expr) 0 
{-
		cost env = sum $ map ((^2).($env)) funs
		derivs = map to_deriv ()
		
	minimize cost derivs (Map.fromList $ map (,0) all_vars) Nothing
	where
	to_term :: CExpr -> CExpr
	to_term (CUnary unaryop expr1 _) = case unaryop of
			CNegOp -> 1/v1
			_      -> error $ "to_fun_top: unaryop " ++ show unaryop ++ " not implemented yet."
	to_term (CBinary binop expr1 expr2 _) = \ xs -> let
		v1 = (to_fun expr1) xs
		v2 = (to_fun expr2) xs
		in case binop of
			CEqOp  -> v1-v2
			CLeqOp -> if v1>v2 then v1-v2 else 0
			CGeqOp -> if v1<v2 then v2-v1 else 0
			CLeOp  -> if v1>=v2 then abs (v1-v2) + epsilon else 0
			CGrOp  -> if v1<=v2 then abs (v2-v1) + epsilon else 0
			_      -> error $ "to_fun_top: binop " ++ show binop ++ " not implemented yet."

	to_term :: CExpr -> ConstraintWeightFun
	to_term (CUnary unaryop expr1 _) = \ xs -> let
		v1 = (to_fun expr1) xs
		in case unaryop of
			CNegOp -> 1/v1
			_      -> error $ "to_fun_top: unaryop " ++ show unaryop ++ " not implemented yet."
	to_term (CBinary binop expr1 expr2 _) = \ xs -> let
		v1 = (to_fun expr1) xs
		v2 = (to_fun expr2) xs
		in case binop of
			CEqOp  -> v1-v2
			CLeqOp -> if v1>v2 then v1-v2 else 0
			CGeqOp -> if v1<v2 then v2-v1 else 0
			CLeOp  -> if v1>=v2 then abs (v1-v2) + epsilon else 0
			CGrOp  -> if v1<=v2 then abs (v2-v1) + epsilon else 0
			_      -> error $ "to_fun_top: binop " ++ show binop ++ " not implemented yet."

to_fun :: CExpr -> ConstraintWeightFun
to_fun (CBinary binop expr1 expr2 _) = \ xs -> let
	v1 = (to_fun expr1) xs
	v2 = (to_fun expr2) xs
	in (case binop of
		CMulOp -> (*)
		CAddOp -> (+)
		_      -> error $ "to_fun: binop " ++ show binop ++ " not implemented yet."
		) v1 v2
to_fun (CVar (Ident name _ _) _) = \ xs -> case Map.lookup name xs of
	Nothing -> error $ "to_fun: Variable " ++ show name ++ " not found!"
	Just x  -> fromIntegral x

minimize :: [ConstraintWeightFun] -> Env -> Maybe Double -> CovVecM (Maybe Env)
minimize funs derivs env mb_prev_cost = case
	cost env < epsilon &&
	all (<epsilon) (map derivs env) &&
	maybe False (\ prevcost -> abs (cost-prevcost) < epsilon) mb_prev_cost
	of
		True -> Just env
		False -> 
	where
-}
