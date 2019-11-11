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

import Interfaces.MZinHaskell
import Interfaces.MZPrinter
import qualified Interfaces.MZAST as MZAST
import Interfaces.FZSolutionParser

import DataTree

_PRINT_TRACESTMTS_TRACE = False

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

	let traces_filename = filename <.> "traces"
	writeFile traces_filename ""

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
					lss <- forM res $ \ (trace,constraints,model,solution) -> do
						return $
							[ "TRACE:" ] ++
							map (render.pretty) trace ++
							[ "","CONSTRAINTS:" ] ++
							map (render.pretty) constraints ++
							[ "","MODEL:" ] ++
							[ layout model ] ++
							[ "","SOLUTION:" ] ++
							[ show solution ] ++
							[ "","------","" ]
					writeFile traces_filename (unlines $ concat lss)

type Constraint = CExpr

lookupFunM :: Ident -> CovVecM FunDef
lookupFunM ident = do
	map <- gets allDefs
	case Map.lookup ident map of
		Just (FunctionDef fundef) -> return fundef
		_ -> error $ "Function " ++ (show ident) ++ " not found"

funCovVectorsM :: Ident -> CovVecM [(Trace,[Constraint],[MZAST.ModelData],Maybe Solution)]
funCovVectorsM funident = do
	FunDef (VarDecl _ _ _) stmt _ <- lookupFunM funident
	tracesStmtM True ExpandCalls [] [] [[stmt]] >>= return.(zip [1..]) >>= mapM aggregateconstraintsM
	where
	filtercond (TraceAssign (Ident n _ _) CAssignOp expr) | "cond$" `isPrefixOf` n = [expr]
	filtercond _ = []
	aggregateconstraintsM :: (Int,Trace) -> CovVecM (Trace,[Constraint],[MZAST.ModelData],Maybe Solution)
	aggregateconstraintsM (i,tr) = do
		contrace <- aggregateConstraintsM [] tr
		let filtered_constraints = concatMap filtercond contrace
		(model,solutions) <- solveConstraintsM i filtered_constraints
		return (tr,filtered_constraints,model,solutions)

getNewIdent :: String -> CovVecM Ident
getNewIdent name_prefix = do
	new_var_num <- gets newNameIndex
	modify $ \ s -> s { newNameIndex = newNameIndex s + 1 }
	return $ internalIdent (name_prefix ++ "$" ++ show new_var_num)

data TraceElem = TraceAssign Ident CAssignOp CExpr -- | TraceCondition CExpr
	deriving Show
instance Pretty TraceElem where
	pretty (TraceAssign ident op expr) = pretty ident <+> text ":=" <+> pretty expr

type Trace = [TraceElem]
type FunList = [Ident]
data StmtStage = ExpandCalls | NoCallsLeft deriving (Eq,Show)

tracesStmtM :: Bool -> StmtStage -> [Ident] -> Trace -> [[CStat]] -> CovVecM [Trace]

tracesStmtM True stmtstage funidents traceelems rest = do
	when _PRINT_TRACESTMTS_TRACE $ liftIO $ do
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
	tracesStmt_expandFunCallsM funidents traceelems rest rx cond_expr $ \ cond_expr' -> 
		CIf cond_expr' then_stmt mb_else_stmt if_ni

-- CExpr Nothing -------

tracesStmtM False _ funidents traceelems (((CExpr Nothing _) : rest) : rx) =
	tracesStmtM True ExpandCalls funidents traceelems (rest:rx)

-- CExpr ASSIGNMENT --------

tracesStmtM False NoCallsLeft funidents traceelems (((CExpr (Just (CAssign assign_op (CVar ident _) assigned_expr _)) _) : rest) : rx) = do
	tracesStmtM True ExpandCalls funidents (TraceAssign ident assign_op assigned_expr : traceelems) (rest:rx)

tracesStmtM False ExpandCalls funidents traceelems (((CExpr (Just (CAssign assign_op cvar assigned_expr ni1)) ni2) : rest ) : rx) = do
	tracesStmt_expandFunCallsM funidents traceelems rest rx assigned_expr $ \ expr' -> 
		CExpr (Just (CAssign assign_op cvar expr' ni1)) ni2

-- [DO] WHILE loop

tracesStmtM False NoCallsLeft funidents traceelems (((CWhile cond stmt isdowhile _) : rest ) : rx) = do
	tracesStmtM True ExpandCalls funidents traceelems ((transl_while ++ rest):rx)
	where
	transl_while = 

tracesStmtM False ExpandCalls funidents traceelems (((CWhile cond stmt isdowhile ni) : rest ) : rx) =
	tracesStmt_expandFunCallsM funidents traceelems rest rx cond $ \ cond' ->
		CWhile cond' stmt isdowhile ni

-- RETURN ------------

tracesStmtM False NoCallsLeft funidents traceelems ((CReturn mb_ret_val _ : _):rx) = do
	tracesStmtM True ExpandCalls funidents traceelems $ case mb_ret_val of
		Nothing -> []:rx
		Just ret_val -> case funidents of
			[] -> []:rx
			(funident:_) -> [ CExpr (Just (CAssign CAssignOp (CVar funident undefNode) ret_val undefNode)) undefNode ] : rx

tracesStmtM False ExpandCalls funidents traceelems cret@((CReturn mb_ret_val _ : rest):rx) = do
	case mb_ret_val of
		Nothing -> tracesStmtM True NoCallsLeft funidents traceelems cret
		Just ret_val -> do
			tracesStmt_expandFunCallsM funidents traceelems rest rx ret_val $
				\ ret_val' -> CReturn (Just ret_val') undefNode


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

-- Expand function calls, this is called from tracesStmtM

tracesStmt_expandFunCallsM :: [Ident] -> Trace -> [CStat] -> [[CStat]] -> CExpr -> (CExpr -> CStat) -> CovVecM [Trace]
tracesStmt_expandFunCallsM funidents traceelems rest rx expr f_expr' = do
	(expr',(funcall_stmts,called_funidents)) <- expandFunCallsM expr
	tracesStmtM True NoCallsLeft (called_funidents++funidents) traceelems $
		(if null funcall_stmts then [] else [funcall_stmts]) ++
		((f_expr' expr' : rest) : rx)

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
		
		when _PRINT_TRACESTMTS_TRACE $ liftIO $ do
			putStrLn $ "old_new_idents =\n"
			forM_ old_new_idents $ \ (old,new) ->
				putStrLn $ "old=" ++ (render.pretty) old ++ ", new=" ++ (render.pretty) new

		-- Substitute identifiers in function body
		let
			subst_var b (old,new) = substituteIdentInStmt old new b
			body' = foldl subst_var body_with_arg_assignments old_new_idents

		when _PRINT_TRACESTMTS_TRACE $ liftIO $ putStrLn $ "body' =\n" ++ (render.pretty) body'
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

solveConstraintsM :: Int -> [Constraint] -> CovVecM ([MZAST.ModelData],Maybe Solution)
solveConstraintsM i constraints = do
	liftIO $ putStrLn $ unlines $
		[ "","CONSTRAINTS:" ] ++
		map (render.pretty) constraints ++
		[ "","MODEL:" ] ++
		[ layout model ]
	liftIO $ writeFile ("model" ++ show i ++ ".mzn") $ layout model
	liftIO $ putStrLn "Running model..."
	res <- liftIO $ runModel model (show i) 1 1
	case res of
		Left err -> error $ show err
		Right solutions -> do
			let solution = case solutions of
				[] -> Nothing
				(sol:_) -> Just sol
			liftIO $ putStrLn $ unlines $
				[ "","SOLUTION:" ] ++
				[ show solution ] ++
				[ "","------","" ]
			return (model,solution)
	where
	model = varsG ++ constraintsG ++ [ MZAST.solve MZAST.satisfy ]
	varsG = map (MZAST.var MZAST.Int) $ nub $ everything (++) (mkQ [] searchvar) constraints
	searchvar :: CExpr -> [String]
	searchvar (CVar (Ident name _ _) _) = [ name ]
	searchvar _ = []
	constraintsG = map (MZAST.constraint . expr2constr) constraints
	expr2constr (CUnary CNegOp expr _) = MZAST.U (MZAST.Op $ MZAST.stringToIdent "not") (expr2constr expr)
	expr2constr (CBinary binop expr1 expr2 _) = MZAST.Bi (MZAST.Op $ MZAST.stringToIdent $ render $ pretty binop)
		(expr2constr expr1) (expr2constr expr2)
	expr2constr (CVar (Ident name _ _) _) = MZAST.Var $ MZAST.stringToIdent name
	expr2constr (CConst (CIntConst (CInteger i _ _) _)) = MZAST.IConst $ fromIntegral i
	expr2constr expr = error $ "expr2constr " ++ show expr ++ " not implemented yet"
