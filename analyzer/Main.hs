{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE UnicodeSyntax,LambdaCase,ScopedTypeVariables #-}

module Main where

import System.Environment
import System.FilePath
import Language.C
import Language.C.Data.Ident
import Language.C.Analysis.AstAnalysis
import Language.C.Analysis.TravMonad
import Language.C.Analysis.SemRep
import Language.C.System.GCC
import Control.Monad
import Prelude.Unicode
import Control.Monad.Trans.State.Strict
import Interfaces.FZSolutionParser
import qualified Interfaces.MZAST as MZAST
import Interfaces.MZPrinter
import Control.Monad.IO.Class (liftIO,MonadIO)
import Data.Generics
import qualified Data.Map.Strict as Map
import Text.PrettyPrint

import DataTree
import GlobDecls

analyzerPath = "analyzer"

main = do
	gcc:filename:funname:opts <- getArgs >>= return . \case
		[] -> "gcc" : "test.c" : "f" : [{-"-writeAST"-}]
		args -> args

	parseCFile (newGCC gcc) Nothing [] filename >>= \case
		Left err -> error $ show err
		Right translunit -> do
			when ("-writeAST" ∈ opts) $
				writeFile (analyzerPath </> filename <.> "ast.html") $ genericToHTMLString translunit
			case runTrav_ $ analyseAST translunit of
				Left errs -> putStrLn "ERRORS:" >> forM_ errs print
				Right (globdecls,soft_errors) -> do
					when (not $ null soft_errors) $ putStrLn "Soft errors:" >> forM_ soft_errors print
					when ("-writeGlobalDecls" ∈ opts) $
						writeFile (analyzerPath </> filename <.> "globdecls.html") $ globdeclsToHTMLString globdecls
					covvectors <- evalStateT (covVectorsM funname) $ CovVecState globdecls 1 translunit
					forM_ covvectors $ \ (model,solution) -> do
						putStrLn "\nMODEL:"
--						putStrLn $ layout model
						putStrLn "SOLUTION:"
						print solution

{-
					res <- evalStateT (funCovVectorsM (builtinIdent funname)) $ CovVecState globdecls 1 translunit
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
-}

data CovVecState = CovVecState {
	globDeclsCVS    :: GlobalDecls,
	newNameIndexCVS :: Int,
	translUnitCVS   :: CTranslUnit
	}

type CovVecM = StateT CovVecState IO

lookupFunM :: Ident -> CovVecM FunDef
lookupFunM ident = do
	funs <- gets (gObjs.globDeclsCVS)
	case Map.lookup ident funs of
		Just (FunctionDef fundef) -> return fundef
		_ -> error $ "Function " ++ (show ident) ++ " not found"

createNewIdent :: String -> CovVecM Ident
createNewIdent name_prefix = do
	new_var_num <- gets newNameIndexCVS
	modify $ \ s -> s { newNameIndexCVS = newNameIndexCVS s + 1 }
	return $ internalIdent (name_prefix ++ "_$" ++ show new_var_num)

type EnvItem = (Ident,(Ident,Type))
type Env = [EnvItem]

declaration2EnvItem :: Declaration decl => decl -> CovVecM EnvItem
declaration2EnvItem decl = do
	newident <- createNewIdent (identToString oldident)
	return (oldident,(newident,ty))
	where
	VarDecl (VarName oldident _) _ ty = getVarDecl decl

printEnv :: (MonadIO m) => Env -> m ()
printEnv env = do
	let env' = filter (\ (ident,_) -> not (isBuiltinPos $ posOfNode $ nodeInfo ident)) env
	liftIO $ do
		forM_ env' $ \ (idold,(idnew,ty)) -> putStrLn $ (render.pretty) idold ++ " |-> " ++ (render.pretty) idnew ++ " :: " ++ (render.pretty) ty
		putStrLn "Leaving out builtin bindings.\n"

data Assignment = Assignment Ident CExpr
instance Show Assignment where
	show (Assignment ident expr) = (render.pretty) ident ++ " := " ++ (render.pretty) expr
type Trace = [Assignment]

type TraceAnalysisResult = ([MZAST.ModelData],Maybe Solution)

covVectorsM :: String -> CovVecM [TraceAnalysisResult]
covVectorsM funname = do
	FunDef (VarDecl _ _ (FunctionType (FunType _ paramdecls False) _)) body _ <- lookupFunM (builtinIdent funname)
	globdecls <- gets ((Map.elems).gObjs.globDeclsCVS)
	param_env <- forM paramdecls declaration2EnvItem
	glob_env <- forM globdecls declaration2EnvItem
	let env = param_env++glob_env
--	printEnv env
	followTracesM [env] [] [[CBlockStmt body]] >>= mapM foldTraceM

-- UNFOLD TRACES

followTracesM :: [Env] -> Trace -> [[CBlockItem]] -> CovVecM [Trace]

followTracesM envs trace ( (CBlockStmt stmt : rest) : rest2 ) = case stmt of
	CLabel _ cstat _ _ -> followTracesM envs trace ((CBlockStmt cstat:rest):rest2)
	CCompound _ cbis _ -> followTracesM ([]:envs) trace (cbis : rest : rest2)
	CIf cond then_stmt mb_else_stmt _ -> do
		let cond_cbi = CBlockStmt (CExpr (Just cond) undefNode)
		then_traces <- followTracesM envs trace ( (cond_cbi : CBlockStmt then_stmt : rest) : rest2 )
		else_traces <- case mb_else_stmt of
			Nothing        -> followTracesM envs trace ( (cond_cbi : rest) : rest2 )
			Just else_stmt -> followTracesM envs trace ( (cond_cbi : CBlockStmt else_stmt : rest) : rest2 )
		return $ then_traces ++ else_traces
	CReturn mb_ret_expr _ -> do
		retval_ident <- createNewIdent "ret"
		followTracesM envs trace ([ CBlockStmt (CExpr mb_ret_expr undefNode) ] : rest2)
	CExpr (Just (CAssign CAssignOp (CVar ident _) assigned_expr _)) _ ->
		followTracesM envs (Assignment ident assigned_expr : trace) (rest:rest2)
	CExpr (Just other_expr) _ ->
		followTracesM envs trace (rest:rest2) -- TODO! Could contain function calls!

followTracesM (env:envs) trace ( (CBlockDecl (CDecl _ triples _) : rest) : rest2 ) = do
	forM triples $ \ (Just 
-- CONTINUE HERE!
{-
	case runTrav [] (withExtDeclHandler (analyseDecl True cdecl) handledecl) of
		Left errs -> do
			liftIO $ putStrLn "ERRORS:" >> forM_ errs print
			error "runTrav analyseDecl"
		Right (_,decls) -> do
{-
			liftIO $ do
				putStrLn "decls:"
				mapM_ (print.(render.pretty)) (userState decls)
-}
			let identdecls = userState decls
			new_envitems <- mapM declaration2EnvItem identdecls
			let new_inits = concatMap objdef2assign identdecls
			followTracesM ((new_envitems++env):envs) (new_inits++trace) (rest:rest2)
-}
	where
	handledecl (LocalEvent identdecl) = modifyUserState (identdecl:)
	handledecl _ = return ()
	objdef2assign objdef@(ObjectDef (ObjDef _ (Just (CInitExpr expr _)) _)) = [ Assignment (declIdent objdef) expr ]
	objdef2assign objdef@(ObjectDef (ObjDef _ (Just (CInitList _ _)) _)) = error "objdef2assign: CInitList not yet implemented"
	objdef2assign _ = []

followTracesM (_:restenvs) trace ([]:rest2) = followTracesM restenvs trace rest2

followTracesM _ trace [] = return [trace]

followTracesM _ _ ((cbi:_):_) = error $ "followTraceM " ++ (render.pretty) cbi ++ " not implemented yet."


-- FOLD TRACE BY SUBSTITUTING ASSIGNMENTS BACKWARDS

foldTraceM :: Trace -> CovVecM TraceAnalysisResult
foldTraceM trace = do
	liftIO $ print trace
	return ([],Nothing)

{-
import Control.Monad.Trans.State.Strict
import Control.Monad
import Control.Monad.Trans.Class (lift)
import System.Environment
--import Control.Applicative hiding (empty)
import Control.Monad.IO.Class (liftIO,MonadIO)
import Language.C
import Language.C.Data.Ident
import Language.C.Analysis.AstAnalysis
import Language.C.Analysis.TravMonad
import Language.C.Analysis.SemRep
import Language.C.System.GCC
import System.FilePath
import Text.Printf
import qualified Data.Map.Strict as Map
import Text.PrettyPrint as TPP
import Data.Generics
import Data.List
import Data.Maybe (fromJust)

import Interfaces.MZinHaskell
import Interfaces.MZPrinter
import qualified Interfaces.MZAST as MZAST
import Interfaces.FZSolutionParser

import DataTree
import GlobDecls
import CDSL


_PRINT_TRACESTMTS_TRACE = False
_WRITE_GLOBALDECLS = True
_WRITE_AST = True

{--
stack build :analyzer-exe
stack exec analyzer-exe -- test.c
stack build :analyzer-exe && stack exec analyzer-exe


fp-bit.i: Function _fpdiv_parts
--}

gcc = newGCC "gcc"

data CovVecState = CovVecState {
	globDecls :: GlobalDecls,
	newNameIndex :: Int,
	translUnit :: CTranslUnit
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
--			translunit <- uniquifyIdents translunit1
			when _WRITE_AST $ writeFile ("analyzer" </> filename ++ ".ast.html") $ genericToHTMLString translunit
			case runTrav_ $ analyseAST translunit of
				Left errs -> forM_ errs print
				Right (globdecls,soft_errors) -> do
					forM_ soft_errors print
					when _WRITE_GLOBALDECLS $ writeFile ("analyzer" </> filename++".globdecls.html") $ globdeclsToHTMLString globdecls
					res <- evalStateT (funCovVectorsM (builtinIdent funname)) $ CovVecState globdecls 1 translunit
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

{-
data UniquifyState = UniquifyState {
	uniqueIndexUS :: Int,
	envUS :: [(Ident,Ident)]
	}
initialUniquifyState = UniquifyState 1 []

uniquifyIdents :: (MonadIO m) => CTranslUnit -> m CTranslUnit
uniquifyIdents ctranslunit = everywhere (mkT $ searchfundefs) ctranslunit
	where
	searchfundefs :: CFunDef -> CFunDef
	searchfundefs (CFunDef declspecs fundeclr cdecls stmt ni) = CFunDef declspecs fundeclr cdecls' stmt' ni
		where
		
		stmt' = everywhere (mkT searchandreplace) stmt
		searchandreplace :: CStat -> CStat
		searchandreplace (CCompound idents cbis ni) = CCompound idents cbis' ni where
			cbis' = 
		
		
		(cdecls',env) = runStateT (everywhereM (mkM collectandreplace) cdecls) []
		
		collectandreplace :: 
		
		stmt' = searchandreplace env stmt
-}
		
type Constraint = CExpr

lookupFunM :: Ident -> CovVecM FunDef
lookupFunM ident = do
	map <- gets (gObjs.globDecls)
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
	pretty (TraceAssign ident op expr) = pretty ident TPP.<+> text ":=" TPP.<+> pretty expr

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
	tracesStmtM True ExpandCalls funidents traceelems ((transl_while : rest):rx)
	where
	while2if 0 = []
	while2if i = [ CBlockStmt (CIf cond (CCompound [] (CBlockStmt stmt : while2if (i-1)) undefNode) Nothing undefNode) ]
	transl_while = case isdowhile of
		True -> error "do-while not implemented yet"
		False -> CCompound [] (while2if 5) undefNode

tracesStmtM False ExpandCalls funidents traceelems (((CWhile cond stmt isdowhile ni) : rest ) : rx) =
	tracesStmt_expandFunCallsM funidents traceelems rest rx cond $ \ cond' ->
		CWhile cond' stmt isdowhile ni

-- FOR loop

{-
tracesStmtM False NoCallsLeft funidents traceelems (((CFor expr_or_decl mb_cond mb_inc stmt _) : rest ) : rx) = do
	tracesStmtM True ExpandCalls funidents traceelems ((transl_while : rest):rx)
	where
	for2if 0 = []
	for2if i = [ CBlockStmt (CIf cond (CCompound [] (CBlockStmt stmt : while2if (i-1)) undefNode) Nothing undefNode) ]
	transl_while = case isdowhile of
		True -> error "do-while not implemented yet"
		False -> CCompound [] (while2if 5) undefNode

tracesStmtM False ExpandCalls funidents traceelems (((CFor expr_or_decl mb_cond mb_inc stmt _) : rest ) : rx) =
	tracesStmt_expandFunCallsM funidents traceelems rest rx cond $ \ cond' ->
		CWhile cond' stmt isdowhile ni
-}

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
translateSemRepType ty = case ty of
	DirectType (TyIntegral TyInt) _ _ -> CIntType undefNode
	DirectType (TyIntegral TyShort) _ _ -> CShortType undefNode
	DirectType (TyIntegral TyLong) _ _ -> CLongType undefNode
	DirectType (TyFloating TyFloat) _ _ -> CFloatType undefNode
	DirectType (TyFloating TyDouble) _ _ -> CDoubleType undefNode
{-
	PtrType ty' _ _ ->
	ArrayType ty' _ _ _ ->
	FunctionType (FunType ty' paramdecls isvariadic) _ ->
	FunctionType (FunTypeIncomplete ty') _ ->
	TypeDefType (TypeDefRef ident ty' _) _ _ -> 
-}
	_ -> error $ "translateSemRepType " ++ (render.pretty) ty ++ " not implemented yet"

-- Expand function calls, this is called by tracesStmtM

tracesStmt_expandFunCallsM :: [Ident] -> Trace -> [CStat] -> [[CStat]] -> CExpr -> (CExpr -> CStat) -> CovVecM [Trace]
tracesStmt_expandFunCallsM funidents traceelems rest rx expr f_expr' = do
	(expr',(funcall_stmts,called_funidents)) <- expandFunCallsM expr
	tracesStmtM True NoCallsLeft (called_funidents++funidents) traceelems $
		(if null funcall_stmts then [] else [funcall_stmts]) ++
		((f_expr' expr' : rest) : rx)
--	tracesStmt_expandFunCallsM funidents traceelems rest rx expr f_expr'

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

substituteIdentInStmt :: Ident -> Ident -> Stmt -> Stmt
substituteIdentInStmt ident1 new_ident stmt = everywhere (mkT substident) stmt where
	substident :: Ident -> Ident
	substident ident2 | ident1 == ident2 = new_ident
	substident ident2 = ident2

substituteVarInTraceElem :: Ident -> CExpr -> TraceElem -> TraceElem
substituteVarInTraceElem ident expr (TraceAssign ident2 assignop expr2) =
	TraceAssign ident2 assignop (everywhere (mkT substvar) expr2) where
	substvar :: CExpr -> CExpr
	substvar (CVar varid ni) | varid == ident = expr
	substvar e = e

aggregateConstraintsM :: Trace -> Trace -> CovVecM Trace
aggregateConstraintsM traceelems [] = return traceelems
aggregateConstraintsM traceelems (traceelem@(TraceAssign ident assignop expr) : rest) = do
	aggregateConstraintsM (traceelem : map (substituteVarInTraceElem ident expr') traceelems) rest
	where
	expr' = if assignop==CAssignOp then expr else CBinary assignop' (CVar ident undefNode) expr undefNode
	Just assignop' = lookup assignop [
		(CMulAssOp,CMulOp),(CDivAssOp,CDivOp),(CRmdAssOp,CRmdOp),(CAddAssOp,CAddOp),(CSubAssOp,CSubOp),
		(CShlAssOp,CShlOp),(CShrAssOp,CShrOp),(CAndAssOp,CAndOp),(CXorAssOp,CXorOp),(COrAssOp,COrOp) ]

solveConstraintsM :: Int -> [Constraint] -> CovVecM ([MZAST.ModelData],Maybe Solution)
solveConstraintsM i constraints = do
	constraintsG <- mapM constrToMZ constraints
	let
		includesG = [ MZAST.include "include.mzn" ]
		vars = nub $ everything (++) (mkQ [] searchvar) constraints
	varsG <- mapM var2MZ vars
	let
		model = includesG ++ varsG ++ constraintsG ++ [ MZAST.solve MZAST.satisfy ]

	liftIO $ putStrLn $ unlines $
		[ "","CONSTRAINTS:" ] ++
		map (render.pretty) constraints ++
		[ "","MODEL:" ] ++
		[ layout model ]
	liftIO $ writeFile ("analyzer" </> "model" ++ show i ++ ".mzn") $ layout model
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
	searchvar :: CExpr -> [Ident]
	searchvar (CVar ident _) = [ ident ]
	searchvar _ = []

var2MZ :: (MZAST.Varr i) => Ident -> CovVecM (MZAST.GItem i)
var2MZ ident@(Ident name _ _) = do
	return $ MZAST.var (MZAST.Int) name
{-
	globdecls <- gets globDecls
	case identType ident globdecls of
		[ ty ] -> do
			liftIO $ putStrLn $ name ++ " is of type " ++ (render.pretty) ty
			return $ MZAST.var (MZAST.Int) name
		tys -> error $ "Could not find unique VarDecl declaring " ++ name ++ " in GlobalDecls" ++ "\n" ++
			"Found " ++ show (map (render.pretty) tys)
-}

constrToMZ :: Constraint -> CovVecM MZAST.ModelData
constrToMZ constr = return $ MZAST.constraint (expr2constr . (flatten_not False) . (insert_eq0 True) $ constr)
	where
	eq0 :: Constraint -> Constraint
	eq0 constr = CBinary CEqOp constr (CConst (CIntConst (cInteger 0) undefNode)) undefNode
	insert_eq0 :: Bool -> Constraint -> Constraint
	insert_eq0 _ (CUnary CNegOp expr ni) = CUnary CNegOp (insert_eq0 True expr) ni
	insert_eq0 must_be_bool (CUnary CCompOp expr ni) = (if must_be_bool then eq0 else id) $ CUnary CCompOp (insert_eq0 False expr) ni
	insert_eq0 must_be_bool cvar@(CVar ident ni) = (if must_be_bool then eq0 else id) cvar
	insert_eq0 must_be_bool const@(CConst _) = (if must_be_bool then eq0 else id) const
	insert_eq0 must_be_bool (CBinary binop expr1 expr2 ni) = mb_eq0 $
		CBinary binop (insert_eq0 must_be_bool' expr1) (insert_eq0 must_be_bool' expr2) ni
		where
		(must_be_bool',mb_eq0) = case must_be_bool of
			_ | binop `elem` [CLndOp,CLorOp] -> (True,id)
			_ | binop `elem` [CLeOp,CGrOp,CLeqOp,CGeqOp,CEqOp,CNeqOp] -> (False,id)
			True -> (False,eq0)
			_ -> (False,id)
	insert_eq0 _ expr = error $ "insert_eq0 " ++ (render.pretty) expr ++ " not implemented yet."

	flatten_not :: Bool -> Constraint -> Constraint
	flatten_not is_neg (CUnary CNegOp expr ni) = flatten_not (not is_neg) expr
	flatten_not True un@(CUnary CCompOp _ _) = error $ "flatten_not True " ++ (render.pretty) un ++ " is impossible!"
	flatten_not False (CUnary CCompOp expr ni) = CUnary CCompOp (flatten_not False expr) ni
	flatten_not False cvar@(CVar ident ni) = cvar
	flatten_not True cvar@(CVar ident ni) = error $ "flatten_not True " ++ (render.pretty) cvar ++ " is impossible!"
	flatten_not False cconst@(CConst _) = cconst
	flatten_not True cconst@(CConst _) = error $ "flatten_not True " ++ show cconst
	flatten_not False (CBinary binop expr1 expr2 ni) = CBinary binop (flatten_not False expr1) (flatten_not False expr2) ni
	flatten_not True (CBinary binop expr1 expr2 ni) = CBinary binop' (flatten_not is_neg' expr1) (flatten_not is_neg' expr2) ni
		where
		(binop',is_neg') = case binop of
			CLeOp -> (CGeqOp,False)
			CGrOp -> (CLeqOp,False)
			CLeqOp -> (CGrOp,False)
			CGeqOp -> (CLeOp,False)
			CEqOp -> (CNeqOp,False)
			CNeqOp -> (CEqOp,False)
			CLndOp -> (CLorOp,True)
			CLorOp -> (CLndOp,True)
			op -> error $ "flatten_not True " ++ show op ++ " is impossible!"
	flatten_not is_neg expr = error $ "flatten_not " ++ show is_neg ++ " " ++ (render.pretty) expr ++ " not implemented yet"

	expr2constr (CUnary CNegOp _ _) = error $ "expr2constr CUnaryOp CNegOp!"
	expr2constr (CVar (Ident name _ _) _) = MZAST.Var $ MZAST.stringToIdent name
	expr2constr (CConst (CIntConst (CInteger i _ _) _)) = MZAST.IConst $ fromIntegral i
	expr2constr (CUnary CCompOp expr _) = MZAST.Call (MZAST.stringToIdent "bitwise_not") [MZAST.AnnExpr (expr2constr expr) []]
	expr2constr (CBinary binop expr1 expr2 _) = case lookup binop
		[(CAndOp,"bitwise_and"),(COrOp,"bitwise_or"),(CXorOp,"bitwise_xor"),(CShrOp,"bitshift_right"),(CShlOp,"bitshift_left")] of
			Just funname -> MZAST.Call (MZAST.stringToIdent funname) [MZAST.AnnExpr expr1' [],MZAST.AnnExpr expr2' []]
			Nothing -> MZAST.Bi (MZAST.Op $ MZAST.stringToIdent mznop) expr1' expr2'
		where
		expr1' = expr2constr expr1
		expr2' = expr2constr expr2
		mznop = maybe ((render.pretty) binop) id $ lookup binop [(CEqOp,"=")]
	expr2constr expr = error $ "expr2constr " ++ show expr ++ " not implemented yet"

-- TODO: a->normal, enumerations, global variables
-}