{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE UnicodeSyntax,LambdaCase,ScopedTypeVariables,TupleSections,TypeSynonymInstances,FlexibleInstances,FlexibleContexts,StandaloneDeriving,DeriveDataTypeable #-}

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
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict

import Interfaces.FZSolutionParser
import qualified Interfaces.MZAST as MZAST
import Interfaces.MZPrinter
import Interfaces.MZinHaskell
import qualified Interfaces.MZBuiltIns as MZB

import Control.Monad.IO.Class (liftIO,MonadIO)
import Data.Generics
import qualified Data.Map.Strict as Map
import Text.PrettyPrint
import Data.Time.LocalTime
import Data.Foldable
import Data.List
import System.IO
import Data.Either

import DataTree
import GlobDecls

for = flip map

{--
stack build :analyzer-exe
stack exec analyzer-exe -- test.c
stack build :analyzer-exe && stack exec analyzer-exe


fp-bit.i: Function _fpdiv_parts, Zeile 1039
--}

solveIt = True
_UNROLLING_DEPTH = 3

analyzerPath = "analyzer"
logFile = analyzerPath </> "log.txt"

printLog :: (MonadIO m) => String -> m ()
printLog text = liftIO $ do
	putStrLn text
	appendFile logFile (text++"\n")

main = do
	hSetBuffering stdout NoBuffering

	gcc:filename:funname:opts <- getArgs >>= return . \case
--		[] -> "gcc" : (analyzerPath++"\\test.c") : "g" : [] --["-writeAST","-writeGlobalDecls"]
--		[] -> "gcc" : (analyzerPath++"\\fp-bit.i") : "_fpdiv_parts" : [] --["-writeAST","-writeGlobalDecls"]
--		[] -> "gcc" : (analyzerPath++"\\iftest.c") : "f" : [] --["-writeAST","-writeGlobalDecls"]
--		[] -> "gcc" : (analyzerPath++"\\whiletest.c") : "f" : [] --["-writeAST","-writeGlobalDecls"]
		[] -> "gcc" : (analyzerPath++"\\ptrtest.c") : "e" : [] --["-writeAST","-writeGlobalDecls"]
		args -> args

	getZonedTime >>= return.(++"\n\n").show >>= writeFile logFile
	
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
					forM_ covvectors $ \ (i,origtrace,trace,model,mb_solution) -> let is = show i in printLog $ unlines $
						[ "","=== ORIG TRACE " ++ is ++ " ====================","<leaving out builtins...>" ] ++
						map show (filter isnotbuiltin origtrace) ++
						[ "","--- TRACE " ++ is ++ " -------------------------","<leaving out builtins...>" ] ++
						map show (filter isnotbuiltin trace) ++
						[ "",
						"--- MODEL " ++ is ++ " -------------------------",
						layout model,
						"",
						"--- SOLUTION " ++ is ++ " ----------------------",
						show_solution mb_solution ]
					where
					show_solution Nothing = "No solution"
					show_solution (Just (env,solution,mb_retval)) = unlines [ show solution,
						funname ++ " ( " ++ intercalate " , " (map showarg env) ++ " )",
						"    = " ++ maybe "<NO_RETURN>" (render.pretty) mb_retval ]
						where
						showarg (oldident,(newident,_)) =
							identToString oldident ++ " = " ++ case lookup (identToString newident) solution of
								Nothing -> "DONT_CARE"
								Just (MInt i) -> show i
								Just (MFloat f) -> show f
								val -> error $ "showarg " ++ show val ++ " not yet implemented"

isnotbuiltin (NewDeclaration (ident,_)) = not $ "__" `isPrefixOf` (identToString ident)
isnotbuiltin _ = True

data CovVecState = CovVecState {
	globDeclsCVS    :: GlobalDecls,
	newNameIndexCVS :: Int,
	translUnitCVS   :: CTranslUnit
	}

type CovVecM = StateT CovVecState IO

lookupTypeDefM :: Ident -> CovVecM Type
lookupTypeDefM ident = do
	typedefs <- gets (gTypeDefs.globDeclsCVS)
	case Map.lookup ident typedefs of
		Just (TypeDef _ ty _ _) -> return ty
		Nothing -> error $ "TypeDef " ++ (show ident) ++ " not found"

lookupFunM :: Ident -> CovVecM FunDef
lookupFunM ident = do
	funs <- gets (gObjs.globDeclsCVS)
	case Map.lookup ident funs of
		Just (FunctionDef fundef) -> return fundef
		Nothing -> error $ "Function " ++ (show ident) ++ " not found"

lookupTagM :: SUERef -> CovVecM TagDef
lookupTagM ident = do
	tags <- gets (gTags.globDeclsCVS)
	case Map.lookup ident tags of
		Just tagdef -> return tagdef
		Nothing -> error $ "Tag " ++ (show ident) ++ " not found"

type EnvItem = (Ident,(Ident,Type))
instance Pretty EnvItem where
	pretty (idold,(idnew,ty)) = pretty idold <+> text " |-> " <+> pretty idnew <+> text " :: " <+> pretty ty
type Env = [EnvItem]

createNewIdent :: String -> CovVecM Ident
createNewIdent name_prefix = do
	new_var_num <- gets newNameIndexCVS
	modify $ \ s -> s { newNameIndexCVS = newNameIndexCVS s + 1 }
	return $ internalIdent (name_prefix ++ "_" ++ show new_var_num)

oldident2EnvItem :: Bool -> Type -> Ident -> CovVecM [EnvItem]
oldident2EnvItem createnewident ty oldident = do
	newident <- if createnewident then createNewIdent (identToString oldident) else return oldident
	ty' <- elimTypeDefsM ty
	mvns <- createMemberVarNamesM (CVar newident undefNode) ty'
	let memberenvitems = for mvns $ \ (membername,memberty) -> (internalIdent membername,(internalIdent membername,memberty))
	return $ (oldident,(newident,ty')) : memberenvitems

declaration2EnvItem :: Declaration decl => Bool -> decl -> CovVecM [EnvItem]
declaration2EnvItem createnewident decl = do
	let VarDecl (VarName oldident _) _ ty = getVarDecl decl
	oldident2EnvItem createnewident ty oldident

instance Eq CExpr where
	(CVar id1 _) == (CVar id2 _) = id1==id2
	(CMember ptrexpr1 ident1 isptr1 _) == (CMember ptrexpr2 ident2 isptr2 _) =
		ident1==ident2 && isptr1==isptr2 && ptrexpr1 == ptrexpr2
	_ == _ = False
deriving instance Data LValue

data LValue = LIdent Ident | LMember CExpr Ident Bool deriving Eq
instance Show LValue where
	show (LIdent ident) = (render.pretty) ident
	show (LMember ptrexpr member isptr) = (render.pretty) $ CMember ptrexpr member isptr undefNode

data TraceElem =
	Assignment LValue CAssignOp CExpr |
	Condition CExpr |
	NewDeclaration (Ident,Type) |
	SideEffects CExpr |
	Return CExpr
deriving instance Data TraceElem
instance Show TraceElem where
	show (Assignment lvalue assignop expr)  = show lvalue ++ " " ++ (render.pretty) assignop ++ " " ++ (render.pretty) expr
	show (Condition expr)            = "COND " ++ (render.pretty) expr
	show (NewDeclaration (ident,ty)) = "DECL " ++ (render.pretty) ident ++ " :: " ++ (render.pretty) ty
	show (SideEffects expr)          = "SIDE " ++ (render.pretty) expr
	show (Return expr)               = "RET  " ++ (render.pretty) expr
type Trace = [TraceElem]

type TraceAnalysisResult = (Int,Trace,Trace,[MZAST.ModelData],Maybe (Env,Solution,Maybe CExpr))


-- Prepares and follows the traces, returning the traces
-- (including or excluding the trace elements that stem from the global environment)

prepareFollowTracesM :: Bool -> String -> CovVecM ([(Int,Trace,Trace)],Env)
prepareFollowTracesM include_glob_trace funname = do
	globdecls <- gets ((Map.elems).gObjs.globDeclsCVS)
	glob_env <- concatMapM (declaration2EnvItem False) globdecls
	FunDef (VarDecl _ _ (FunctionType (FunType _ paramdecls False) _)) body _ <- lookupFunM (builtinIdent funname)
	param_env <- concatMapM (declaration2EnvItem True) paramdecls
	let env = param_env ++ glob_env
	let
		envitem2newdecl :: EnvItem -> TraceElem
		envitem2newdecl (_,(newident,ty)) = NewDeclaration (newident,ty)
		newdecls = map envitem2newdecl $ if include_glob_trace then env else param_env
	let
		enumdefs = concatMap enum2stmt globdecls
		enum2stmt :: IdentDecl -> [CBlockItem]
		enum2stmt (EnumeratorDef (Enumerator ident expr _ _)) =
			[ CBlockStmt (CExpr (Just $ CAssign CAssignOp (CVar ident undefNode) expr undefNode) undefNode) ]
		enum2stmt _ = []

	followTracesM [env] newdecls [enumdefs++[CBlockStmt body]] >>=
		return . (zip [1..]) >>=
		mapM elimAssignmentsM >>=
		mapM (expandCallsM glob_env) >>=
		return . (,param_env)


-- Find input vectors that fully cover the given function

covVectorsM :: String -> CovVecM [TraceAnalysisResult]
covVectorsM funname = do
	(triples,param_env) <- prepareFollowTracesM True funname
	mapM (solveTraceM param_env) triples


-- UNFOLD TRACES

followTracesM :: [Env] -> Trace -> [[CBlockItem]] -> CovVecM [Trace]

followTracesM envs trace ( (CBlockStmt stmt : rest) : rest2 ) = case stmt of
	CLabel _ cstat _ _ -> followTracesM envs trace ((CBlockStmt cstat:rest):rest2)

	CCompound _ cbis _ -> followTracesM ([]:envs) trace (cbis : rest : rest2)

	CIf cond then_stmt mb_else_stmt _ -> do
		then_traces <- followTracesM envs (translateteidents (Condition cond) : trace) ( (CBlockStmt then_stmt : rest) : rest2 )
		let not_cond = translateteidents $ Condition (CUnary CNegOp cond undefNode)
		else_traces <- case mb_else_stmt of
			Nothing        -> followTracesM envs (not_cond : trace) ( rest : rest2 )
			Just else_stmt -> followTracesM envs (not_cond : trace) ( (CBlockStmt else_stmt : rest) : rest2 )
		return $ then_traces ++ else_traces

	CReturn Nothing _ -> return [ trace ]
	CReturn (Just ret_expr) _ -> return [ translateteidents (Return ret_expr) : trace ]

	CExpr (Just cass@(CAssign assignop lexpr assigned_expr _)) _ -> do
		let
			lvalue = case lexpr of
				CVar ident _ -> LIdent ident
				CMember expr ident isptr _ -> LMember expr ident isptr
			lval_assignment = Assignment lvalue assignop assigned_expr
			
			member_assignments = []
{-
		member_assignments <- case lvalue of
			LMember expr ident isptr -> return []
			LIdent ident -> do
				case lookup ident (concat envs) of
					Nothing -> error $ "followTracesM " ++ (render.pretty) stmt ++ " : Could not find " ++ (render.pretty) ident
					Just (_,ty) -> do
						printLog $ "ty= " ++ (render.pretty
						mvns <- createMemberVarNamesM ident ty
						forM mvns $ \ (varname,_) -> do
							let memberident = internalIdent varname
							return $ Assignment (LMember (CVar ident undefNode) memberident True) CAssignOp
								(CMember assigned_expr memberident True undefNode)
		printLog $ "MEMBER ASSIGNMENTS: "
		mapM_ (printLog.show) member_assignments 
-}
		followTracesM envs (map translateteidents (lval_assignment:member_assignments) ++ trace) (rest:rest2)

	CExpr (Just (CUnary unaryop expr _)) _ -> followTracesM envs trace ( (CBlockStmt stmt' : rest) : rest2 ) where
		stmt' = CExpr (Just $ CAssign assignop expr (CConst $ CIntConst (cInteger 1) undefNode) undefNode) undefNode
		Just assignop = lookup unaryop [ (CPreIncOp,CAddAssOp),(CPostIncOp,CAddAssOp),(CPreDecOp,CSubAssOp),(CPostDecOp,CSubAssOp) ]

	CWhile cond body False _ -> followTracesM envs trace ((unroll_loop _UNROLLING_DEPTH ++ rest) : rest2 )
		where
		unroll_loop :: Int -> [CBlockItem]
		unroll_loop 0 = []
		unroll_loop i = [ CBlockStmt $ CIf cond (CCompound [] (CBlockStmt body : unroll_loop (i-1)) undefNode) Nothing undefNode ]

--  TODO: extend to arbitrary CExpr Statements
-- 	CExpr (Just expr) _ -> followTracesM envs (translateidents (SideEffects expr) : trace) ( rest : rest2 )

	_ -> error $ "followTracesM " ++ (render.pretty) stmt ++ " not implemented yet" --followTracesM envs trace (rest:rest2)	

	where

	translateteidents = translateTEidents envs

followTracesM allenv@(env:envs) trace ( (CBlockDecl (CDecl [CTypeSpec typespec] triples _) : rest) : rest2 ) = do
	(env',trace') <- foldrM folddecls (env,trace) triples
	followTracesM (env':envs) trace' (rest:rest2)
	where
	-- folding right-to-left, hence env/trace order is correct when putting new elements as head
	folddecls (Just (CDeclr (Just ident) derivdeclrs _ _ _),mb_init,Nothing) (envitems,traceelems) = do
		ty <- tyspec2TypeM typespec >>= elimTypeDefsM   -- TODO: consider derivdeclrs
		envitems' <- oldident2EnvItem True ty ident
		let assign_items = case mb_init of
			Nothing -> []
			Just (CInitExpr expr _) -> [ translateTEidents ((envitems'++env):envs) $ Assignment (LIdent ident) CAssignOp expr ]
		let declitems = map NewDeclaration (map snd envitems')
		return (envitems' ++ envitems, assign_items ++ declitems ++ traceelems)

followTracesM (_:restenvs) trace ([]:rest2) = followTracesM restenvs trace rest2

followTracesM allenv trace [] = return [trace]

followTracesM _ _ ((cbi:_):_) = error $ "followTraceM " ++ (render.pretty) cbi ++ " not implemented yet."


translateTEidents envs (Condition cond) = Condition $ translateIdents envs cond
translateTEidents envs (Return ret_expr) = Return $ translateIdents envs ret_expr
translateTEidents envs (Assignment lval assop expr) = Assignment lval' assop (translateIdents envs expr) where
	lval' = case lval of
		LIdent ident -> let CVar ident' _ = translateIdents envs (CVar ident undefNode) in LIdent ident'
		LMember expr ident bool -> LMember (translateIdents envs expr) ident bool

-- Translate C source idents to new unique idents given in environment

translateIdents :: [Env] -> CExpr -> CExpr
translateIdents envs expr = case expr of
	CMember expr ident isptr _ -> CMember (trans expr) ident isptr undefNode
	CVar ident _ -> CVar (transident ident) undefNode
	CAssign cassop expr1 expr2 _ -> CAssign cassop (trans expr1) (trans expr2) undefNode
	CBinary binop expr1 expr2 _ -> CBinary binop (trans expr1) (trans expr2) undefNode
	CUnary unop expr _ -> CUnary unop (trans expr) undefNode
	CCall expr args _ -> CCall (trans expr) (map trans args) undefNode
	CConst const -> CConst const
	_ -> error $ "translateIdents " ++ (render.pretty) expr ++ " not implemented yet!"
	where
	trans = translateIdents envs
	transident ident = case lookup ident (concat envs) of
		Just (ident',_) -> ident'
		Nothing -> error $ "translateIdents: Could not find " ++ (render.pretty) ident

tyspec2TypeM :: CTypeSpec -> CovVecM Type
tyspec2TypeM typespec = case typespec of
	CVoidType _  -> return $ DirectType TyVoid noTypeQuals noAttributes
	CIntType _   -> return $ DirectType (TyIntegral TyInt) noTypeQuals noAttributes
	CCharType _  -> return $ DirectType (TyIntegral TyChar) noTypeQuals noAttributes
	CShortType _ -> return $ DirectType (TyIntegral TyShort) noTypeQuals noAttributes
	CFloatType _ -> return $ DirectType (TyFloating TyFloat) noTypeQuals noAttributes
	CTypeDef ident _ -> lookupTypeDefM ident
	_ -> error $ "typespec " ++ (render.pretty) typespec ++ " not implemented yet."

elimTypeDefsM :: Type -> CovVecM Type
elimTypeDefsM directtype@(DirectType _ _ _) = pure directtype
elimTypeDefsM (TypeDefType (TypeDefRef ident _ _) tyquals attrs) = lookupTypeDefM ident >>= elimTypeDefsM
elimTypeDefsM (PtrType ty tyquals attrs) = PtrType <$> elimTypeDefsM ty <*> pure tyquals <*> pure attrs
elimTypeDefsM (ArrayType ty size tyquals attrs) = ArrayType <$> elimTypeDefsM ty <*> pure size <*> pure tyquals <*> pure attrs
elimTypeDefsM (FunctionType (FunType funty paramdecls bool) attrs) = FunctionType <$> (
	FunType <$> elimTypeDefsM funty <*> mapM eliminparamdecl paramdecls <*> pure bool) <*> pure attrs
	where
	eliminparamdecl (ParamDecl (VarDecl varname declattrs ty) ni) =
		ParamDecl <$> (VarDecl <$> pure varname <*> pure declattrs <*> elimTypeDefsM ty) <*> pure ni
	eliminparamdecl (AbstractParamDecl (VarDecl varname declattrs ty) ni) =
		AbstractParamDecl <$> (VarDecl <$> pure varname <*> pure declattrs <*> elimTypeDefsM ty) <*> pure ni


-- FOLD TRACE BY SUBSTITUTING ASSIGNMENTS BACKWARDS

elimAssignmentsM :: (Int,Trace) -> CovVecM (Int,Trace,Trace)
elimAssignmentsM (i,trace) = do
	foldedtrace <- foldtraceM [] trace
	return (i,trace,foldedtrace)
	where
	foldtraceM :: Trace -> Trace -> CovVecM Trace
	foldtraceM result [] = return result
	foldtraceM result (Assignment lvalue assignop expr : rest) = do
		let
			result' = subst lvalue expr result
		foldtraceM result' rest
		where
		expr' = if assignop==CAssignOp then expr else CBinary assignop' (lvalueToExpr lvalue) expr undefNode
		Just assignop' = lookup assignop [
			(CMulAssOp,CMulOp),(CDivAssOp,CDivOp),(CRmdAssOp,CRmdOp),(CAddAssOp,CAddOp),(CSubAssOp,CSubOp),
			(CShlAssOp,CShlOp),(CShrAssOp,CShrOp),(CAndAssOp,CAndOp),(CXorAssOp,CXorOp),(COrAssOp,COrOp) ]
	foldtraceM result (traceitem : rest) = foldtraceM (traceitem:result) rest

	substlvalue :: LValue -> CExpr -> CExpr -> CExpr
	substlvalue lvalue expr found_expr = if (lvalueToExpr lvalue) == found_expr then expr else found_expr

	subst :: LValue -> CExpr -> Trace -> Trace
	subst lvalue expr trace = everywhere (mkT (substlvalue lvalue expr)) trace

lvalueToExpr :: LValue -> CExpr
lvalueToExpr (LMember lexpr ident isptr) = CMember lexpr ident isptr undefNode
lvalueToExpr (LIdent ident) = CVar ident undefNode


-- Expand Calls in Expressions

expandCallsM :: Env -> (Int,Trace,Trace) -> CovVecM (Int,Trace,Trace)
expandCallsM env (i,orig_trace,trace) = do
	(trace',additional_traceelems) <- runStateT (everywhereM (mkM expandcall) trace) []
	return (i,orig_trace,additional_traceelems++trace')

	where

	expandcall :: CExpr -> StateT [TraceElem] CovVecM CExpr
	expandcall (CCall funexpr@(CVar funident _) args _) = do
		let Just (_,FunctionType (FunType ty _ _) _) = lookup funident env
		newfunident <- lift $ createNewIdent (identToString funident)
		fun_name_mem_names <- lift $ createMemberVarNamesM (CVar newfunident undefNode) ty
		let fun_name_mem_decls = for fun_name_mem_names $ \ (name,mem_ty) -> NewDeclaration (internalIdent name,mem_ty)
		modify (( NewDeclaration (newfunident,ty) : fun_name_mem_decls ) ++)
		(traces,calledfun_param_env) <- lift $ prepareFollowTracesM False (identToString funident)
		constraintss <- mapM (lift . createpredicatesM ty newfunident) traces
		arg_constrss <- forM (zip calledfun_param_env args) $ \ ((_,(param_ident,param_ty)),arg) -> do
			memberconds <- lift $ createMemberCondsM param_ty (CVar param_ident undefNode) arg
			return $ Condition (CBinary CEqOp (CVar param_ident undefNode) arg undefNode) : memberconds
		modify ((concat constraintss ++ concat arg_constrss) ++)
		return $ CVar newfunident undefNode
	expandcall ccall@(CCall funexpr _ _) = error $ "expandcall: " ++ (render.pretty) ccall ++ " not implemented yet"
	expandcall x = return x

	createpredicatesM :: Type -> Ident -> (Int,Trace,Trace) -> CovVecM Trace
	createpredicatesM ret_type fun_ret_ident (i,_,trace) = case last trace of
		Return ret_expr -> do
			printLog $ "createpredicates TRACE:"
			mapM_ printLog $ map show (filter isnotbuiltin trace)
			let
				(condexprs,decls) = partitionEithers $ map (\case
					Condition cond          -> Left  cond
					decl@(NewDeclaration _) -> Right decl) (init trace)
			foldedconds <- case condexprs of
					[] -> return []
					condexprs -> do
						retval_mem_eq_results <- createMemberExprsM ret_type (CVar fun_ret_ident undefNode) ret_expr
						return [ Condition $ CBinary CLorOp (CUnary CNegOp (fold_and condexprs) undefNode)
							(fold_and $ retval_eq_result : retval_mem_eq_results) undefNode ]
						where
						retval_eq_result = CBinary CEqOp (CVar fun_ret_ident undefNode) ret_expr undefNode
						fold_and exprs = foldl1 (\ c1 c2 -> CBinary CLndOp c1 c2 undefNode) exprs
			return $ decls ++ foldedconds
			
		_ -> error $ "createpredicatesM: no RET found in last position"

createMemberCondsM :: Type -> CExpr -> CExpr -> CovVecM [TraceElem]
createMemberCondsM ty expr1 expr2 = do
	exprs <- createMemberExprsM ty expr1 expr2
	return $ map Condition exprs

createMemberExprsM :: Type -> CExpr -> CExpr -> CovVecM [CExpr]
createMemberExprsM ty expr1 expr2 = do
	mvns1 <- createMemberVarNamesM expr1 ty
	mvns2 <- createMemberVarNamesM expr2 ty
	forM (zip mvns1 mvns2) $ \ ((name1,_),(name2,_)) -> return $
		CBinary CEqOp (CVar (internalIdent name1) undefNode) (CVar (internalIdent name2) undefNode) undefNode

createMemberVarNamesM :: CExpr -> Type -> CovVecM [(String,Type)]
createMemberVarNamesM expr@(CVar ident _) ty = do
	case ty of
		PtrType (DirectType (TyComp (CompTypeRef sueref _ _)) _ _) _ _ -> do
			lookupTagM sueref >>= \case
				CompDef (CompType _ _ memberdecls _ _) -> forM memberdecls $ \case
					MemberDecl (VarDecl (VarName memberident Nothing) _ memberty) Nothing _ -> do
						return (createMemberVarName expr memberident, memberty)
					md@(MemberDecl _ _ _) -> error $ "createMemberVarNamesM: MemberDecl " ++ (render.pretty) md ++ " not implemented yet!"
					bf -> error $ "createMemberVarNamesM: AnonBitField " ++ (render.pretty) bf ++ " not implemented yet!"
				EnumDef _ -> error $ "createMemberVarNamesM looking up CompType should not result in a EnumDef!"
		_ -> return []

createMemberVarName :: CExpr -> Ident -> String
createMemberVarName expr ident = expr_str ++ "_" ++ identToString ident where
	expr_str = case normalizeExpr expr of
		CVar varident _ -> identToString varident
		_ -> error $ "createMemberName for ptrexpr=" ++ (render.pretty) expr ++ " not implemented yet"

normalizeExpr :: CExpr -> CExpr
normalizeExpr expr = expr -- TODO!


-- MiniZinc Model Generation

type TyEnv = [(Ident,Type)]

var2MZ :: TyEnv -> Ident -> CovVecM [MZAST.ModelData]
var2MZ tyenv ident = do
	let ty = case lookup ident tyenv of
		Just ty -> ty
		Nothing -> error $ "Could not find " ++ show ident ++ " in\n" ++
			unlines (map (\ (ident,ty) -> (render.pretty) ident ++ " |-> " ++ (render.pretty) ty ) tyenv)
--	mzmembervars <- createMemberVarNamesM (CVar ident undefNode) ty >>= mapM mkmzvarM
	mzvar <- mkmzvarM (identToString ident,ty)
	return $ mzvar : [] --mzmembervars
	where
	mkmzvarM :: (String,Type) -> CovVecM MZAST.ModelData
	mkmzvarM (mzident,ty) = do
		mzty <- elimTypeDefsM ty >>= ty2mz
		return $ MZAST.var mzty mzident
	ty2mz ty@(DirectType tyname _ _) = case tyname of
		TyVoid -> error "ty2mz DirectType TyVoid should not occur!"
		TyIntegral intty -> case intty of
			TyBool -> return MZAST.Bool
			TyShort -> return $ MZAST.Range (MZAST.IConst (-32768)) (MZAST.IConst 32767)
			TyInt -> return $ MZAST.Range (MZAST.IConst (-10)) (MZAST.IConst 10) --MZAST.Int
			_ -> error $ "ty2mz " ++ (render.pretty) ty ++ " not implemented yet"
		TyFloating floatty -> case floatty of
			TyFloat -> return MZAST.Float
			_ -> error $ "ty2mz " ++ (render.pretty) ty ++ " not implemented yet"
		TyEnum (EnumTypeRef sueref _) -> do
			EnumDef (EnumType _ enums _ _) <- lookupTagM sueref
			return $ MZAST.CT $ MZAST.SetLit $
				map (\ (Enumerator _ (CConst (CIntConst (CInteger i _ _) _)) _ _) ->
					MZAST.IConst (fromIntegral i)) enums
		_ -> error $ "ty2mz " ++ (render.pretty) ty ++ " not implemented yet"
	ty2mz (PtrType target_ty _ _) = return $ MZAST.Range (MZAST.IConst 0) (MZAST.IConst 65535)
	ty2mz ty = error $ "ty2mz " ++ (render.pretty) ty ++ " not implemented yet"

type Constraint = CExpr

traceelemToMZ :: TraceElem -> CovVecM [MZAST.ModelData]
traceelemToMZ (Condition constr) = return [ MZAST.constraint (expr2constr . (flatten_not False) . (insert_eq0 True) $ constr) ]
	where
	eq0 :: Constraint -> Constraint
	eq0 constr = CBinary CEqOp constr (CConst (CIntConst (cInteger 0) undefNode)) undefNode
	insert_eq0 :: Bool -> Constraint -> Constraint
	insert_eq0 must_be_bool (CUnary CCompOp expr ni) = (if must_be_bool then eq0 else id) $ CUnary CCompOp (insert_eq0 False expr) ni
	insert_eq0 must_be_bool (CUnary unop expr ni) = case unop of
		CNegOp -> CUnary CNegOp (insert_eq0 True expr) ni
		unop   -> (if must_be_bool then eq0 else id) $ CUnary unop (insert_eq0 False expr) ni
	insert_eq0 must_be_bool (CCast _ expr _) = insert_eq0 must_be_bool expr
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
	insert_eq0 must_be_bool cmember@(CMember _ _ _ _) = (if must_be_bool then eq0 else id) cmember
	insert_eq0 _ expr = error $ "insert_eq0 " ++ (render.pretty) expr ++ " not implemented yet."
	
	flatten_not :: Bool -> Constraint -> Constraint
	flatten_not is_neg (CUnary CNegOp expr ni) = flatten_not (not is_neg) expr
	flatten_not True un@(CUnary CCompOp _ _) = error $ "flatten_not True " ++ (render.pretty) un ++ " is impossible!"
	flatten_not False (CUnary CCompOp expr ni) = CUnary CCompOp (flatten_not False expr) ni
	flatten_not is_neg (CUnary unop expr ni) = CUnary unop (flatten_not is_neg expr) ni
	flatten_not False cmember@(CMember ptr_expr ident isptr ni) = cmember
	flatten_not True cmember@(CMember ptr_expr ident isptr ni) = error $ "flatten_not True " ++ (render.pretty) cmember ++ " is impossible!"
	flatten_not False cvar@(CVar ident ni) = cvar
	flatten_not True cvar@(CVar ident ni) = error $ "flatten_not True " ++ (render.pretty) cvar ++ " is impossible!"
	flatten_not False cconst@(CConst _) = cconst
	flatten_not True cconst@(CConst _) = error $ "flatten_not True " ++ show cconst ++ " is impossible!"
	flatten_not False (CBinary binop expr1 expr2 ni) = CBinary binop (flatten_not False expr1) (flatten_not False expr2) ni
	flatten_not True (CBinary binop expr1 expr2 ni) = CBinary binop' (flatten_not is_neg' expr1) (flatten_not is_neg' expr2) ni
		where
		(binop',is_neg') = case binop of
			CLeOp  -> (CGeqOp,False)
			CGrOp  -> (CLeqOp,False)
			CLeqOp -> (CGrOp, False)
			CGeqOp -> (CLeOp, False)
			CEqOp  -> (CNeqOp,False)
			CNeqOp -> (CEqOp, False)
			CLndOp -> (CLorOp,True)
			CLorOp -> (CLndOp,True)
			op -> error $ "flatten_not True " ++ show op ++ " is impossible!"
	flatten_not is_neg expr = error $ "flatten_not " ++ show is_neg ++ " " ++ (render.pretty) expr ++ " not implemented yet"

	expr2constr (CUnary CCompOp expr _) = MZAST.Call (MZAST.stringToIdent "bitwise_not") [MZAST.AnnExpr (expr2constr expr) []]
	expr2constr (CUnary CNegOp expr _) = error $ "expr2constr CUnaryOp CNegOp!"
	expr2constr (CUnary unop expr _) = MZAST.U (MZAST.Op $ MZAST.stringToIdent $ (render.pretty) unop) (expr2constr expr)
	expr2constr cmember@(CMember ptr_expr memberident _ _) = MZAST.Var $ MZAST.stringToIdent $ createMemberVarName ptr_expr memberident
	expr2constr (CVar (Ident name _ _) _) = MZAST.Var $ MZAST.stringToIdent name
	expr2constr (CConst (CIntConst (CInteger i _ _) _)) = MZAST.IConst $ fromIntegral i
	expr2constr (CConst (CFloatConst (CFloat s_float) _)) = MZAST.FConst $ read s_float
	expr2constr (CBinary binop expr1 expr2 _) = case lookup binop
		[(CAndOp,"bitwise_and"),(COrOp,"bitwise_or"),(CXorOp,"bitwise_xor"),(CShrOp,"bitshift_right"),(CShlOp,"bitshift_left")] of
			Just funname -> MZAST.Call (MZAST.stringToIdent funname) [MZAST.AnnExpr expr1' [],MZAST.AnnExpr expr2' []]
			Nothing -> MZAST.Bi (MZAST.Op $ MZAST.stringToIdent mznop) expr1' expr2'
		where
		expr1' = expr2constr expr1
		expr2' = expr2constr expr2
		-- Leaving out brackets: Hopefully, the minzinc operators have the same precedences as in C
		mznop = maybe ((render.pretty) binop) id $ lookup binop [(CEqOp,"="),(CLndOp,"/\\"),(CLorOp,"\\/")]
	expr2constr expr = error $ "expr2constr " ++ show expr ++ " not implemented yet"

traceelemToMZ _ = return []

solveTraceM :: Env -> (Int,Trace,Trace) -> CovVecM TraceAnalysisResult
solveTraceM param_env (i,orig_trace,trace) = do
	let mb_ret_val = case last trace of
		Return ret_expr -> Just ret_expr
		_               -> Nothing

	constraintsG <- concatMapM traceelemToMZ trace
	
	let tyenv = concatMap traceitem2tyenv trace where
		traceitem2tyenv (NewDeclaration tyenvitem) = [tyenvitem]
		traceitem2tyenv _ = []
	let
		includesG = [ MZAST.include "include.mzn" ]
		vars = nub $ everything (++) (mkQ [] searchvar) trace where
        	searchvar :: CExpr -> [Ident]
        	searchvar (CVar ident _) = [ ident ]
        	searchvar (CMember expr ident _ _) = [ internalIdent $ createMemberVarName expr ident ]
        	searchvar _ = []

	varsG <- concatMapM (var2MZ tyenv) vars
	let
		solution_vars =
			(map (\ (_,(argident,_)) -> identToString argident) param_env)
			`intersect`
			(map identToString vars)
		model = includesG ++ varsG ++ constraintsG ++
			[ MZAST.solve $ MZAST.satisfy MZAST.|: MZAST.Annotation "int_search" [
				MZAST.E (MZAST.ArrayLit $ map (MZAST.Var . MZAST.Simpl) solution_vars),
				MZAST.E (MZAST.Var $ MZAST.Simpl "input_order"),
				MZAST.E (MZAST.Var $ MZAST.Simpl "indomain_min"),
				MZAST.E (MZAST.Var $ MZAST.Simpl "complete") ] ]

	let modelpath = analyzerPath </> "model" ++ show i
	liftIO $ writeFile (modelpath ++ ".mzn") $ layout model
	printLog "Running model..."

	res <- liftIO $ case solveIt of
		False -> return $ Right []
		True -> runModel model modelpath 1 1
	mb_solution <- case res of
		Left err -> do
			printLog $ show err
			return Nothing
		Right [] -> return Nothing
		Right (sol:_) -> return $ Just (param_env,sol,mb_ret_val)

	return (i,orig_trace,trace,model,mb_solution)

