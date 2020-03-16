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
import Data.Maybe
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
--		[] -> "gcc" : (analyzerPath++"\\ptrtest_flat.c") : "f" : ["-writeAST"]
		[] -> "gcc" : (analyzerPath++"\\assigntest.c") : "f" : [] --["-writeAST","-writeGlobalDecls"]
		args -> args

	getZonedTime >>= return.(++"\n\n").show >>= writeFile logFile
	
	parseCFile (newGCC gcc) Nothing [] filename >>= \case
		Left err -> error $ show err
		Right translunit -> do
			when ("-writeAST" ∈ opts) $
				writeFile (filename <.> "ast.html") $ genericToHTMLString translunit
			case runTrav_ $ analyseAST translunit of
				Left errs -> putStrLn "ERRORS:" >> forM_ errs print
				Right (globdecls,soft_errors) -> do
					when (not $ null soft_errors) $ putStrLn "Soft errors:" >> forM_ soft_errors print
					when ("-writeGlobalDecls" ∈ opts) $
						writeFile (analyzerPath </> filename <.> "globdecls.html") $ globdeclsToHTMLString globdecls
					covvectors <- evalStateT (covVectorsM funname) $ CovVecState globdecls 1 translunit
					forM_ covvectors $ \ (is,origtrace,trace,model,mb_solution) -> printLog $ unlines $
						[ "","=== ORIG TRACE " ++ is ++ " ====================","<leaving out builtins...>" ] ++
						map show (filter isnotbuiltin origtrace) ++
						[ "","--- TRACE " ++ is ++ " -------------------------","<leaving out builtins...>" ] ++
						map show (filter isnotbuiltin trace) ++
						[ "",
						"--- MODEL " ++ is ++ " -------------------------",
						if null model then "<empty>" else layout model,
						"",
						"--- SOLUTION " ++ is ++ " ----------------------",
						show_solution mb_solution ]
					where
					show_solution Nothing = "No solution"
					show_solution (Just (env,solution,mb_retval)) = unlines [ show solution,
						funname ++ " ( " ++ intercalate " , " (map showarg env) ++ " )",
						"    = " ++ maybe "<NO_RETURN>" (render.pretty) mb_retval ]
						where
						showarg :: EnvItem -> String
						showarg (oldident,(newident,_)) =
							identToString oldident ++ " = " ++ case lookup (identToString newident) solution of
								Nothing -> "DONT_CARE"
								Just (MInt i) -> show i
								Just (MFloat f) -> show f
								val -> error $ "showarg " ++ show val ++ " not yet implemented"

isnotbuiltin (NewDeclaration (tident,_)) = not $ "__" `isPrefixOf` (identToString tident)
isnotbuiltin _ = True

data CovVecState = CovVecState {
	globDeclsCVS    :: GlobalDecls,
	newNameIndexCVS :: Int,
	translUnitCVS   :: CTranslUnit
	}
type CovVecM = StateT CovVecState IO

instance Eq CExpr where
	(CVar id1 _) == (CVar id2 _) = id1==id2
	(CMember ptrexpr1 ident1 isptr1 _) == (CMember ptrexpr2 ident2 isptr2 _) =
		ident1==ident2 && isptr1==isptr2 && ptrexpr1 == ptrexpr2
	(CUnary CIndOp expr1 _) ==  (CUnary CIndOp expr2 _) = expr1==expr2
	_ == _ = False

{-
data LValue = LIdent Ident | LMember CExpr Ident Bool | LPtr CExpr deriving Eq
deriving instance Data LValue
instance Show LValue where
	show (LIdent ident) = (render.pretty) ident
	show (LMember ptrexpr member isptr) = (render.pretty) $ CMember ptrexpr member isptr undefNode
	show (LPtr expr) = (render.pretty) $ CUnary CIndOp expr undefNode
-}

-- Normalizes an expression and creates a variable name for it

normalizeExpr :: CExpr -> String
normalizeExpr expr = case expr of
	CVar varident _ -> identToString varident
	CBinary binop (CVar varident _) (CConst const) _ ->
		identToString varident ++ "_" ++ binop2str binop ++ "_" ++ const2str const
	where
	binop2str binop = case lookup binop [
		(CMulOp,"mul"),(CDivOp,"div"),(CRmdOp,"rmd"),(CAddOp,"plus"),(CSubOp,"minus"),
		(CShlOp,"shl"),(CShrOp,"shr"),(CAndOp,"and"),(CXorOp,"xor"),(COrOp,"or") ] of
			Nothing -> error $ "binop2str " ++ (render.pretty) binop ++ " not implemented"
			Just s -> s

	const2str (CIntConst cint _) = (if i<0 then "m" else "") ++ show (abs i)
		where
		i = getCInteger cint
	const2str x = error $ "const2str " ++ (render.pretty) x ++ " not implemented"

lValueToVarName :: CExpr -> String
lValueToVarName cvar@(CVar _ _) = normalizeExpr cvar
lValueToVarName (CMember ptrexpr member isptr _) =
	normalizeExpr ptrexpr ++ if isptr then "_ARROW_" else "_DOT_" ++ identToString member
lValueToVarName (CUnary CIndOp expr _) = "PTR_" ++ normalizeExpr expr

data TraceElem =
	Assignment CExpr CExpr |
	Condition CExpr |
	NewDeclaration (Ident,Type) |
	Return CExpr
deriving instance Data TraceElem
instance Show TraceElem where
	show (Assignment lvalue expr)   = "ASSN " ++ (render.pretty) lvalue ++ " = " ++ (render.pretty) expr
	show (Condition expr)           = "COND " ++ (render.pretty) expr
	show (NewDeclaration (lval,ty)) = "DECL " ++ (render.pretty) lval ++ " :: " ++ (render.pretty) ty
	show (Return expr)              = "RET  " ++ (render.pretty) expr
type Trace = [TraceElem]

type EnvItem = (Ident,(Ident,Type))
instance Pretty EnvItem where
	pretty (idold,(idnew,ty)) = pretty idold <+> text " |-> " <+> pretty idnew <+> text " :: " <+> pretty ty
type Env = [EnvItem]

type TraceAnalysisResult = (String,Trace,Trace,[MZAST.ModelData],Maybe (Env,Solution,Maybe CExpr))

lookupFunM :: Ident -> CovVecM FunDef
lookupFunM ident = do
	funs <- gets (gObjs.globDeclsCVS)
	case Map.lookup ident funs of
		Just (FunctionDef fundef) -> return fundef
		Nothing -> error $ "Function " ++ (show ident) ++ " not found"

lookupTypeDefM :: Ident -> CovVecM Type
lookupTypeDefM ident = do
	typedefs <- gets (gTypeDefs.globDeclsCVS)
	case Map.lookup ident typedefs of
		Just (TypeDef _ ty _ _) -> return ty
		Nothing -> error $ "TypeDef " ++ (show ident) ++ " not found"

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

createNewIdentM :: String -> CovVecM Ident
createNewIdentM name_prefix = do
	new_var_num <- gets newNameIndexCVS
	modify $ \ s -> s { newNameIndexCVS = newNameIndexCVS s + 1 }
	return $ internalIdent $ name_prefix ++ "_" ++ show new_var_num

declaration2EnvItemM :: Declaration decl => Bool -> decl -> CovVecM [EnvItem]
declaration2EnvItemM makenewidents decl = do
	let VarDecl (VarName srcident _) _ ty = getVarDecl decl
	identTy2EnvItemM makenewidents srcident ty

identTy2EnvItemM :: Bool -> Ident -> Type -> CovVecM [EnvItem]
identTy2EnvItemM makenewidents srcident ty = do
	ty' <- elimTypeDefsM ty
	newident <- case makenewidents of
		True -> createNewIdentM $ lValueToVarName (CVar srcident undefNode)
		False -> return srcident
	
	let newenvitems = []
{-
	let newenvitems = case ty' of
		PtrType target_ty _ _ -> [ (ptrident,(ptrident,target_ty)) ] where
			ptrident = internalIdent $ lValueToVarName $ CUnary CIndOp (CVar newident undefNode) undefNode
		_ -> []
-}

	return $ (srcident,(newident,ty')) : newenvitems

enterFunctionM :: String -> CovVecM ([TraceElem],Env,CStat)
enterFunctionM funname = do
	FunDef (VarDecl _ _ (FunctionType (FunType _ paramdecls False) _)) body _ <- lookupFunM (builtinIdent funname)
	param_env <- concatMapM (declaration2EnvItemM True) paramdecls
	let newdecls = map (NewDeclaration . snd) $ param_env
	return (newdecls,param_env,body)

covVectorsM :: String -> CovVecM [TraceAnalysisResult]
covVectorsM funname = do
	globdecls <- gets ((Map.elems).gObjs.globDeclsCVS)
	glob_env <- concatMapM (declaration2EnvItemM False) globdecls
	(newdecls,param_env,body) <- enterFunctionM funname
	
	let
		enumdefs = concatMap enum2stmt globdecls
		enum2stmt :: IdentDecl -> [CBlockItem]
		enum2stmt (EnumeratorDef (Enumerator ident expr _ _)) =
			[ CBlockStmt (CExpr (Just $ CAssign CAssignOp (CVar ident undefNode) expr undefNode) undefNode) ]
		enum2stmt _ = []

	unfoldTracesM [ param_env ++ glob_env ] newdecls [ enumdefs ++ [CBlockStmt body] ] >>=
		return . (zip (map (:[]) [1..])) >>=
		mapM elimAssignmentsM >>=
		mapM (solveTraceM param_env)


-- Just unfold the traces

unfoldTracesM :: [Env] -> Trace -> [[CBlockItem]] -> CovVecM [Trace]
unfoldTracesM envs trace ((CBlockStmt stmt : rest) : rest2) = case stmt of

	CLabel _ cstat _ _ -> unfoldTracesM envs trace ((CBlockStmt cstat : rest) : rest2)

	CCompound _ cbis _ -> unfoldTracesM ([]:envs) trace (cbis : (rest : rest2))

	CIf cond then_stmt mb_else_stmt _ -> do
		(cond',add_decls) <- transids cond
		then_traces <- unfoldTracesM envs (Condition cond' : (add_decls ++ trace)) ( (CBlockStmt then_stmt : rest) : rest2 )
		let not_cond = Condition (CUnary CNegOp cond' undefNode)
		else_traces <- case mb_else_stmt of
			Nothing        -> unfoldTracesM envs (not_cond : (add_decls ++ trace)) ( rest : rest2 )
			Just else_stmt -> unfoldTracesM envs (not_cond : (add_decls ++ trace)) ( (CBlockStmt else_stmt : rest) : rest2 )
		return $ then_traces ++ else_traces
 
	CReturn Nothing _ -> return [ trace ]
	CReturn (Just ret_expr) _ -> do
		(ret_expr',add_decls) <- transids ret_expr
		return [ Return ret_expr' : (add_decls ++ trace) ]

	CExpr (Just cass@(CAssign assignop lexpr assigned_expr _)) _ -> do
		(lexpr',add_decls_lexpr) <- transids lexpr
		(assigned_expr'',add_decls_assexpr) <- transids assigned_expr'
		unfoldTracesM envs (Assignment lexpr' assigned_expr'' : (add_decls_lexpr ++ add_decls_assexpr ++ trace)) (rest:rest2)
		where
		mb_binop = lookup assignop [
			(CMulAssOp,CMulOp),(CDivAssOp,CDivOp),(CRmdAssOp,CRmdOp),(CAddAssOp,CAddOp),(CSubAssOp,CSubOp),
			(CShlAssOp,CShlOp),(CShrAssOp,CShrOp),(CAndAssOp,CAndOp),(CXorAssOp,CXorOp),(COrAssOp,COrOp) ]
		assigned_expr' = case mb_binop of
			Nothing -> assigned_expr
			Just binop -> CBinary binop lexpr assigned_expr undefNode

	CExpr (Just (CUnary unaryop expr _)) _ | unaryop ∈ map fst unaryops -> do
		unfoldTracesM envs trace ( (CBlockStmt stmt' : rest) : rest2 )
		where
		stmt' = CExpr (Just $ CAssign assignop expr (CConst $ CIntConst (cInteger 1) undefNode) undefNode) undefNode
		Just assignop = lookup unaryop unaryops
		unaryops = [ (CPreIncOp,CAddAssOp),(CPostIncOp,CAddAssOp),(CPreDecOp,CSubAssOp),(CPostDecOp,CSubAssOp) ]

 	CWhile cond body False _ -> unfoldTracesM envs trace ((unroll_loop _UNROLLING_DEPTH ++ rest) : rest2 )
		where
		unroll_loop :: Int -> [CBlockItem]
		unroll_loop 0 = []
		unroll_loop i = [ CBlockStmt $ CIf cond (CCompound [] (CBlockStmt body : unroll_loop (i-1)) undefNode) Nothing undefNode ]

	_ -> error $ "followTracesM " ++ (render.pretty) stmt ++ " not implemented yet" --followTracesM envs trace (rest:rest2)

	where
	
	env = concat envs
	transids = translateIdents envs

unfoldTracesM (env:envs) trace ( (CBlockDecl (CDecl [CTypeSpec typespec] triples _) : rest) : rest2 ) = do
	ty <- tyspec2TypeM typespec
	new_env_items <- forM triples $ \case
		(Just (CDeclr (Just ident) []{-derivdeclrs-} _ _ _),mb_init,Nothing) -> do
			-- TODO: consider the derivdeclrs
			newenvitems <- identTy2EnvItemM True ident ty
			let newdecls = map (NewDeclaration . snd) newenvitems
			return (newenvitems,newdecls)
		triple -> error $ "unfoldTracesM: triple " ++ show triple ++ " not implemented!"
	let (newenvs,newitemss) = unzip new_env_items
	unfoldTracesM ((concat newenvs ++ env) : envs) (concat newitemss ++ trace) (rest:rest2)
	
unfoldTracesM (_:restenvs) trace ([]:rest2) = unfoldTracesM restenvs trace rest2

unfoldTracesM _ trace [] = return [trace]

unfoldTracesM _ _ ((cbi:_):_) = error $ "unfoldTracesM " ++ (render.pretty) cbi ++ " not implemented yet."

{-
replacePtrM :: Trace -> CovVecM Trace
replacePtrM trace = do
	let
		searchptr :: CExpr -> StateT [TraceElem] CovVecM CExpr
		searchptr cmember@(CMember ptrexpr memberident isptr _) = do
			ty <- lift $ inferTypeM ptrexpr
			substptr cmember ty
		searchptr cptr@(CUnary CIndOp ptrexpr _) = do
			ty <- lift $ inferTypeM ptrexpr
			substptr cptr ty
		searchptr expr = return expr

	(trace',decls) <- runStateT (everywhereM (mkM searchptr) trace) []
	return $ trace'++decls
-}


-- Translates all identifiers in an expression to fresh ones,
-- and replaces Ptr and member expressions with variables.

translateIdents :: [Env] -> CExpr -> CovVecM (CExpr,[TraceElem])
translateIdents envs expr = runStateT (everywhereM (mkM transexpr) expr) []
	where
	env = concat envs
	
	transexpr :: CExpr -> StateT [TraceElem] CovVecM CExpr
	transexpr (CVar ident _) = case lookup ident env of
		Just (ident',_) -> return $ CVar ident' undefNode
		Nothing -> error $ "translateIdents: Could not find " ++ (render.pretty) ident
	transexpr cptr@(CUnary CIndOp ptrexpr _) = do
		ty <- lift $ inferTypeM ptrexpr
		let ty' = case ty of
			PtrType target_ty _ _ -> target_ty
			_ -> error $ "translateIdents transexpr: inferTypeM of " ++ (render.pretty) ptrexpr ++
				" gives no PtrType, but " ++ (render.pretty) ty'
		substptr cptr ty'
	transexpr cmember@(CMember ptrexpr member_ident True _) = do
		ty <- lift $ inferTypeM ptrexpr
		ty' <- case ty of
			PtrType target_ty _ _ -> getMemberTypeM target_ty member_ident
			_ -> error $ "translateIdents transexpr: inferTypeM of " ++ (render.pretty) ptrexpr ++
				" gives no PtrType, but " ++ (render.pretty) ty'
		substptr cmember ty'
	transexpr cmember@(CMember ptrexpr member_ident False _) = error "ispre = FALSE!"
	transexpr expr = return expr

	substptr :: CExpr -> Type -> StateT [TraceElem] CovVecM CExpr
	substptr expr ty = do
		let ident' = internalIdent $ lValueToVarName expr
		ty' <- lift $ elimTypeDefsM ty
		modify (NewDeclaration (ident',ty') :)
		return $ CVar ident' undefNode

	inferTypeM :: CExpr -> CovVecM Type
	inferTypeM (CVar ident _) = do
		case lookup ident (map snd env) of
			Nothing -> error $ "inferTypeM " ++ (render.pretty) ident ++ " : not found"
			Just ty -> return ty
	inferTypeM (CBinary _ expr1@(CVar _ _) _ _) = inferTypeM expr1
	inferTypeM (CBinary _ _ expr2@(CVar _ _) _) = inferTypeM expr2
	inferTypeM expr = error $ "inferTypeM " ++ (render.pretty) expr ++ " not implemented"

	getMemberTypeM :: Type -> Ident -> CovVecM Type
	getMemberTypeM (DirectType (TyComp (CompTypeRef sueref)) _ _) member_ident = do
		CompDef (CompType _ _ [memberdecls] _ _) <- lookupTagM sueref
		[ty] = concatMap (\ (MemberDecl (VarDecl (VarName ident _) _ ty) _ _) -> if ident==member_ident then [ty] else []
		return ty

tyspec2TypeM :: CTypeSpec -> CovVecM Type
tyspec2TypeM typespec = case typespec of
	CVoidType _  -> return $ DirectType TyVoid noTypeQuals noAttributes
	CIntType _   -> return $ DirectType (TyIntegral TyInt) noTypeQuals noAttributes
	CCharType _  -> return $ DirectType (TyIntegral TyChar) noTypeQuals noAttributes
	CShortType _ -> return $ DirectType (TyIntegral TyShort) noTypeQuals noAttributes
	CFloatType _ -> return $ DirectType (TyFloating TyFloat) noTypeQuals noAttributes
	CTypeDef ident _ -> lookupTypeDefM ident
	_ -> error $ "tyspec2TypeM: " ++ (render.pretty) typespec ++ " not implemented yet."


-- FOLD TRACE BY SUBSTITUTING ASSIGNMENTS BACKWARDS

elimAssignmentsM :: ([Int],Trace) -> CovVecM ([Int],Trace,Trace)
elimAssignmentsM (is,trace) = foldtraceM [] trace >>= return . (is,trace,) where

	foldtraceM :: Trace -> Trace -> CovVecM Trace
	foldtraceM result [] = return result
	foldtraceM result (Assignment lvalue expr : rest) = foldtraceM (subst result) rest
		where
		subst :: Trace -> Trace
		subst trace = everywhere (mkT substlvalue) trace
			where
			substlvalue :: CExpr -> CExpr
			substlvalue found_expr | lvalue == found_expr = expr
			substlvalue found_expr                        = found_expr
	foldtraceM result (traceitem : rest) = foldtraceM (traceitem:result) rest

lookupTagM :: SUERef -> CovVecM TagDef
lookupTagM ident = do
	tags <- gets (gTags.globDeclsCVS)
	case Map.lookup ident tags of
		Just tagdef -> return tagdef
		Nothing -> error $ "Tag " ++ (show ident) ++ " not found"

-- MiniZinc Model Generation

type TyEnv = [(Ident,Type)]

var2MZ :: TyEnv -> Ident -> CovVecM [MZAST.ModelData]
var2MZ tyenv ident = do
	let ty = case lookup ident tyenv of
		Just ty -> ty
		Nothing -> error $ "Could not find " ++ (render.pretty) ident ++ " in\n" ++
			unlines (map (\ (ident,ty) -> (render.pretty) ident ++ " |-> " ++ (render.pretty) ty ) tyenv)
	mzvar <- mkmzvarM (identToString ident,ty)
	return $ mzvar : []

	where

	mkmzvarM :: (String,Type) -> CovVecM MZAST.ModelData
	mkmzvarM (mzident,ty) = do
		mzty <- elimTypeDefsM ty >>= ty2mz
		return $ MZAST.var mzty mzident
		where
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
		ty2mz (PtrType target_ty _ _) = return $
			MZAST.Range (MZAST.IConst 0) (MZAST.IConst 65535)
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

solveTraceM :: Env -> ([Int],Trace,Trace) -> CovVecM TraceAnalysisResult
solveTraceM param_env (is,orig_trace,trace) = do
	let mb_ret_val = case last trace of
		Return ret_expr -> Just ret_expr
		_               -> Nothing

	constraintsG <- concatMapM traceelemToMZ trace
	
	let tyenv = concatMap traceitem2tyenv trace where
		traceitem2tyenv (NewDeclaration tyenvitem) = [tyenvitem]
		traceitem2tyenv _ = []
	let constr_trace = concatMap traceitem2constr trace where
		traceitem2constr (Condition expr) = [expr]
		traceitem2constr _ = []
	let
		includesG = [ MZAST.include "include.mzn" ]
		vars :: [Ident] = nub $ everything (++) (mkQ [] searchvar) constr_trace where
			searchvar :: Ident -> [Ident]
			searchvar ident = [ ident ]

	varsG <- concatMapM (var2MZ tyenv) vars
	let
		solution_vars =
			(map (\ (_,(ident,_)) -> identToString ident) param_env)
			`intersect`
			(map identToString vars)
		model = includesG ++ varsG ++ constraintsG ++
			[ MZAST.solve $ MZAST.satisfy MZAST.|: MZAST.Annotation "int_search" [
				MZAST.E (MZAST.ArrayLit $ map (MZAST.Var . MZAST.Simpl) solution_vars),
				MZAST.E (MZAST.Var $ MZAST.Simpl "input_order"),
				MZAST.E (MZAST.Var $ MZAST.Simpl "indomain_min"),
				MZAST.E (MZAST.Var $ MZAST.Simpl "complete") ] ]

	let tracename = intercalate "_" $ map show is
	let modelpath = analyzerPath </> "model_" ++ tracename
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

	return (tracename,orig_trace,trace,model,mb_solution)
