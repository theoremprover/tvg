{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE UnicodeSyntax,LambdaCase,ScopedTypeVariables,TupleSections,TypeSynonymInstances,FlexibleInstances,FlexibleContexts,StandaloneDeriving,DeriveDataTypeable,DeriveGeneric #-}

module Main where

import System.Environment
import System.FilePath
import System.Process
import System.Directory
import System.Exit
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
import qualified GHC.Generics as GHCG
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

for :: [a] -> (a -> b) -> [b]
for = flip map

concatForM = flip concatMapM

{--
stack build :analyzer-exe
stack exec analyzer-exe -- test.c
stack build :analyzer-exe && stack exec analyzer-exe


fp-bit.i: Function _fpdiv_parts, Zeile 1039
--}

solveIt = True
showOnlySolutions = False
don'tShowTraces = False
checkSolutions = True

returnval_var_name = "return_val"

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
--		[] -> "gcc" : (analyzerPath++"\\fp-bit.i") : "_fpdiv_parts" : ["-writeAST","-writeGlobalDecls"]
		[] -> "gcc" : (analyzerPath++"\\branchtest.c") : "f" : ["-writeTree"] --["-writeAST","-writeGlobalDecls"]
--		[] -> "gcc" : (analyzerPath++"\\iftest.c") : "f" : [] --["-writeAST","-writeGlobalDecls"]
--		[] -> "gcc" : (analyzerPath++"\\whiletest.c") : "f" : [] --["-writeAST","-writeGlobalDecls"]
--		[] -> "gcc" : (analyzerPath++"\\ptrtest_flat.c") : "f" : ["-writeAST"]
--		[] -> "gcc" : (analyzerPath++"\\assigntest.c") : "g" : [] --["-writeAST","-writeGlobalDecls"]
--		[] -> "gcc" : (analyzerPath++"\\ptrrettest.c") : "g" : [] --["-writeAST","-writeGlobalDecls"]
--		[] -> "gcc" : (analyzerPath++"\\calltest.c") : "g" : ["-writeTraceTree"] --["-writeAST","-writeGlobalDecls"]
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
						writeFile (filename <.> "globdecls.html") $ globdeclsToHTMLString globdecls

					mb_checkexename <- case checkSolutions of
						False -> return Nothing
						True  -> do
							let
								srcfilename = takeFileName filename
								chkexefilename = replaceExtension srcfilename "exe"
							absolute_filename <- makeAbsolute filename
							withCurrentDirectory (takeDirectory absolute_filename) $ do
								(exitcode,stdout,stderr) <- readProcessWithExitCode gcc ["-o",chkexefilename,"-DCALC",srcfilename] ""
								case exitcode of
									ExitFailure _ -> error $ "Compilation failed:\n" ++ stderr
									ExitSuccess -> return $ Just chkexefilename 

					traceanalysisresults <- evalStateT (covVectorsM filename opts funname) $
						CovVecState globdecls 1 translunit filename mb_checkexename

					forM_ traceanalysisresults $ \ (traceid,trace,(model,mb_solution)) -> do
						case not showOnlySolutions || maybe False (not.null.(\(_,b,_)->b)) mb_solution of
							False -> return ()
							True -> printLog $ unlines $ mbshowtraces (
								[ "","=== TRACE " ++ show traceid ++ " ========================","<leaving out builtins...>" ] ++
								map show (filter isnotbuiltin trace) ++
								[ "",
								"--- MODEL " ++ show traceid ++ " -------------------------",
								if null model then "<empty>" else layout model,
								"" ]) ++ [
								"--- SOLUTION " ++ show traceid ++ " ----------------------",
								show_solution mb_solution ]
							where
		
							mbshowtraces ts = if don'tShowTraces then [] else ts
							show_solution Nothing = "No solution"
							show_solution (Just (_,[],_)) = "Empty solution"
							show_solution (Just (env,solution,mb_retval)) = unlines [ show solution,
								funname ++ " ( " ++ intercalate " , " (map showarg env) ++ " )" ++
								"    = " ++ maybe "<NO_RETURN>" (const $ show $ getPredictedResult solution) mb_retval ]
								where
								showarg :: EnvItem -> String
								showarg (oldident,(newident,_)) =
									identToString oldident ++ " = " ++ case lookup (identToString newident) solution of
										Nothing -> "DONT_CARE"
										Just (MInt i) -> show i
										Just (MFloat f) -> show f
										val -> error $ "showarg " ++ show val ++ " not yet implemented"

data CovVecState = CovVecState {
	globDeclsCVS    :: GlobalDecls,
	newNameIndexCVS :: Int,
	translUnitCVS   :: CTranslUnit,
	srcFilenameCVS  :: String,
	checkExeNameCVS :: Maybe String
	}
type CovVecM = StateT CovVecState IO

data TraceElem =
	Assignment CExpr CExpr |
	Condition CExpr |
	NewDeclaration (Ident,Type) |
	Return CExpr |
	TraceOr [Trace] |
	TraceAnd [Trace]
	deriving Data

--data DataTree = DataTree String [DataTree] | Leaf String deriving (Show)
traceToHTMLString :: Trace -> String
traceToHTMLString trace = dataTreeToHTMLString [ trace2datatree trace ]
	where
	trace2datatree :: Trace -> DataTree
	trace2datatree trace = DataTree "list" $ map conv (filter isnotbuiltin trace)
	conv :: TraceElem -> DataTree
	conv (TraceOr traces) = DataTree "OR" $ map trace2datatree traces 
	conv (TraceAnd traces) = DataTree "AND" $ map trace2datatree traces 
	conv other = Leaf $ show other

--deriving instance Data TraceElem
instance Show TraceElem where
	show (Assignment lvalue expr)   = "ASSN " ++ (render.pretty) lvalue ++ " = " ++ (render.pretty) expr
	show (Condition expr)           = "COND " ++ (render.pretty) expr
	show (NewDeclaration (lval,ty)) = "DECL " ++ (render.pretty) lval ++ " :: " ++ (render.pretty) ty
	show (Return expr)              = "RET  " ++ (render.pretty) expr
	show (TraceOr traces)           = "OR   " ++ show traces
	show (TraceAnd traces)          = "AND  " ++ show traces
type Trace = [TraceElem]

showTrace :: Int -> Trace -> String
showTrace _ [] = ""
showTrace ind (te:trace) | not (isnotbuiltin te) = showTrace ind trace
showTrace ind (te:trace) = indent ind ++ case te of
	TraceOr traces  -> "OR\n" ++ showlist traces
	TraceAnd traces -> "AND\n" ++ showlist traces
	te              -> show te ++ "\n" ++ showTrace ind trace
	where
	indent i = concat $ replicate i ":   "
	showlist traces = indent (ind+1) ++ "[\n" ++ showitems traces ++ indent (ind+1) ++ "]\n"
	showitems traces = intercalate (indent (ind+1) ++ ",\n") (map (showTrace (ind+2)) traces)

type ResultData = ([MZAST.ModelData],Maybe (Env,Solution,Maybe CExpr))
type TraceAnalysisResult = ([Int],Trace,ResultData)

covVectorsM :: String -> [String] -> String -> CovVecM [TraceAnalysisResult]
covVectorsM filename opts funname = do
	globdecls <- gets ((Map.elems).gObjs.globDeclsCVS)
	glob_env <- concatMapM (declaration2EnvItemM False) globdecls
	let
		-- creates the assignment statements from the global context
		defs = concatMap def2stmt globdecls
		def2stmt :: IdentDecl -> [CBlockItem]
		def2stmt (EnumeratorDef (Enumerator ident expr _ _)) = assnstmt ident expr
		def2stmt (ObjectDef (ObjDef (VarDecl (VarName ident _) _ _) (Just (CInitExpr expr _)) _)) = assnstmt ident expr
		def2stmt _ = []
		assnstmt ident expr = [ CBlockStmt (CExpr (Just $ CAssign CAssignOp (CVar ident undefNode) expr undefNode) undefNode) ] 

	FunDef (VarDecl _ _ (FunctionType (FunType ret_type funparamdecls False) _)) body _ <- lookupFunM (builtinIdent funname)
	param_env <- concatMapM (declaration2EnvItemM True) funparamdecls
	
	let decls = map (NewDeclaration . snd) (param_env++glob_env)

	trace <- unfoldTracesM True (param_env:[glob_env]) decls [ defs ++ [ CBlockStmt body ] ]
	when ("-writeTree" ∈ opts) $ liftIO $ writeFile (filename ++ "_tree" <.> "html") $ traceToHTMLString trace

	analyzeTreeM opts ret_type param_env [] [] trace

{-	
type ResultData = ([MZAST.ModelData],Maybe (Env,Solution,Maybe CExpr))
type TraceAnalysisResult = ([Int],Trace,ResultData)
solveTraceM :: Type -> Env -> [Int] -> Trace -> CovVecM ResultData
checkSolutionM :: [Int] -> ResultData -> CovVecM ResultData
-}

analyzeTreeM :: [String] -> Type -> Env -> [Int] -> [TraceElem] -> Trace -> CovVecM [TraceAnalysisResult]

analyzeTreeM opts ret_type param_env traceid res_line [] = do
	res_trace <- elimAssignmentsM res_line
	solveTraceM ret_type param_env traceid res_trace >>= checkSolutionM traceid >>= return . (:[]) . (traceid,res_trace,)

analyzeTreeM opts ret_type param_env traceid res_line (TraceOr traces : rest) = case rest of
	[] -> do
		results <- forM (zip [1..] traces) (\ (i,trace) -> analyzeTreeM opts ret_type param_env (traceid++[i]) res_line trace)
		when (not $ any is_solution (concat results)) $ do
			printLog $ "DEAD CODE: Did not find any solutions for trace " ++ showTrace 0 res_line
		return $ concat results
	_ -> error $ "analyzeTreeM: TraceOr not last element in " ++ showTrace 0 res_line

analyzeTreeM opts ret_type param_env traceid res_line (TraceAnd traces : rest) = case rest of
	[] -> do
		results <- forM (zip [1..] traces) (\ (i,trace) -> analyzeTreeM opts ret_type param_env (traceid++[i]) res_line trace)
		when (any (not.is_solution) (concat results)) $ do
			printLog $ "DEAD CODE: Did not find any solutions for trace " ++ showTrace 0 res_line
		return $ concat results
	_ -> error $ "analyzeTreeM: TraceAnd not last element in " ++ showTrace 0 res_line

analyzeTreeM opts ret_type param_env traceid res_line (te:rest) =
	analyzeTreeM opts ret_type param_env traceid (te:res_line) rest

is_solution :: TraceAnalysisResult -> Bool
is_solution (_,_,(_,mb_solution)) = isJust mb_solution

lookupFunM :: Ident -> CovVecM FunDef
lookupFunM ident = do
	funs <- gets (gObjs.globDeclsCVS)
	case Map.lookup ident funs of
		Just (FunctionDef fundef) -> return fundef
		Just other -> error $ "lookupFunM " ++ (render.pretty) ident ++ " yielded " ++ (render.pretty) other
		Nothing -> error $ "Function " ++ (show ident) ++ " not found"

isnotbuiltinIdent ident = not $ "__" `isPrefixOf` (identToString ident)

isnotbuiltin (NewDeclaration (ident,_)) = isnotbuiltinIdent ident
isnotbuiltin _ = True

instance Eq CExpr where
	(CVar id1 _) == (CVar id2 _) = id1==id2
	(CMember ptrexpr1 ident1 isptr1 _) == (CMember ptrexpr2 ident2 isptr2 _) =
		ident1==ident2 && isptr1==isptr2 && ptrexpr1 == ptrexpr2
	(CUnary CIndOp expr1 _) ==  (CUnary CIndOp expr2 _) = expr1==expr2
	_ == _ = False

-- Normalizes an expression and creates a variable name for the result

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
	CMember (CUnary CAdrOp expr _) member True _ -> normalizeExpr $ CMember expr member False undefNode
	CMember (CUnary CIndOp expr _) member False _ -> normalizeExpr $ CMember expr member True undefNode
	other -> error $ "normalizeExpr " ++ (render.pretty) other ++ " not implemented yet."

	where

	const2str (CIntConst cint _) = (if i<0 then "m" else "") ++ show (abs i)
		where
		i = getCInteger cint
	const2str x = error $ "const2str " ++ (render.pretty) x ++ " not implemented"

lValueToVarName :: CExpr -> String
lValueToVarName cvar@(CVar _ _) = normalizeExpr cvar
lValueToVarName (CMember ptrexpr member isptr _) =
	normalizeExpr ptrexpr ++ (if isptr then "_ARROW_" else "_DOT_") ++ identToString member
lValueToVarName (CUnary CIndOp expr _) = "PTR_" ++ normalizeExpr expr


type EnvItem = (Ident,(Ident,Type))
instance Pretty EnvItem where
	pretty (idold,(idnew,ty)) = pretty idold <+> text " |-> " <+> pretty idnew <+> text " :: " <+> pretty ty
type Env = [EnvItem]

envToString :: Env -> String
envToString env = unlines $ map (render.pretty) $ filter (isnotbuiltinIdent.fst) env

lookupTypeDefM :: Ident -> CovVecM Type
lookupTypeDefM ident = do
	typedefs <- gets (gTypeDefs.globDeclsCVS)
	case Map.lookup ident typedefs of
		Just (TypeDef _ ty _ _) -> return ty
		Nothing -> error $ "TypeDef " ++ (show ident) ++ " not found"

lookupTagM :: SUERef -> CovVecM TagDef
lookupTagM ident = do
	tags <- gets (gTags.globDeclsCVS)
	case Map.lookup ident tags of
		Just tagdef -> return tagdef
		Nothing -> error $ "Tag " ++ (show ident) ++ " not found"

getMembersM :: SUERef -> CovVecM [(Ident,Type)]
getMembersM sueref = do
	CompDef (CompType _ _ memberdecls _ _) <- lookupTagM sueref
	return $ for memberdecls $ \ (MemberDecl (VarDecl (VarName ident _) _ ty) Nothing _) -> (ident,ty)

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

makeMemberExprsM :: Type -> CExpr -> CovVecM [EnvItem]
makeMemberExprsM ty ptr_expr = case ty of
	DirectType (TyComp (CompTypeRef sueref _ _)) _ _ -> do
		members <- getMembersM sueref 
		return $ for members $ \ (member_ident,member_ty) ->
			let
				lexpr = CMember ptr_expr member_ident False undefNode
				new_mem_ident_ptr = internalIdent $ lValueToVarName lexpr
				in
				(new_mem_ident_ptr,(new_mem_ident_ptr,member_ty))
	PtrType (DirectType (TyComp (CompTypeRef sueref _ _)) _ _) _ _ -> do
		members <- getMembersM sueref 
		return $ for members $ \ (member_ident,member_ty) ->
			let
				lexpr = CMember ptr_expr member_ident True undefNode
				new_mem_ident_ptr = internalIdent $ lValueToVarName lexpr
				in
				(new_mem_ident_ptr,(new_mem_ident_ptr,member_ty))
	_ -> return []

identTy2EnvItemM :: Bool -> Ident -> Type -> CovVecM [EnvItem]
identTy2EnvItemM makenewidents srcident ty = do
	ty' <- elimTypeDefsM ty
	newident <- case makenewidents of
		True -> createNewIdentM $ lValueToVarName (CVar srcident undefNode)
		False -> return srcident
	other_envitems <- makeMemberExprsM ty' (CVar newident undefNode)
	return $ (srcident,(newident,ty')) : other_envitems

-- Just unfold the traces
unfoldTracesM :: Bool -> [Env] -> Trace -> [[CBlockItem]] -> CovVecM Trace
unfoldTracesM toplevel envs trace cbss = do
	cbss_txt <- case cbss of
		[] -> return "[]"
		(l : _) -> return $ "[ " ++ (intercalate " , " (map (render.pretty) l)) ++ " ] : _"
	res <- unfoldTraces1M toplevel envs trace cbss
	printLog $ "============================================================================="
	printLog $ "unfoldTracesM " ++ show toplevel ++ " envs trace " ++ cbss_txt
	printLog $ showTrace 0 res
	return res

unfoldTraces1M :: Bool -> [Env] -> Trace -> [[CBlockItem]] -> CovVecM Trace
unfoldTraces1M toplevel envs trace ((CBlockStmt stmt : rest) : rest2) = case stmt of

	CLabel _ cstat _ _ -> unfoldTracesM toplevel envs trace ((CBlockStmt cstat : rest) : rest2)

	CCompound _ cbis _ -> unfoldTracesM toplevel ([]:envs) trace (cbis : (rest : rest2))

	CIf cond then_stmt mb_else_stmt _ -> do
		transids cond trace $ \ (cond',trace') -> do
			then_trace <- unfoldTracesM toplevel envs (Condition cond' : trace') ( (CBlockStmt then_stmt : rest) : rest2 )
			let not_cond = Condition (CUnary CNegOp cond' undefNode)
			else_trace <- case mb_else_stmt of
				Nothing        -> unfoldTracesM toplevel envs (trace' ++ [not_cond]) ( rest : rest2 )
				Just else_stmt -> unfoldTracesM toplevel envs (trace' ++ [not_cond]) ( (CBlockStmt else_stmt : rest) : rest2 )
			return $ [ (if toplevel then TraceAnd else TraceOr) [ then_trace, else_trace ] ]

	CReturn Nothing _ -> return trace
	CReturn (Just ret_expr) _ -> do
		transids ret_expr trace $ \ (ret_expr',trace') -> do
			return $ trace' ++ [ Return ret_expr' ]

	CExpr (Just cass@(CAssign assignop lexpr assigned_expr _)) _ -> do
		transids assigned_expr' trace $ \ (assigned_expr'',trace') -> do
			let lexpr' = subst_var envs lexpr
			unfoldTracesM toplevel envs (trace' ++ [Assignment lexpr' assigned_expr'']) (rest:rest2)
		where
		mb_binop = lookup assignop [
			(CMulAssOp,CMulOp),(CDivAssOp,CDivOp),(CRmdAssOp,CRmdOp),(CAddAssOp,CAddOp),(CSubAssOp,CSubOp),
			(CShlAssOp,CShlOp),(CShrAssOp,CShrOp),(CAndAssOp,CAndOp),(CXorAssOp,CXorOp),(COrAssOp,COrOp) ]
		assigned_expr' = case mb_binop of
			Nothing -> assigned_expr
			Just binop -> CBinary binop lexpr assigned_expr undefNode

	CExpr (Just (CUnary unaryop expr _)) _ | unaryop ∈ map fst unaryops -> do
		unfoldTracesM toplevel envs trace ( (CBlockStmt stmt' : rest) : rest2 )
		where
		stmt' = CExpr (Just $ CAssign assignop expr (CConst $ CIntConst (cInteger 1) undefNode) undefNode) undefNode
		Just assignop = lookup unaryop unaryops
		unaryops = [ (CPreIncOp,CAddAssOp),(CPostIncOp,CAddAssOp),(CPreDecOp,CSubAssOp),(CPostDecOp,CSubAssOp) ]

	CExpr (Just expr) _ -> do
		error $ "not implemented yet."

	-- I'd like to create an algorithm that infers the invariant from the loop's body, so I don't have to unroll...
 	CWhile cond body False _ -> unfoldTracesM toplevel envs trace ((unroll_loop _UNROLLING_DEPTH ++ rest) : rest2 )
		where
		unroll_loop :: Int -> [CBlockItem]
		unroll_loop 0 = []
		unroll_loop i = [ CBlockStmt $ CIf cond (CCompound [] (CBlockStmt body : unroll_loop (i-1)) undefNode) Nothing undefNode ]

	_ -> error $ "followTracesM " ++ (render.pretty) stmt ++ " not implemented yet" --followTracesM envs trace (rest:rest2)

	where
	
	transids :: CExpr -> Trace -> ((CExpr,Trace) -> CovVecM Trace) -> CovVecM Trace
	transids expr trace cont = do
		additional_expr_traces :: [(CExpr,Trace)] <- translateExprM toplevel envs expr
		conts :: [Trace] <- forM additional_expr_traces $ \ (expr',trace') -> do
			cont (expr',trace++trace')
		let junction = if toplevel then TraceAnd else TraceOr
		case conts of
			[] -> error $ "transids Strange: conts empty!"
			[e] -> return e
			conts -> return [ junction conts ]

unfoldTraces1M toplevel (env:envs) trace ( (CBlockDecl (CDecl [CTypeSpec typespec] triples _) : rest) : rest2 ) = do
	ty <- tyspec2TypeM typespec
	new_env_items <- forM triples $ \case
		(Just (CDeclr (Just ident) derivdeclrs _ _ _),mb_init,Nothing) -> do
			-- TODO: consider the derivdeclrs
			let ty' = case derivdeclrs of
				[] -> ty
				[CPtrDeclr _ _] -> PtrType ty noTypeQuals noAttributes
			newenvitems <- identTy2EnvItemM True ident ty'
			let newdecls = map (NewDeclaration . snd) newenvitems
			initializers <- case mb_init of
				Nothing -> return []
				Just initializer -> cinitializer2blockitems (CVar ident undefNode) ty' initializer
					where
					cinitializer2blockitems :: CExpr -> Type -> CInit -> CovVecM [CBlockItem]
					cinitializer2blockitems lexpr ty initializer =
						case initializer of
							CInitExpr expr _ -> return [ CBlockStmt $ CExpr (
								Just $ CAssign CAssignOp lexpr expr undefNode) undefNode ]
							CInitList initlist _ -> case ty of
								DirectType (TyComp (CompTypeRef sueref _ _)) _ _ -> do
									memberidentstypes <- getMembersM sueref
									concatForM (zip initlist memberidentstypes) $ \case
										(([],initializer),(memberident,memberty)) ->
											cinitializer2blockitems (CMember lexpr memberident False undefNode) memberty initializer
										_ -> error $ "unfoldTracesM initializers: CPartDesignators not implemented yet!"
								_ -> error $ "unfoldTracesM initializers: " ++ (render.pretty) ty ++ " is no composite type!"
			return (newenvitems,newdecls,initializers)
		triple -> error $ "unfoldTracesM: triple " ++ show triple ++ " not implemented!"
	let (newenvs,newitems,initializerss) = unzip3 new_env_items
	unfoldTracesM toplevel ((concat newenvs ++ env) : envs) (trace ++ concat newitems) ((concat initializerss ++ rest):rest2)

unfoldTraces1M toplevel (_:restenvs) trace ([]:rest2) = unfoldTracesM toplevel restenvs trace rest2

unfoldTraces1M _ _ trace [] = return trace

unfoldTraces1M _ _ _ ((cbi:_):_) = error $ "unfoldTracesM " ++ (render.pretty) cbi ++ " not implemented yet."

-- Translates all identifiers in an expression to fresh ones,
-- and expands function calls.

translateExprM :: Bool -> [Env] -> CExpr -> CovVecM [(CExpr,Trace)]
translateExprM toplevel envs expr = do
	let
		to_call :: CExpr -> StateT [(Ident,[CExpr],NodeInfo)] CovVecM CExpr
		to_call (CCall funexpr args ni) = case funexpr of
			CVar funident _ -> do
				modify ( (funident,args,ni): )
				return $ CConst $ CStrConst undefined ni
			_  -> error $ "is_call: found call " ++ (render.pretty) funexpr
		to_call expr = return expr
	(expr',calls) <- runStateT (everywhereM (mkM to_call) expr) []

	let expr'' = everywhere (mkT (subst_var envs)) expr'

	funcalls_traces :: [(NodeInfo,[(Trace,CExpr)])] <- forM calls $ \ (funident,args,ni) -> do
		FunDef (VarDecl _ _ (FunctionType (FunType _ paramdecls False) _)) body _ <- lookupFunM funident
		let body' = replace_param_with_arg (zip paramdecls args) body
		funtrace <- unfoldTracesM False envs [] [ [ CBlockStmt body' ] ]
		let funtraces = extract_traces_rets [] funtrace
		return (ni,funtraces) 

	return $ create_combinations expr'' [] funcalls_traces

	where

	extract_traces_rets :: [TraceElem] -> Trace -> [(Trace,CExpr)]
	extract_traces_rets traceelems (Return retexpr : _) = [(traceelems,retexpr)]
	extract_traces_rets traceelems (TraceOr traces : rest) = case rest of
		[] -> concat $ for traces (extract_traces_rets traceelems)
		_ -> error $ "extract_traces_rets: TraceOr not last element " ++ showTrace 0 traceelems
	extract_traces_rets traceelems (TraceAnd traces : rest) = case rest of
		[] -> concat $ for traces (extract_traces_rets traceelems)
		_ -> error $ "extract_traces_rets: TraceAnd not last element in trace " ++ showTrace 0 traceelems
	extract_traces_rets traceelems [] = error $ "trace of no return : " ++ showTrace 0 traceelems
	extract_traces_rets traceelems (te : rest) = extract_traces_rets (te:traceelems) rest

	create_combinations :: CExpr -> Trace -> [(NodeInfo,[(Trace,CExpr)])] -> [(CExpr,Trace)]
	create_combinations expr trace [] = [(expr,trace)]
	create_combinations expr trace ((ni,tes):rest) =
		concat $ for tes $ \ (fun_trace,ret_expr) -> let
			-- substitute the function call by the return expression
			expr' = everywhere (mkT subst_ret_expr) expr
			subst_ret_expr :: CExpr -> CExpr
			subst_ret_expr expr = if nodeInfo expr == ni then ret_expr else expr
			in
			create_combinations expr' (fun_trace++trace) rest

	replace_param_with_arg :: [(ParamDecl,CExpr)] -> CStat -> CStat
	replace_param_with_arg [] body = body
	replace_param_with_arg ((paramdecl,arg):rest) body = replace_param_with_arg rest body' where
		VarDecl (VarName srcident _) _ _ = getVarDecl paramdecl
		body' = everywhere (mkT substparamarg) body
		substparamarg :: CExpr -> CExpr
		substparamarg (CVar ident _) | ident==srcident = arg
		substparamarg expr = expr

subst_var :: [Env] -> CExpr -> CExpr
subst_var envs (CVar ident _) = case lookup ident (concat envs) of
	Just (ident',_) -> CVar ident' undefNode
	Nothing -> error $ " in subst_var : Could not find " ++ (render.pretty) ident ++ " in\n" ++ envToString (concat envs)
subst_var _ expr = expr

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

elimAssignmentsM :: Trace -> CovVecM Trace
elimAssignmentsM trace = foldtraceM [] trace
	where
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


-- MiniZinc Model Generation

type TyEnv = [(Ident,Type)]

var2MZ :: TyEnv -> Ident -> CovVecM [MZAST.ModelData]
var2MZ tyenv ident = do
	let ty = case lookup ident tyenv of
		Just ty -> ty
		Nothing -> error $ "var2MZ: Could not find " ++ (render.pretty) ident ++ " in\n" ++
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
				TyInt -> return $ MZAST.Range (MZAST.IConst (-30)) (MZAST.IConst 30) --MZAST.Int
				_ -> error $ "ty2mz " ++ (render.pretty) ty ++ " not implemented yet"
			TyFloating floatty -> case floatty of
				TyFloat -> return MZAST.Float
				_ -> error $ "ty2mz " ++ (render.pretty) ty ++ " not implemented yet"
			TyEnum (EnumTypeRef sueref _) -> do
				EnumDef (EnumType _ enums _ _) <- lookupTagM sueref
				return $ MZAST.CT $ MZAST.SetLit $
					map (\ (Enumerator _ (CConst (CIntConst (CInteger i _ _) _)) _ _) ->
						MZAST.IConst (fromIntegral i)) enums
			TyComp (CompTypeRef sueref _ _) -> do
				error "CompTypeRef not implemented yet"
			_ -> error $ "ty2mz " ++ (render.pretty) ty ++ " not implemented yet"
		ty2mz (PtrType target_ty _ _) = return $
			MZAST.Range (MZAST.IConst 0) (MZAST.IConst 65535)
		ty2mz ty = error $ "ty2mz " ++ (render.pretty) ty ++ " not implemented yet"

type Constraint = CExpr

traceelemToMZ :: TraceElem -> CovVecM [MZAST.ModelData]
traceelemToMZ (Condition constr) = do
--	liftIO $ putStrLn $ (render.pretty) constr
	return [ MZAST.constraint (expr2constr . (flatten_not False) . (insert_eq0 True) $ constr) ]
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
--	expr2constr (CMember (CVar ptrident _) member _ _) = expr2constr ()
	expr2constr expr = error $ "expr2constr " ++ show expr ++ " not implemented yet"

traceelemToMZ _ = return []

{-
type ResultData = (Trace,([MZAST.ModelData],Maybe (Env,Solution,Maybe CExpr)))
type TraceAnalysisResult = (Int,[(Int,ResultData)])
-}
solveTraceM :: Type -> Env -> [Int] -> Trace -> CovVecM ResultData
solveTraceM _ _ _ trace | not solveIt = return ([],Nothing)
solveTraceM ret_type param_env traceid trace = do
	let
		tracename = show traceid

	let mb_ret_val = case last trace of
		Return ret_expr -> Just ret_expr
		_               -> Nothing

	let
		returnval_ident = internalIdent returnval_var_name
		trace' = trace ++ case mb_ret_val of
			Nothing -> []
			Just ret_expr -> [
				Condition $ CBinary CEqOp (CVar returnval_ident undefNode) ret_expr undefNode,
				NewDeclaration (returnval_ident,ret_type) ]

	constraintsG <- concatMapM traceelemToMZ trace'
	
	let tyenv = concatMap traceitem2tyenv trace' where
		traceitem2tyenv (NewDeclaration tyenvitem) = [tyenvitem]
		traceitem2tyenv _ = []
	let constr_trace = concatMap traceitem2constr trace' where
		traceitem2constr (Condition expr) = [expr]
		traceitem2constr _ = []
	let
		includesG = [ MZAST.include "include.mzn" ]
		vars :: [Ident] = nub $ everything (++) (mkQ [] searchvar) constr_trace where
			searchvar :: CExpr -> [Ident]
			searchvar (CVar ident _) = [ ident ]
			searchvar _ = []

	varsG <- concatMapM (var2MZ tyenv) vars
	let
		solution_vars = returnval_var_name : (
			(map (\ (_,(ident,_)) -> identToString ident) param_env)
			`intersect`
			(map identToString vars) )
		model = includesG ++ varsG ++ [] ++ constraintsG ++ [] ++
			[ MZAST.solve $ MZAST.satisfy MZAST.|: MZAST.Annotation "int_search" [
				MZAST.E (MZAST.ArrayLit $ map (MZAST.Var . MZAST.Simpl) solution_vars),
				MZAST.E (MZAST.Var $ MZAST.Simpl "input_order"),
				MZAST.E (MZAST.Var $ MZAST.Simpl "indomain_min"),
				MZAST.E (MZAST.Var $ MZAST.Simpl "complete") ] ]

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

	return (model,mb_solution)

checkSolutionM :: [Int] -> ResultData -> CovVecM ResultData
checkSolutionM _ resultdata | not checkSolutions = return resultdata
checkSolutionM traceid resultdata@(_,Nothing) = do
	printLog $ "No solution to check for " ++ show traceid
	return resultdata
checkSolutionM traceid resultdata@(_,Just (_,[],_)) = do
	printLog $ "Empty solution cannot be checked for " ++ show traceid
	return resultdata
checkSolutionM traceid resultdata@(_,Just (env,solution,Just res_expr)) = do
	srcfilename <- gets srcFilenameCVS
	Just filename <- gets checkExeNameCVS
	absolute_filename <- liftIO $ makeAbsolute srcfilename
	let
		args = for env $ \ (_,(newident,_)) -> case lookup (identToString newident) solution of
			Nothing -> "99"
			Just (MInt i) -> show i
			Just (MFloat f) -> show f
			val -> error $ "checkSolutionM: " ++ show val ++ " not yet implemented"
	(exitcode,stdout,stderr) <- liftIO $ withCurrentDirectory (takeDirectory absolute_filename) $ do
		readProcessWithExitCode (takeFileName filename) args ""
	case exitcode of
		ExitFailure _ -> error $ "Execution of " ++ filename ++ " failed:\n" ++ stdout ++ stderr
		ExitSuccess -> do
			let exec_result = (read $ last $ lines stdout) :: Int
			let (MInt predicted_result) = getPredictedResult solution
			case exec_result == predicted_result of
				False -> printLog $ "ERROR in " ++ show traceid ++ " exec_result=" ++ show exec_result ++ " /= predicted_result=" ++ show predicted_result
				True  -> printLog $ "checkSolutionM " ++ show traceid ++ " OK."
			return resultdata

getPredictedResult solution = predicted_result
	where
	Just predicted_result = lookup returnval_var_name solution
