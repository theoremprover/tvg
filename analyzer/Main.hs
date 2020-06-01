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
import Language.C.Data.Node
import Language.C.Data.Position
import Language.C.Analysis.AstAnalysis
import Language.C.Analysis.TravMonad
import Language.C.Analysis.SemRep
import Language.C.System.GCC
import Control.Monad
import Prelude.Unicode
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import qualified Data.Set as Set

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
showOnlySolutions = True
don'tShowTraces = True
checkSolutions = solveIt && False

returnval_var_name = "return_val"

_UNROLLING_DEPTH = 5

analyzerPath = "analyzer"
logFile = analyzerPath </> "log.txt"

printLog :: (MonadIO m) => String -> m ()
printLog text = liftIO $ do
	putStrLn text
	appendFile logFile (text++"\n")

showLine :: Trace -> String
showLine trace = unlines $ map show (filter isnotbuiltin trace)

show_solution _ Nothing = "No solution"
show_solution _ (Just (_,[],_)) = "Empty solution"
show_solution funname (Just v@(_,solution,_)) = unlines [ show solution, showTestVector funname v ]

main = do
	hSetBuffering stdout NoBuffering

	gcc:filename:funname:opts <- getArgs >>= return . \case
--		[] -> "gcc" : (analyzerPath++"\\test.c") : "g" : [] --["-writeAST","-writeGlobalDecls"]
		[] -> "gcc" : (analyzerPath++"\\myfp-bit.c") : "_fpdiv_parts" : [] --"-writeAST","-writeGlobalDecls"]
--		[] -> "gcc" : (analyzerPath++"\\branchtest.c") : "f" : ["-writeTree"] --["-writeAST","-writeGlobalDecls"]
--		[] -> "gcc" : (analyzerPath++"\\iftest.c") : "f" : [] --["-writeAST","-writeGlobalDecls"]
--		[] -> "gcc" : (analyzerPath++"\\deadtest.c") : "f" : [] --["-writeAST","-writeGlobalDecls"]
--		[] -> "gcc" : (analyzerPath++"\\whiletest.c") : "f" : [] --["-writeAST","-writeGlobalDecls"]
--		[] -> "gcc" : (analyzerPath++"\\ptrtest_flat.c") : "f" : ["-writeAST"]
--		[] -> "gcc" : (analyzerPath++"\\ptrtest.c") : "f" : ["-writeTree"] --["-writeAST"]
--		[] -> "gcc" : (analyzerPath++"\\assigntest.c") : "g" : [] --["-writeAST","-writeGlobalDecls"]
--		[] -> "gcc" : (analyzerPath++"\\ptrrettest.c") : "g" : [] --["-writeAST","-writeGlobalDecls"]
--		[] -> "gcc" : (analyzerPath++"\\calltest.c") : "g" : ["-writeTraceTree"] --["-writeAST","-writeGlobalDecls"]
		args -> args

	getZonedTime >>= return.(++"\n\n").show >>= writeFile logFile
	
	parseCFile (newGCC gcc) Nothing [] filename
		>>= \case
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
	
						(full_coverage,(testvectors,covered,alls)) <- evalStateT (covVectorsM filename opts) $
							CovVecState globdecls 1 translunit filename mb_checkexename funname undefined
	
						printLog ""
	
						let deaths = Set.toList $ Set.difference alls covered
	
						when (full_coverage && not (null deaths)) $ error "full coverage but deaths!"
						when (not full_coverage && null deaths) $ error "coverage gaps but no deaths!"
	
						printLog $ "\n####### FINAL RESULT #######\n"
	
						forM_ testvectors $ \ (traceid,trace,(model,mb_solution)) -> do
							case not showOnlySolutions || maybe False (not.null.(\(_,b,_)->b)) mb_solution of
								False -> return ()
								True -> printLog $ unlines $ mbshowtraces (
									[ "","=== TRACE " ++ show traceid ++ " ========================","<leaving out builtins...>" ] ++
									[ showLine trace ] ++
									[ "",
									"--- MODEL " ++ show traceid ++ " -------------------------",
									if null model then "<empty>" else layout model,
									"" ]) ++ [
									"--- SOLUTION " ++ show traceid ++ " ----------------------",
									show_solution funname mb_solution ]
									where
									mbshowtraces ts = if don'tShowTraces then [] else ts
	
						printLog $ "\n===== SUMMARY =====\n"
	
						forM_ testvectors $ \ (traceid,trace,(model,Just v)) -> do
							printLog $ "Test Vector covering " ++ show traceid ++ " : "
							printLog $ "    " ++ showTestVector funname v ++ "\n"
						forM_ deaths $ \ branch -> do
							printLog $ "DEAD " ++ show branch ++ "\n"
	
						printLog $ case full_coverage of
							False -> "FAIL, there are coverage gaps!"
							True  -> "OK, we have full branch coverage."

showEnv :: Env -> String
showEnv env = "{\n    " ++ intercalate " ,\n    " (map (render.pretty) env) ++ "\n    }"

showTestVector :: String -> (Env,Solution,Maybe CExpr) -> String
showTestVector funname (env,solution,mb_retval) = funname ++ " ( " ++ intercalate " , " (map showarg env) ++ " )" ++
	" = " ++ maybe "<NO_RETURN>" (const $ show $ getPredictedResult solution) mb_retval
	where
	showarg :: EnvItem -> String
	showarg (oldident,(newident,_)) =
		identToString oldident ++ " = " ++ case lookup (identToString newident) solution of
			Nothing         -> "DONT_CARE"
			Just (MInt i)   -> show i
			Just (MFloat f) -> show f
			val             -> error $ "showarg " ++ show val ++ " not yet implemented"

data CovVecState = CovVecState {
	globDeclsCVS    :: GlobalDecls,
	newNameIndexCVS :: Int,
	translUnitCVS   :: CTranslUnit,
	srcFilenameCVS  :: String,
	checkExeNameCVS :: Maybe String,
	funNameCVS      :: String,
	funStartEndCVS  :: ((Int,Int),(Int,Int))
	}
type CovVecM = StateT CovVecState IO

data TraceElem =
	Assignment CExpr CExpr |
	Condition Bool CExpr |
	NewDeclaration (Ident,Type) |
	Return CExpr |
	TraceOr [Trace] |
	TraceAnd [Trace]
	deriving Data

data Branch = Then Location | Else Location
	deriving (Eq,Ord)
instance Show Branch where
	show (Then loc) = "Then branch in " ++ showLocation loc
	show (Else loc) = "Else branch in " ++ showLocation loc

instance CNode TraceElem where
	nodeInfo (Assignment lexpr _)       = nodeInfo lexpr
	nodeInfo (Condition _ expr)         = nodeInfo expr
	nodeInfo (NewDeclaration (ident,_)) = nodeInfo ident
	nodeInfo (Return expr)              = nodeInfo expr
	nodeInfo (TraceOr (tr:_))           = nodeInfo $ head tr
	nodeInfo (TraceAnd (tr:_))          = nodeInfo $ head tr

instance Pretty NodeInfo where
	pretty ni = text $ "line " ++ show line ++ ", col " ++ show col
		where
		(line,col) = lineColNodeInfo ni

traceToHTMLString :: Trace -> String
traceToHTMLString trace = dataTreeToHTMLString [ trace2datatree trace ]
	where
	trace2datatree :: Trace -> DataTree
	trace2datatree trace = DataTree "list" $ map conv (filter isnotbuiltin trace)
	conv :: TraceElem -> DataTree
	conv (TraceOr traces)  = DataTree "OR" $ map trace2datatree traces
	conv (TraceAnd traces) = DataTree "AND" $ map trace2datatree traces
	conv other             = Leaf $ show other

instance Show TraceElem where
	show te = ( case te of
		(Assignment lvalue expr)   -> "ASSN " ++ (render.pretty) lvalue ++ " = " ++ (render.pretty) expr
		(Condition b expr)         -> "COND " ++ (if b then "(THEN) " else "(ELSE) ") ++ (render.pretty) expr
		(NewDeclaration (lval,ty)) -> "DECL " ++ (render.pretty) lval ++ " :: " ++ (render.pretty) ty
		(Return expr)              -> "RET  " ++ (render.pretty) expr
		(TraceOr traces)           -> "OR   " ++ show traces
		(TraceAnd traces)          -> "AND  " ++ show traces
		) ++ "  (" ++ (render.pretty) (nodeInfo te) ++ ")"

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

covVectorsM :: String -> [String] -> CovVecM (Bool,([TraceAnalysisResult],Set.Set Branch,Set.Set Branch))
covVectorsM filename opts = do
	funname <- gets funNameCVS
	globdecls <- gets ((Map.elems).gObjs.globDeclsCVS)
	glob_env <- concatMapM declaration2EnvItemM globdecls
	let
		-- creates the assignment statements from the global context
		defs = concatMap def2stmt globdecls
		def2stmt :: IdentDecl -> [CBlockItem]
		def2stmt ed@(EnumeratorDef (Enumerator ident expr _ _)) = assnstmt ident expr (nodeInfo ed)
		def2stmt od@(ObjectDef (ObjDef (VarDecl (VarName ident _) _ _) (Just (CInitExpr expr _)) _)) = assnstmt ident expr (nodeInfo od)
		def2stmt _ = []
		assnstmt ident expr ni = [ CBlockStmt (CExpr (Just $ CAssign CAssignOp (CVar ident (nodeInfo ident)) expr ni) ni) ] 

	FunDef (VarDecl _ _ (FunctionType (FunType ret_type funparamdecls False) _)) body fundef_ni <-
		lookupFunM (builtinIdent funname)
	
	let
		fun_lc = lineColNodeInfo fundef_ni
		next_lc = case sort $ filter (> lineColNodeInfo fundef_ni) $ map lineColNodeInfo globdecls of
			[] -> (9999999999,9999999999)
			next : _ -> next
	modify $ \ s -> s { funStartEndCVS = (fun_lc,next_lc) }

	param_env <- createInterfaceM funparamdecls
	printLog $ "param_env = " ++ showEnv param_env
	
	let decls = map (NewDeclaration .snd) (reverse param_env ++ glob_env)

	trace <- unfoldTracesM True (param_env:[glob_env]) decls [ defs ++ [ CBlockStmt body ] ]
	when ("-writeTree" ∈ opts) $ liftIO $ writeFile (filename ++ "_tree" <.> "html") $ traceToHTMLString trace

	runStateT (analyzeTreeM opts ret_type param_env [] [] trace) ([],Set.empty,Set.empty)

type Location = (Int,Int)

showLocation :: Location -> String
showLocation (l,c) = "line " ++ show l ++ ", col " ++ show c

type AnalyzeTreeM a = StateT ([TraceAnalysisResult],Set.Set Branch,Set.Set Branch) CovVecM a
-- Unfolds the tree to all execution paths, collecting the solutions and promoting them upwards.

analyzeTreeM :: [String] -> Type -> Env -> [Int] -> [TraceElem] -> Trace -> AnalyzeTreeM Bool

analyzeTreeM opts ret_type param_env traceid res_line [] = do
--	printLog $ "=== TRACE " ++ show traceid ++ " ========================\n<leaving out builtins...>\n"
--	printLog $ showLine res_line
	
	res_trace <- lift $ elimInds res_line
	when (not don'tShowTraces) $ printLog $ "\n=== TRACE after elimInds " ++ show traceid ++ " =========\n<leaving out builtins...>\n"
	when (not don'tShowTraces) $ printLog $ showLine res_trace
	
	res_trace' <- lift $ elimAssignmentsM res_trace
	when (not don'tShowTraces) $ printLog $ "\n--- TRACE after elimAssignmentsM " ++ show traceid ++ " -----------\n<leaving out builtins...>\n"
	when (not don'tShowTraces) $ printLog $ showLine res_trace'

	res_trace'' <- lift $ substIndM [] (map fst $ createTyEnv res_trace') res_trace'
	when (not don'tShowTraces) $ printLog $ "\n--- TRACE after substIndM " ++ show traceid ++ " -----------\n<leaving out builtins...>\n"
	when (not don'tShowTraces) $ printLog $ showLine res_trace''

	resultdata@(model,mb_solution) <- lift $ solveTraceM ret_type param_env traceid res_trace''
	when (not don'tShowTraces) $ printLog $ "\n--- MODEL " ++ show traceid ++ " -------------------------\n" ++
		if null model then "<empty>" else layout model
	funname <- lift $ gets funNameCVS
	printLog $ "\n--- SOLUTION " ++ show traceid ++ " ----------------------\n" ++ show_solution funname mb_solution

	startend <- lift $ gets funStartEndCVS
	let visible_trace = Set.fromList $ concatMap to_branch res_line
		where
		to_branch cond@(Condition b _) | is_visible_traceelem startend cond =
			[ (if b then Then else Else) (lineColNodeInfo cond) ]
		to_branch _ = []

	let traceanalysisresult :: TraceAnalysisResult = (traceid,res_line,resultdata)
	case is_solution traceanalysisresult of
		False -> do
			modify $ \ (tas,covered,alls) -> (tas,covered,Set.union visible_trace alls)
			return False
		True  -> do
			lift $ checkSolutionM traceid resultdata
			modify $ \ (tas,covered,alls) -> case visible_trace `Set.isSubsetOf` covered of
				False -> (traceanalysisresult:tas,Set.union visible_trace covered,Set.union visible_trace alls)
				True  -> (tas,covered,alls)
			return True

analyzeTreeM opts ret_type param_env traceid res_line (TraceOr traces : rest) = case rest of
	[] -> try_traces (zip [1..] traces)
		where
		try_traces :: [(Int,Trace)] -> AnalyzeTreeM Bool
		try_traces [] = return False
		try_traces ((i,trace):rest) = do
			success <- analyzeTreeM opts ret_type param_env (traceid++[i]) res_line trace
			case success of
				True -> return True
				False -> try_traces rest
	_ -> error $ "analyzeTreeM: TraceOr not last element in " ++ showTrace 1 res_line

analyzeTreeM opts ret_type param_env traceid res_line (TraceAnd traces : rest) = case rest of
	[] -> do
		results <- forM (zip [1..] traces) $ \ (i,trace) ->
			analyzeTreeM opts ret_type param_env (traceid++[i]) res_line trace
		return $ all (==True) results
	_ -> error $ "analyzeTreeM: TraceAnd not last element in " ++ showTrace 1 res_line

analyzeTreeM opts ret_type param_env traceid res_line (te:rest) = do
	analyzeTreeM opts ret_type param_env traceid (te:res_line) rest

is_solution :: TraceAnalysisResult -> Bool
is_solution (_,_,(_,Just (_,solution,_))) = not $ null solution
is_solution _ = False

is_visible_traceelem :: (CNode a) => ((Int,Int),(Int,Int)) -> a -> Bool
is_visible_traceelem (start,end) cnode = start <= lc && lc < end where
	lc = lineColNodeInfo cnode

lineColNodeInfo :: (CNode a) => a -> Location
lineColNodeInfo cnode = if isSourcePos pos_te then (posRow pos_te,posColumn pos_te) else (-1,-1)
	where
	pos_te = posOfNode $ nodeInfo cnode

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
	CMember (CUnary CAdrOp expr _) member True ni -> normalizeExpr $ CMember expr member False ni
	CMember (CUnary CIndOp expr _) member False ni -> normalizeExpr $ CMember expr member True ni
	expr -> lValueToVarName expr
--	other -> error $ "normalizeExpr " ++ (render.pretty) other ++ " not implemented yet."

	where

	const2str (CIntConst cint _) = (if i<0 then "m" else "") ++ show (abs i)
		where
		i = getCInteger cint
	const2str x = error $ "const2str " ++ (render.pretty) x ++ " not implemented"

lValueToVarName :: CExpr -> String
lValueToVarName cvar@(CVar _ _) = normalizeExpr cvar
lValueToVarName (CMember ptrexpr member isptr _) =
	normalizeExpr ptrexpr ++ (if isptr then "_ARROW_" else "_DOT_") ++ identToString member
lValueToVarName (CUnary CIndOp expr _) = "PTR_" ++ lValueToVarName expr
lValueToVarName (CUnary CAdrOp expr _) = "ADR_" ++ lValueToVarName expr
lValueToVarName lval = error $ "lValueToVarName " ++ (render.pretty) lval ++ " not implemented!"

type TyEnvItem = (Ident,Type)
type EnvItem = (Ident,TyEnvItem)
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
	forM memberdecls $ \ (MemberDecl (VarDecl (VarName ident _) _ ty) Nothing _) -> do
		ty' <- elimTypeDefsM ty
		return (ident,ty')

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


-- Extracts the declared identifer and the type from a Declaration

declaration2EnvItemM :: Declaration decl => decl -> CovVecM [EnvItem]
declaration2EnvItemM decl = do
	let VarDecl (VarName srcident@(Ident srcname i ni) _) _ ty = getVarDecl decl
	ty' <- elimTypeDefsM ty
	let srcident' = if "_" `isPrefixOf` srcname then Ident ('a':srcname) i ni else srcident
	return $ [ (srcident,(srcident',ty')) ]

mkIdentWithCNodePos :: (CNode cnode) => cnode -> String -> Ident
mkIdentWithCNodePos cnode name = mkIdent (posOfNode $ nodeInfo cnode) name (Name 99999)


-- Takes an identifier and a type, and creates env item(s) from that.

identTy2EnvItemM :: Ident -> Type -> CovVecM [EnvItem]
identTy2EnvItemM srcident@(Ident _ i ni) ty = do
	ty' <- elimTypeDefsM ty
	new_var_num <- gets newNameIndexCVS
	modify $ \ s -> s { newNameIndexCVS = newNameIndexCVS s + 1 }
	let
		name_prefix = lValueToVarName (CVar srcident (nodeInfo srcident))
		newident = Ident (name_prefix ++ "_" ++ show new_var_num) i ni
	return $ [ (srcident,(newident,ty')) ]


-- Recursively create all "interface" variables for the top level function to be analyzed

createInterfaceM :: (Declaration decl) => [decl] -> CovVecM [EnvItem]
createInterfaceM decls = concatForM decls $ \ decl -> do
	let VarDecl (VarName srcident _) _ ty = getVarDecl decl
	ty' <- elimTypeDefsM ty
	create_interfaceM (CVar srcident (nodeInfo srcident)) ty'
	
	where

	mk_envitemM :: CExpr -> Type -> [EnvItem] -> CovVecM [EnvItem]
	mk_envitemM expr ty rest = do
		ty' <- elimTypeDefsM ty
		let srcident = mkIdentWithCNodePos expr (lValueToVarName expr)
		return $ (srcident,(srcident,ty')) : rest
		
	create_interfaceM :: CExpr -> Type -> CovVecM [EnvItem]

	-- STRUCT* p
	create_interfaceM expr ty@(PtrType (DirectType (TyComp (CompTypeRef sueref _ _)) _ _) _ _) = do
		member_ty_s <- getMembersM sueref
		members <- concatForM member_ty_s $ \ (m_ident,m_ty) -> create_interfaceM (CMember expr m_ident True (nodeInfo expr)) m_ty
		mk_envitemM expr ty members

	-- ty* p
	create_interfaceM expr ty@(PtrType target_ty _ _) = do
		target_ty' <- elimTypeDefsM target_ty
		targets <- create_interfaceM (CUnary CIndOp expr (nodeInfo expr)) target_ty'
		mk_envitemM expr ty targets

	-- STRUCT expr
	create_interfaceM expr ty@(DirectType (TyComp (CompTypeRef sueref _ _)) _ _) = do
		member_ty_s <- getMembersM sueref
		members <- concatForM member_ty_s $ \ (m_ident,m_ty) -> do
			m_ty' <- elimTypeDefsM m_ty
			create_interfaceM (CMember expr m_ident False (nodeInfo expr)) m_ty'
		mk_envitemM expr ty members

	-- direct-type expr  where  direct-type is no struct/union
	create_interfaceM expr ty@(DirectType _ _ _) = do
		mk_envitemM expr ty []

	create_interfaceM expr ty = error $ "create_interfaceM " ++ (render.pretty) expr ++ " " ++ (render.pretty) ty ++ " not implemented"


-- Just unfold the traces
unfoldTracesM :: Bool -> [Env] -> Trace -> [[CBlockItem]] -> CovVecM Trace
unfoldTracesM toplevel envs trace cbss = do
	cbss_txt <- case cbss of
		[] -> return "[]"
		(l : _) -> return $ "[ " ++ (intercalate " , " (map (render.pretty) l)) ++ " ] : _"
	res <- unfoldTraces1M toplevel envs trace cbss
--	printLog $ "============================================================================="
--	printLog $ "unfoldTracesM " ++ show toplevel ++ " envs trace " ++ cbss_txt
--	printLog $ showTrace 0 res
	return res

unfoldTraces1M :: Bool -> [Env] -> Trace -> [[CBlockItem]] -> CovVecM Trace
unfoldTraces1M toplevel envs trace ((CBlockStmt stmt : rest) : rest2) = case stmt of

	CLabel _ cstat _ _ -> unfoldTracesM toplevel envs trace ((CBlockStmt cstat : rest) : rest2)

	CCompound _ cbis _ -> unfoldTracesM toplevel ([]:envs) trace (cbis : (rest : rest2))

	CIf cond then_stmt mb_else_stmt ni -> do
		transids cond trace $ \ (cond',trace') -> do
			then_trace <- unfoldTracesM toplevel envs (Condition True cond' : trace') ( (CBlockStmt then_stmt : rest) : rest2 )
			let not_cond = Condition False (CUnary CNegOp cond' (nodeInfo cond'))
			else_trace <- case mb_else_stmt of
				Nothing        -> unfoldTracesM toplevel envs (not_cond : trace') ( rest : rest2 )
				Just else_stmt -> unfoldTracesM toplevel envs (not_cond : trace') ( (CBlockStmt else_stmt : rest) : rest2 )
			return $ [ (if toplevel then TraceAnd else TraceOr) [ then_trace, else_trace ] ]

	CReturn Nothing _ -> return trace
	CReturn (Just ret_expr) _ -> do
		transids ret_expr trace $ \ (ret_expr',trace') -> do
			return $ Return ret_expr' : trace'

	CExpr (Just cass@(CAssign assignop lexpr assigned_expr ni)) _ -> do
		transids assigned_expr' trace $ \ (assigned_expr'',trace') ->
			transids lexpr trace' $ \ (lexpr',trace'') -> do
				unfoldTracesM toplevel envs (Assignment lexpr' assigned_expr'' : trace'') (rest:rest2)
		where
		mb_binop = lookup assignop [
			(CMulAssOp,CMulOp),(CDivAssOp,CDivOp),(CRmdAssOp,CRmdOp),(CAddAssOp,CAddOp),(CSubAssOp,CSubOp),
			(CShlAssOp,CShlOp),(CShrAssOp,CShrOp),(CAndAssOp,CAndOp),(CXorAssOp,CXorOp),(COrAssOp,COrOp) ]
		assigned_expr' = case mb_binop of
			Nothing -> assigned_expr
			Just binop -> CBinary binop lexpr assigned_expr ni

	CExpr (Just (CUnary unaryop expr ni_op)) ni | unaryop ∈ map fst unaryops -> do
		unfoldTracesM toplevel envs trace ( (CBlockStmt stmt' : rest) : rest2 )
		where
		stmt' = CExpr (Just $ CAssign assignop expr (CConst $ CIntConst (cInteger 1) ni_op) ni) ni
		Just assignop = lookup unaryop unaryops
		unaryops = [ (CPreIncOp,CAddAssOp),(CPostIncOp,CAddAssOp),(CPreDecOp,CSubAssOp),(CPostDecOp,CSubAssOp) ]

	CExpr (Just expr) _ -> do
		error $ "not implemented yet."

	-- I'd like to create an algorithm that infers the invariant from the loop's body, so I don't have to unroll...
 	CWhile cond body False ni -> unfoldTracesM toplevel envs trace ((unroll_loop _UNROLLING_DEPTH ++ rest) : rest2 )
		where
		unroll_loop :: Int -> [CBlockItem]
		unroll_loop 0 = []
		unroll_loop i = [ CBlockStmt $ CIf cond (CCompound [] (CBlockStmt body : unroll_loop (i-1)) ni) Nothing (nodeInfo body) ]

	_ -> error $ "unfoldTracesM " ++ (render.pretty) stmt ++ " not implemented yet"

	where
	
	transids :: CExpr -> Trace -> ((CExpr,Trace) -> CovVecM Trace) -> CovVecM Trace
	transids expr trace cont = do
		additional_expr_traces :: [(CExpr,Trace)] <- translateExprM toplevel envs expr
		conts :: [Trace] <- forM additional_expr_traces $ \ (expr',trace') -> do
			cont (expr',trace'++trace)
		case conts of
			[] -> error $ "transids Strange: conts empty!"
			[e] -> return e
			conts -> return [ TraceOr conts ]

unfoldTraces1M toplevel (env:envs) trace ( (CBlockDecl (CDecl [CTypeSpec typespec] triples _) : rest) : rest2 ) = do
	ty <- tyspec2TypeM typespec
	new_env_items <- forM triples $ \case
		(Just (CDeclr (Just ident) derivdeclrs _ _ ni),mb_init,Nothing) -> do
			let ty' = case derivdeclrs of
				[] -> ty
				[CPtrDeclr _ _] -> PtrType ty noTypeQuals noAttributes
			newenvitems <- identTy2EnvItemM ident ty'
			let newdecls = map (NewDeclaration . snd) newenvitems
			initializers <- case mb_init of
				Nothing -> return []
				Just initializer -> cinitializer2blockitems (CVar ident ni) ty' initializer
					where
					cinitializer2blockitems :: CExpr -> Type -> CInit -> CovVecM [CBlockItem]
					cinitializer2blockitems lexpr ty initializer =
						case initializer of
							CInitExpr expr ni_init -> return [ CBlockStmt $ CExpr (
								Just $ CAssign CAssignOp lexpr expr ni_init) ni_init ]
							CInitList initlist _ -> case ty of
								DirectType (TyComp (CompTypeRef sueref _ _)) _ _ -> do
									memberidentstypes <- getMembersM sueref
									concatForM (zip initlist memberidentstypes) $ \case
										(([],initializer),(memberident,memberty)) ->
											cinitializer2blockitems (CMember lexpr memberident False (nodeInfo memberident)) memberty initializer
										_ -> error $ "unfoldTracesM initializers: CPartDesignators not implemented yet!"
								_ -> error $ "unfoldTracesM initializers: " ++ (render.pretty) ty ++ " is no composite type!"
			return (newenvitems,newdecls,initializers)
		triple -> error $ "unfoldTracesM: triple " ++ show triple ++ " not implemented!"
	let (newenvs,newitems,initializerss) = unzip3 $ reverse new_env_items
	unfoldTracesM toplevel ((concat newenvs ++ env) : envs) (concat newitems ++ trace) ((concat initializerss ++ rest):rest2)

unfoldTraces1M toplevel (_:restenvs) trace ([]:rest2) = unfoldTracesM toplevel restenvs trace rest2

unfoldTraces1M _ _ trace [] = return trace

unfoldTraces1M _ _ _ ((cbi:_):_) = error $ "unfoldTracesM " ++ (render.pretty) cbi ++ " not implemented yet."

-- Translates all identifiers in an expression to fresh ones,
-- and expands function calls.

translateExprM :: Bool -> [Env] -> CExpr -> CovVecM [(CExpr,Trace)]
translateExprM toplevel envs expr = do
--	printLog $ "===== translateExpr " ++ (render.pretty) expr ++ " ==============================="

	let	
		to_call :: CExpr -> StateT [(Ident,[CExpr],NodeInfo)] CovVecM CExpr
		to_call (CCall funexpr args ni) = case funexpr of
			CVar (Ident "__builtin_expect" _ _) _ -> return $ head args
			CVar funident _ -> do
				modify ( (funident,args,ni): )
				return $ CConst $ CStrConst undefined ni
			_  -> error $ "is_call: found call " ++ (render.pretty) funexpr
		to_call expr = return expr
	(expr',calls) <- runStateT (everywhereM (mkM to_call) expr) []

	let
		expr'' = renameVars envs expr'

	funcalls_traces :: [(NodeInfo,[(Trace,CExpr)])] <- forM calls $ \ (funident,args,ni) -> do
		FunDef (VarDecl _ _ (FunctionType (FunType _ paramdecls False) _)) body _ <- lookupFunM funident
		expanded_params_args <- expand_params_argsM paramdecls args
		let body' = replace_param_with_arg expanded_params_args body
		funtrace <- unfoldTracesM False envs [] [ [ CBlockStmt body' ] ]
		printLog $ "##### extract_traces_rets " ++ showTrace 0 (reverse funtrace) ++ "\n"
		let funtraces = extract_traces_rets [] (reverse funtrace)
		printLog $ "#### = " ++ show (map (\(tr,cex) -> (tr,(render.pretty) cex)) funtraces) ++ "\n"
		return (ni,funtraces) 

	combs <- create_combinations expr'' [] funcalls_traces

--	printLog $ "===== END OF translateExpr " ++ (render.pretty) expr ++ " ===============================\n"

	return combs
	
	where

	expand_params_argsM ::  [ParamDecl] -> [CExpr] -> CovVecM [(Ident,CExpr)]
	expand_params_argsM paramdecls args = concatForM (zip paramdecls args) expandparam where
		expandparam :: (ParamDecl,CExpr) -> CovVecM [(Ident,CExpr)]
		expandparam (paramdecl,arg) = do
			let VarDecl (VarName srcident _) _ arg_ty = getVarDecl paramdecl
			return [(srcident,arg)]


	-- Flattens the trace tree to a list of all paths through the tree, together with their return exprs

	extract_traces_rets :: [TraceElem] -> Trace -> [(Trace,CExpr)]
	extract_traces_rets traceelems (Return retexpr : _) = [(traceelems,retexpr)]
	extract_traces_rets traceelems (TraceOr traces : rest) = case rest of
		[] -> concat $ for (map reverse traces) (extract_traces_rets traceelems)
		_ -> error $ "extract_traces_rets: TraceOr not last element " ++ showTrace 0 traceelems
	extract_traces_rets traceelems (TraceAnd traces : rest) = case rest of
		[] -> concat $ for (map reverse traces) (extract_traces_rets traceelems)
		_ -> error $ "extract_traces_rets: TraceAnd not last element in trace " ++ showTrace 0 traceelems
	extract_traces_rets traceelems [] = error $ "trace of no return : " ++ showTrace 0 traceelems
	extract_traces_rets traceelems (te : rest) = extract_traces_rets (te:traceelems) rest

	create_combinations :: CExpr -> Trace -> [(NodeInfo,[(Trace,CExpr)])] -> CovVecM [(CExpr,Trace)]
	create_combinations expr trace [] = return [(expr,trace)]
	create_combinations expr trace ((ni,tes):rest) = do
		concatForM tes $ \ (fun_trace,ret_expr) -> do
			let
				-- substitute the function call by the return expression
				expr' = everywhere (mkT subst_ret_expr) expr
				subst_ret_expr :: CExpr -> CExpr
				subst_ret_expr expr = if nodeInfo expr == ni then ret_expr else expr
--			printLog $ "fun_trace=" ++ show fun_trace
			create_combinations expr' (fun_trace++trace) rest

	-- β-reduction
	replace_param_with_arg :: [(Ident,CExpr)] -> CStat -> CStat
	replace_param_with_arg [] body = body
	replace_param_with_arg ((srcident,arg):rest) body = replace_param_with_arg rest body' where
		body' = everywhere (mkT substparamarg) body
		substparamarg :: CExpr -> CExpr
		substparamarg (CVar ident _) | ident==srcident = arg
		substparamarg expr = expr


-- Renames Variables to unique names

renameVars :: [Env] -> CExpr -> CExpr
renameVars envs expr = everywhere (mkT subst_var) expr where
	subst_var :: CExpr -> CExpr
	subst_var (CVar ident ni) = case lookup ident (concat envs) of
		Just (ident',_) -> CVar ident' ni
		Nothing -> error $ " in subst_var : Could not find " ++ (render.pretty) ident ++ " in\n" ++ envToString (concat envs)
--	subst_var expr@(CMember _ _ _ ni) = CVar (mkIdentWithCNodePos expr (lValueToVarName expr)) ni
	subst_var expr = expr

tyspec2TypeM :: CTypeSpec -> CovVecM Type
tyspec2TypeM typespec = case typespec of
	CVoidType _  -> return $ DirectType TyVoid noTypeQuals noAttributes
	CIntType _   -> return $ DirectType (TyIntegral TyInt) noTypeQuals noAttributes
	CCharType _  -> return $ DirectType (TyIntegral TyChar) noTypeQuals noAttributes
	CShortType _ -> return $ DirectType (TyIntegral TyShort) noTypeQuals noAttributes
	CFloatType _ -> return $ DirectType (TyFloating TyFloat) noTypeQuals noAttributes
	CTypeDef ident _ -> lookupTypeDefM ident
	_ -> error $ "tyspec2TypeM: " ++ (render.pretty) typespec ++ " not implemented yet."


-- Substitutes an expression x by y everywhere in a
substituteBy :: (Data a) => CExpr -> CExpr -> a -> a
substituteBy x y a = everywhere (mkT substexpr) a
	where
	substexpr :: CExpr -> CExpr
	substexpr found_expr | x == found_expr = y
	substexpr found_expr                   = found_expr


-- Eliminate Indirections 
-- (trace is in straight order, not reversed)

elimInds :: Trace -> CovVecM Trace
elimInds trace = elim_indsM [] $ reverse trace
	where
	tyenv = createTyEnv trace
	elim_indsM :: Trace -> Trace -> CovVecM Trace
	elim_indsM res_trace [] = return res_trace
	elim_indsM res_trace (ti@(Assignment ptr@(CVar ptr_ident _) expr) : rest) = do
		case lookup ptr_ident tyenv of
			Nothing -> error $ "elemInds: could not find " ++ (render.pretty) ptr_ident
			Just (PtrType _ _ _) -> elim_indsM (cancel_ind_adrs $ substituteBy ptr expr res_trace) rest
			_ -> elim_indsM (ti : res_trace) rest
	elim_indsM res_trace (ti : rest) = elim_indsM (ti : res_trace) rest

	cancel_ind_adrs :: Trace -> Trace
	cancel_ind_adrs trace = everywhere (mkT cancel_ind_adr) trace
		where
		cancel_ind_adr :: CExpr -> CExpr
		cancel_ind_adr (CUnary CIndOp (CUnary CAdrOp expr _) _) = expr
		cancel_ind_adr expr = expr


-- FOLD TRACE BY SUBSTITUTING ASSIGNMENTS BACKWARDS
-- trace should be reversed!

elimAssignmentsM :: Trace -> CovVecM Trace
elimAssignmentsM trace = foldtraceM [] $ reverse trace
	where
	foldtraceM :: Trace -> Trace -> CovVecM Trace
	foldtraceM result [] = return result
	foldtraceM result (Assignment lvalue expr : rest) = foldtraceM (substituteBy lvalue expr result) rest
	foldtraceM result (traceitem : rest) = foldtraceM (traceitem:result) rest


-- Substitute leftover indirections

substIndM :: Trace -> [Ident] -> Trace -> CovVecM Trace
substIndM res_trace _ [] = return $ reverse res_trace
substIndM res_trace new_idents (ti : rest) = do
	(ti',add_tis) <- runStateT (everywhereM (mkM subst_indM) ti) []
	substIndM (ti' : (map NewDeclaration add_tis) ++ res_trace) (map fst add_tis ++ new_idents) rest
	where
	
	create_var :: CExpr -> Type -> StateT [(Ident,Type)] CovVecM CExpr
	create_var expr ty = do
		let newident = mkIdentWithCNodePos expr $ lValueToVarName expr
		when (not $ newident ∈ new_idents) $
			modify ((newident,ty) : )
		return $ CVar newident (nodeInfo expr)

	subst_indM :: CExpr -> StateT [(Ident,Type)] CovVecM CExpr
	subst_indM expr@(CUnary CIndOp (CVar ptr_ident _) ni) = do
		let Just (PtrType ty _ _) = lookup ptr_ident $ createTyEnv res_trace
		create_var expr ty
	subst_indM (CMember (CUnary CAdrOp s _) member True ni) = return $ CMember s member False ni
	subst_indM (CMember (CUnary CIndOp p _) member False ni) = return $ CMember p member True ni
	subst_indM expr@(CMember (CVar ptr_ident _) _ True _) = do
		let Just (PtrType ty _ _) = lookup ptr_ident $ createTyEnv res_trace
		create_var expr ty
	subst_indM expr@(CMember (CVar a_ident _) _ False _) = do
		let Just ty = lookup a_ident $ createTyEnv res_trace
		create_var expr ty
	subst_indM expr@(CUnary CAdrOp (CVar a_ident _) _) = do
		let Just ty = lookup a_ident $ createTyEnv res_trace
		create_var expr ty
	subst_indM expr = return expr


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
--				TyShort -> return $ MZAST.Range (MZAST.IConst (-32768)) (MZAST.IConst 32767)
				TyInt -> return $ MZAST.Range (MZAST.IConst (-30)) (MZAST.IConst 30) --MZAST.Int
				TyUShort -> return $ MZAST.Range (MZAST.IConst 0) (MZAST.IConst 65535)
				TyChar -> return $ MZAST.Range (MZAST.IConst (-128)) (MZAST.IConst 127)
				TySChar -> return $ MZAST.Range (MZAST.IConst (-128)) (MZAST.IConst 127)
				TyUChar -> return $ MZAST.Range (MZAST.IConst 0) (MZAST.IConst 255)
				TyUInt -> return $ MZAST.Range (MZAST.IConst 0) (MZAST.IConst 300) --2147483646)
				_ -> error $ "ty2mz " ++ (render.pretty) ty ++ " not implemented yet"
			TyFloating floatty -> case floatty of
				TyFloat -> return MZAST.Float
				_ -> error $ "ty2mz " ++ (render.pretty) ty ++ " not implemented yet"
			TyEnum (EnumTypeRef sueref _) -> do
				EnumDef (EnumType _ enums _ _) <- lookupTagM sueref
				return $ MZAST.CT $ MZAST.SetLit $
					map (\ (Enumerator _ (CConst (CIntConst (CInteger i _ _) _)) _ _) ->
						MZAST.IConst (fromIntegral i)) enums
			TyComp (CompTypeRef sueref _ _) -> return $ MZAST.Range (MZAST.IConst 100000) (MZAST.IConst 199999)
			_ -> error $ "ty2mz " ++ (render.pretty) ty ++ " not implemented yet"
		ty2mz (PtrType target_ty _ _) = return $ MZAST.Range (MZAST.IConst 65000) (MZAST.IConst 99999)
		ty2mz ty = error $ "ty2mz " ++ (render.pretty) ty ++ " not implemented yet"

type Constraint = CExpr

traceelemToMZ :: TraceElem -> CovVecM [MZAST.ModelData]
traceelemToMZ (Condition _ constr) = do
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
	expr2constr (CUnary CNegOp expr _) = error $ "expr2constr CUnaryOp CNegOp should not occur!"
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
--	expr2constr lexpr@(CMember (CVar _ _) _ _ _) = expr2constr $ CVar (internalIdent (lValueToVarName lexpr)) undefNode
	expr2constr expr = error $ "expr2constr " ++ show expr ++ " not implemented yet"

traceelemToMZ _ = return []

createTyEnv :: Trace -> [TyEnvItem]
createTyEnv trace = concatMap traceitem2tyenv trace
	where
	traceitem2tyenv (NewDeclaration tyenvitem) = [tyenvitem]
	traceitem2tyenv _ = []

solveTraceM :: Type -> Env -> [Int] -> Trace -> CovVecM ResultData
solveTraceM ret_type param_env traceid trace = do
	let
		tracename = show traceid
		param_names = map (fst.snd) param_env

	let mb_ret_val = case last trace of
		Return ret_expr -> Just ret_expr
		_               -> Nothing

	let
		returnval_ident = internalIdent returnval_var_name
		trace' = trace ++ case mb_ret_val of
			Nothing -> []
			Just ret_expr -> [
				Condition undefined $ CBinary CEqOp (CVar returnval_ident (nodeInfo returnval_ident)) ret_expr (nodeInfo ret_expr),
				NewDeclaration (returnval_ident,ret_type) ]

	constraintsG <- concatMapM traceelemToMZ trace'
	
	let tyenv = createTyEnv trace'
	
	let constr_trace = concatMap traceitem2constr trace' where
		traceitem2constr (Condition _ expr) = [expr]
		traceitem2constr _ = []
	let
		includesG = [ MZAST.include "include.mzn" ]
		vars :: [Ident] = nub $ param_names ++ everything (++) (mkQ [] searchvar) constr_trace where
			searchvar :: CExpr -> [Ident]
			searchvar (CVar ident _) = [ ident ]
			searchvar _ = []

	varsG <- concatMapM (var2MZ tyenv) vars
	let
		solution_vars = map identToString vars
		model = includesG ++ varsG ++ [] ++ constraintsG ++ [] ++
			[ MZAST.solve $ MZAST.satisfy MZAST.|: MZAST.Annotation "int_search" [
				MZAST.E (MZAST.ArrayLit $ map (MZAST.Var . MZAST.Simpl) solution_vars),
				MZAST.E (MZAST.Var $ MZAST.Simpl "input_order"),
				MZAST.E (MZAST.Var $ MZAST.Simpl "indomain_median"),
				MZAST.E (MZAST.Var $ MZAST.Simpl "complete") ] ]

	let modelpath = analyzerPath </> "model_" ++ tracename
	liftIO $ writeFile (modelpath ++ ".mzn") $ layout model
	
	case solveIt of
		False -> return (model,Just (param_env,[],mb_ret_val))
		True -> do
			printLog $ "Running model " ++ tracename ++ "..." 
		
			res <- liftIO $ runModel model modelpath 1 1
			mb_solution <- case res of
				Left err -> do
					printLog $ show err
					return Nothing
				Right [] -> error $ "Empty solution for " ++ tracename ++ " !"
				Right [sol] -> do
					let sol_params = filter (\(varname,_) -> varname `elem` (returnval_var_name : map identToString param_names)) sol
					return $ Just (param_env,sol_params,mb_ret_val)
				Right _ -> error $ "Found more than one solution for " ++ show traceid ++ " !"
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
--	printLog $ "checkSolution env = " ++ showEnv env
	srcfilename <- gets srcFilenameCVS
	Just filename <- gets checkExeNameCVS
	absolute_filename <- liftIO $ makeAbsolute srcfilename
	let
		args = concat $ for env $ \ (_,(newident,ty)) -> case ty of
			DirectType _ _ _ -> case lookup (identToString newident) solution of
				Nothing -> ["99"]
				Just (MInt i) -> [show i]
				Just (MFloat f) -> [show f]
				val -> error $ "checkSolutionM: " ++ show val ++ " not yet implemented"
			PtrType target_ty _ _ -> ["65000"]
			ty -> error $ "checkSolutionM args: type " ++ (render.pretty) ty ++ " not implemented!"
	printLog $ " checkSolution args = " ++ show args
	(exitcode,stdout,stderr) <- liftIO $ withCurrentDirectory (takeDirectory absolute_filename) $ do
		readProcessWithExitCode (takeFileName filename) args ""
	case exitcode of
		ExitFailure _ -> error $ "Execution of " ++ filename ++ " failed:\n" ++ stdout ++ stderr
		ExitSuccess -> do
			let exec_result = (read $ last $ lines stdout) :: Int
			let (MInt predicted_result) = getPredictedResult solution
			case exec_result == predicted_result of
				False -> do
					let txt = "ERROR in " ++ show traceid ++ " exec_result=" ++ show exec_result ++ " /= predicted_result=" ++ show predicted_result
					printLog txt
					error txt
				True  -> printLog $ "checkSolutionM " ++ show traceid ++ " OK."
			return resultdata

getPredictedResult solution = predicted_result
	where
	Just predicted_result = lookup returnval_var_name solution
