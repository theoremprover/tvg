{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE PackageImports,QuasiQuotes,UnicodeSyntax,LambdaCase,ScopedTypeVariables,TupleSections,TypeSynonymInstances,FlexibleInstances,FlexibleContexts,StandaloneDeriving,DeriveDataTypeable,DeriveGeneric #-}

module Main where

import System.Environment
import System.FilePath
import System.Process
import System.Directory
import System.Exit
import "language-c" Language.C
import Language.C.Data.Ident
import Language.C.Data.Node
import Language.C.Data.Position
import Language.C.Analysis.AstAnalysis
import Language.C.Analysis.TravMonad
import Language.C.Analysis.SemRep
import Language.C.System.GCC
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import qualified Data.Set as Set
import Data.Set.Unicode
import Prelude.Unicode ((∧),(∨))
import Text.Printf
import Text.Regex.TDFA
import Numeric (readHex)
import Data.Either
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

import "language-c-quote" Language.C.Quote.GCC
import "language-c-quote" Language.C.Pretty
import Text.PrettyPrint.Mainland.Class (ppr)
import Text.PrettyPrint.Mainland (prettyCompact)

import DataTree
import GlobDecls


type Trace = [TraceElem]
type ResultData = (String,Maybe (Env,Env,Solution))
type TraceAnalysisResult = ([Int],Trace,ResultData)
type UnfoldTracesRet = Either [Trace] Bool
type SolveFunRet = (Bool,([TraceAnalysisResult],Set.Set Branch))


for :: [a] -> (a -> b) -> [b]
for = flip map

concatForM = flip concatMapM

intSize = 32
longIntSize = 64
longLongIntSize = 64
--modeList = [ ("QI",8),("HI",16),("SI",32),("DI",64),("SF",32),("DF",64) ]

showInitialTrace = True
solveIt = True
showOnlySolutions = True
don'tShowTraces = True
checkSolutions = solveIt && True
returnval_var_name = "return_val"
outputVerbosity = 1
floatTolerance = 1e-7 :: Float
doubleTolerance = 1e-10 :: Double
showBuiltins = False
cutOffs = False
logToFile = True

mAX_UNROLLS = 30
uNROLLING_STRATEGY = [0..mAX_UNROLLS] --[mAX_UNROLLS,(mAX_UNROLLS-1)..0]

sizeConditionChunks = 4

z3FilePath = "C:\\z3-4.8.8-x64-win\\bin\\z3.exe"

analyzerPath = "analyzer"
logFile = analyzerPath </> "log.txt"

printLog :: (MonadIO m) => String -> m ()
printLog text = liftIO $ do
	putStrLn text
	when logToFile $ appendFile logFile (text++"\n")

printLogV :: (MonadIO m) => Int -> String -> m ()
printLogV verbosity text = when (verbosity<=outputVerbosity) $ printLog text

myError :: (MonadIO m) => forall a . String -> m a
myError txt = do
	printLog txt
	error txt

showLine :: Trace -> String
showLine trace = unlines $ map show (filter isnotbuiltin trace)

show_solution _ Nothing = "No solution"
show_solution _ (Just (_,_,[])) = "Empty solution"
show_solution funname (Just v@(_,_,solution)) = unlines $ map show solution ++ [ showTestVector funname v ]

main :: IO ()
main = do
	-- when there is an error, we'd like to have *all* output till then
	hSetBuffering stdout NoBuffering

	gcc:filename:funname:opts <- getArgs >>= return . \case
		[] -> "gcc" : (analyzerPath++"\\myfp-bit_mul.c") : "_fpmul_parts" : ["-writeModels"] --"-writeAST","-writeGlobalDecls"]
--		[] -> "gcc" : (analyzerPath++"\\test.c") : "g" : [] --["-writeAST","-writeGlobalDecls"]
--		[] -> "gcc" : (analyzerPath++"\\iffuntest.c") : "f" : [] --["-writeAST","-writeGlobalDecls"]
--		[] -> "gcc" : (analyzerPath++"\\myfp-bit.c") : "_fpdiv_parts" : [] --"-writeAST","-writeGlobalDecls"]
--		[] -> "gcc" : (analyzerPath++"\\OscarsChallenge\\sin\\oscar.c") : "_Sinx" : [] --"-writeAST","-writeGlobalDecls"]
--		[] -> "gcc" : (analyzerPath++"\\switchtest.c") : "f" : [] --"-writeAST","-writeGlobalDecls"]
--		[] -> "gcc" : (analyzerPath++"\\whiletest2.c") : "_fpdiv_parts" : [] --"-writeAST","-writeGlobalDecls"]
--		[] -> "gcc" : (analyzerPath++"\\branchtest.c") : "f" : ["-writeTree"] --["-writeAST","-writeGlobalDecls"]
--		[] -> "gcc" : (analyzerPath++"\\iftest.c") : "f" : [] --["-writeAST","-writeGlobalDecls"]
--		[] -> "gcc" : (analyzerPath++"\\deadtest.c") : "f" : [] --["-writeAST","-writeGlobalDecls"]
--		[] -> "gcc" : (analyzerPath++"\\whiletest.c") : "f" : ["-writeModels"] --["-writeAST","-writeGlobalDecls"]
--		[] -> "gcc" : (analyzerPath++"\\ptrtest_flat.c") : "f" : ["-writeAST"]
--		[] -> "gcc" : (analyzerPath++"\\ptrtest.c") : "f" : [] --["-writeAST"]
--		[] -> "gcc" : (analyzerPath++"\\assigntest.c") : "g" : [] --["-writeAST","-writeGlobalDecls"]
--		[] -> "gcc" : (analyzerPath++"\\ptrrettest.c") : "g" : [] --["-writeAST","-writeGlobalDecls"]
--		[] -> "gcc" : (analyzerPath++"\\calltest.c") : "g" : ["-writeTraceTree"] --["-writeAST","-writeGlobalDecls"]
		args -> args

	getZonedTime >>= return.(++"\n\n").show >>= writeFile logFile
	
	parseCFile (newGCC gcc) Nothing [] filename
		>>= \case
			Left err -> myError $ show err
			Right translunit -> do
				when ("-writeAST" `elem` opts) $
					writeFile (filename <.> "ast.html") $ genericToHTMLString translunit
				case runTrav_ $ analyseAST translunit of
					Left errs -> putStrLn "ERRORS:" >> forM_ errs print
					Right (globdecls,soft_errors) -> do
						when (not $ null soft_errors) $ putStrLn "Soft errors:" >> forM_ soft_errors print
						when ("-writeGlobalDecls" `elem` opts) $
							writeFile (filename <.> "globdecls.html") $ globdeclsToHTMLString globdecls
	
						(full_coverage,s) <- runStateT covVectorsM $
							CovVecState globdecls 1 translunit filename Nothing funname undefined 0 gcc opts
								Nothing ([],Set.empty) Set.empty intialStats
						let
							(testvectors_rev,covered) = analysisStateCVS s
							testvectors = reverse testvectors_rev
							alls = allCondPointsCVS s
	
						printLog ""
	
						let deaths = Set.toList $ alls ∖ covered
	
						printLog $ "\n####### FINAL RESULT #######\n"
	
						forM_ testvectors $ \ (traceid,trace,(model_string,mb_solution)) -> do
							case not showOnlySolutions || maybe False (not.null.(\(_,_,b)->b)) mb_solution of
								False -> return ()
								True -> printLog $ unlines $ mbshowtraces (
									[ "","=== TRACE " ++ show traceid ++ " ========================","<leaving out builtins...>" ] ++
									[ showLine trace ] ++
									[ "",
									"--- MODEL " ++ show traceid ++ " -------------------------",
									model_string,
									"" ]) ++
									[ "--- SOLUTION " ++ show traceid ++ " ----------------------",
									show_solution funname mb_solution ]
									where
									mbshowtraces ts = if don'tShowTraces then [] else ts
	
						printLog $ "\n===== SUMMARY =====\n"
	
						forM_ testvectors $ \ (traceid,trace,(model,Just v)) -> do
							printLog $ "Test Vector covering " ++ show traceid ++ " : "
							printLog $ "    " ++ showTestVector funname v ++ "\n"
						forM_ deaths $ \ branch -> do
							printLog $ "DEAD " ++ show branch ++ "\n"
	
						when (full_coverage && not (null deaths)) $ error "full coverage but deaths!"
						when (not full_coverage && null deaths) $ error "not full_coverage and no deaths!"
	
						printLog $ case full_coverage of
							False -> "FAIL, there are coverage gaps!"
							True  -> "OK, we have full branch coverage."

showEnv :: Env -> String
showEnv env = "{\n    " ++ intercalate " ,\n    " (map (render.pretty) env) ++ "\n    }"

showTyEnv :: TyEnv -> String
showTyEnv tyenv = "{\n    " ++ intercalate " ,\n    " (map (render.pretty) tyenv) ++ "\n    }"

showIdents :: [Ident] -> String
showIdents idents = "[ " ++ intercalate ", " (map (render.pretty) idents) ++ " ]"

showTestVector :: String -> (Env,Env,Solution) -> String
showTestVector funname (env,ret_env,solution) = funname ++ " ( " ++ intercalate " , " (map showarg env) ++ " )" ++
	"\n    = " ++ intercalate " , " (map showarg ret_env)
	where
	showarg :: EnvItem -> String
	showarg (oldident,(newident,_)) =
		identToString oldident ++ " = " ++ case lookup (identToString newident) solution of
			Nothing           -> "DONT_CARE"
			Just (IntVal i)   -> show i
			Just (FloatVal f) -> show f

data CovVecState = CovVecState {
	globDeclsCVS     :: GlobalDecls,
	newNameIndexCVS  :: Int,
	translUnitCVS    :: CTranslUnit,
	srcFilenameCVS   :: String,
	checkExeNameCVS  :: Maybe String,
	funNameCVS       :: String,
	funStartEndCVS   :: ((Int,Int),(Int,Int)),
	numTracesCVS     :: Int,
	compilerCVS      :: String,
	optsCVS          :: [String],
	paramEnvCVS      :: Maybe Env,
	analysisStateCVS :: ([TraceAnalysisResult],Set.Set Branch),
	allCondPointsCVS :: Set.Set Branch,
	statsCVS         :: Stats
	}

data Stats = Stats { cutoffTries :: Int, cutoffsS :: Int }
	deriving (Show)
intialStats = Stats 0 0
incCutoffTriesM :: CovVecM ()
incCutoffTriesM = modify $ \ s -> s { statsCVS = (statsCVS s) { cutoffTries = cutoffTries (statsCVS s) + 1 } }
incCutoffsM :: CovVecM ()
incCutoffsM = modify $ \ s -> s { statsCVS = (statsCVS s) { cutoffsS = cutoffsS (statsCVS s) + 1 } }
printStatsM :: CovVecM ()
printStatsM = gets statsCVS >>= (printLogV 1) . show 

type CovVecM = StateT CovVecState IO

data TraceElem =
	Assignment CExpr CExpr |
	Condition (Maybe Bool) CExpr |
	NewDeclaration (Ident,Type) |
	Return CExpr |
	DebugOutput String (CExpr,Type)
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
	nodeInfo (DebugOutput _ _)          = undefNode

instance Pretty NodeInfo where
	pretty ni = text $ "line " ++ show line ++ ", col " ++ show col
		where
		(line,col) = lineColNodeInfo ni

instance (Pretty a) => Pretty [a] where
	pretty xs = brackets $ hcat $ punctuate comma (map pretty xs)

instance Show TraceElem where
	show te = ( case te of
		(Assignment lvalue expr)   -> "ASSN " ++ (render.pretty) lvalue ++ " = " ++ (render.pretty) expr
		(Condition mb_b expr)      -> "COND " ++ maybe "" (\b->if b then "(THEN) " else "(ELSE) ") mb_b ++ (render.pretty) expr
		(NewDeclaration (lval,ty)) -> "DECL " ++ (render.pretty) lval ++ " :: " ++ (render.pretty) ty
		(Return exprs)             -> "RET  " ++ (render.pretty) exprs
		(DebugOutput varname (expr,_)) -> "DBGOUT " ++ varname ++ " " ++ (render.pretty) expr
		) ++ "  (" ++ (render.pretty) (nodeInfo te) ++ ")"

showTrace :: Trace -> String
showTrace trace = unlines $ concatMap show_te trace where
	show_te te | showBuiltins || not (isnotbuiltin te) = [show te]
	show_te _ = []


covVectorsM :: CovVecM Bool
covVectorsM = do
	funname <- gets funNameCVS
	globdecls <- gets ((Map.elems).gObjs.globDeclsCVS)
	glob_env <- concatMapM declaration2EnvItemM globdecls
	let
		def2stmt :: IdentDecl -> CovVecM [CBlockItem]
		def2stmt (EnumeratorDef (Enumerator ident expr _ ni)) = return $
			[ CBlockStmt (CExpr (Just $ (CVar ident (nodeInfo ident)) ≔ expr) ni) ]
		def2stmt (ObjectDef (ObjDef (VarDecl (VarName ident _) _ ty) (Just initializer) ni)) = do
			ty' <- elimTypeDefsM ty
			cinitializer2blockitems (CVar ident ni) ty' initializer
		def2stmt _ = return []
	-- creates the assignment statements from the global context
	defs <- concatMapM def2stmt globdecls

	FunDef (VarDecl _ _ (FunctionType (FunType ret_type funparamdecls False) _)) body fundef_ni <-
		lookupFunM (builtinIdent funname)
	ret_type' <- elimTypeDefsM ret_type

	let condition_points = Set.fromList $ everything (++) (mkQ [] searchcondpoint) body
		where
		n2loc node = nodeInfo node
		searchcondpoint :: CStat -> [Branch]
		searchcondpoint (CWhile cond _ _ _) = [ Then (lineColNodeInfo cond), Else (lineColNodeInfo cond) ]
		searchcondpoint (CCase expr _ _) = [ Then (lineColNodeInfo expr) ]
		searchcondpoint (CDefault stmt _) = [ Then (lineColNodeInfo stmt) ]
		searchcondpoint (CFor _ (Just cond) _ _ _) = [ Then (lineColNodeInfo cond), Else (lineColNodeInfo cond) ]
		searchcondpoint (CIf cond _ _ _) = [ Then (lineColNodeInfo cond), Else (lineColNodeInfo cond) ]
		searchcondpoint _ = []
	modify $ \ s -> s { allCondPointsCVS = condition_points }
	
	let
		fun_lc = lineColNodeInfo fundef_ni
		next_lc = case sort $ filter (> lineColNodeInfo fundef_ni) $ map lineColNodeInfo globdecls of
			[] -> (9999999999,9999999999)
			next : _ -> next
	modify $ \ s -> s { funStartEndCVS = (fun_lc,next_lc) }

	param_env_exprs <- createInterfaceM $ for (map getVarDecl funparamdecls) $ \ (VarDecl (VarName srcident _) _ ty) -> (srcident,ty)
	let param_env = map fst param_env_exprs
	modify $ \ s -> s { paramEnvCVS = Just param_env }
	printLogV 2 $ "param_env = " ++ showEnv param_env

	let decls = map (NewDeclaration .snd) (reverse param_env ++ glob_env)

	when checkSolutions $ do
		filename <- gets srcFilenameCVS
		let
			srcfilename = takeFileName filename
			chkexefilename = replaceExtension srcfilename "exe"
		absolute_filename <- liftIO $ makeAbsolute filename
		gcc <- gets compilerCVS
		(exitcode,stdout,stderr) <- liftIO $ withCurrentDirectory (takeDirectory absolute_filename) $ do
			readProcessWithExitCode gcc ["-Wno-builtin-declaration-mismatch","-Wno-int-conversion","-o",
				chkexefilename,"-DCALC",srcfilename] ""
		case exitcode of
			ExitFailure _ -> myError $ "Compilation failed:\n" ++ stderr
			ExitSuccess -> modify $ \ s -> s { checkExeNameCVS = Just chkexefilename }

	Right all_covered <- unfoldTracesM (Just ret_type') [] (param_env:[glob_env]) decls [ defs ++ [ CBlockStmt body ] ]
	return all_covered

type Location = (Int,Int)

showLocation :: Location -> String
showLocation (l,c) = "line " ++ show l ++ ", col " ++ show c

-- In case of a cutoff, mb_ret_type is Nothing.
analyzeTraceM :: Maybe Type -> [TraceElem] -> CovVecM Bool
analyzeTraceM mb_ret_type res_line = do
	let
		trace = reverse res_line
		traceid = concatMap extract_conds trace where
			extract_conds (Condition (Just b) _) = [ if b then 1 else 2 ]
			extract_conds _ = []

	when showInitialTrace $ do
		printLog $ "=== TRACE " ++ show traceid ++ " ========================\n" ++
			if showBuiltins then "" else "<leaving out builtins...>\n"
		printLog $ showLine trace

	-- Eliminate/Expand all assignments to pointers in the later code
	res_trace_elim_inds <- elimInds trace
	when (not don'tShowTraces) $ do
		printLog $ "\n=== TRACE after elimInds " ++ show traceid ++ " =========\n" ++
			if showBuiltins then "" else "<leaving out builtins...>\n"
		printLog $ showLine res_trace_elim_inds

	-- Eliminate (*&), & and * in members
	res_trace_simplified1 <- simplifyTraceM res_trace_elim_inds
	when (not don'tShowTraces) $ do
		printLog $ "\n--- TRACE after simplifyTraceM 1 " ++ show traceid ++ " -----------\n" ++
			if showBuiltins then "" else "<leaving out builtins...>\n"
		printLog $ showLine res_trace_simplified1

	-- Eliminate Assignments
	res_trace_elim'd_assigns <- elimAssignmentsM res_trace_simplified1
	when (not don'tShowTraces) $ do
		printLog $ "\n--- TRACE after elimAssignmentsM " ++ show traceid ++ " -----------\n" ++
			if showBuiltins then "" else "<leaving out builtins...>\n"
		printLog $ showLine res_trace_elim'd_assigns

	-- Eliminate (*&), & and * in members
	res_trace_simplified2 <- simplifyTraceM res_trace_elim'd_assigns
	when (not don'tShowTraces) $ do
		printLog $ "\n--- TRACE after simplifyTraceM 2 " ++ show traceid ++ " -----------\n" ++
			if showBuiltins then "" else "<leaving out builtins...>\n"
		printLog $ showLine res_trace_simplified2

	-- Create variables for leftover members and addressOfs:
	-- p->m ~> p_ARROW_m
	-- a.m  ~> a_DOT_m
	-- &a   ~> ADR_a
	res_trace_symbolic <- createSymbolicVarsM [] (map fst $ createTyEnv res_trace_simplified2) res_trace_simplified2
	when (not don'tShowTraces) $ do
		printLog $ "\n--- TRACE after createSymbolicVarsM " ++ show traceid ++ " -----------\n" ++
			if showBuiltins then "" else "<leaving out builtins...>\n"
		printLog $ showLine res_trace_symbolic

	either_resultdata <- solveTraceM mb_ret_type traceid res_trace_symbolic
	case either_resultdata of
		Left solvable -> return solvable
		Right resultdata@(model_string,mb_solution) -> do
			when (not don'tShowTraces) $ printLog $ "\n--- MODEL " ++ show traceid ++ " -------------------------\n" ++ model_string
			funname <- gets funNameCVS
			printLogV 1 $ "--- TRACE " ++ show traceid ++ " ----------------------\n" ++
				show_solution funname mb_solution ++ "\n"
		
			startend <- gets funStartEndCVS
			let visible_trace = Set.fromList $ concatMap to_branch res_line
				where
				to_branch cond@(Condition (Just b) _) | is_visible_traceelem startend cond =
					[ (if b then Then else Else) (lineColNodeInfo cond) ]
				to_branch _ = []
		
			let traceanalysisresult :: TraceAnalysisResult = (traceid,res_line,resultdata)
			case is_solution traceanalysisresult of
				False -> do
					printLogV 2  $ "### FALSE : " ++ show traceid ++ " no solution!"
				True  -> do
					printLogV 2  $ "### TRUE : " ++ show traceid ++ " Is Solution"
					when (isJust mb_ret_type) $ checkSolutionM traceid resultdata >> return ()
					modify $ \ s -> s { analysisStateCVS = let (tas,covered) = analysisStateCVS s in
						case visible_trace ⊆ covered of
							False -> (traceanalysisresult:tas,visible_trace ∪ covered)
							True  -> (tas,covered) }
			return $ is_solution traceanalysisresult


is_solution :: TraceAnalysisResult -> Bool
is_solution (_,_,(_,Just (_,_,solution))) = not $ null solution
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
		Just other -> myError $ "lookupFunM " ++ (render.pretty) ident ++ " yielded " ++ (render.pretty) other
		Nothing -> myError $ "Function " ++ (show ident) ++ " not found"

isnotbuiltinIdent ident = not $ any (`isPrefixOf` (identToString ident)) ["__","a__"]

isnotbuiltin (NewDeclaration (ident,_)) = isnotbuiltinIdent ident
isnotbuiltin _ = True

instance Eq CExpr where
	(CVar id1 _) == (CVar id2 _) = id1==id2
	(CMember ptrexpr1 ident1 isptr1 _) == (CMember ptrexpr2 ident2 isptr2 _) =
		ident1==ident2 && isptr1==isptr2 && ptrexpr1 == ptrexpr2
	(CUnary op1 expr1 _) == (CUnary op2 expr2 _) = op1==op2 && expr1==expr2
	(CBinary op1 expr11 expr12 _) == (CBinary op2 expr21 expr22 _) = op1==op2 && expr11==expr21 && expr12==expr22
	(CAssign op1 expr11 expr12 _) == (CAssign op2 expr21 expr22 _) = op1==op2 && expr11==expr21 && expr12==expr22
	(CCall fun1 args1 _) == (CCall fun2 args2 _) = fun1==fun2 && args1==args2
	(CConst const1) == (CConst const2) = const1==const2
	_ == _ = False

instance Eq CConst where
	(CIntConst c1 _)   == (CIntConst c2 _)   = c1==c2
	(CCharConst c1 _)  == (CCharConst c2 _)  = c1==c2
	(CFloatConst c1 _) == (CFloatConst c2 _) = c1==c2
	(CStrConst c1 _)   == (CStrConst c2 _)   = c1==c2


lValueToVarName :: CExpr -> String
lValueToVarName (CVar ident _) = identToString ident
lValueToVarName (CMember ptrexpr member isptr _) =
	lValueToVarName ptrexpr ++ (if isptr then "_ARROW_" else "_DOT_") ++ identToString member
lValueToVarName (CUnary CIndOp expr _) = "PTR_" ++ lValueToVarName expr
lValueToVarName (CUnary CAdrOp expr _) = "ADR_" ++ lValueToVarName expr
lValueToVarName (CBinary binop expr1 expr2 _) =
	lValueToVarName expr1 ++ "_" ++ binop2str binop ++ "_" ++ lValueToVarName expr2
	where
	binop2str binop = case lookup binop [
		(CMulOp,"mul"),(CDivOp,"div"),(CRmdOp,"rmd"),(CAddOp,"plus"),(CSubOp,"minus"),
		(CShlOp,"shl"),(CShrOp,"shr"),(CAndOp,"and"),(CXorOp,"xor"),(COrOp,"or") ] of
			Nothing -> error $ "binop2str " ++ (render.pretty) binop ++ " not implemented"
			Just s -> s
lValueToVarName (CConst (CIntConst cint _)) = (if i<0 then "m" else "") ++ show (abs i) where
	i = getCInteger cint
lValueToVarName lval = error $ "lValueToVarName " ++ (render.pretty) lval ++ " not implemented!"

type TyEnvItem = (Ident,Type)
instance Pretty TyEnvItem where
	pretty (idnew,ty) = pretty idnew <+> text " :: " <+> pretty ty
type EnvItem = (Ident,TyEnvItem)
instance Pretty EnvItem where
	pretty (idold,tyenvitem) = pretty idold <+> text " |-> " <+> pretty tyenvitem
type Env = [EnvItem]

envToString :: Env -> String
envToString env = unlines $ map (render.pretty) $ filter (isnotbuiltinIdent.fst) env

lookupTypeDefM :: Ident -> CovVecM Type
lookupTypeDefM ident = do
	typedefs <- gets (gTypeDefs.globDeclsCVS)
	case Map.lookup ident typedefs of
		Just (TypeDef _ ty attrs _) -> return $ case ty of
			DirectType tyname tyquals tyattrs    -> DirectType tyname tyquals (tyattrs++attrs)
			PtrType ty tyquals tyattrs           -> PtrType ty tyquals (tyattrs++attrs)
			ArrayType ty size tyquals tyattrs    -> ArrayType ty size tyquals (tyattrs++attrs)
			FunctionType ty tyattrs              -> FunctionType ty (tyattrs++attrs)
			TypeDefType tydefref tyquals tyattrs -> TypeDefType tydefref tyquals (tyattrs++attrs)
		Nothing -> myError $ "TypeDef " ++ (show ident) ++ " not found"

lookupTagM :: SUERef -> CovVecM TagDef
lookupTagM ident = do
	tags <- gets (gTags.globDeclsCVS)
	case Map.lookup ident tags of
		Just tagdef -> return tagdef
		Nothing -> myError $ "Tag " ++ (show ident) ++ " not found"

getMemberTypeM :: Type -> Ident -> CovVecM Type
getMemberTypeM ty@(DirectType (TyComp (CompTypeRef sueref _ _)) _ _) member = do
	mem_tys <- getMembersM sueref
	case lookup member mem_tys of
		Nothing -> myError $ "getMemberTypeM: Could not find member " ++ (render.pretty) member ++ " in " ++ (render.pretty) ty
		Just mem_ty -> return mem_ty

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
		name_prefix = identToString srcident
		newident = Ident (name_prefix ++ "_" ++ show new_var_num) i ni
	return $ [ (srcident,(newident,ty')) ]


-- Recursively create all "interface" variables for the top level function to be analyzed

createInterfaceM :: [(Ident,Type)] -> CovVecM [(EnvItem,CExpr)]
createInterfaceM ty_env = concatForM ty_env $ \ (srcident,ty) ->
	createInterfaceFromExprM (CVar srcident (nodeInfo srcident)) ty

createInterfaceFromExprM :: CExpr -> Type -> CovVecM [(EnvItem,CExpr)]
createInterfaceFromExprM expr ty = do
	ty' <- elimTypeDefsM ty
	case ty' of
	
		-- STRUCT* p
		PtrType (DirectType (TyComp (CompTypeRef sueref _ _)) _ _) _ _ -> prepend_plainvar ty' $ do
			member_ty_s <- getMembersM sueref
			concatForM member_ty_s $ \ (m_ident,m_ty) ->
				createInterfaceFromExprM (CMember expr m_ident True (nodeInfo expr)) m_ty
	
		-- ty* p
		PtrType target_ty _ _ -> prepend_plainvar ty' $ do
			createInterfaceFromExprM (CUnary CIndOp expr (nodeInfo expr)) target_ty
	
		-- STRUCT expr
		DirectType (TyComp (CompTypeRef sueref _ _)) _ _ -> do
			member_ty_s <- getMembersM sueref
			concatForM member_ty_s $ \ (m_ident,m_ty) -> do
				createInterfaceFromExprM (CMember expr m_ident False (nodeInfo expr)) m_ty
	
		-- direct-type expr where direct-type is no struct/union
		DirectType _ _ _ -> prepend_plainvar ty' $ return []
	
		_ ->
			myError $ "create_interfaceM " ++ (render.pretty) expr ++ " " ++ (render.pretty) ty' ++ " not implemented"

		where
	
		prepend_plainvar :: Type -> CovVecM [(EnvItem,CExpr)] -> CovVecM [(EnvItem,CExpr)]
		prepend_plainvar ty' rest_m = do
			let srcident = internalIdent $ lValueToVarName expr
			rest <- rest_m
			return $ ((srcident,(srcident,ty')),expr) : rest

unfoldTracesM :: Maybe Type -> [Int] -> [Env] -> Trace -> [[CBlockItem]] -> CovVecM UnfoldTracesRet
unfoldTracesM mb_ret_type break_stack envs trace cbss = do
	unfoldTraces1M mb_ret_type break_stack envs trace cbss

unfoldTraces1M :: Maybe Type -> [Int] -> [Env] -> Trace -> [[CBlockItem]] -> CovVecM UnfoldTracesRet
unfoldTraces1M mb_ret_type break_stack envs trace bstss@((CBlockStmt stmt : rest) : rest2) = case stmt of

	CLabel _ cstat _ _ -> unfoldTracesM mb_ret_type break_stack envs trace ((CBlockStmt cstat : rest) : rest2)

	CCompound _ cbis _ -> unfoldTracesM mb_ret_type break_stack ([]:envs) trace (cbis : (rest : rest2))

	CSwitch condexpr (CCompound [] cbis _) switch_ni -> do
		let
			cond_ni = nodeInfo condexpr
			(l,c) = lineColNodeInfo condexpr
			cond_var_ident = mkIdentWithCNodePos condexpr $ "cond_" ++ show l ++ "_" ++ show c
			cond_var = CVar cond_var_ident cond_ni

			filtercases = map $ \case
				CBlockStmt (CCase _ stmt _) -> CBlockStmt stmt
				CBlockStmt (CDefault stmt _) -> CBlockStmt stmt
				cbi -> cbi

			collect_stmts :: [CBlockItem] -> [CBlockItem]
			collect_stmts [] = []
			collect_stmts [ CBlockStmt (CDefault stmt _) ] = [
				CBlockStmt $ CGotoPtr (CConst $ CIntConst (cInteger 1) (nodeInfo stmt)) undefNode, CBlockStmt stmt ]
			collect_stmts (CBlockStmt (CCase caseexpr stmt _) : rest) = [ CBlockStmt $ CIf (CBinary CEqOp cond_var caseexpr (nodeInfo caseexpr))
				(CCompound [] (CBlockStmt stmt : filtercases rest) undefNode) (Just $ CCompound [] (collect_stmts rest) undefNode) undefNode ]
			collect_stmts (_:rest) = collect_stmts rest

			case_replacement = collect_stmts cbis
		printLogV 1 $ (render.pretty) case_replacement
		unfoldTracesM mb_ret_type (length bstss : break_stack) envs trace ( (
			CBlockDecl (CDecl [CTypeSpec $ CLongType cond_ni]
				[(Just $ CDeclr (Just cond_var_ident) [] Nothing [] cond_ni, Just $ CInitExpr condexpr cond_ni, Nothing)] cond_ni) :
			case_replacement ++
			rest) : rest2 )

	CBreak _ -> case break_stack of
		[] -> error $ "unfoldTraces1M " ++ (render.pretty) stmt ++ " : empty break stack!"
		(b:rest) -> do
			let through_compounds = length bstss - b
			unfoldTracesM mb_ret_type rest (drop through_compounds envs) trace (drop through_compounds bstss)

	CIf cond then_stmt mb_else_stmt ni -> do
		let then_trace_m real_cond = transids real_cond trace $ \ (cond',trace') -> do
			unfoldTracesM mb_ret_type break_stack envs (Condition (Just True) cond' : trace') ( (CBlockStmt then_stmt : rest) : rest2 )
		let else_trace_m real_cond = transids real_cond trace $ \ (cond',trace') -> do
			let not_cond = Condition (Just False) (CUnary CNegOp cond' (nodeInfo cond'))
			case mb_else_stmt of
				Nothing        -> unfoldTracesM mb_ret_type break_stack envs (not_cond : trace') ( rest : rest2 )
				Just else_stmt -> unfoldTracesM mb_ret_type break_stack envs (not_cond : trace') ( (CBlockStmt else_stmt : rest) : rest2 )
		case recognizeAnnotation cond of
			(real_cond,Just (ns,num_reached)) -> do
				printLogV 1 $ "Recognized IF annotation " ++ show (ns!!num_reached) ++ " to " ++ (render.pretty) real_cond
				case ns!!num_reached of
					1 -> then_trace_m real_cond
					2 -> else_trace_m real_cond
			(real_cond,_) -> do
				(if conditions_reached > 0 && conditions_reached `mod` sizeConditionChunks == 0 then maybe_cutoff else id) $ do
					either_then <- then_trace_m real_cond
					either_else <- else_trace_m real_cond
					return $ case (either_then,either_else) of
						(Left then_traces,Left else_traces) -> Left $ then_traces ++ else_traces
						(Right then_success,Right else_success) -> case mb_ret_type of
							Nothing -> Right $ then_success || else_success
							Just _  -> Right $ then_success && else_success

	CReturn Nothing _ -> case mb_ret_type of
		Nothing       -> return $ Left [trace]
		Just ret_type -> analyzeTraceM (Just ret_type) trace >>= return.Right

	CReturn (Just ret_expr) _ -> do
		transids ret_expr trace $ \ (ret_expr',trace') -> do
			case mb_ret_type of
				Nothing -> return $ Left [Return ret_expr' : trace']
				Just ret_type -> do
					ret_var_expr <- createInterfaceM [(internalIdent returnval_var_name,ret_type)]
					ret_env_expr <- createInterfaceFromExprM ret_expr' ret_type
					when (length ret_var_expr /= length ret_env_expr) $ error "unfoldTraces1M CReturn: length ret_var_expr /= length ret_env_expr !"
					let ret_trace = concat $ for (zip ret_var_expr ret_env_expr) $
						\ ( ((_,(ret_var_ident,ret_var_ty)),_) , (_,ret_member_expr)) -> [
							Condition Nothing $ CVar ret_var_ident undefNode ⩵ ret_member_expr,
							NewDeclaration (ret_var_ident,ret_var_ty) ]
					analyzeTraceM (Just ret_type) (Return ret_expr' : (ret_trace ++ trace'))
						>>= return.Right

	CExpr (Just (CCall (CVar (Ident "solver_debug" _ _) _) args ni)) _ -> do
		let vars = for args $ \ (CVar ident _) -> fromJust $ lookup ident (concat envs)
		unfoldTracesM mb_ret_type break_stack envs (map to_dbg_output (reverse vars) ++ trace) (rest:rest2) where
			to_dbg_output (name_id,ty) = DebugOutput ("solver_debug_" ++ identToString name_id) (CVar name_id undefNode,ty)

	CExpr (Just cass@(CAssign assignop lexpr assigned_expr ni)) _ -> do
		transids assigned_expr' trace $ \ (assigned_expr'',trace') -> do
			transids lexpr trace' $ \ (lexpr',trace'') -> do
				unfoldTracesM mb_ret_type break_stack envs (Assignment lexpr' assigned_expr'' : trace'') (rest:rest2)
		where
		mb_binop = lookup assignop [
			(CMulAssOp,CMulOp),(CDivAssOp,CDivOp),(CRmdAssOp,CRmdOp),(CAddAssOp,CAddOp),(CSubAssOp,CSubOp),
			(CShlAssOp,CShlOp),(CShrAssOp,CShrOp),(CAndAssOp,CAndOp),(CXorAssOp,CXorOp),(COrAssOp,COrOp) ]
		assigned_expr' = case mb_binop of
			Nothing -> assigned_expr
			Just binop -> CBinary binop lexpr assigned_expr ni

	CExpr (Just (CUnary unaryop expr ni_op)) ni | unaryop `elem` (map fst unaryops) -> do
		unfoldTracesM mb_ret_type break_stack envs trace ( (CBlockStmt stmt' : rest) : rest2 )
		where
		stmt' = CExpr (Just $ CAssign assignop expr (ⅈ 1) ni) ni
		Just assignop = lookup unaryop unaryops
		unaryops = [ (CPreIncOp,CAddAssOp),(CPostIncOp,CAddAssOp),(CPreDecOp,CSubAssOp),(CPostDecOp,CSubAssOp) ]

	CExpr (Just expr) _ -> do
		myError $ "unfoldTraces: " ++ (render.pretty) stmt ++ " not implemented yet."

	-- That's cheating: Insert condition into trace (for loop unrolling)
	CGotoPtr cond ni -> do
		transids cond trace $ \ (cond',trace') -> do
			unfoldTracesM mb_ret_type break_stack envs (Condition (Just $ isUndefNode ni) cond' : trace') ( rest : rest2 )

 	CWhile cond body False ni -> do
 		(mb_unrolling_depth,msg) <- infer_loopingsM cond body
 		printLogV 1 msg
 		unroll_loopM $ case mb_unrolling_depth of
			Nothing -> uNROLLING_STRATEGY
			Just ns -> ns

		where

		unroll_loopM :: [Int] -> CovVecM UnfoldTracesRet
		unroll_loopM [] = case mb_ret_type of
			Nothing -> return $ Left []
			Just _  -> return $ Right False
		unroll_loopM (depth:depths) = do
			printLogV 1 $ "unroll_loopM " ++ show depth
			unfoldTracesM mb_ret_type break_stack envs trace ((unroll cond depth ++ rest) : rest2 ) >>= \case
				Right True  -> return $ Right True
				Right False -> unroll_loopM depths
				Left traces -> do
					Left traces' <- unroll_loopM depths
					return $ Left $ traces ++ traces'

		unroll :: CExpr -> Int -> [CBlockItem]
		unroll while_cond n = 
			concat ( replicate n [ CBlockStmt (CGotoPtr while_cond undefNode), CBlockStmt body ] ) ++
			[ CBlockStmt $ CGotoPtr (not_c while_cond) ni ]

	_ -> myError $ "unfoldTracesM " ++ (render.pretty) stmt ++ " not implemented yet"

	where

	recognizeAnnotation :: CExpr -> (CExpr,Maybe ([Int],Int))
	recognizeAnnotation (CBinary CLndOp (CCall (CVar (Ident "solver_pragma" _ _) _) args _) real_cond _) =
		(real_cond,Just (map arg2int args,num_reached)) where
			num_reached = length $ filter is_this_cond trace
			is_this_cond (Condition _ c) | nodeInfo c == nodeInfo real_cond = True
			is_this_cond _ = False
			arg2int (CConst (CIntConst (CInteger i _ _) _)) = fromIntegral i
	recognizeAnnotation real_cond = (real_cond,Nothing)

	maybe_cutoff :: CovVecM UnfoldTracesRet -> CovVecM UnfoldTracesRet
	maybe_cutoff cont | cutOffs = do
		incCutoffTriesM
		printLogV 2 $ "******* Probing for CutOff in depth " ++ show (length trace) ++ " ..."
		analyzeTraceM Nothing trace >>= \case
			False -> do
				printLogV 2 $ "******** Cutting off."
				incCutoffsM
				return $ Right False
			True  -> do
				printLogV 2 $ "******** Continuing..."
				cont
	maybe_cutoff cont = cont

--	num_reached :: (CNode cnode) => cnode -> Int
--	num_reached cnode = length $ filter ((== nodeInfo cnode).nodeInfo) trace

	conditions_reached :: Int
	conditions_reached = length $ filter is_condition trace where
		is_condition (Condition _ _) = True
		is_condition _ = False

	infer_loopingsM :: CExpr -> CStat -> CovVecM (Maybe [Int],String)
 	infer_loopingsM cond0 body = do
		case recognizeAnnotation cond0 of
			(real_cond,Just (ns,_)) -> return (Just ns,"Recognized LOOP annoation to " ++ (render.pretty) cond0)
			(real_cond,Nothing) -> do
				translateExprM envs real_cond >>= \case
					[(cond,[])] -> do
						let
							-- get all variables used in the condition
							cond_idents = fvar cond
						-- unfold body to all body traces and filter for all Assignments to variables from the condition
						Left body_traces <- unfoldTracesM Nothing [] envs [] [[CBlockStmt body]]
						let
							body_traces_ass = map (concatMap from_ass) body_traces where
								from_ass (Assignment a@(CVar i _) b) | i `elem` cond_idents = [(a,b)]
								from_ass _ = []
						printLogV 2 $ "body_traces_ass =\n" ++
							(unlines $ for body_traces_ass $ \ bta ->
								intercalate " , " (map (\(a,b) -> "(" ++ (render.pretty) a ++ " = " ++ (render.pretty) b ++ ")") bta))
		
						-- Filter for all assignments that occur exactly once in every body trace
						let
							body_assigns = foldl1 intersect (map (exists_once) body_traces_ass)
							exists_once l = filter (\ e -> length (filter (==e) l) == 1) l
						printLogV 2 $ "body_assigns = \n" ++
							intercalate " , " (map (\(a,b) -> "(" ++ (render.pretty) a ++ " = " ++ (render.pretty) b ++ ")") body_assigns)

						case body_assigns of
							[ (counter_var@(CVar ass_ident _),ass_expr) ] -> do
								let
									is_ass_to_ass_var (Assignment (CVar ident _) _) | ident==ass_ident = True
									is_ass_to_ass_var _ = False
								case filter is_ass_to_ass_var trace of
									[] -> return (Nothing,"infer_loopingsM: There is no assignment to the loop counter " ++ (render.pretty) counter_var ++ " prior to the loop")
									ass@(Assignment _ i_0) : _ | null (fvar i_0)-> do
										printLogV 1 $ "last assignment to loop counter is " ++ show ass
										let i_n :: CExpr -> CExpr = case ass_expr of
											-- for all binops where the following holds (Linearity?):
											-- i_n = i_(n-1) `binop` c  =>  i_n = i_0 `binop` c
											CBinary binop (CVar ident _) cconst@(CConst _) _ | ident == ass_ident ∧ binop `elem` [CSubOp,CAddOp,CShrOp,CShlOp] ->
												\ n_var → CBinary binop i_0 (n_var ∗ cconst) undefNode
											_ -> error $ "infer_loopingsM: assignment " ++ (render.pretty) ass_ident ++ " := " ++ (render.pretty) ass_expr ++ " not implemented!"
										let
											n_name = "n_loopings"
											n_ident = internalIdent n_name
											n_var = CVar n_ident undefNode
											modelpath = analyzerPath </> n_name ++ show (lineColNodeInfo cond) ++ ".smtlib2"
										n_type <- case lookup ass_ident (map snd $ concat envs) of
											Nothing -> myError $ "infer_loopingsM: Could not find type of " ++ (render.pretty) counter_var
											Just ty -> return ty
										(model_string,mb_sol) <- makeAndSolveZ3ModelM
											((n_ident,n_type) : map snd (concat envs))
											(let
												cond_n       = (counter_var `substituteBy` (i_n n_var)) cond
												cond_nminus1 = (counter_var `substituteBy` (i_n $ n_var − ⅈ 1)) cond
												cond_0       = (counter_var `substituteBy` (i_n (ⅈ 0))) cond
												in
												[
													n_var ⩾ ⅈ 0,
													not_c cond_0  ⋏  n_var ⩵ ⅈ 0
														⋎
														cond_nminus1 ⋏ n_var ⩾ ⅈ 1 ⋏ not_c cond_n
												])
											[ SExpr [SLeaf "minimize",SLeaf n_name] ]
											[n_ident]
											modelpath
										return $ case mb_sol of
											Nothing                 -> (Nothing,"Found no solution for " ++ modelpath)
											Just sol@[(_,IntVal n)] -> (Just [n], "Found looping solution n = " ++ show sol)
											_                       -> (Nothing,"n_looping: Strange mb_sol=" ++ show mb_sol)
									ass -> return (Nothing,"infer_loopingsM: " ++ show ass ++ " is not assigning a constant.")
							other -> return (Nothing,"body contains not exactly one assignment of a variable from the condition " ++ (render.pretty) cond ++ ":\n" ++
								unlines (map (\(ass_var,_) -> (render.pretty) ass_var) other))
						
					_ -> return (Nothing,"condition " ++ (render.pretty) cond0 ++ " at " ++ (showLocation.lineColNodeInfo) cond0 ++ " contains a function call!")

	transids :: CExpr -> Trace -> ((CExpr,Trace) -> CovVecM UnfoldTracesRet) -> CovVecM UnfoldTracesRet
	transids expr trace cont = do
		additional_expr_traces :: [(CExpr,Trace)] <- translateExprM envs expr
		case mb_ret_type of
			Nothing -> do
				conts :: [UnfoldTracesRet] <- forM additional_expr_traces $ \ (expr',trace') -> do
					cont (expr',trace'++trace)
				let (conttracess,[]) = partitionEithers conts
				return $ Left $ concat conttracess
			Just _ -> try_next additional_expr_traces where
				try_next [] = return $ Right False
				try_next ((expr',trace'):rest) = do
					cont (expr',trace'++trace) >>= \case
						Right success -> case success of
							True -> return $ Right True
							False -> try_next rest

unfoldTraces1M mb_ret_type break_stack (env:envs) trace ( (CBlockDecl (CDecl [CTypeSpec typespec] triples _) : rest) : rest2 ) = do
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
			return (newenvitems,newdecls,initializers)
		triple -> myError $ "unfoldTracesM: triple " ++ show triple ++ " not implemented!"
	let (newenvs,newitems,initializerss) = unzip3 $ reverse new_env_items
	unfoldTracesM mb_ret_type break_stack ((concat newenvs ++ env) : envs) (concat newitems ++ trace) ((concat initializerss ++ rest):rest2)

unfoldTraces1M mb_ret_type break_stack (_:restenvs) trace ([]:rest2) = do
	let break_stack' = dropWhile (> (length rest2)) break_stack
	unfoldTracesM mb_ret_type break_stack' restenvs trace rest2

unfoldTraces1M mb_ret_type _ _ trace [] = case mb_ret_type of
	Nothing       -> return $ Left [trace]
	Just ret_type -> analyzeTraceM (Just ret_type) trace >>= return.Right

unfoldTraces1M _ _ _ _ ((cbi:_):_) = myError $ "unfoldTracesM " ++ (render.pretty) cbi ++ " not implemented yet."


infix 4 ⩵
(⩵) :: CExpr -> CExpr -> CExpr
a ⩵ b = CBinary CEqOp a b undefNode

infix 4 !⩵
(!⩵) :: CExpr -> CExpr -> CExpr
a !⩵ b = not_c $ CBinary CEqOp a b undefNode

infix 4 ⩾
(⩾) :: CExpr -> CExpr -> CExpr
a ⩾ b = CBinary CGeqOp a b undefNode

infixr 3 ⋏
(⋏) :: CExpr -> CExpr -> CExpr
a ⋏ b = CBinary CLndOp a b undefNode

infixr 2 ⋎
(⋎) :: CExpr -> CExpr -> CExpr
a ⋎ b = CBinary CLorOp a b undefNode

infixr 7 ∗
(∗) :: CExpr -> CExpr -> CExpr
a ∗ b = CBinary CMulOp a b undefNode

infixr 6 −
(−) :: CExpr -> CExpr -> CExpr
a − b = CBinary CSubOp a b undefNode

not_c :: CExpr -> CExpr
not_c e = CUnary CNegOp e (nodeInfo e)

ⅈ :: Integer -> CExpr
ⅈ i = CConst $ CIntConst (cInteger i) undefNode

infix 1 ≔
(≔) :: CExpr -> CExpr -> CExpr
ass ≔ expr = CAssign CAssignOp ass expr undefNode


fvar :: Data d => d -> [Ident]
fvar expr = nub $ everything (++) (mkQ [] searchvar) expr
	where
	searchvar :: CExpr -> [Ident]
	searchvar (CVar ident _) = [ ident ]
	searchvar _ = []

cinitializer2blockitems :: CExpr -> Type -> CInit -> CovVecM [CBlockItem]
cinitializer2blockitems lexpr ty initializer =
	case initializer of
		CInitExpr expr ni_init -> return [ CBlockStmt $ CExpr (
			Just $ lexpr ≔ expr ) ni_init ]
		CInitList initlist _ -> case ty of
			DirectType (TyComp (CompTypeRef sueref _ _)) _ _ -> do
				memberidentstypes <- getMembersM sueref
				concatForM (zip initlist memberidentstypes) $ \case
					(([],initializer),(memberident,memberty)) ->
						cinitializer2blockitems (CMember lexpr memberident False (nodeInfo memberident)) memberty initializer
					_ -> myError $ "cinitializer2blockitems: CPartDesignators not implemented yet!"
			_ -> myError $ "cinitializer2blockitems: " ++ (render.pretty) ty ++ " at " ++ (show $ nodeInfo lexpr) ++ " is no composite type!"

-- Translates all identifiers in an expression to fresh ones,
-- and expands function calls.
-- It needs to keep the original NodeInfos, because of the coverage information with is derived from the original source tree.
translateExprM :: [Env] -> CExpr -> CovVecM [(CExpr,Trace)]
translateExprM envs expr = do
	let	
		to_call :: CExpr -> StateT [(Ident,[CExpr],NodeInfo)] CovVecM CExpr
		to_call (CCall funexpr args ni) = case funexpr of
			CVar (Ident "__builtin_expect" _ _) _ -> return $ head args
			CVar (Ident "solver_pragma" _ _) _ -> return $ ⅈ 1
			CVar funident _ -> do
				modify ( (funident,args,ni): )
				return $ CConst $ CStrConst undefined ni
			_  -> myError $ "is_call: found call " ++ (render.pretty) funexpr
		to_call expr = return expr
	(expr',calls) <- runStateT (everywhereM (mkM to_call) expr) []

	let
		expr'' = renameVars envs expr'

	funcalls_traces :: [(NodeInfo,[(Trace,CExpr)])] <- forM calls $ \ (funident,args,ni) -> do
		FunDef (VarDecl _ _ (FunctionType (FunType _ paramdecls False) _)) body _ <- lookupFunM funident
		expanded_params_args <- expand_params_argsM paramdecls args
		let body' = replace_param_with_arg expanded_params_args body
		printLogV 2 $ "body'= " ++ (render.pretty) body'
		Left funtraces <- unfoldTracesM Nothing [] envs [] [ [ CBlockStmt body' ] ]
		forM_ funtraces $ \ tr -> printLogV 2 $ "funtrace = " ++ showTrace tr
		let funtraces_rets = concat $ for funtraces $ \case
			Return retexpr : tr -> [(tr,retexpr)]
			tr -> error $ "funcalls_traces: trace of no return:\n" ++ showTrace tr
		return (ni,funtraces_rets) 

	create_combinations expr'' [] funcalls_traces
	
	where

	expand_params_argsM ::  [ParamDecl] -> [CExpr] -> CovVecM [(Ident,CExpr)]
	expand_params_argsM paramdecls args = concatForM (zip paramdecls args) expandparam where
		expandparam :: (ParamDecl,CExpr) -> CovVecM [(Ident,CExpr)]
		expandparam (paramdecl,arg) = do
			let VarDecl (VarName srcident _) _ arg_ty = getVarDecl paramdecl
			return [(srcident,arg)]

	set_node_info :: CExpr -> CExpr
	set_node_info cexpr = everywhere (mkT subst_ni) cexpr where
		subst_ni :: NodeInfo -> NodeInfo
		subst_ni _ = nodeInfo expr

	create_combinations :: CExpr -> Trace -> [(NodeInfo,[(Trace,CExpr)])] -> CovVecM [(CExpr,Trace)]
	create_combinations expr trace [] = return [(set_node_info expr,trace)]
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
	subst_var expr = expr

tyspec2TypeM :: CTypeSpec -> CovVecM Type
tyspec2TypeM typespec = case typespec of
	CVoidType _  -> return $ DirectType TyVoid noTypeQuals noAttributes
	CIntType _   -> return $ DirectType (TyIntegral TyInt) noTypeQuals noAttributes
	CLongType _  -> return $ DirectType (TyIntegral TyLong) noTypeQuals noAttributes
	CCharType _  -> return $ DirectType (TyIntegral TyChar) noTypeQuals noAttributes
	CShortType _ -> return $ DirectType (TyIntegral TyShort) noTypeQuals noAttributes
	CFloatType _ -> return $ DirectType (TyFloating TyFloat) noTypeQuals noAttributes
	CTypeDef ident _ -> lookupTypeDefM ident
	_ -> myError $ "tyspec2TypeM: " ++ (render.pretty) typespec ++ " not implemented yet."


-- Substitutes an expression x by y everywhere in a
substituteBy :: (Data a) => CExpr -> CExpr -> a -> a
substituteBy x y a = everywhere (mkT substexpr) a
	where
	substexpr :: CExpr -> CExpr
	substexpr found_expr | x == found_expr = y
	substexpr found_expr                   = found_expr


-- Going from the end of the trace backwards,
-- for all ASSN ptr = expr where ptr is a pointer (probably expr=&...),
-- substitute ptr by expr in the already processed trace
-- and cancel */& operators in *(&x)
-- (input trace is in straight order, not reversed)

{- EXAMPLE:
	int mem;
	int x,y;
	int* p;
	p = &x;
	*p = 3;
	mem = *p;
	p = &y;
	*p = 4;
-}

elimInds :: Trace -> CovVecM Trace
elimInds trace = elim_indsM [] $ reverse trace
	where
	tyenv = createTyEnv trace
	elim_indsM :: Trace -> Trace -> CovVecM Trace
	elim_indsM res_trace [] = return res_trace
	elim_indsM res_trace (ti@(Assignment ptr@(CVar ptr_ident _) expr) : rest) = do
		case lookup ptr_ident tyenv of
			Nothing -> myError $ "elemInds: could not find " ++ (render.pretty) ptr_ident
			Just (PtrType _ _ _) -> elim_indsM (cancel_ind_adrs $ substituteBy ptr expr res_trace) rest
			_ -> elim_indsM (ti : res_trace) rest
	elim_indsM res_trace (ti : rest) = elim_indsM (ti : res_trace) rest

	cancel_ind_adrs :: Trace -> Trace
	cancel_ind_adrs trace = everywhere (mkT cancel_ind_adr) trace
		where
		cancel_ind_adr :: CExpr -> CExpr
		cancel_ind_adr (CUnary CIndOp (CUnary CAdrOp expr _) _) = expr
		cancel_ind_adr (CMember (CUnary CAdrOp obj _) member True ni) = CMember obj member False ni
		cancel_ind_adr expr = expr


-- FOLD TRACE BY SUBSTITUTING ASSIGNMENTS BACKWARDS
-- trace should be given reversed!

elimAssignmentsM :: Trace -> CovVecM Trace
elimAssignmentsM trace = foldtraceM [] $ reverse trace
	where
	foldtraceM :: Trace -> Trace -> CovVecM Trace
	foldtraceM result [] = return result
	foldtraceM result (Assignment lvalue expr : rest) = foldtraceM (substituteBy lvalue expr result) rest
	foldtraceM result (traceitem : rest) = foldtraceM (traceitem:result) rest


-- Simplify:
-- *(&x)  ~> x
-- &s->m  ~> s.m
-- (*p).m ~> p->m
-- (t*) p ~> p

simplifyTraceM :: Trace -> CovVecM Trace
simplifyTraceM trace = everywhereM (mkM simplify) trace where
	simplify :: CExpr -> CovVecM CExpr
	simplify (CUnary CIndOp (CUnary CAdrOp expr _) _) = return expr
	simplify (CMember (CUnary CAdrOp s _) member True ni) = return $ CMember s member False ni
	simplify (CMember (CUnary CIndOp p _) member False ni) = return $ CMember p member True ni
	simplify (CCast (CDecl _ [(Just (CDeclr Nothing [CPtrDeclr [] _] Nothing [] _),Nothing,Nothing)] _) subexpr _) =
		return subexpr
	simplify expr = return expr


-- Create symbolic vars for leftover expressions

createSymbolicVarsM :: Trace -> [Ident] -> Trace -> CovVecM Trace
createSymbolicVarsM res_trace _ [] = return $ reverse res_trace
createSymbolicVarsM res_trace new_idents (ti : rest) = do
	(ti',add_tis) <- runStateT (everywhereM (mkM createsymvar_m) ti) []
	createSymbolicVarsM (ti' : (map NewDeclaration add_tis) ++ res_trace) (map fst add_tis ++ new_idents) rest
	where
	
	create_var :: CExpr -> Type -> StateT [(Ident,Type)] CovVecM CExpr
	create_var expr ty = do
		let newident = mkIdentWithCNodePos expr $ lValueToVarName expr
		when (not $ newident `elem` new_idents) $
			modify ((newident,ty) : )
		return $ CVar newident (nodeInfo expr)

	createsymvar_m :: CExpr -> StateT [(Ident,Type)] CovVecM CExpr

	createsymvar_m expr@(CUnary CIndOp (CVar ptr_ident _) ni) = do
		let Just (PtrType ty _ _) = lookup ptr_ident $ createTyEnv res_trace
		create_var expr ty

	--  for ptr->member   create    p1_ARROW_member :: member_type
	createsymvar_m expr@(CMember (CVar ptr_ident _) member True _) = do
		let tyenv = createTyEnv res_trace
		case lookup ptr_ident tyenv of
			Nothing -> myError $ "createsymvar_m: Could not find " ++ (render.pretty) ptr_ident ++ " of " ++ (render.pretty) expr ++ " in " ++ showTyEnv tyenv
			Just (PtrType sue_ty _ _) -> do
				member_ty <- lift $ getMemberTypeM sue_ty member
				create_var expr member_ty

	--  for a.member   create    a_DOT_member :: member_type
	createsymvar_m expr@(CMember (CVar a_ident _) member False ni) = do
		return $ CVar (mkIdentWithCNodePos expr $ lValueToVarName expr) ni

	createsymvar_m expr@(CUnary CAdrOp (CVar a_ident _) _) = do
		let Just ty = lookup a_ident $ createTyEnv res_trace
		create_var expr $ PtrType ty noTypeQuals noAttributes

	createsymvar_m expr = return expr


type TyEnv = [(Ident,Type)]

createTyEnv :: Trace -> [TyEnvItem]
createTyEnv trace = concatMap traceitem2tyenv trace
	where
	traceitem2tyenv (NewDeclaration tyenvitem) = [tyenvitem]
	traceitem2tyenv _ = []

data SExpr = SExpr [SExpr] | SLeaf String | SComment String
instance Show SExpr where
	show (SLeaf s) = s
	show (SExpr sexprs) = "(" ++ intercalate " " (map show sexprs) ++ ")"
	show (SComment s) = "; " ++ s

data Z3_Type = Z3_Bool | Z3_BitVector Int Bool | Z3_Float | Z3_Double
	deriving (Show,Eq,Ord)
-- the derived ordering intentionally coincides with the type casting hierarchy :-)
z3Int = Z3_BitVector intSize False

type Constraint = CExpr

expr2SExpr :: TyEnv -> Expr -> CovVecM (SExpr,CExpr)
expr2SExpr tyenv expr = do
	let expr_inseq0 = insert_eq0 True expr
	(sexpr,_) <- expr2sexpr expr_inseq0
	return (sexpr,expr_inseq0)

	where

	make_intconstant :: Int -> Integer -> SExpr
	make_intconstant size const = SLeaf (printf "#x%*.*x" (size `div` 4) (size `div` 4) const)

	mb_cast :: SExpr -> Z3_Type -> Z3_Type -> SExpr
	mb_cast sexpr from_ty to_ty | from_ty == to_ty = sexpr
	mb_cast sexpr from_ty to_ty = case (from_ty,to_ty) of
		( Z3_BitVector size_from _, Z3_Bool ) -> SExpr [ SLeaf "ite", cond_sexpr, SLeaf "false", SLeaf "true" ]
			where
			cond_sexpr = SExpr [ SLeaf "=", sexpr, make_intconstant size_from 0 ]
		( Z3_BitVector size_from _, Z3_BitVector size_to True ) -> case size_from < size_to of
			True  -> SExpr [ SLeaf "concat", SExpr [SLeaf "_",SLeaf "bv0",SLeaf (show $ size_to-size_from)], sexpr ]
			False -> SExpr [ SExpr [ SLeaf "_", SLeaf "extract", SLeaf (show $ size_to - 1), SLeaf "0"], sexpr ]
		_ -> error $ "cast_expr " ++ (render.pretty) expr ++ " " ++
			show from_ty ++ " " ++ show to_ty ++ " not implemented!"

	bool_result_ops = [CLndOp,CLorOp,CLeOp,CGrOp,CLeqOp,CGeqOp,CEqOp,CNeqOp]

	neq0 :: Constraint -> Constraint
	neq0 constr = not_c $ constr ⩵ ⅈ 0

	insert_eq0 :: Bool -> Constraint -> Constraint
	insert_eq0 must_be_bool (CUnary unop expr ni) = case unop of
		CNegOp -> CUnary CNegOp (insert_eq0 True expr) ni
		unop   -> (if must_be_bool then neq0 else id) $ CUnary unop (insert_eq0 False expr) ni
	insert_eq0 must_be_bool (CCast ty expr ni) = (if must_be_bool then neq0 else id) $
		CCast ty (insert_eq0 False expr) ni
	insert_eq0 must_be_bool cvar@(CVar ident ni) = (if must_be_bool then neq0 else id) cvar
	insert_eq0 must_be_bool const@(CConst _) = (if must_be_bool then neq0 else id) const
	insert_eq0 False cond@(CBinary binop expr1 expr2 ni) | binop `elem` bool_result_ops =
		CCond (insert_eq0 True cond) (Just $ ⅈ 1) (ⅈ 0) ni
	insert_eq0 must_be_bool (CBinary binop expr1 expr2 ni) = mb_eq0 $
		CBinary binop (insert_eq0 must_be_bool' expr1) (insert_eq0 must_be_bool' expr2) ni
		where
		(must_be_bool',mb_eq0) = case must_be_bool of
			_ | binop `elem` [CLndOp,CLorOp] -> (True,id)
			_ | binop `elem` [CLeOp,CGrOp,CLeqOp,CGeqOp,CEqOp,CNeqOp] -> (False,id)
			True -> (False,neq0)
			_ -> (False,id)
	insert_eq0 must_be_bool cmember@(CMember _ _ _ _) = (if must_be_bool then neq0 else id) cmember
	insert_eq0 _ expr = error $ "insert_eq0 " ++ (render.pretty) expr ++ " not implemented yet."

	expr2sexpr :: CExpr -> CovVecM (SExpr,Z3_Type)

	expr2sexpr (CConst cconst) = return $ case cconst of
		CIntConst intconst@(CInteger _ _ flags) _ -> ( const_sexpr, ty2Z3Type $ DirectType (TyIntegral intty) noTypeQuals noAttributes )
			where
			const_sexpr = make_intconstant int_size (getCInteger intconst)
			(intty,int_size) = case map ($ flags) (map testFlag [FlagUnsigned,FlagLong,FlagLongLong,FlagImag]) of
				[False,False,False,False] -> (TyUInt,intSize)
				[False,True, False,False] -> (TyULong,longIntSize)
				[False,False,True, False] -> (TyULLong,longLongIntSize)
				[True, False,False,False] -> (TyInt,intSize)
				[True, True, False,False] -> (TyLong,longIntSize)
				[True, False,True, False] -> (TyLLong,longLongIntSize)
				_ -> error $ "infer_type: Strange flags in " ++ (render.pretty) cconst
		CCharConst cchar _   -> ( SLeaf $ (render.pretty) cconst, ty2Z3Type $ DirectType (TyIntegral TyChar) noTypeQuals noAttributes )
		CFloatConst cfloat _ -> ( SLeaf $ (render.pretty) cconst, ty2Z3Type $ DirectType (TyFloating TyDouble) noTypeQuals noAttributes )
		CStrConst cstr _     -> ( SLeaf $ (render.pretty) cconst, ty2Z3Type $ PtrType undefined noTypeQuals noAttributes )

	expr2sexpr (CVar ident _) =
		return ( SLeaf $ (render.pretty) ident, ty2Z3Type $ fromJust $ lookup ident tyenv )

	expr2sexpr ccast@(CCast (CDecl [CTypeSpec ctypespec] [] _) subexpr _) = do
		(subsexpr,from_ty) <- expr2sexpr subexpr
		ty_m <- tyspec2TypeM ctypespec
		ty_elimd <- elimTypeDefsM ty_m
		let to_ty = ty2Z3Type ty_elimd
		printLogV 0 $ "#### expr2sexpr " ++ (render.pretty) ccast ++ " : subsexpr=" ++ show subsexpr
		printLogV 0 $ "####            ctypespec=" ++ (render.pretty) ctypespec
		printLogV 0 $ "####            ty_m=" ++ (render.pretty) ty_m
		printLogV 0 $ "####            ty_elimd=" ++ (render.pretty) ty_elimd
		printLogV 0 $ "####            from_ty=" ++ show from_ty ++ " , to_ty=" ++ show to_ty
		return (mb_cast subsexpr from_ty to_ty,to_ty)

	expr2sexpr (CUnary CPlusOp expr _) = expr2sexpr expr
	expr2sexpr (CUnary CNegOp expr _) = do
		(sexpr,Z3_Bool) <- expr2sexpr expr
		return ( SExpr [ SLeaf "not", sexpr ], Z3_Bool )
	expr2sexpr (CUnary op expr _) = do
		(sexpr,ty) <- expr2sexpr expr
		return ( SExpr [ SLeaf op_str, mb_cast sexpr ty ty], ty )
		where
		op_str = case op of
			CMinOp  -> "bvneg"
			CCompOp -> "bvnot"
			_ -> error $ "expr2sexpr " ++ (render.pretty) op ++ " should not occur!"

	expr2sexpr (CBinary CNeqOp expr1 expr2 _) = expr2sexpr (CUnary CNegOp (CBinary CEqOp expr1 expr2 undefNode) undefNode)
	expr2sexpr expr@(CBinary op expr1 expr2 _) = do
		(sexpr1,ty1) <- expr2sexpr expr1
		printLogV 0 $ "#### expr2sexpr " ++ (render.pretty) expr ++ " sexpr1=" ++ show sexpr1
		(sexpr2,ty2) <- expr2sexpr expr2
		let (op_sexpr,operand_target_ty,expr_target_ty) = opexpr_ty ty1 ty2
		return ( SExpr [ op_sexpr, mb_cast sexpr1 ty1 operand_target_ty, mb_cast sexpr2 ty2 operand_target_ty ], expr_target_ty )
		where
		opexpr_ty ty1 ty2 = case op of
		--            (function,     operands' types,  operation's result type)
			CMulOp -> (SLeaf "bvmul",operand_target_ty,operand_target_ty)
			CDivOp -> (SLeaf "bvdiv",operand_target_ty,operand_target_ty)
			CAddOp -> (SLeaf "bvadd",operand_target_ty,operand_target_ty)
			CSubOp -> (SLeaf "bvsub",operand_target_ty,operand_target_ty)
			CRmdOp -> (unSigned operand_target_ty "bvurem" "bvsrem",operand_target_ty,operand_target_ty)
			CShlOp -> (unSigned operand_target_ty "bvshl"  "bvshl", operand_target_ty,operand_target_ty)
			CShrOp -> (unSigned operand_target_ty "bvlshr" "bvashr",operand_target_ty,operand_target_ty)
			CLeOp  -> (unSigned operand_target_ty "bvult"  "bvslt", operand_target_ty,Z3_Bool)
			CGrOp  -> (unSigned operand_target_ty "bvugt"  "bvsgt", operand_target_ty,Z3_Bool)
			CLeqOp -> (unSigned operand_target_ty "bvule"  "bvsle", operand_target_ty,Z3_Bool)
			CGeqOp -> (unSigned operand_target_ty "bvuge"  "bvsge", operand_target_ty,Z3_Bool)
			CLndOp -> (SLeaf "and",Z3_Bool,Z3_Bool)
			CLorOp -> (SLeaf "or", Z3_Bool,Z3_Bool)
			CEqOp  -> (SLeaf "=",    operand_target_ty,Z3_Bool)
			CAndOp -> (SLeaf "bvand",operand_target_ty,operand_target_ty)
			COrOp  -> (SLeaf "bvor", operand_target_ty,operand_target_ty)
			CXorOp -> (SLeaf "bvxor",operand_target_ty,operand_target_ty)
			_ -> error $ "expr2sexpr " ++ (render.pretty) expr ++ " : operand not implemented!"
			where
			operand_target_ty = max ty1 ty2

		unSigned op_ty unsigned signed = case op_ty of
			Z3_BitVector _ is_unsigned -> SLeaf $ if is_unsigned then unsigned else signed
			_ -> error $ "unSigned: target_ty of " ++ (render.pretty) expr ++ " is no bitvector!"

	expr2sexpr ccond@(CCond cond (Just then_expr) else_expr _) = do
		(cond_sexpr,Z3_Bool) <- expr2sexpr cond
		(then_sexpr,then_ty) <- expr2sexpr then_expr
		(else_sexpr,else_ty) <- expr2sexpr else_expr
		when (then_ty /= else_ty) $ myError $ "expr2sexpr " ++ (render.pretty) ccond ++
			" : then_ty=" ++ show then_ty ++ " /= else_ty=" ++ show else_ty
		return ( SExpr [ SLeaf "ite", cond_sexpr, then_sexpr, else_sexpr ], then_ty )

	expr2sexpr (cmember@(CMember _ _ _ _)) = myError $ "expr2sexpr " ++ (render.pretty) cmember ++ " should not occur!"

	expr2sexpr (ccall@(CCall _ _ _)) = myError $ "expr2SExpr " ++ (render.pretty) ccall ++ " should not occur!"
	
	expr2sexpr expr = myError $ "expr2SExpr " ++ (render.pretty) expr ++ " not implemented" 


ty2Z3Type :: Type -> Z3_Type
ty2Z3Type ty = case ty of
	DirectType (TyComp (CompTypeRef _ _ _)) _ _ -> Z3_BitVector 4 True
	DirectType tyname _ attrs -> case tyname of
		TyIntegral intty -> case (intty,map to_mode attrs) of
			(TyChar,[])   -> Z3_BitVector 8 True
			(TySChar,[])  -> Z3_BitVector 8 False
			(TyUChar,[]) -> Z3_BitVector 8 True
			(TyShort,[]) -> Z3_BitVector 16 False
			(TyUShort,[]) -> Z3_BitVector 16 True
			(TyInt,[])   -> Z3_BitVector intSize False
			(TyInt,["SI"])   -> Z3_BitVector 32 False
			(TyInt,["DI"])   -> Z3_BitVector 64 False
			(TyUInt,[])  -> Z3_BitVector intSize True
			(TyUInt,["SI"])  -> Z3_BitVector 32 True
			(TyUInt,["DI"])  -> Z3_BitVector 64 True
			(TyLong,[])  -> Z3_BitVector longIntSize False
			(TyULong,[])  -> Z3_BitVector longIntSize True
			(TyLLong,[])  -> Z3_BitVector 64 False
			(TyULLong,[]) -> Z3_BitVector 64 True
			other         -> error $ "ty2Z3Type " ++ show other ++ " not implemented!"
		TyFloating TyFloat  -> Z3_Float
		TyFloating TyDouble -> Z3_Double
		TyEnum _ -> Z3_BitVector intSize True
		TyComp _ -> Z3_BitVector 16 True
		_ -> error $ "ty2Z3Type " ++ (render.pretty) ty ++ " not implemented!"
		where
		to_mode (Attr (Ident "mode" _ _) [CVar (Ident mode _ _) _] _) = mode
		to_mode attr = error $ "unknown attr " ++ (render.pretty) attr
	PtrType _ _ _ -> Z3_BitVector 16 True
	_ -> error $ "ty2Z3Type " ++ (render.pretty) ty ++ " should not occur!"

ty2SExpr :: Type -> SExpr
ty2SExpr ty = case ty2Z3Type ty of
	Z3_BitVector size _ -> SExpr [ SLeaf "_", SLeaf "BitVec", SLeaf (show size) ]
	Z3_Float            -> SLeaf "Float32"
	Z3_Double           -> SLeaf "Float64"

type Solution = [(String,SolutionVal)]

data SolutionVal = IntVal Int | FloatVal Float | DoubleVal Double
instance Eq SolutionVal where
	IntVal i1    == IntVal i2    = i1==i2
	FloatVal f1  == FloatVal f2  = f2-f1 <= floatTolerance
	DoubleVal f1 == DoubleVal f2 = f2-f1 <= doubleTolerance

instance Show SolutionVal where
	show (IntVal i)    = show i
	show (FloatVal f)  = show f
	show (DoubleVal f) = show f

makeAndSolveZ3ModelM :: TyEnv -> [CExpr] -> [SExpr] -> [Ident] -> String -> CovVecM (String,Maybe Solution)
makeAndSolveZ3ModelM tyenv constraints additional_sexprs output_idents modelpathfile = do
	opts <- gets optsCVS
	printLogV 2 $ "output_idents = " ++ showIdents output_idents
	let
		constraints_vars = fvar constraints
	printLogV 2 $ "constraints_vars = " ++ showIdents constraints_vars

	let
		varsZ3 = for (filter ((`elem` (constraints_vars ++ output_idents)).fst) tyenv) $ \ (ident,ty) ->
			SExpr [ SLeaf "declare-const", SLeaf (identToString ident), ty2SExpr ty ]
	constraintsZ3 <- concatForM constraints $ \ expr -> do
		(assert_sexpr,real_expr) <- expr2SExpr tyenv expr
		return [ SComment ((render.pretty) real_expr), SExpr [SLeaf "assert", assert_sexpr] ]
	let
		outputvarsZ3 = for output_idents $ \ ident -> SExpr [SLeaf "get-value", SExpr [ SLeaf $ identToString ident ] ]
		model = [
			SExpr [SLeaf "set-option", SLeaf ":smt.relevancy", SLeaf "0"],
			SExpr [SLeaf "set-option", SLeaf ":produce-models", SLeaf "true"] ] ++
			varsZ3 ++
			constraintsZ3 ++
			additional_sexprs ++
			[ SExpr [SLeaf "check-sat"] ] ++
			outputvarsZ3
		model_string = unlines $ map show model
		model_string_linenumbers = unlines $ map (\ (i,l) -> show i ++ ": " ++ l) (zip [1..] (lines model_string))
	when ("-writeModels" `elem` opts) $ liftIO $ writeFile modelpathfile model_string
	printLogV 2 $ "Model " ++ takeFileName modelpathfile ++ " =\n" ++ model_string_linenumbers
	printStatsM
	printLogV 1 $ "Running model " ++ takeFileName modelpathfile ++ "..."
	(_,output,_) <- liftIO $ withCurrentDirectory (takeDirectory modelpathfile) $ do
		readProcessWithExitCode z3FilePath ["-smt2","-in","parallel.enable=true"] model_string
	printLogV 2 output
	case lines output of
		"unsat"   : _ -> return (model_string_linenumbers,Nothing)
		"unknown" : _ -> return (model_string_linenumbers,Nothing)
		"sat" : rest -> do
			sol_params <- forM output_idents $ \ ident -> do
				let is = identToString ident
				case (unlines rest) =~ ("^\\(\\(" ++ is ++ " ([^\\)]+)\\)\\)$") :: (String,String,String,[String]) of
					(_,_,_,[val_string]) -> case lookup ident tyenv of
						Nothing -> myError $ "Parsing z3 output: Could not find type of " ++ is
						Just ty -> return (is, case ty2Z3Type ty of
							Z3_BitVector size unsigned -> let
								'#':'x':hexdigits = val_string
								[(i :: Integer,"")] = readHex hexdigits
								in
								IntVal $ case unsigned of
									True -> fromIntegral i
									False  -> fromIntegral $ if i < 2^(size-1) then i else i - 2^size
							Z3_Float -> FloatVal (read val_string :: Float) )

					_ -> myError $ "Parsing z3 output: Could not find " ++ is
			return (model_string_linenumbers,Just sol_params)
		_ -> myError $ "Execution of " ++ z3FilePath ++ " failed:\n" ++ output ++ "\n\n" ++
			"Model is\n" ++ model_string_linenumbers


-- In case of a cutoff, mb_ret_type is Nothing.
solveTraceM :: Maybe Type -> [Int] -> Trace -> CovVecM (Either Bool ResultData)
solveTraceM mb_ret_type traceid trace = do
	let
		tracename = show traceid
	retval_env_exprs  <- case mb_ret_type of
		Nothing  -> return []
		Just ret_type -> createInterfaceM [(internalIdent returnval_var_name,ret_type)]
	Just param_env <- gets paramEnvCVS
	let
		retval_env  = map fst retval_env_exprs
		param_names = map (fst.snd) param_env
		ret_names   = map (fst.snd) retval_env
		constraints = concatMap traceitem2constr trace where
		traceitem2constr (Condition _ expr) = [expr]
		traceitem2constr _ = []
		debug_outputs = concatMap is_debug_output trace where
			is_debug_output (DebugOutput name (expr,ty)) = [(name,expr,ty)]
			is_debug_output _ = []
		(debug_idents,debug_constraints,debug_tyenv) = unzip3 $ for (zip [1..] debug_outputs) $ \ (i,(name,expr,ty)) ->
			let name_id = internalIdent (name ++ "_" ++ show i) in (name_id,CVar name_id undefNode ⩵ expr,(name_id,ty))

		tyenv = createTyEnv trace ++ debug_tyenv

	(model_string,mb_sol) <- makeAndSolveZ3ModelM
		tyenv
		(constraints ++ debug_constraints)
		(for param_names $ \ name -> SExpr [SLeaf "minimize",SLeaf (identToString name)])
		(param_names ++ ret_names ++ debug_idents)
		(analyzerPath </> "models" </> "model_" ++ tracename ++ ".smtlib2")

	return $ case mb_ret_type of
		Nothing -> Left $ isJust mb_sol
		Just _ -> Right (model_string,case mb_sol of
			Nothing -> Nothing
			Just sol -> Just (param_env,retval_env,sol))


checkSolutionM :: [Int] -> ResultData -> CovVecM ResultData
checkSolutionM _ resultdata | not checkSolutions = return resultdata
checkSolutionM traceid resultdata@(_,Nothing) = do
	printLog $ "No solution to check for " ++ show traceid
	return resultdata
checkSolutionM traceid resultdata@(_,Just (_,_,[])) = do
	printLog $ "Empty solution cannot be checked for " ++ show traceid
	return resultdata
checkSolutionM traceid resultdata@(_,Just (param_env,ret_env,solution)) = do
	printLogV 2 $ "checkSolution param_env =\n" ++ showEnv param_env
	printLogV 2 $ "checkSolution ret_env =\n" ++ showEnv ret_env
	srcfilename <- gets srcFilenameCVS
	Just filename <- gets checkExeNameCVS
	absolute_filename <- liftIO $ makeAbsolute srcfilename
	let
		args = concat $ for param_env $ \ (_,(newident,ty)) -> case ty of
			DirectType _ _ _ -> case lookup (identToString newident) solution of
				Nothing -> ["99"]
				Just v -> [show v]
			PtrType target_ty _ _ -> ["65000"]
			ty -> error $ "checkSolutionM args: type " ++ (render.pretty) ty ++ " not implemented!"
	printLogV 2 $ "checkSolution args = " ++ show args
	(exitcode,stdout,stderr) <- liftIO $ withCurrentDirectory (takeDirectory absolute_filename) $ do
		readProcessWithExitCode (takeFileName filename) args ""
	case exitcode of
		ExitFailure _ -> myError $ "Execution of " ++ filename ++ " failed:\n" ++ stdout ++ stderr
		ExitSuccess -> do
			let
				outputs = words $ last $ lines stdout
				-- get all solution vars that are in the ret_env
				ret_solution = filter ((`elem` (map (identToString.fst) ret_env)).fst) solution
			when (length ret_env /= length outputs || length outputs /= length ret_solution) $
				myError $ "checkSolutionM: lengths of ret_env, solution, outputs differ:\n" ++
					"ret_env = " ++ showEnv ret_env ++ "\n" ++
					"ret_solution = " ++ show ret_solution ++ "\n" ++
					"outputs = " ++ show outputs ++ "\n"
			forM_ (zip3 ret_env outputs ret_solution) $ \ ((sourceident,(ident,ty)),s,(ident_s,predicted_result)) -> do
				when (identToString ident /= ident_s) $
					myError $ "checkSolutionM: ident=" ++ identToString ident ++ " and ident_s=" ++ ident_s ++ " mismatch"
				case ty of
					PtrType _ _ _ -> return ()
					DirectType (TyComp _) _ _ -> return ()
					_ -> do
						let exec_result = case ty of
							DirectType (TyIntegral _) _ _       -> IntVal (read s)
							DirectType (TyFloating TyFloat) _ _ -> FloatVal (read s)
							DirectType (TyEnum _) _ _           -> IntVal (read s)
							_ -> error $ "checkSolutionM: parsing type " ++ (render.pretty) ty ++ " of " ++ ident_s ++ " not implemented!"
						when (exec_result /= predicted_result) $ do
							let txt = "ERROR in " ++ show traceid ++ " for " ++ ident_s ++ " : exec_val=" ++ show exec_result ++ " /= predicted_result=" ++ show predicted_result
							myError txt

			printLog $ "checkSolutionM " ++ show traceid ++ " OK."
			return resultdata

{-
(set-option :pp.fp_real_literals true)
(declare-const i Int)
(declare-const ar (Array Int Float32))

(assert (<= 0 i))
(assert (<= i 3))
(assert (= (store ar 1 1.0) ar))
(assert (= (store ar 2 4.0) ar))
(assert (= (store ar 3 9.0) ar))

(assert (>= (select ar i) (roundTowardZero (/ 5 1))))

(check-sat)
(get-model)
(get-value (i))

Data.SBV
-}