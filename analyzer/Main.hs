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
import Data.Either

import "language-c-quote" Language.C.Quote.GCC
import "language-c-quote" Language.C.Pretty
import Text.PrettyPrint.Mainland.Class (ppr)
import Text.PrettyPrint.Mainland (prettyCompact)

import DataTree
import GlobDecls

for :: [a] -> (a -> b) -> [b]
for = flip map

concatForM = flip concatMapM

intSize = 32
longIntSize = 64

solveIt = True
showOnlySolutions = True
don'tShowTraces = True
checkSolutions = solveIt && True
returnval_var_name = "return_val"
outputVerbosity = 1
floatTolerance = 1e-7 :: Float
doubleTolerance = 1e-10 :: Double
showBuiltins = False
cutOffs = True

sameConditionThreshold = 1000
sameConditionThresholdExceptions = []
_UNROLLING_DEPTHS = [0..32]
sizeConditionChunks = 4

z3FilePath = "C:\\z3-4.8.8-x64-win\\bin\\z3.exe"

analyzerPath = "analyzer"
logFile = analyzerPath </> "log.txt"

printLog :: (MonadIO m) => String -> m ()
printLog text = liftIO $ do
	putStrLn text
	appendFile logFile (text++"\n")

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
show_solution funname (Just v@(_,_,solution)) = unlines [ show solution, showTestVector funname v ]

main :: IO ()
main = do
	-- when there is an error, we'd like to have *all* output till then
	hSetBuffering stdout NoBuffering

	gcc:filename:funname:opts <- getArgs >>= return . \case
--		[] -> "gcc" : (analyzerPath++"\\test.c") : "g" : [] --["-writeAST","-writeGlobalDecls"]
		[] -> "gcc" : (analyzerPath++"\\myfp-bit_mul.c") : "_fpmul_parts" : [] --"-writeAST","-writeGlobalDecls"]
--		[] -> "gcc" : (analyzerPath++"\\myfp-bit.c") : "_fpdiv_parts" : [] --"-writeAST","-writeGlobalDecls"]
--		[] -> "gcc" : (analyzerPath++"\\OscarsChallenge\\sin\\oscar.c") : "_Sinx" : [] --"-writeAST","-writeGlobalDecls"]
--		[] -> "gcc" : (analyzerPath++"\\switchtest.c") : "f" : [] --"-writeAST","-writeGlobalDecls"]
--		[] -> "gcc" : (analyzerPath++"\\whiletest2.c") : "_fpdiv_parts" : [] --"-writeAST","-writeGlobalDecls"]
--		[] -> "gcc" : (analyzerPath++"\\branchtest.c") : "f" : ["-writeTree"] --["-writeAST","-writeGlobalDecls"]
--		[] -> "gcc" : (analyzerPath++"\\iftest.c") : "f" : [] --["-writeAST","-writeGlobalDecls"]
--		[] -> "gcc" : (analyzerPath++"\\deadtest.c") : "f" : [] --["-writeAST","-writeGlobalDecls"]
--		[] -> "gcc" : (analyzerPath++"\\whiletest.c") : "f" : [] --["-writeAST","-writeGlobalDecls"]
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
	
						(full_coverage,testvectors,covered,alls) <- evalStateT (covVectorsM filename opts) $
							CovVecState globdecls 1 translunit filename Nothing funname undefined 0 gcc opts Nothing ([],Set.empty,Set.empty)

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
	analysisStateCVS :: ([TraceAnalysisResult],Set.Set Branch,Set.Set Branch)
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

instance Pretty NodeInfo where
	pretty ni = text $ "line " ++ show line ++ ", col " ++ show col
		where
		(line,col) = lineColNodeInfo ni

instance (Pretty a) => Pretty [a] where
	pretty xs = brackets $ hcat $ punctuate comma (map pretty xs)

instance Show TraceElem where
	show te = ( case te of
		(Assignment lvalue expr)   -> "ASSN " ++ (render.pretty) lvalue ++ " = " ++ (render.pretty) expr
		(Condition b expr)         -> "COND " ++ (if b then "(THEN) " else "(ELSE) ") ++ (render.pretty) expr
		(NewDeclaration (lval,ty)) -> "DECL " ++ (render.pretty) lval ++ " :: " ++ (render.pretty) ty
		(Return exprs)             -> "RET  " ++ (render.pretty) exprs
		) ++ "  (" ++ (render.pretty) (nodeInfo te) ++ ")"

type Trace = [TraceElem]

showTrace :: Trace -> String
showTrace trace = unlines $ concatMap show_te trace where
	show_te te | showBuiltins || not (isnotbuiltin te) = [show te]
	show_te _ = []


type ResultData = (String,Maybe (Env,Env,Solution))
type TraceAnalysisResult = ([Int],Trace,ResultData)


type SolveFunRet = (Bool,([TraceAnalysisResult],Set.Set Branch,Set.Set Branch))
--type UnfoldTracesRet = Either [Trace] SolveFunRet

covVectorsM :: String -> CovVecM SolveFunRet
covVectorsM filename = do
	funname <- gets funNameCVS
	globdecls <- gets ((Map.elems).gObjs.globDeclsCVS)
	glob_env <- concatMapM declaration2EnvItemM globdecls
	let
		def2stmt :: IdentDecl -> CovVecM [CBlockItem]
		def2stmt (EnumeratorDef (Enumerator ident expr _ ni)) = return $
			[ CBlockStmt (CExpr (Just $ CAssign CAssignOp (CVar ident (nodeInfo ident)) expr ni) ni) ]
		def2stmt (ObjectDef (ObjDef (VarDecl (VarName ident _) _ ty) (Just initializer) ni)) = do
			ty' <- elimTypeDefsM ty
			cinitializer2blockitems (CVar ident ni) ty' initializer
		def2stmt _ = return []
	-- creates the assignment statements from the global context
	defs <- concatMapM def2stmt globdecls

	FunDef (VarDecl _ _ (FunctionType (FunType ret_type funparamdecls False) _)) body fundef_ni <-
		lookupFunM (builtinIdent funname)
	ret_type' <- elimTypeDefsM ret_type

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
		let
			srcfilename = takeFileName filename
			chkexefilename = replaceExtension srcfilename "exe"
		absolute_filename <- liftIO $ makeAbsolute filename
		gcc <- gets compilerCVS
		(exitcode,stdout,stderr) <- liftIO $ withCurrentDirectory (takeDirectory absolute_filename) $ do
			readProcessWithExitCode gcc ["-o",chkexefilename,"-DCALC",srcfilename] ""
		case exitcode of
			ExitFailure _ -> myError $ "Compilation failed:\n" ++ stderr
			ExitSuccess -> modify $ \ s -> s { checkExeNameCVS = Just chkexefilename }

{-
	let solve_fun cutoff tr = runStateT (analyzeTreeM cutoff opts ret_type' param_env [] [] tr) ([],Set.empty,Set.empty)
	modify $ \ s -> s { solveFunCVS = Just solve_fun }
-}

--type UnfoldTracesRet = Either [Trace] SolveFunRet
	Right solvefunret <- unfoldTracesM (Just ret_type') [] (param_env:[glob_env]) decls [ defs ++ [ CBlockStmt body ] ]
	return solvefunret

{-
	case mb_trace of
		Nothing -> return (False,([],Set.empty,Set.empty))
		Just trace -> do
			when ("-writeTree" ∈ opts) $ liftIO $ writeFile (filename ++ "_tree" <.> "html") $ traceToHTMLString trace
			when (False {-not don'tShowTraces-}) $ do
				printLog $ "\n********** TRACE ***********\n" ++ showTrace 0 trace
				printLog $ "****************************\n"
		
			solve_fun False trace
-}

type Location = (Int,Int)

showLocation :: Location -> String
showLocation (l,c) = "line " ++ show l ++ ", col " ++ show c


--type AnalyzeTraceM a = StateT ([TraceAnalysisResult],Set.Set Branch,Set.Set Branch) CovVecM a

analyzeTraceM :: Bool -> Type -> [Int] -> [TraceElem] -> CovVecM Bool
analyzeTraceM cutoff ret_type traceid res_line = do

	when (not don'tShowTraces) $ do
		printLog $ "=== TRACE " ++ show traceid ++ " ========================\n" ++
			if showBuiltins then "" else "<leaving out builtins...>\n"
		printLog $ showLine res_line

	res_trace_elim_inds <- elimInds res_line
	when (not don'tShowTraces) $ do
		printLog $ "\n=== TRACE after elimInds " ++ show traceid ++ " =========\n" ++
			if showBuiltins then "" else "<leaving out builtins...>\n"
		printLog $ showLine res_trace_elim_inds

	res_trace_simplified1 <- simplifyTraceM res_trace_elim_inds
	when (not don'tShowTraces) $ do
		printLog $ "\n--- TRACE after simplifyTraceM 1 " ++ show traceid ++ " -----------\n" ++
			if showBuiltins then "" else "<leaving out builtins...>\n"
		printLog $ showLine res_trace_simplified1

	res_trace_elim'd_assigns <- elimAssignmentsM res_trace_simplified1
	when (not don'tShowTraces) $ do
		printLog $ "\n--- TRACE after elimAssignmentsM " ++ show traceid ++ " -----------\n" ++
			if showBuiltins then "" else "<leaving out builtins...>\n"
		printLog $ showLine res_trace_elim'd_assigns

	res_trace_simplified2 <- simplifyTraceM res_trace_elim'd_assigns
	when (not don'tShowTraces) $ do
		printLog $ "\n--- TRACE after simplifyTraceM 2 " ++ show traceid ++ " -----------\n" ++
			if showBuiltins then "" else "<leaving out builtins...>\n"
		printLog $ showLine res_trace_simplified2

	res_trace_symbolic <- createSymbolicVarsM [] (map fst $ createTyEnv res_trace_simplified2) res_trace_simplified2
	when (not don'tShowTraces) $ do
		printLog $ "\n--- TRACE after createSymbolicVarsM " ++ show traceid ++ " -----------\n" ++
			if showBuiltins then "" else "<leaving out builtins...>\n"
		printLog $ showLine res_trace_symbolic

	resultdata@(model_string,mb_solution) <- solveTraceM cutoff ret_type traceid res_trace_symbolic
	when (not don'tShowTraces) $ printLog $ "\n--- MODEL " ++ show traceid ++ " -------------------------\n" ++ model_string
	funname <- lift $ gets funNameCVS
	printLogV 1 $ "--- TRACE " ++ show traceid ++ " ----------------------\n" ++
		show_solution funname mb_solution ++ "\n"

	startend <- lift $ gets funStartEndCVS
	let visible_trace = Set.fromList $ concatMap to_branch res_line
		where
		to_branch cond@(Condition b _) | is_visible_traceelem startend cond =
			[ (if b then Then else Else) (lineColNodeInfo cond) ]
		to_branch _ = []

	let traceanalysisresult :: TraceAnalysisResult = (traceid,res_line,resultdata)
	case is_solution traceanalysisresult of
		False -> do
			printLogV 2  $ "### FALSE : " ++ show traceid ++ " no solution!"
			modify $ \ (tas,covered,alls) -> (tas,covered,Set.union visible_trace alls)
			return False
		True  -> do
			printLogV 2  $ "### TRUE : " ++ show traceid ++ " Is Solution"
			when (not cutoff) $ lift $ checkSolutionM traceid resultdata >> return ()
			modify $ \ (tas,covered,alls) -> case visible_trace `Set.isSubsetOf` covered of
				False -> (traceanalysisresult:tas,Set.union visible_trace covered,Set.union visible_trace alls)
				True  -> (tas,covered,alls)
			return True
{-
type AnalyzeTreeM a = StateT ([TraceAnalysisResult],Set.Set Branch,Set.Set Branch) CovVecM a
-- Unfolds the tree to all execution paths, collecting the solutions and promoting them upwards.

analyzeTreeM :: Bool -> [String] -> Type -> Env -> [Int] -> [TraceElem] -> Trace -> AnalyzeTreeM Bool

analyzeTreeM cutoff opts ret_type param_env traceid res_line [] = do
	when (not don'tShowTraces) $ do
		printLog $ "=== TRACE " ++ show traceid ++ " ========================\n" ++
			if showBuiltins then "" else "<leaving out builtins...>\n"
		printLog $ showLine res_line

	res_trace_elim_inds <- lift $ elimInds res_line
	when (not don'tShowTraces) $ do
		printLog $ "\n=== TRACE after elimInds " ++ show traceid ++ " =========\n" ++
			if showBuiltins then "" else "<leaving out builtins...>\n"
		printLog $ showLine res_trace_elim_inds

	res_trace_simplified1 <- lift $ simplifyTraceM res_trace_elim_inds
	when (not don'tShowTraces) $ do
		printLog $ "\n--- TRACE after simplifyTraceM 1 " ++ show traceid ++ " -----------\n" ++
			if showBuiltins then "" else "<leaving out builtins...>\n"
		printLog $ showLine res_trace_simplified1

	res_trace_elim'd_assigns <- lift $ elimAssignmentsM res_trace_simplified1
	when (not don'tShowTraces) $ do
		printLog $ "\n--- TRACE after elimAssignmentsM " ++ show traceid ++ " -----------\n" ++
			if showBuiltins then "" else "<leaving out builtins...>\n"
		printLog $ showLine res_trace_elim'd_assigns

	res_trace_simplified2 <- lift $ simplifyTraceM res_trace_elim'd_assigns
	when (not don'tShowTraces) $ do
		printLog $ "\n--- TRACE after simplifyTraceM 2 " ++ show traceid ++ " -----------\n" ++
			if showBuiltins then "" else "<leaving out builtins...>\n"
		printLog $ showLine res_trace_simplified2

	res_trace_symbolic <- lift $ createSymbolicVarsM [] (map fst $ createTyEnv res_trace_simplified2) res_trace_simplified2
	when (not don'tShowTraces) $ do
		printLog $ "\n--- TRACE after createSymbolicVarsM " ++ show traceid ++ " -----------\n" ++
			if showBuiltins then "" else "<leaving out builtins...>\n"
		printLog $ showLine res_trace_symbolic

	resultdata@(model_string,mb_solution) <- lift $ solveTraceM cutoff ret_type param_env traceid res_trace_symbolic
	when (not don'tShowTraces) $ printLog $ "\n--- MODEL " ++ show traceid ++ " -------------------------\n" ++ model_string
	funname <- lift $ gets funNameCVS
	printLogV 1 $ "--- TRACE " ++ show traceid ++ " ----------------------\n" ++
		show_solution funname mb_solution ++ "\n"

	startend <- lift $ gets funStartEndCVS
	let visible_trace = Set.fromList $ concatMap to_branch res_line
		where
		to_branch cond@(Condition b _) | is_visible_traceelem startend cond =
			[ (if b then Then else Else) (lineColNodeInfo cond) ]
		to_branch _ = []

	let traceanalysisresult :: TraceAnalysisResult = (traceid,res_line,resultdata)
	case is_solution traceanalysisresult of
		False -> do
			printLogV 2  $ "### FALSE : " ++ show traceid ++ " no solution!"
			modify $ \ (tas,covered,alls) -> (tas,covered,Set.union visible_trace alls)
			return False
		True  -> do
			printLogV 2  $ "### TRUE : " ++ show traceid ++ " Is Solution"
			when (not cutoff) $ lift $ checkSolutionM traceid resultdata >> return ()
			modify $ \ (tas,covered,alls) -> case visible_trace `Set.isSubsetOf` covered of
				False -> (traceanalysisresult:tas,Set.union visible_trace covered,Set.union visible_trace alls)
				True  -> (tas,covered,alls)
			return True

analyzeTreeM cutoff opts ret_type param_env traceid res_line (TraceOr traces : rest) = case rest of
	[] -> do
		printLogV 2 $ "### analyzeTreeM : TraceOr " ++ show traceid ++ " ..."
		try_traces (zip [1..] traces)
			where
			num_subtraces = length traces
			try_traces :: [(Int,Trace)] -> AnalyzeTreeM Bool
			try_traces [] = return False
			try_traces ((i,trace):rest) = do
				let traceid' = (traceid++[i])
				success <- analyzeTreeM cutoff opts ret_type param_env traceid' res_line trace
				case success of
					True -> do
						printLogV 2  $ "### analyzeTreeM : TraceOr " ++ show traceid' ++ " returned TRUE"
						return True
					False -> do
						printLogV 2  $ "### analyzeTreeM : TraceOr " ++ show traceid' ++ " returned FALSE, trying the rest..."
						try_traces rest
	_ -> myError $ "analyzeTreeM: TraceOr not last element in " ++ showTrace res_line

analyzeTreeM cutoff opts ret_type param_env traceid res_line (TraceAnd traces : rest) = case rest of
	[] -> do
		printLogV 2 $ "### analyzeTreeM : TraceAnd " ++ show traceid ++ " ..."
		let num_subtraces = length traces
		results <- forM (zip [1..] traces) $ \ (i,trace) -> do
			analyzeTreeM cutoff opts ret_type param_env (traceid++[i]) res_line trace
		return $ all (==True) results
	_ -> myError $ "analyzeTreeM: TraceAnd not last element in " ++ showTrace res_line

analyzeTreeM cutoff opts ret_type param_env traceid res_line (te:rest) = do
	analyzeTreeM cutoff opts ret_type param_env traceid (te:res_line) rest
-}

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
		Just (TypeDef _ ty _ _) -> return ty
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

type UnfoldTracesRet = Either [Trace] SolveFunRet
--type SolveFunRet = (Bool,([TraceAnalysisResult],Set.Set Branch,Set.Set Branch))

unfoldTracesM :: Maybe Type -> [Int] -> [Env] -> [Int] -> Trace -> [[CBlockItem]] -> CovVecM UnfoldTracesRet
unfoldTracesM mb_ret_type break_stack envs traceid trace cbss = do
	cbss_txt <- case cbss of
		[] -> return "[]"
		(l : _) -> return $ "[ " ++ (intercalate " , " (map (render.pretty) l)) ++ " ] : _"
	unfoldTraces1M mb_ret_type break_stack envs trace cbss

unfoldTraces1M :: Maybe Type -> [Int] -> [Env] -> [Int] -> Trace -> [[CBlockItem]] -> CovVecM UnfoldTracesRet
unfoldTraces1M mb_ret_type break_stack envs traceid trace bstss@((CBlockStmt stmt : rest) : rest2) = case stmt of

	CLabel _ cstat _ _ -> unfoldTracesM mb_ret_type break_stack envs traceid trace ((CBlockStmt cstat : rest) : rest2)

	CCompound _ cbis _ -> unfoldTracesM mb_ret_type break_stack ([]:envs) traceid trace (cbis : (rest : rest2))

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
			collect_stmts [ CBlockStmt (CDefault stmt _) ] = [CBlockStmt stmt]
			collect_stmts (CBlockStmt (CCase caseexpr stmt case_ni) : rest) = [ CBlockStmt $ CIf (CBinary CEqOp cond_var caseexpr case_ni)
				(CCompound [] (CBlockStmt stmt : filtercases rest) undefNode) (Just $ CCompound [] (collect_stmts rest) undefNode) case_ni ]
			collect_stmts (_:rest) = collect_stmts rest

			case_replacement = collect_stmts cbis
		printLogV 1 $ (render.pretty) case_replacement
		unfoldTracesM mb_ret_type (length bstss : break_stack) envs traceid trace ( (
			CBlockDecl (CDecl [CTypeSpec $ CLongType cond_ni]
				[(Just $ CDeclr (Just cond_var_ident) [] Nothing [] cond_ni, Just $ CInitExpr condexpr cond_ni, Nothing)] cond_ni) :
			case_replacement ++
			rest) : rest2 )

	CBreak _ -> case break_stack of
		[] -> error $ "unfoldTraces1M " ++ (render.pretty) stmt ++ " : empty break stack!"
		(b:rest) -> do
			let through_compounds = length bstss - b
			unfoldTracesM mb_ret_type rest (drop through_compounds envs) traceid trace (drop through_compounds bstss)

	CIf cond then_stmt mb_else_stmt ni -> do
		let then_trace_m = transids cond trace $ \ (cond',trace') -> do
			unfoldTracesM mb_ret_type break_stack envs (traceid++[1]) (Condition True cond' : trace') ( (CBlockStmt then_stmt : rest) : rest2 )
		let else_trace_m = transids cond trace $ \ (cond',trace') -> do
			let not_cond = Condition False (CUnary CNegOp cond' (nodeInfo cond'))
			case mb_else_stmt of
				Nothing        -> unfoldTracesM mb_ret_type break_stack envs (traceid++[2]) (not_cond : trace') ( rest : rest2 )
				Just else_stmt -> unfoldTracesM mb_ret_type break_stack envs (traceid++[2]) (not_cond : trace') ( (CBlockStmt else_stmt : rest) : rest2 )

{-
		case num_reached cond of
			num | num <= sameConditionThreshold || num ∈ sameConditionThresholdExceptions -> do
				then_trace <- then_trace_m
				else_trace <- else_trace_m
				return [ (if isJust mb_ret_type then TraceAnd else TraceOr) [then_trace,else_trace] ]
			num -> do
--				printLogV 1 $ "Condition at " ++ (render.pretty) (nodeInfo cond) ++ " already reached " ++ show num ++ " times, cutting off one branch."
				case num `mod` 2 == 0 of
					True  -> then_trace_m
					False -> else_trace_m
-}

		(if conditions_reached > 0 && conditions_reached `mod` sizeConditionChunks == 0 then maybe_cutoff else id) $ do
			either_then_trace <- then_trace_m
			either_else_trace <- else_trace_m
			return $ combineUnfoldTracesRets either_then_trace either_else_trace

--type UnfoldTracesRet = Either [Trace] SolveFunRet
--type SolveFunRet = (Bool,[TraceAnalysisResult],Set.Set Branch,Set.Set Branch)

	CReturn Nothing _ -> return $ case mb_ret_type of
		Nothing -> Left [trace]
		Just _  -> analyzeTreeM cutoff ret_type traceid trace

	CReturn (Just ret_expr) _ -> do
		transids ret_expr trace $ \ (ret_expr',trace') -> do
			ret_trace <- case mb_ret_type of
				Nothing -> return []
				Just ret_type -> do
					ret_var_expr <- createInterfaceM [(internalIdent returnval_var_name,ret_type)]
					ret_env_expr <- createInterfaceFromExprM ret_expr' ret_type
					when (length ret_var_expr /= length ret_env_expr) $ error "unfoldTraces1M CReturn: length ret_var_expr /= length ret_env_expr !"
					return $ concat $ for (zip ret_var_expr ret_env_expr) $
						\ ( ((_,(ret_var_ident,ret_var_ty)),_) , (_,ret_member_expr)) -> [
							Condition True $ CBinary CEqOp
								(CVar ret_var_ident (nodeInfo ret_var_ident))
								ret_member_expr
								(nodeInfo ret_member_expr),
							NewDeclaration (ret_var_ident,ret_var_ty) ]
			return $ Just $ Return ret_expr' : (ret_trace ++ trace')

	CExpr (Just cass@(CAssign assignop lexpr assigned_expr ni)) _ -> do
		transids assigned_expr' trace $ \ (assigned_expr'',trace') -> do
			transids (error "more than one transid results for lexpr!") lexpr trace' $ \ (lexpr',trace'') -> do
				unfoldTracesM mb_ret_type break_stack envs traceid (Assignment lexpr' assigned_expr'' : trace'') (rest:rest2)
		where
		mb_binop = lookup assignop [
			(CMulAssOp,CMulOp),(CDivAssOp,CDivOp),(CRmdAssOp,CRmdOp),(CAddAssOp,CAddOp),(CSubAssOp,CSubOp),
			(CShlAssOp,CShlOp),(CShrAssOp,CShrOp),(CAndAssOp,CAndOp),(CXorAssOp,CXorOp),(COrAssOp,COrOp) ]
		assigned_expr' = case mb_binop of
			Nothing -> assigned_expr
			Just binop -> CBinary binop lexpr assigned_expr ni

	CExpr (Just (CUnary unaryop expr ni_op)) ni | unaryop ∈ map fst unaryops -> do
		unfoldTracesM mb_ret_type break_stack envs traceid trace ( (CBlockStmt stmt' : rest) : rest2 )
		where
		stmt' = CExpr (Just $ CAssign assignop expr (CConst $ CIntConst (cInteger 1) ni_op) ni) ni
		Just assignop = lookup unaryop unaryops
		unaryops = [ (CPreIncOp,CAddAssOp),(CPostIncOp,CAddAssOp),(CPreDecOp,CSubAssOp),(CPostDecOp,CSubAssOp) ]

	CExpr (Just expr) _ -> do
		myError $ "not implemented yet."

	-- That's cheating: Insert condition into trace (for loop unrolling)
	CGotoPtr cond _ -> do
		transids TraceOr cond trace $ \ (cond',trace') -> do
			unfoldTracesM mb_ret_type break_stack envs traceid (Condition True cond' : trace') ( rest : rest2 )

 	CWhile cond body False _ -> do --maybe_cutoff $ do
 		(mb_unrolling_depth,msg) <- infer_loopingsM cond body
 		printLogV 2 msg
		let unrolling_depths = case mb_unrolling_depth of
 			Nothing -> _UNROLLING_DEPTHS
 			Just n -> [n]
 
		unrolleds <- forM unrolling_depths $ \ n ->
			unfoldTracesM mb_ret_type break_stack envs traceid trace ((unroll cond n ++ rest) : rest2 )
		return $ foldl1 combineUnfoldTracesRets unrolleds

		where

		unroll :: CExpr -> Int -> [CBlockItem]
		unroll while_cond n = concat ( replicate n [ CBlockStmt (CGotoPtr while_cond undefNode), CBlockStmt body ] ) ++
			[ CBlockStmt $ CGotoPtr (CUnary CNegOp while_cond undefNode) undefNode ]

	_ -> myError $ "unfoldTracesM " ++ (render.pretty) stmt ++ " not implemented yet"

	where

	maybe_cutoff :: CovVecM (Maybe Trace) -> CovVecM (Maybe Trace)
	maybe_cutoff cont | cutOffs = do
		printLogV 1 $ "******* Probing for CutOff in depth " ++ show (length trace) ++ " ..."
		Just solve_fun <- gets solveFunCVS
		(success,_) <- solve_fun True trace
		case success of
			False -> do
				printLogV 1 $ "******** Cutting off."
				return Nothing
			True  -> do
				printLogV 1 $ "******** Continuing..."
				cont
	maybe_cutoff cont = cont

	num_reached :: (CNode cnode) => cnode -> Int
	num_reached cnode = length $ filter ((== nodeInfo cnode).nodeInfo) trace

	conditions_reached :: Int
	conditions_reached = length $ filter is_condition trace where
		is_condition (Condition _ _) = True
		is_condition _ = False

	infer_loopingsM :: CExpr -> CStat -> CovVecM (Maybe Int,String)
 	infer_loopingsM cond0 body = do
 		translateExprM envs cond0 >>= \case
 			[(cond,[])] -> do
				let
					-- get all variables used in the condition
					cond_idents = fvar cond
				-- unfold body to all body traces and filter for all Assignments to variables from the condition
 				mb_body_trace <- unfoldTracesM mb_ret_type break_stack envs [] [[CBlockStmt body]]
 				case mb_body_trace of
 					Nothing -> return (Nothing,"No body trace.")
 					Just body_trace -> do
						let
							body_traces = flattenTrace [] (reverse body_trace)
							body_traces_ass = map (concatMap from_ass) body_traces where
								from_ass (Assignment a@(CVar i _) b) | i ∈ cond_idents = [(a,b)]
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
											CBinary binop (CVar ident _) cconst@(CConst _) _ | ident ≡ ass_ident ∧ binop ∈ [CSubOp,CAddOp,CShrOp,CShlOp] ->
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
											Just sol@[(_,IntVal n)] -> (Just n, "Found looping solution n = " ++ show sol)
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
				conts <- forM additional_expr_traces $ \ (expr',trace') -> do
					cont (expr',trace'++trace)
				return $ concat $ map fromLeft' conts
			Just _ -> try_next additional_expr_traces where
				try_next [] = return (False,[],Set.empty,Set.empty)
				try_next ((expr',trace'):rest) = do
					cont (expr',trace'++trace) >>= \case
						cont_res@(Right (success,_,_,_)) -> case success of
							True -> return cont_res
							False -> try_next rest
		
{-
		forM additional_expr_traces $ \ (expr',trace') -> do
			cont (expr',trace'++trace)
		return $ case partitionEithers conts of
			(tracess,[]) -> Left $ concat tracess
			([],solverets) -> foldSolveRets_Or solverets
		case catMaybes conts of
			[]  -> return Nothing --myError $ "transids Strange: conts empty!"
			[e] -> return $ Just e
			ts  -> return $ Just [ compose ts ]
-}

unfoldTraces1M mb_ret_type break_stack (env:envs) traceid trace ( (CBlockDecl (CDecl [CTypeSpec typespec] triples _) : rest) : rest2 ) = do
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
	unfoldTracesM mb_ret_type break_stack ((concat newenvs ++ env) : envs) traceid (concat newitems ++ trace) ((concat initializerss ++ rest):rest2)

unfoldTraces1M mb_ret_type break_stack (_:restenvs) traceid trace ([]:rest2) = do
	let break_stack' = dropWhile (> (length rest2)) break_stack
	unfoldTracesM mb_ret_type break_stack' restenvs traceid trace rest2

unfoldTraces1M _ _ _ traceid trace [] = return $ case mb_ret_type of
	Nothing -> Left [trace]
	Just _  -> analyzeTraceM cutoff ret_type traceid trace

unfoldTraces1M _ _ _ _ _ ((cbi:_):_) = myError $ "unfoldTracesM " ++ (render.pretty) cbi ++ " not implemented yet."

--type UnfoldTracesRet = Either [Trace] SolveFunRet
--type SolveFunRet = (Bool,[TraceAnalysisResult],Set.Set Branch,Set.Set Branch)
combineUnfoldTracesRets :: UnfoldTracesRet -> UnfoldTracesRet -> UnfoldTracesRet
combineUnfoldTracesRets (Left traces1) (Left traces2) = Left $ traces1 ++ traces2
combineUnfoldTracesRets r1@(Right (success1,(tars1,covered1,alls1))) r2@(Right (success2,(tars2,covered2,alls2))) = 
	case (success1,success2) of
		(True,True)   -> Right (True, (tars1 ++ tars2, covered1 ∪ covered1, alls1 ∪ alls2 ))
		(False,True)  -> r2
		(True,False)  -> r1
		(False,False) -> Right (False, ([],Set.empty,Set.empty))

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
not_c e = CUnary CNegOp e undefNode

ⅈ :: Integer -> CExpr
ⅈ i = CConst $ CIntConst (cInteger i) undefNode

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
			Just $ CAssign CAssignOp lexpr expr ni_init) ni_init ]
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

translateExprM :: [Env] -> CExpr -> CovVecM [(CExpr,Trace)]
translateExprM envs expr = do
	let	
		to_call :: CExpr -> StateT [(Ident,[CExpr],NodeInfo)] CovVecM CExpr
		-- eliminate casts
		to_call (CCast _ cexpr _) = return cexpr
		to_call (CCall funexpr args ni) = case funexpr of
			CVar (Ident "__builtin_expect" _ _) _ -> return $ head args
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
		Left funtrace <- unfoldTracesM Nothing [] envs [] [ [ CBlockStmt body' ] ]
--		printLog $ "##### extract_traces_rets " ++ showTrace 0 (reverse funtrace) ++ "\n"
		let funtraces = concat $ for (flattenTrace [] (reverse funtrace)) $ \case
			Return retexpr : tr -> [(tr,retexpr)]
			tr -> error $ "funcalls_traces: trace of no return:\n" ++ showTrace 0 tr
--		printLog $ "#### = " ++ show (map (\(tr,cex) -> (tr,(render.pretty) cex)) funtraces) ++ "\n"
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


-- Flattens the trace tree to a list of all paths through the tree

flattenTrace :: Trace -> Trace -> [Trace]
flattenTrace traceelems [] = [traceelems]
flattenTrace traceelems (TraceOr traces : rest) = case rest of
	[] -> concat $ for (map reverse traces) (flattenTrace traceelems)
	_ -> error $ "extract_traces_rets: TraceOr not last element " ++ showTrace 0 traceelems
flattenTrace traceelems (TraceAnd traces : rest) = case rest of
	[] -> concat $ for (map reverse traces) (flattenTrace traceelems)
	_ -> error $ "extract_traces_rets: TraceAnd not last element " ++ showTrace 0 traceelems
flattenTrace traceelems (te : rest) = flattenTrace (te:traceelems) rest


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


-- Simplify

simplifyTraceM :: Trace -> CovVecM Trace
simplifyTraceM trace = everywhereM (mkM simplify) trace where
	simplify :: CExpr -> CovVecM CExpr
	simplify (CUnary CIndOp (CUnary CAdrOp expr _) _) = return expr
	simplify (CMember (CUnary CAdrOp s _) member True ni) = return $ CMember s member False ni
	simplify (CMember (CUnary CIndOp p _) member False ni) = return $ CMember p member True ni
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
		when (not $ newident ∈ new_idents) $
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

data SExpr = SExpr [SExpr] | SLeaf String
instance Show SExpr where
	show (SLeaf s) = s
	show (SExpr sexprs) = "(" ++ intercalate " " (map show sexprs) ++ ")"

data Z3_Type = Z3_BitVector Int Bool | Z3_Float | Z3_Double
	deriving (Show,Eq,Ord)

type Constraint = CExpr

expr2SExpr :: TyEnv -> Expr -> SExpr
expr2SExpr tyenv expr = expr2sexpr (infer_type expr) (insert_eq0 True expr)

	where

	neq0 :: Constraint -> Constraint
	neq0 constr = not_c $ constr ⩵ ⅈ 0

	insert_eq0 :: Bool -> Constraint -> Constraint
	insert_eq0 must_be_bool (CUnary CCompOp expr ni) = (if must_be_bool then neq0 else id) $ CUnary CCompOp (insert_eq0 False expr) ni
	insert_eq0 must_be_bool (CUnary unop expr ni) = case unop of
		CNegOp -> CUnary CNegOp (insert_eq0 True expr) ni
		unop   -> (if must_be_bool then neq0 else id) $ CUnary unop (insert_eq0 False expr) ni
	insert_eq0 must_be_bool (CCast _ expr _) = insert_eq0 must_be_bool expr
	insert_eq0 must_be_bool cvar@(CVar ident ni) = (if must_be_bool then neq0 else id) cvar
	insert_eq0 must_be_bool const@(CConst _) = (if must_be_bool then neq0 else id) const
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

	expr2sexpr :: Maybe Z3_Type -> CExpr -> SExpr
	expr2sexpr cur_ty expr = case expr of
		CUnary CPlusOp expr _ -> expr2sexpr cur_ty expr
		CUnary op expr _ -> SExpr [ SLeaf op_str , expr2sexpr cur_ty expr ] where
			op_str = case op of
				CMinOp  -> "bvneg"
				CCompOp -> "bvnot"
				CNegOp  -> "not"
				_       -> error $ "expr2sexpr " ++ (render.pretty) op ++ " should not occur!"
		CBinary CNeqOp expr1 expr2 _ -> expr2sexpr cur_ty $ expr1 !⩵ expr2
		CBinary op expr1 expr2 _ -> SExpr [ op_sexpr , expr2sexpr subtype1 expr1 , expr2sexpr subtype2 expr2 ] where
			(op_sexpr,subtype1,subtype2) = case op of
				CMulOp -> (SLeaf "bvmul",cur_ty,cur_ty)
				CDivOp -> (SLeaf "bvdiv",cur_ty,cur_ty)
				CRmdOp -> (SLeaf $ unSigned "bvurem" "bvsrem",cur_ty,cur_ty)
				CAddOp -> (SLeaf "bvadd",cur_ty,cur_ty)
				CSubOp -> (SLeaf "bvsub",cur_ty,cur_ty)
				CShlOp -> (SLeaf $ unSigned "bvshl" "bvshl",cur_ty,cur_ty)
				CShrOp -> (SLeaf $ unSigned "bvlshr" "bvashr",cur_ty,cur_ty)
				CLeOp  -> (SLeaf $ unSigned "bvult" "bvslt",cur_ty,cur_ty)
				CGrOp  -> (SLeaf $ unSigned "bvugt" "bvsgt",cur_ty,cur_ty)
				CLeqOp -> (SLeaf $ unSigned "bvule" "bvsle",cur_ty,cur_ty)
				CGeqOp -> (SLeaf $ unSigned "bvuge" "bvsge",cur_ty,cur_ty)
				CEqOp  -> (SLeaf "=",infer_type expr,infer_type expr)
				CAndOp -> (SLeaf "bvand",cur_ty,cur_ty)
				COrOp  -> (SLeaf "bvor",cur_ty,cur_ty)
				CXorOp -> (SLeaf "bvxor",cur_ty,cur_ty)
				CLndOp -> (SLeaf "and",cur_ty,cur_ty)
				CLorOp -> (SLeaf "or",cur_ty,cur_ty)

			unSigned unsigned signed = case (infer_type expr1,infer_type expr2) of
				(Just (Z3_BitVector _ is_signed1), Just (Z3_BitVector _ is_signed2)) | is_signed1==is_signed2 ->
					if is_signed1 then signed else unsigned
				(Just (Z3_BitVector _ is_signed), Nothing) -> if is_signed then signed else unsigned
				(Nothing, Just (Z3_BitVector _ is_signed)) -> if is_signed then signed else unsigned
				(Nothing, Nothing) -> signed
--				other -> myError $ "unSigned " ++ (render.pretty) expr1 ++ " " ++ (render.pretty) expr2 ++ " yielded " ++ show other

		CVar ident _ -> SLeaf $ (render.pretty) ident
		CConst cconst -> SLeaf $ case cconst of
			CIntConst intconst _ -> let i = getCInteger intconst in
				case cur_ty of
					Just (Z3_BitVector size _) -> printf "#x%*.*x" (size `div` 4) (size `div` 4) i
					Nothing -> printf "#x%*.*x" (32 `div` 4 :: Int) (32 `div` 4 :: Int) i
					_ -> error $ "expr2SExpr: cur_ty " ++ show cur_ty
			_ -> (render.pretty) cconst
		cmember@(CMember _ _ _ _) -> error $ "expr2SExpr " ++ (render.pretty) cmember ++ " should not occur!"
		ccall@(CCall _ _ _) ->error $ "expr2SExpr " ++ (render.pretty) ccall ++ " should not occur!"
		expr -> error $ "expr2SExpr " ++ (render.pretty) expr ++ " not implemented" 

	infer_type :: CExpr -> Maybe Z3_Type
	infer_type (CVar ident _) = case lookup ident tyenv of
		Nothing -> error $ "infer_type: " ++ (render.pretty) ident ++ " not found in tyenv\n" ++
			(unlines $ map (\(a,b) -> (render.pretty) a ++ " |-> " ++ (render.pretty) b) tyenv)
		Just ty -> Just $ ty2Z3Type ty
	infer_type (CConst _) = Nothing
	infer_type (CUnary _ expr _) = infer_type expr
	infer_type expr@(CBinary _ expr1 expr2 _) = case [infer_type expr1, infer_type expr2] of
		[Nothing,Nothing] -> Nothing
		[Just t1,Just t2] | t1/=t2 -> error $ "infer_type " ++ (render.pretty) expr ++ " yields different types for operands: " ++ show t1 ++ " and " ++ show t2
		res -> maximum res
	infer_type other = error $ "infer_type " ++ (render.pretty) other ++ " not implemented!"

ty2Z3Type :: Type -> Z3_Type
ty2Z3Type ty = case ty of
	DirectType (TyComp (CompTypeRef _ _ _)) _ _ -> Z3_BitVector 4 False
	DirectType tyname _ _ -> case tyname of
		TyIntegral intty -> case intty of
			TyChar   -> Z3_BitVector 8 False
			TySChar  -> Z3_BitVector 8 True
			TyUChar  -> Z3_BitVector 8 False
			TyShort  -> Z3_BitVector 16 True
			TyUShort -> Z3_BitVector 16 False
			TyInt    -> Z3_BitVector intSize True
			TyUInt   -> Z3_BitVector intSize False
			TyLong   -> Z3_BitVector longIntSize True
			TyULong  -> Z3_BitVector longIntSize False
			TyLLong  -> Z3_BitVector 64 True
			TyULLong -> Z3_BitVector 64 False
			other    -> error $ "ty2Z3Type " ++ show other ++ " not implemented!"
		TyFloating TyFloat -> Z3_Float
		TyFloating TyDouble -> Z3_Double
		TyEnum _ -> Z3_BitVector intSize False
		TyComp _ -> Z3_BitVector 16 False
		_ -> error $ "ty2Z3Type " ++ (render.pretty) ty ++ " not implemented!"
	PtrType _ _ _ -> Z3_BitVector 16 False
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
--	printLog $ "tyenv=" ++ showTyEnv tyenv
	printLogV 2 $ "output_idents = " ++ showIdents output_idents
	let
		constraints_vars = fvar constraints
	printLogV 2 $ "constraints_vars = " ++ showIdents constraints_vars

	let
		varsZ3 = for (filter ((∈ (constraints_vars ++ output_idents)).fst) tyenv) $ \ (ident,ty) ->
			SExpr [ SLeaf "declare-const", SLeaf (identToString ident), ty2SExpr ty ]
		constraintsZ3 = for constraints $ \ expr -> SExpr [SLeaf "assert", expr2SExpr tyenv expr]
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
	liftIO $ writeFile modelpathfile model_string
	printLogV 2 $ "Model " ++ takeFileName modelpathfile ++ " =\n" ++ model_string_linenumbers
	printLog $ "Running model " ++ takeFileName modelpathfile ++ "..."
	(_,output,_) <- liftIO $ withCurrentDirectory (takeDirectory modelpathfile) $ do
		readProcessWithExitCode z3FilePath ["-smt2","parallel.enable=true",takeFileName modelpathfile] ""
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
							Z3_BitVector size signed -> let
								'#':'x':hexdigits = val_string
								[(i :: Integer,"")] = readHex hexdigits
								in
								IntVal $ case signed of
									False -> fromIntegral i
									True  -> fromIntegral $ if i < 2^(size-1) then i else i - 2^size
							Z3_Float -> FloatVal (read val_string :: Float) )

					_ -> myError $ "Parsing z3 output: Could not find " ++ is
			return (model_string_linenumbers,Just sol_params)
		_ -> myError $ "Execution of " ++ z3FilePath ++ " failed:\n" ++ output


solveTraceM :: Bool -> Type -> Env -> [Int] -> Trace -> CovVecM ResultData
solveTraceM cutoff ret_type param_env traceid trace = do
	let
		tracename = show traceid
	retval_env_exprs  <- case cutoff of
		True  -> return []
		False -> createInterfaceM [(internalIdent returnval_var_name,ret_type)]
	let
		retval_env = map fst retval_env_exprs
		param_names = map (fst.snd) param_env
		ret_names   = map (fst.snd) retval_env
		constraints = concatMap traceitem2constr trace where
		traceitem2constr (Condition _ expr) = [expr]
		traceitem2constr _ = []
		
		tyenv = createTyEnv trace

	(model_string,mb_sol) <- makeAndSolveZ3ModelM
		tyenv
		constraints
		(for param_names $ \ name -> SExpr [SLeaf "minimize",SLeaf (identToString name)])
		(param_names ++ ret_names)
		(analyzerPath </> "models" </> "model_" ++ tracename ++ ".smtlib2")

	return (model_string,case mb_sol of
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
	printLogV 1 $ "checkSolution param_env =\n" ++ showEnv param_env
	printLogV 1 $ "checkSolution ret_env =\n" ++ showEnv ret_env
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
				ret_solution = filter ((∈ map (identToString.fst) ret_env).fst) solution
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