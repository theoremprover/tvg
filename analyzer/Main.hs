{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE PackageImports,QuasiQuotes,UnicodeSyntax,LambdaCase,ScopedTypeVariables,TupleSections,TypeSynonymInstances,FlexibleInstances,FlexibleContexts,StandaloneDeriving,DeriveDataTypeable,DeriveGeneric #-}

module Main where

import Prelude hiding ((<>)) -- Making way for Text.Pretty.<>
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
import Language.C.Analysis.DeclAnalysis
import Language.C.Analysis.TypeUtils
import Language.C.Analysis.TypeConversions
import Language.C.Analysis.TravMonad
import Language.C.Analysis.SemRep
import Language.C.Analysis.Export
import Language.C.Syntax.Ops
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
longIntSize = 32
longLongIntSize = 64

intType = integral TyInt :: Type
charType = integral TyChar :: Type
ptrType to_ty = PtrType to_ty noTypeQuals noAttributes :: Type

flags2IntType flags = integral (getIntType flags) :: Type
string2FloatType flags = floating (getFloatType flags) :: Type

showInitialTrace = True
solveIt = True
showOnlySolutions = True
don'tShowTraces = False
checkSolutions = solveIt && True
returnval_var_name = "return_val"
outputVerbosity = 2
floatTolerance = 1e-7 :: Float
doubleTolerance = 1e-10 :: Double
showBuiltins = False
cutOffs = False
logToFile = True

mAX_UNROLLS = 30
uNROLLING_STRATEGY = [0..mAX_UNROLLS]

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

	-- TODO: Automatically find out int/long/longlong sizes of the compiler!

	gcc:filename:funname:opts <- getArgs >>= return . \case
		[] -> "gcc" : (analyzerPath++"\\myfp-bit_mul.c") : "_fpmul_parts" : ["-writeModels"] --,"-exportPaths" "-writeAST","-writeGlobalDecls"]
--		[] -> "gcc" : (analyzerPath++"\\myfp-bit_mul_exp.c") : "_fpmul_parts" : ["-writeModels"] --"-writeAST","-writeGlobalDecls"]
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
	Assignment CExprWithType CExprWithType |
	Condition (Maybe Bool) CExprWithType |
	NewDeclaration (Ident,Type) |
	Return CExprWithType |
	DebugOutput String (CExprWithType,Type)
	deriving Data

data Branch = Then Location | Else Location
	deriving (Eq,Ord)
instance Show Branch where
	show (Then loc) = "Then branch in " ++ showLocation loc
	show (Else loc) = "Else branch in " ++ showLocation loc

instance CNode TraceElem where
	nodeInfo (Assignment lexpr _)       = extractNodeInfo lexpr
	nodeInfo (Condition _ expr)         = extractNodeInfo expr
	nodeInfo (NewDeclaration (ident,_)) = nodeInfo ident
	nodeInfo (Return expr)              = extractNodeInfo expr
	nodeInfo (DebugOutput _ _)          = undefNode

instance CNode NodeInfoWithType where
	nodeInfo (ni,_) = ni

instance Pretty NodeInfo where
	pretty ni = text $ "line " ++ show line ++ ", col " ++ show col
		where
		(line,col) = lineColNodeInfo ni

instance Pretty CExprWithType where
	pretty cexpr = pretty $ fmap fst cexpr

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
		def2stmt (EnumeratorDef (Enumerator ident const_expr _ ni)) = do
			return [ CBlockStmt $ CExpr (Just $ CVar ident (nodeInfo ident) ≔ const_expr) ni ]
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

	let decls = map (NewDeclaration . snd) (reverse param_env ++ glob_env)

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

	Right all_covered <- unfoldTracesM ret_type' True [] (param_env:[glob_env]) decls [ defs ++ [ CBlockStmt body ] ]
	return all_covered

type Location = (Int,Int)

showLocation :: Location -> String
showLocation (l,c) = "line " ++ show l ++ ", col " ++ show c

-- In case of a cutoff, mb_ret_type is Nothing.
analyzeTraceM :: Maybe Type -> [TraceElem] -> CovVecM Bool
analyzeTraceM mb_ret_type res_line = do
	printLogV 1 $ "Analyzing trace..."
	let
		trace = reverse res_line
		traceid = concatMap extract_conds trace where
			extract_conds (Condition (Just b) _) = [ if b then 1 else 2 ]
			extract_conds _ = []

	opts <- gets optsCVS
	when ("-exportPaths" `elem` opts) $ liftIO $ do
		writeFile (analyzerPath </> "models" </> "path_" ++ show traceid <.> ".c") $ unlines $ concat $ for trace $ \case
			Assignment lexpr assexpr -> [ (render.pretty) lexpr ++ " = " ++ (render.pretty) assexpr ++ " ;" ]
			NewDeclaration (ident,ty) -> [ "(" ++ (render.pretty) ty ++ ") " ++ (render.pretty) ident ++ " ;" ]
			Return expr -> [ "return " ++ (render.pretty) expr ++ " ;" ]
			_ -> []
			
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

instance Eq (CExpression a) where
	(CVar id1 _) == (CVar id2 _) = id1==id2
	(CMember ptrexpr1 ident1 isptr1 _) == (CMember ptrexpr2 ident2 isptr2 _) =
		ident1==ident2 && isptr1==isptr2 && ptrexpr1 == ptrexpr2
	(CUnary op1 expr1 _) == (CUnary op2 expr2 _) = op1==op2 && expr1==expr2
	(CBinary op1 expr11 expr12 _) == (CBinary op2 expr21 expr22 _) = op1==op2 && expr11==expr21 && expr12==expr22
	(CAssign op1 expr11 expr12 _) == (CAssign op2 expr21 expr22 _) = op1==op2 && expr11==expr21 && expr12==expr22
	(CCall fun1 args1 _) == (CCall fun2 args2 _) = fun1==fun2 && args1==args2
	(CConst const1) == (CConst const2) = const1==const2
	_ == _ = False

instance Eq (CConstant a) where
	(CIntConst c1 _)   == (CIntConst c2 _)   = c1==c2
	(CCharConst c1 _)  == (CCharConst c2 _)  = c1==c2
	(CFloatConst c1 _) == (CFloatConst c2 _) = c1==c2
	(CStrConst c1 _)   == (CStrConst c2 _)   = c1==c2

deriving instance Eq BuiltinType

instance Eq TypeName where
	(TyEnum _) == (TyIntegral TyInt) = True
	(TyIntegral TyInt) == (TyEnum _) = True
	TyVoid                == TyVoid                = True
	(TyIntegral intty1)   == (TyIntegral intty2)   = intty1==intty2
	(TyFloating floatty1) == (TyFloating floatty2) = floatty1==floatty2
	(TyComplex floatty1)  == (TyComplex floatty2)  = floatty1==floatty2
	(TyComp (CompTypeRef sueref1 _ _)) == (TyComp (CompTypeRef sueref2 _ _)) = sueref1==sueref2
	(TyEnum (EnumTypeRef sueref1 _))   == (TyEnum (EnumTypeRef sueref2 _))   = sueref1==sueref2
	(TyBuiltin builtinty1)             == (TyBuiltin builtinty2)             = builtinty1==builtinty2
	_ == _ = False

instance Eq Type where
	(DirectType tyname1 _ _) == (DirectType tyname2 _ _) = tyname1==tyname2
	(PtrType ty1 _ _) == (PtrType ty2 _ _) = ty1 == ty2
	_ == _ = False

lValueToVarName :: CExprWithType -> String
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

envs2tyenv :: [Env] -> TyEnv
envs2tyenv envs = map snd $ concat envs

{-
type2DeclM :: Type -> CovVecM (CDeclaration NodeInfoWithType)
type2DeclM ty = do
	ty' <- elimTypeDefsM ty
	let anno = (undefNode,ty)
	typespecs <- case ty' of
		DirectType tyname _ _ -> case (tyname,sizeofIntTy ty') of
			(TyVoid,_)              -> return [ CTypeSpec (CVoidType anno) ]
			(TyIntegral TyChar,_)   -> return [ CTypeSpec (CCharType anno) ]
			(TyIntegral TySChar,_)  -> return [ CTypeSpec (CSignedType anno), CTypeSpec (CCharType anno) ]
			(TyIntegral TyUChar,_)  -> return [ CTypeSpec (CUnsigType anno), CTypeSpec (CCharType anno) ]
			(TyIntegral TyShort,_)  -> return [ CTypeSpec (CShortType anno) ]
			(TyIntegral TyInt,32)    -> return [ CTypeSpec (CIntType anno) ]
			(TyIntegral TyInt,64)    -> return [ CTypeSpec (CLongType anno) ]
			(TyIntegral TyUInt,32)   -> return [ CTypeSpec (CUnsigType anno), CTypeSpec (CIntType anno) ]
			(TyIntegral TyUInt,64)   -> return [ CTypeSpec (CUnsigType anno), CTypeSpec (CLongType anno) ]
			(TyIntegral TyLong,_)   -> return [ CTypeSpec (CLongType anno) ]
			(TyIntegral TyULong,_)  -> return [ CTypeSpec (CUnsigType anno), CTypeSpec (CLongType anno) ]
			(TyFloating TyFloat,_)  -> return [ CTypeSpec (CFloatType anno) ]
			(TyFloating TyDouble,_) -> return [ CTypeSpec (CDoubleType anno) ]
			(TyEnum (EnumTypeRef sueref _),_) -> do
				EnumDef (EnumType (NamedRef enum_ident) enums _ _) <- lookupTagM sueref
				let ids_inits = for enums $ \ (Enumerator val_ident _ _ _) -> (val_ident,Nothing)
				return [ CTypeSpec $ CEnumType (CEnum (Just enum_ident) (Just ids_inits) [] anno) anno ]
			(other,_) -> myError $ "type2DeclM " ++ (render.pretty) ty ++ " not implemented"
		other -> myError $ "type2DeclM " ++ (render.pretty) ty ++ " not implemented"
	return $ CDecl typespecs [] anno

-}
decl2TypeM :: (Show a) => CDeclaration a -> CovVecM Type
decl2TypeM (CDecl declspecs _ _) = case declspecs of
	[CTypeSpec (CVoidType _)]      -> return $ DirectType TyVoid noTypeQuals noAttributes
	[CTypeSpec (CCharType _)]      -> return $ DirectType (TyIntegral TyChar) noTypeQuals noAttributes
	[CTypeSpec (CUnsigType _), CTypeSpec (CCharType _)] -> return $ DirectType (TyIntegral TyUChar) noTypeQuals noAttributes
	[CTypeSpec (CSignedType _), CTypeSpec (CCharType _)] -> return $ DirectType (TyIntegral TySChar) noTypeQuals noAttributes
	[CTypeSpec (CShortType _)]     -> return $ DirectType (TyIntegral TyShort) noTypeQuals noAttributes
	[CTypeSpec (CIntType _)]       -> return $ DirectType (TyIntegral TyInt) noTypeQuals noAttributes
	[CTypeSpec (CUnsigType _), CTypeSpec (CIntType _)] -> return $ DirectType (TyIntegral TyUInt) noTypeQuals noAttributes
	[CTypeSpec (CLongType _)]      -> return $ DirectType (TyIntegral TyLong) noTypeQuals noAttributes
	[CTypeSpec (CUnsigType _), CTypeSpec (CLongType _)] -> return $ DirectType (TyIntegral TyULong) noTypeQuals noAttributes
	[CTypeSpec (CFloatType _)]     -> return $ DirectType (TyFloating TyFloat) noTypeQuals noAttributes
	[CTypeSpec (CDoubleType _)]    -> return $ DirectType (TyFloating TyDouble) noTypeQuals noAttributes
--	[CTypeSpec (CEnumType (CEnum (Just ident) Nothing _ _) _)] -> lookupTypeDefM ident		
--		return $ DirectType (TyEnum (EnumTypeRef sueref undefNode)) noTypeQuals noAttributes
	[CTypeSpec (CTypeDef ident _)] -> lookupTypeDefM ident
	other -> myError $ "decl2TypeM: " ++ show other ++ " not implemented yet."

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
		Just mem_ty -> elimTypeDefsM mem_ty

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

createInterfaceM :: [(Ident,Type)] -> CovVecM [(EnvItem,CExprWithType)]
createInterfaceM ty_env = concatForM ty_env $ \ (srcident,ty) ->
	createInterfaceFromExprM (CVar srcident (nodeInfo srcident,ty)) ty

createInterfaceFromExprM :: CExprWithType -> Type -> CovVecM [(EnvItem,CExprWithType)]
createInterfaceFromExprM expr ty = do
	ty' <- elimTypeDefsM ty
	case ty' of
	
		-- STRUCT* p
		PtrType (DirectType (TyComp (CompTypeRef sueref _ _)) _ _) _ _ -> prepend_plainvar ty' $ do
			member_ty_s <- getMembersM sueref
			concatForM member_ty_s $ \ (m_ident,m_ty) ->
				createInterfaceFromExprM (CMember expr m_ident True (annotation expr)) m_ty
	
		-- ty* p
		PtrType target_ty _ _ -> prepend_plainvar ty' $ do
			createInterfaceFromExprM (CUnary CIndOp expr (annotation expr)) target_ty
	
		-- STRUCT expr
		DirectType (TyComp (CompTypeRef sueref _ _)) _ _ -> do
			member_ty_s <- getMembersM sueref
			concatForM member_ty_s $ \ (m_ident,m_ty) -> do
				createInterfaceFromExprM (CMember expr m_ident False (annotation expr)) m_ty
	
		-- direct-type expr where direct-type is no struct/union
		DirectType _ _ _ -> prepend_plainvar ty' $ return []
	
		_ ->
			myError $ "create_interfaceM " ++ (render.pretty) expr ++ " " ++ (render.pretty) ty' ++ " not implemented"

		where
	
		prepend_plainvar :: Type -> CovVecM [(EnvItem,CExprWithType)] -> CovVecM [(EnvItem,CExprWithType)]
		prepend_plainvar ty' rest_m = do
			let srcident = internalIdent $ lValueToVarName expr
			rest <- rest_m
			return $ ((srcident,(srcident,ty')),expr) : rest

unfoldTracesM :: Type -> Bool -> [Int] -> [Env] -> Trace -> [[CBlockItem]] -> CovVecM UnfoldTracesRet
unfoldTracesM ret_type toplevel break_stack envs trace cbss = do
	unfoldTraces1M ret_type toplevel break_stack envs trace cbss

unfoldTraces1M :: Type -> Bool -> [Int] -> [Env] -> Trace -> [[CBlockItem]] -> CovVecM UnfoldTracesRet
unfoldTraces1M ret_type toplevel break_stack envs trace bstss@((CBlockStmt stmt : rest) : rest2) = case stmt of

	CLabel _ cstat _ _ -> unfoldTracesM ret_type toplevel break_stack envs trace ((CBlockStmt cstat : rest) : rest2)

	CCompound _ cbis _ -> unfoldTracesM ret_type toplevel break_stack ([]:envs) trace (cbis : (rest : rest2))

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
		unfoldTracesM ret_type toplevel (length bstss : break_stack) envs trace ( (
			CBlockDecl (CDecl [CTypeSpec $ CLongType cond_ni]
				[(Just $ CDeclr (Just cond_var_ident) [] Nothing [] cond_ni, Just $ CInitExpr condexpr cond_ni, Nothing)] cond_ni) :
			case_replacement ++
			rest) : rest2 )

	CBreak _ -> case break_stack of
		[] -> error $ "unfoldTraces1M " ++ (render.pretty) stmt ++ " : empty break stack!"
		(b:rest) -> do
			let through_compounds = length bstss - b
			unfoldTracesM ret_type toplevel rest (drop through_compounds envs) trace (drop through_compounds bstss)

	CIf cond then_stmt mb_else_stmt ni -> do
		let then_trace_m real_cond = transids real_cond trace intType $ \ (cond',trace') -> do
			unfoldTracesM ret_type toplevel break_stack envs (Condition (Just True) cond' : trace') ( (CBlockStmt then_stmt : rest) : rest2 )
		let else_trace_m real_cond = transids real_cond trace intType $ \ (cond',trace') -> do
			let not_cond = Condition (Just False) (CUnary CNegOp cond' (annotation cond'))
			case mb_else_stmt of
				Nothing        -> unfoldTracesM ret_type toplevel break_stack envs (not_cond : trace') ( rest : rest2 )
				Just else_stmt -> unfoldTracesM ret_type toplevel break_stack envs (not_cond : trace') ( (CBlockStmt else_stmt : rest) : rest2 )
		case recognizeAnnotation cond of
			(real_cond,Just (ns,num_reached)) | ns!!num_reached /= 12 -> do
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
						(Right then_success,Right else_success) -> case toplevel of
							False -> Right $ then_success || else_success
							True  -> Right $ then_success && else_success

	CReturn Nothing _ | toplevel -> analyzeTraceM (Just ret_type) trace >>= return.Right
	CReturn Nothing _            -> return $ Left [trace]

	CReturn (Just ret_expr) _ -> do
		transids ret_expr trace ret_type $ \ (ret_expr',trace') -> do
			case toplevel of
				False -> return $ Left [Return ret_expr' : trace']
				True  -> do
					ret_var_expr <- createInterfaceM [(internalIdent returnval_var_name,ret_type)]
					ret_env_expr <- createInterfaceFromExprM ret_expr' ret_type
					when (length ret_var_expr /= length ret_env_expr) $ error "unfoldTraces1M CReturn: length ret_var_expr /= length ret_env_expr !"
					let ret_trace = concat $ for (zip ret_var_expr ret_env_expr) $
						\ ( ((_,(ret_var_ident,ret_var_ty)),_) , (_,ret_member_expr)) -> [
							Condition Nothing $ CVar ret_var_ident (undefNode,ret_var_ty) ⩵ ret_member_expr,
							NewDeclaration (ret_var_ident,ret_var_ty) ]
					analyzeTraceM (Just ret_type) (Return ret_expr' : (ret_trace ++ trace'))
						>>= return.Right

	CExpr (Just (CCall (CVar (Ident "solver_debug" _ _) _) args ni)) _ -> do
		let vars = for args $ \ (CVar ident _) -> fromJust $ lookup ident (concat envs)
		unfoldTracesM ret_type toplevel break_stack envs (map to_dbg_output (reverse vars) ++ trace) (rest:rest2) where
			to_dbg_output (name_id,ty) = DebugOutput ("solver_debug_" ++ identToString name_id) (CVar name_id (undefNode,ty),ty)

	CExpr (Just cass@(CAssign assignop lexpr assigned_expr ni)) _ -> do
		expr_ty <- inferLExprTypeM (envs2tyenv envs) (renameVars envs lexpr)
		transids assigned_expr' trace expr_ty $ \ (assigned_expr'',trace') -> do
			[(lexpr',trace'')] <- translateExprM envs lexpr expr_ty
			unfoldTracesM ret_type toplevel break_stack envs (Assignment lexpr' assigned_expr'' : trace''++trace') (rest:rest2)
		where
		assigned_expr' = case assignop of
			CAssignOp -> assigned_expr
			ass_op -> CBinary (assignBinop ass_op) lexpr assigned_expr ni

	CExpr (Just (CUnary unaryop expr ni_op)) ni | unaryop `elem` (map fst unaryops) -> do
		unfoldTracesM ret_type toplevel break_stack envs trace ( (CBlockStmt stmt' : rest) : rest2 )
		where
		stmt' = CExpr (Just $ CAssign assignop expr (ⅈ 1) ni) ni
		Just assignop = lookup unaryop unaryops
		unaryops = [ (CPreIncOp,CAddAssOp),(CPostIncOp,CAddAssOp),(CPreDecOp,CSubAssOp),(CPostDecOp,CSubAssOp) ]

	CExpr (Just expr) _ -> do
		myError $ "unfoldTraces: " ++ (render.pretty) stmt ++ " not implemented yet."

	-- That's cheating: Insert condition into trace (for loop unrolling)
	CGotoPtr cond ni -> do
		transids cond trace intType $ \ (cond',trace') -> do
			unfoldTracesM ret_type toplevel break_stack envs (Condition (Just $ isUndefNode ni) cond' : trace') ( rest : rest2 )

 	CWhile cond body False ni -> do
 		(mb_unrolling_depth,msg) <- infer_loopingsM cond body
 		printLogV 1 msg
 		unroll_loopM $ case mb_unrolling_depth of
			Nothing -> uNROLLING_STRATEGY
			Just ns -> ns

		where

		unroll_loopM :: [Int] -> CovVecM UnfoldTracesRet
		unroll_loopM [] = case toplevel of
			False -> return $ Left []
			True  -> return $ Right False
		unroll_loopM (depth:depths) = do
			printLogV 1 $ "unroll_loopM " ++ show depth
			unfoldTracesM ret_type toplevel break_stack envs trace ((unroll cond depth ++ rest) : rest2 ) >>= \case
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
			is_this_cond (Condition _ c) | extractNodeInfo c == nodeInfo real_cond = True
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
				translateExprM envs real_cond intType >>= \case
					[(cond,[])] -> do
						let
							-- get all variables used in the condition
							cond_idents = fvar cond
						-- unfold body to all body traces and filter for all Assignments to variables from the condition
						Left body_traces <- unfoldTracesM ret_type False [] envs [] [[CBlockStmt body]]
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

						case body_assigns :: [(CExprWithType,CExprWithType)] of
							[ (counter_var@(CVar ass_ident _),ass_expr) ] -> do
								let
									is_ass_to_ass_var (Assignment (CVar ident _) _) | ident==ass_ident = True
									is_ass_to_ass_var _ = False
								case filter is_ass_to_ass_var trace of
									[] -> return (Nothing,"infer_loopingsM: There is no assignment to the loop counter " ++ (render.pretty) counter_var ++ " prior to the loop")
									ass@(Assignment _ i_0) : _ | null (fvar i_0)-> do
										printLogV 1 $ "last assignment to loop counter is " ++ show ass
										let i_n :: CExprWithType -> CExprWithType = case ass_expr of
											-- for all binops where the following holds (Linearity?):
											-- i_n = i_(n-1) `binop` c  =>  i_n = i_0 `binop` c
											CBinary binop (CVar ident _) cconst@(CConst _) _ | ident == ass_ident ∧ binop `elem` [CSubOp,CAddOp,CShrOp,CShlOp] ->
												\ n_var → CBinary binop i_0 (n_var ∗ cconst) (undefNode,intType)
											_ -> error $ "infer_loopingsM: assignment " ++ (render.pretty) ass_ident ++ " := " ++ (render.pretty) ass_expr ++ " not implemented!"
										let
											n_name = "n_loopings"
											n_ident = internalIdent n_name
											n_var = CVar n_ident (undefNode,intType)
											modelpath = analyzerPath </> n_name ++ show (lineColNodeInfo cond) ++ ".smtlib2"
										n_type <- case lookup ass_ident (envs2tyenv envs) of
											Nothing -> myError $ "infer_loopingsM: Could not find type of " ++ (render.pretty) counter_var
											Just ty -> return ty
										(model_string,mb_sol) <- makeAndSolveZ3ModelM
											[]
											((n_ident,n_type) : envs2tyenv envs)
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

	transids :: CExpr -> Trace -> Type -> ((CExprWithType,Trace) -> CovVecM UnfoldTracesRet) -> CovVecM UnfoldTracesRet
	transids expr trace expr_ty cont = do
		additional_expr_traces :: [(CExprWithType,Trace)] <- translateExprM envs expr expr_ty
		case toplevel of
			False -> do
				conts :: [UnfoldTracesRet] <- forM additional_expr_traces $ \ (expr',trace') -> do
					cont (expr',trace'++trace)
				let (conttracess,[]) = partitionEithers conts
				return $ Left $ concat conttracess
			True -> try_next additional_expr_traces where
				try_next [] = return $ Right False
				try_next ((expr',trace'):rest) = do
					cont (expr',trace'++trace) >>= \case
						Right success -> case success of
							True -> return $ Right True
							False -> try_next rest

unfoldTraces1M ret_type toplevel break_stack (env:envs) trace ( (CBlockDecl decl@(CDecl typespecs triples _) : rest) : rest2 ) = do
	ty <- decl2TypeM decl
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
	unfoldTracesM ret_type toplevel break_stack ((concat newenvs ++ env) : envs) (concat newitems ++ trace) ((concat initializerss ++ rest):rest2)

unfoldTraces1M ret_type toplevel break_stack (_:restenvs) trace ([]:rest2) = do
	let break_stack' = dropWhile (> (length rest2)) break_stack
	unfoldTracesM ret_type toplevel break_stack' restenvs trace rest2

unfoldTraces1M ret_type False _ _ trace [] = return $ Left [trace]
unfoldTraces1M ret_type True  _ _ trace [] = analyzeTraceM (Just ret_type) trace >>= return.Right

unfoldTraces1M _ _ _ _ _ ((cbi:_):_) = myError $ "unfoldTracesM " ++ (render.pretty) cbi ++ " not implemented yet."


infix 4 ⩵
(⩵) :: CExpression a -> CExpression a -> CExpression a
a ⩵ b = CBinary CEqOp a b (annotation a)

infix 4 !⩵
(!⩵) :: CExpression a -> CExpression a -> CExpression a
a !⩵ b = not_c $ CBinary CEqOp a b (annotation a)

infix 4 ⩾
(⩾) :: CExpression a -> CExpression a -> CExpression a
a ⩾ b = CBinary CGeqOp a b (annotation a)

infixr 3 ⋏
(⋏) :: CExpression a -> CExpression a -> CExpression a
a ⋏ b = CBinary CLndOp a b (annotation a)

infixr 2 ⋎
(⋎) :: CExpression a -> CExpression a -> CExpression a
a ⋎ b = CBinary CLorOp a b (annotation a)

infixr 7 ∗
(∗) :: CExpression a -> CExpression a -> CExpression a
a ∗ b = CBinary CMulOp a b (annotation a)

infixr 6 −
(−) :: CExpression a -> CExpression a -> CExpression a
a − b = CBinary CSubOp a b (annotation a)

not_c :: CExpression a -> CExpression a
not_c e = CUnary CNegOp e (annotation e)

class CreateInt a where
	ⅈ :: Integer -> a
instance CreateInt CExpr where
	ⅈ i = CConst $ CIntConst (cInteger i) undefNode
instance CreateInt CExprWithType where
	ⅈ i = CConst $ CIntConst (cInteger i) (undefNode,intType)

infix 1 ≔
(≔) :: CExpression a -> CExpression a -> CExpression a
ass ≔ expr = CAssign CAssignOp ass expr (annotation ass)


fvar :: CExprWithType -> [Ident]
fvar expr = nub $ everything (++) (mkQ [] searchvar) (everywhere (mkT delete_attrs) expr)
	where

	delete_attrs :: CAttribute NodeInfoWithType -> CAttribute NodeInfoWithType
	delete_attrs (CAttr ident _ a) = CAttr ident [] a

	searchvar :: CExpression NodeInfoWithType -> [Ident]
	searchvar (CVar ident _) = [ ident ]
	searchvar _ = []

cinitializer2blockitems :: CExpr -> Type -> CInit -> CovVecM [CBlockItem]
cinitializer2blockitems lexpr ty initializer =
	case initializer of
		CInitExpr expr ni_init -> return [ CBlockStmt $ CExpr (Just $ lexpr ≔ expr ) ni_init ]
		CInitList initlist _ -> case ty of
			DirectType (TyComp (CompTypeRef sueref _ _)) _ _ -> do
				memberidentstypes <- getMembersM sueref
				concatForM (zip initlist memberidentstypes) $ \case
					(([],initializer),(memberident,memberty)) ->
						cinitializer2blockitems (CMember lexpr memberident False (nodeInfo memberident)) memberty initializer
					_ -> myError $ "cinitializer2blockitems: CPartDesignators not implemented yet!"
			_ -> myError $ "cinitializer2blockitems: " ++ (render.pretty) ty ++ " at " ++ (show $ nodeInfo lexpr) ++ " is no composite type!"


inferLExprTypeM :: TyEnv -> CExpr -> CovVecM Type
inferLExprTypeM tyenv expr = case expr of
	CVar ident _ -> case lookup ident tyenv of
		Nothing -> error $ "inferLExprTypeM " ++ (render.pretty) expr ++ " : Could not find " ++ (render.pretty) ident ++ " in " ++ showTyEnv tyenv
		Just ty -> elimTypeDefsM ty
	CMember objexpr member True _ -> do
		PtrType objty _ _ <- inferLExprTypeM tyenv objexpr
		getMemberTypeM objty member
	CMember objexpr member False _ -> do
		objty <- inferLExprTypeM tyenv objexpr
		getMemberTypeM objty member
	other -> myError $ "inferLExprTypeM " ++ (render.pretty) expr ++ " not implemented"

-- Creates an CExprWithType from a CExpr
transcribeExprM :: [Env] -> CExpr -> Type -> CovVecM CExprWithType
transcribeExprM envs expr target_ty = do
	insertImplicitCastsM envs (renameVars envs expr) target_ty

-- Translates all identifiers in an expression to fresh ones,
-- and expands function calls.
-- It needs to keep the original NodeInfos, because of the coverage information which is derived from the original source tree.
translateExprM :: [Env] -> CExpr -> Type -> CovVecM [(CExprWithType,Trace)]
translateExprM envs expr0 target_ty = do
	printLogV 1 $ "translateExprM [envs] " ++ (render.pretty) expr0 ++ " " ++ (render.pretty) target_ty

	-- extract a list of all calls from the input expression (including fun-identifier, the arguments, and NodeInfo)
	let	
		to_call :: CExpr -> StateT [(Ident,[CExpr],NodeInfo)] CovVecM CExpr
		to_call (CCall funexpr args ni) = case funexpr of
			CVar (Ident "__builtin_expect" _ _) _ -> return $ head args
			CVar (Ident "solver_pragma" _ _) _ -> return $ ⅈ 1
			CVar funident _ -> do
				modify ( (funident,args,ni): )
				-- Replace the call by a placeholder with the same NodeInfo
				return $ CConst $ CStrConst (CString (show ni) False) ni
			_  -> myError $ "is_call: found call " ++ (render.pretty) funexpr
		to_call expr = return expr
	(expr,calls) <- runStateT (everywhereM (mkM to_call) expr0) []

	funcalls_traces :: [(NodeInfo,[(Trace,CExprWithType)])] <- forM calls $ \ (funident,args,ni) -> do
		FunDef (VarDecl _ _ (FunctionType (FunType ret_ty paramdecls False) _)) body _ <- lookupFunM funident
		expanded_params_args <- expand_params_argsM paramdecls args
		printLogV 1 $ "body = " ++ (render.pretty) body
		printLogV 1 $ "expanded_params_args = " ++ show expanded_params_args
		-- β-reduction of the arguments:
		let body' = replace_param_with_arg expanded_params_args body
		printLogV 1 $ "body'= " ++ (render.pretty) body'
		Left funtraces <- unfoldTracesM ret_ty False [] envs [] [ [ CBlockStmt body' ] ]
		forM_ funtraces $ \ tr -> printLogV 2 $ "funtrace = " ++ showTrace tr
		let funtraces_rets = concat $ for funtraces $ \case
			Return retexpr : tr -> [(tr,retexpr)]
			tr -> error $ "funcalls_traces: trace of no return:\n" ++ showTrace tr
		return (ni,funtraces_rets) 

	expr' <- transcribeExprM envs expr target_ty

	printLogV 1 $ "creating combinations..."
	create_combinations expr' [] funcalls_traces

	where

	-- From the list of ParamDecls, extract the identifiers from the declarations and pair them with the argument
	expand_params_argsM ::  [ParamDecl] -> [CExpr] -> CovVecM [(Ident,CExpr)]
	expand_params_argsM paramdecls args = concatForM (zip paramdecls args) expandparam where
		expandparam :: (ParamDecl,CExpr) -> CovVecM [(Ident,CExpr)]
		expandparam (paramdecl,arg) = do
			let VarDecl (VarName srcident _) _ arg_ty = getVarDecl paramdecl
			return [(srcident,arg)]

	-- β-reduction
	replace_param_with_arg :: [(Ident,CExpr)] -> CStat -> CStat
	replace_param_with_arg iexprs stmt = foldl
		(\ stmt' (ident,cexpr) -> substituteBy (CVar ident undefNode) cexpr stmt')
		stmt
		iexprs

	set_node_info :: CExprWithType -> CExprWithType
	set_node_info cexpr = everywhere (mkT subst_ni) cexpr where
		subst_ni :: NodeInfo -> NodeInfo
		subst_ni _ = nodeInfo expr0

	create_combinations :: CExprWithType -> Trace -> [(NodeInfo,[(Trace,CExprWithType)])] -> CovVecM [(CExprWithType,Trace)]
	create_combinations expr trace [] = return [(set_node_info expr,trace)]
	create_combinations expr trace ((ni,tes):rest) = do
		concatForM tes $ \ (fun_trace,ret_expr) -> do
			let
				-- substitute the function call by the return expression
				expr' = everywhere (mkT subst_ret_expr) expr
				subst_ret_expr :: CExprWithType -> CExprWithType
				subst_ret_expr expr = if nodeInfo expr == ni then ret_expr else expr
--			printLog $ "fun_trace=" ++ show fun_trace
			create_combinations expr' (fun_trace++trace) rest


-- Renames Variables to unique names, looking up their unique name (wíth a number suffix)

renameVars :: [Env] -> CExpr -> CExpr
renameVars envs expr = everywhere (mkT subst_var) expr where
	subst_var :: CExpr -> CExpr
	subst_var (CVar ident ni) = case lookup ident (concat envs) of
		Just (ident',_) -> CVar ident' ni
		Nothing -> error $ " in subst_var : Could not find " ++ (render.pretty) ident ++ " in\n" ++ envToString (concat envs)
	subst_var expr = expr

-- Substitutes an expression x by y everywhere in a
substituteBy :: (Eq a,Data a,Data d) => a -> a -> d -> d
substituteBy x y d = everywhere (mkT (substexpr x y)) d
	where
	substexpr :: (Eq a) => a -> a -> a -> a
	substexpr x y found_expr | x == found_expr = y
	substexpr _ _ found_expr                   = found_expr

-- elimInds:
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
	
	create_var :: CExprWithType -> Type -> StateT [(Ident,Type)] CovVecM CExprWithType
	create_var expr ty = do
		let newident = mkIdentWithCNodePos expr $ lValueToVarName expr
		when (not $ newident `elem` new_idents) $
			modify ((newident,ty) : )
		return $ CVar newident (annotation expr)

	createsymvar_m :: CExprWithType -> StateT [(Ident,Type)] CovVecM CExprWithType

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

data SCompound = SExprLine SExpr | SComment String | SEmptyLine deriving Show
instance Pretty SCompound where
	pretty SEmptyLine = text ""
	pretty (SComment s) = semi <+> text s
	pretty (SExprLine sexpr) = pretty sexpr

data SExpr = SExpr [SExpr] | SLeaf String | SOnOneLine SExpr deriving Show
instance Pretty SExpr where
	pretty (SOnOneLine sexpr) = prettyOneLine sexpr
	pretty (SLeaf s) = text s
	pretty sexpr@(SExpr (SLeaf "_" : sexprs)) = prettyOneLine sexpr
	pretty (SExpr (sexpr:sexprs)) = (lparen <> pretty sexpr) $+$
		(nest 4 $ vcat (map pretty sexprs)) $+$
		rparen

prettyOneLine (SOnOneLine sexpr) = prettyOneLine sexpr
prettyOneLine (SLeaf s) = text s
prettyOneLine (SExpr (sexpr:sexprs)) =
	lparen <> prettyOneLine sexpr <+> hsep (map prettyOneLine sexprs) <> rparen
prettyOneLine s = error $ "In prettyOneLine: " ++ show s

extractType :: CExprWithType -> Z3_Type
extractType = snd.annotation

extractNodeInfo :: CExprWithType -> NodeInfo
extractNodeInfo = fst.annotation

type NodeInfoWithType = (NodeInfo,Z3_Type)
type CExprWithType = CExpression NodeInfoWithType

{-
implicitOpTypeConversionMax :: Type -> Type -> Type
implicitOpTypeConversionMax ty1@(DirectType tyname1 _ _) ty2@(DirectType tyname2 _ _) =
	case arithmeticConversion tyname1 tyname2 of
		Just max_tyname -> DirectType max_tyname noTypeQuals noAttributes
		Nothing -> error $ "implicitOpTypeConversionMax " ++ (render.pretty) ty1 ++ " " ++ (render.pretty) ty2 ++
			" yielded Nothing!"
implicitOpTypeConversionMax ty1 ty2 = error $ "implicitOpTypeConversionMax " ++ (render.pretty) ty1 ++ " " ++
	(render.pretty) ty2 ++ " : there should be a explicit cast!"
-}

insertImplicitCastsM :: [Env] -> CExpr -> Type -> CovVecM CExprWithType
insertImplicitCastsM envs cexpr target_ty = do
	printLogV 2 $ "insertImplicitCastsM [envs] " ++ (render.pretty) cexpr ++ " " ++ (render.pretty) target_ty
	insert_impl_casts cexpr >>= maybe_cast True target_ty

	where

	tyenv = envs2tyenv envs

	insert_impl_casts :: CExpr -> CovVecM CExprWithType
	
	insert_impl_casts (CBinary binop expr1 expr2 ni) = do
		expr1' <- insert_impl_casts expr1
		expr2' <- insert_impl_casts expr2
		let common_ty = max (extractType expr1') (extractType expr2')
		CBinary <$> pure binop <*>
			maybe_cast False common_ty expr1' <*>
			maybe_cast False common_ty expr2' <*>
			pure (ni,common_ty)

	insert_impl_casts (CCast decl expr ni) = do
		ty' <- decl2TypeM decl >>= elimTypeDefsM
		CCast <$> pure decl <*>
			insert_impl_casts expr <*>
			pure (ni,ty2Z3Type ty')

	insert_impl_casts (CUnary unop expr ni) = do
		expr' <- insert_impl_casts expr
		let ty = extractType expr'
		CUnary <$> pure unop <*> pure expr' <*> pure (ni, case (unop,ty) of
			(CAdrOp, ty) -> Z3_Ptr ty
			(CIndOp, Z3_Ptr ty) -> ty
			(_,      ty)        -> ty )

	insert_impl_casts (CAssign assign_op lexpr ass_expr ni) = do
		lexpr_ty <- inferLExprTypeM tyenv lexpr
		CAssign <$> pure assign_op <*>
			transcribeExprM envs lexpr lexpr_ty <*>
			insert_impl_casts ass_expr <*>
			pure (ni,lexpr_ty)

-- CONTINUE HERE

	insert_impl_casts (CCond cond_expr (Just then_expr) else_expr ni) = do
		then_expr' <- insert_impl_casts then_expr
		else_expr' <- insert_impl_casts else_expr
		let common_ty = max (extractType then_expr') (extractType else_expr')
		CCond <$> (insert_impl_casts cond_expr >>= maybe_cast False intType) <*>
			(Just <$> maybe_cast False common_ty then_expr') <*>
			maybe_cast False common_ty else_expr' <*>
			pure (ni , common_ty)

	insert_impl_casts ccall@(CCall fun_expr args ni) = do
		case fun_expr of
			CVar (Ident "a__builtin_expect" _ _) _ -> insert_impl_casts $ head args
			CVar (Ident "solver_pragma" _ _) _ -> return $ ⅈ 1
			CVar funident fun_ni -> do
				FunDef (VarDecl _ _ fun_ty@(FunctionType (FunType ret_type funparamdecls False) _)) _ _ <- lookupFunM funident
				args' <- forM (zip args funparamdecls) $ \ (arg,paramdecl) -> do
					let VarDecl _ _ arg_ty = getVarDecl paramdecl
					formalparam_ty' <- elimTypeDefsM arg_ty
					arg' <- insert_impl_casts arg
					maybe_cast False formalparam_ty' arg'
				ret_type' <- elimTypeDefsM ret_type
				return $ CCall (CVar funident (fun_ni,fun_ty)) args' (ni,ret_type')
			other -> myError $ "insert_impl_casts " ++ (render.pretty) ccall ++ " not implemented yet!"

	insert_impl_casts cvar@(CVar ident ni) = case lookup ident tyenv of
		Nothing -> error $ "Could not find " ++ (render.pretty) ident ++ " in " ++ showTyEnv tyenv
		Just ty -> do
			CVar <$> pure ident <*> ((ni,) <$> elimTypeDefsM ty)

	insert_impl_casts (CConst ctconst) = return $ CConst $ case ctconst of
		CIntConst cint@(CInteger _ _ flags) ni -> CIntConst cint (ni,flags2IntType flags)
		CFloatConst cfloat@(CFloat s) ni       -> CFloatConst cfloat (ni,string2FloatType s)
		CCharConst cchar@(CChar _ False) ni    -> CCharConst cchar (ni,charType)
		CStrConst cstr ni                      -> CStrConst cstr (ni,ptrType charType)

	insert_impl_casts lexpr@(CMember pexpr member_ident isp ni) =
		CMember <$> insert_impl_casts pexpr <*> pure member_ident <*> pure isp <*> ((ni,) <$> inferLExprTypeM tyenv lexpr)

	insert_impl_casts other = myError $ "insert_impl_casts " ++ (render.pretty) other ++ " not implemented"

	-- first Bool arg says if downcasting is allowed
	maybe_cast :: Bool -> Type -> CExprWithType -> CovVecM CExprWithType
	maybe_cast _ to_ty expr | extractType expr == to_ty = return expr
	maybe_cast False to_ty expr | implicitOpTypeConversionMax (extractType expr) to_ty == extractType expr =
		error $ "maybe_cast\n    " ++ (render.pretty) expr ++ "\n    " ++ (render.pretty) (extractType expr) ++ "\n    " ++
			(render.pretty) to_ty ++ "\n    at " ++ (showLocation.lineColNodeInfo) expr ++ " is a downcast that should not occur implicitly!"
	maybe_cast downcast to_ty expr | downcast || implicitOpTypeConversionMax (extractType expr) to_ty == to_ty = do
		CCast <$> type2DeclM to_ty <*> pure expr <*> pure (nodeInfo expr,to_ty)
	maybe_cast _ to_ty expr =
		error $ "maybe_cast " ++ (render.pretty) expr ++ " " ++ (render.pretty) (extractType expr) ++ " " ++
			(render.pretty) to_ty ++ " : implicitOpTypeConversionMax " ++ (render.pretty) (extractType expr) ++ " " ++
			(render.pretty) to_ty ++ " = " ++ (render.pretty) (implicitOpTypeConversionMax (extractType expr) to_ty) ++
			" is not equal to from_ty or to_ty !"


type Constraint = CExprWithType

expr2SExpr :: Constraint -> CovVecM (SExpr,Constraint)
expr2SExpr expr = do
	sexpr <- expr2sexpr True expr
	return (sexpr,expr)

	where

	make_intconstant :: Type -> Integer -> SExpr
	make_intconstant ty const = SLeaf (printf "#x%*.*x" (sizeofIntTy ty `div` 4) (sizeofIntTy ty `div` 4) const)

	unSignedTy ty unsigned signed = case ty2Z3Type ty of
		Z3_BitVector _ is_unsigned -> if is_unsigned then unsigned else signed
		_ -> error $ "unSigned: ty of " ++ (render.pretty) expr ++ " is no bitvector!"

	expr2sexpr :: Bool -> CExprWithType -> CovVecM SExpr
	expr2sexpr b cexpr = do
		printLogV 2 $ "expr2sexpr " ++ show b ++ " " ++ (render.pretty) cexpr
		expr2sexpr' b cexpr

	expr2sexpr' :: Bool -> CExprWithType -> CovVecM SExpr

	expr2sexpr' must_be_bool expr = case expr of

		-- returns Bool
		CUnary CNegOp subexpr _ -> SExpr <$> sequence [ pure $ SLeaf "not", expr2sexpr True subexpr ]

		-- all binary operators returning Bool
		CBinary CNeqOp expr1 expr2 _ -> expr2sexpr False $ expr1 !⩵ expr2
		CBinary binop expr1 expr2 (_,ty) | binop `elem` [CLndOp,CLorOp,CLeOp,CGrOp,CLeqOp,CGeqOp,CEqOp] -> do
			SExpr <$> sequence [ pure $ SLeaf op_sexpr,
				expr2sexpr (binop `elem` [CLndOp,CLorOp]) expr1,
				expr2sexpr (binop `elem` [CLndOp,CLorOp]) expr2 ]
				where
				op_sexpr = case binop of
					CLndOp -> "and"
					CLorOp -> "or"
					CLeOp  -> unSignedTy ty "bvult" "bvslt"
					CGrOp  -> unSignedTy ty "bvugt" "bvsgt"
					CLeqOp -> unSignedTy ty "bvule" "bvsle"
					CGeqOp -> unSignedTy ty "bvuge" "bvsge"
					CEqOp  -> "="

		-- from here on, then expression may not return Bool,
		-- hence a check has to be made if a (!=0) has to be inserted
		non_bool_expr | must_be_bool -> expr2sexpr False $ non_bool_expr !⩵ ⅈ 0

		non_bool_expr -> do
			case non_bool_expr of
				-- binary operators returning the operands' type
				CBinary binop expr1 expr2 (_,ty) ->
					SExpr <$> sequence [ pure $ SLeaf op_sexpr, expr2sexpr False expr1, expr2sexpr False expr2 ]
					where
					op_sexpr = case binop of
						CMulOp -> "bvmul"
						CDivOp -> "bvdiv"
						CAddOp -> "bvadd"
						CSubOp -> "bvsub"
						CRmdOp -> unSignedTy ty "bvurem" "bvsrem"
						CShlOp -> unSignedTy ty "bvshl" "bvshl"
						CShrOp -> unSignedTy ty "bvlshr" "bvashr"
						CAndOp -> "bvand"
						COrOp  -> "bvor"
						CXorOp -> "bvxor"
	
				cconst@(CConst ctconst) -> return $ case ctconst of
					CIntConst intconst (_,ty) -> make_intconstant ty (getCInteger intconst)
					CCharConst cchar _        -> SLeaf $ (render.pretty) cconst
					CFloatConst cfloat _      -> SLeaf $ (render.pretty) cconst
					CStrConst cstr _          -> SLeaf $ (render.pretty) cconst
		
				CVar ident _ -> return $ SLeaf $ (render.pretty) ident
		
				CUnary CPlusOp subexpr _ -> expr2sexpr False subexpr
				CUnary op subexpr _ -> SExpr <$> sequence [ pure $ SLeaf op_str, expr2sexpr False subexpr ]
					where
					op_str = case op of
						CMinOp  -> "bvneg"
						CCompOp -> "bvnot"
						_ -> error $ "expr2sexpr " ++ (render.pretty) op ++ " should not occur!"
		
				CCast to_decl subexpr (_,from_ty) -> do
					to_ty <- decl2TypeM to_decl
					sexpr <- expr2sexpr False subexpr
					return $ case ( ty2Z3Type from_ty, ty2Z3Type to_ty ) of
						-- SAMECAST: identity
						( Z3_BitVector size_from _, Z3_BitVector size_to _ ) | size_from == size_to -> sexpr
				
						-- DOWNCAST: extract bits (modulo)
						( Z3_BitVector size_from _, Z3_BitVector size_to _ ) | size_from > size_to ->
							SExpr [ SExpr [ SLeaf "_", SLeaf "extract", SLeaf (show $ size_to - 1), SLeaf "0"], sexpr ]
				
						-- UPCAST signed (to signed or unsigned): extend sign bit
						( Z3_BitVector size_from True, Z3_BitVector size_to _ ) ->
							SExpr [ SExpr [ SLeaf "_", SLeaf "sign_extend", SLeaf $ show (size_to-size_from) ], sexpr ] 
				
						-- UPCAST unsigned (to signed or unsigned): extend with zeros
						( Z3_BitVector size_from False, Z3_BitVector size_to _ ) ->
							SExpr [ SExpr [ SLeaf "_", SLeaf "zero_extend", SLeaf $ show (size_to-size_from) ], sexpr ]
				
						_ -> error $ "mb_cast " ++ show sexpr ++ " " ++
							(render.pretty) from_ty ++ " " ++ (render.pretty) to_ty ++ " not implemented!"
		
				ccond@(CCond cond (Just then_expr) else_expr _) -> do
					SExpr <$> sequence [
						pure $ SLeaf "ite",
						expr2sexpr True cond,
						expr2sexpr False then_expr,
						expr2sexpr False else_expr ]

				cmember@(CMember _ _ _ _) -> myError $ "expr2sexpr " ++ (render.pretty) cmember ++ " should not occur!"
			
				ccall@(CCall _ _ _) -> myError $ "expr2sexpr " ++ (render.pretty) ccall ++ " should not occur!"
		
				other -> myError $ "expr2SExpr " ++ (render.pretty) other ++ " not implemented" 
{-
			return $ case must_be_bool of
				False -> sexpr
				True  -> SExpr 
-}

data Z3_Type = Z3_BitVector Int Bool | Z3_Float | Z3_Double | Z3_Ptr Z3_Type
	deriving (Show,Eq,Ord,Data)
-- Z3_BitVector Int (isUnsigned::Bool), hence
-- the derived ordering intentionally coincides with the type casting ordering :-)

sizeofIntTy :: Type -> Int
sizeofIntTy ty@(DirectType tyname _ attrs) = case tyname of
	TyIntegral intty -> case (intty,map to_mode attrs) of
		(TyChar,[])     -> 8
		(TySChar,[])    -> 8
		(TyUChar,[])    -> 8
		(TyShort,[])    -> 16
		(TyUShort,[])   -> 16
		(TyInt,[])      -> intSize
		(TyInt,["SI"])  -> 32
		(TyInt,["DI"])  -> 64
		(TyUInt,[])     -> intSize
		(TyUInt,["SI"]) -> 32
		(TyUInt,["DI"]) -> 64
		(TyLong,[])     -> longIntSize
		(TyULong,[])    -> longIntSize
		(TyLLong,[])    -> 64
		(TyULLong,[])   -> 64
		other           -> error $ "sizeofIntTy " ++ show other ++ " not implemented!"
	other -> error $ "sizeofIntTy: " ++ (render.pretty) ty ++ " is not an Integral type"
	where
	to_mode (Attr (Ident "mode" _ _) [CVar (Ident mode _ _) _] _) = mode
	to_mode attr = error $ "attrs2modes: unknown attr " ++ (render.pretty) attr

ty2Z3Type :: Type -> Z3_Type
ty2Z3Type ty = case ty of
	DirectType (TyComp (CompTypeRef _ _ _)) _ _ -> Z3_BitVector 4 True
	DirectType tyname _ attrs -> case tyname of
		TyIntegral intty    -> Z3_BitVector (sizeofIntTy ty) $ intty `elem` [TyChar,TyUChar,TyUShort,TyUInt,TyULong,TyULLong]
		TyFloating TyFloat  -> Z3_Float
		TyFloating TyDouble -> Z3_Double
		TyEnum _            -> Z3_BitVector intSize True
		TyComp _            -> Z3_BitVector 16 True
		_ -> error $ "ty2Z3Type " ++ (render.pretty) ty ++ " not implemented!"
	PtrType _ _ _ -> Z3_BitVector 16 True
	TypeDefType (TypeDefRef _ ty _) _ _ -> ty2Z3Type ty
	_ -> error $ "ty2Z3Type " ++ (render.pretty) ty ++ " should not occur!"

z3Ty2SExpr :: Z3_Type -> SExpr
z3Ty2SExpr ty = case ty of
	Z3_BitVector size _ -> SExpr [ SLeaf "_", SLeaf "BitVec", SLeaf (show size) ]
	Z3_Float            -> SLeaf "Float32"
	Z3_Double           -> SLeaf "Float64"

type Solution = [(String,SolutionVal)]

data SolutionVal = IntVal Int | FloatVal Float | DoubleVal Double
instance Eq SolutionVal where
	IntVal i1    == IntVal i2    = i1==i2
	FloatVal f1  == FloatVal f2  = abs (f2-f1) <= floatTolerance
	DoubleVal f1 == DoubleVal f2 = abs (f2-f1) <= doubleTolerance

instance Show SolutionVal where
	show (IntVal i)    = show i
	show (FloatVal f)  = show f
	show (DoubleVal f) = show f

makeAndSolveZ3ModelM :: [Int] -> TyEnv -> [CExprWithType] -> [SExpr] -> [Ident] -> String -> CovVecM (String,Maybe Solution)
makeAndSolveZ3ModelM traceid tyenv constraints additional_sexprs output_idents modelpathfile = do
	opts <- gets optsCVS
	printLogV 2 $ "output_idents = " ++ showIdents output_idents
	let
		constraints_vars = nub $ concatMap fvar constraints
	printLogV 2 $ "constraints_vars = " ++ showIdents constraints_vars

	let
		varsZ3 :: [SCompound] = for (filter ((`elem` (constraints_vars ++ output_idents)).fst) tyenv) $ \ (ident,ty) ->
			SExprLine $ SOnOneLine $ SExpr [ SLeaf "declare-const", SLeaf (identToString ident), z3Ty2SExpr ty ]
	constraintsZ3 :: [SCompound] <- concatForM constraints $ \ expr -> do
		(assert_sexpr,orig_expr) <- expr2SExpr expr
		return [ SEmptyLine,
			SComment "----------------------------------------------",
			SComment $ showLocation (lineColNodeInfo orig_expr) ++ " : ",
		 	SComment $ (render.pretty) orig_expr,
			SComment "----------------------------------------------",
			SExprLine $ SExpr [SLeaf "assert", assert_sexpr] ]
	let
		outputvarsZ3 = for output_idents $ \ ident -> SExprLine $ SOnOneLine $ SExpr [SLeaf "get-value", SExpr [ SLeaf $ identToString ident ] ]
		model :: [SCompound] = [
			SComment $ show traceid,
			SEmptyLine,
			SExprLine $ SOnOneLine $ SExpr [SLeaf "set-option", SLeaf ":smt.relevancy", SLeaf "0"],
			SExprLine $ SOnOneLine $ SExpr [SLeaf "set-option", SLeaf ":produce-models", SLeaf "true"],
			SEmptyLine ] ++
			varsZ3 ++
			[SEmptyLine] ++
			constraintsZ3 ++
			[SEmptyLine] ++
			map (SExprLine . SOnOneLine) additional_sexprs ++
			[SEmptyLine] ++
			[ SExprLine $ SOnOneLine $ SExpr [SLeaf "check-sat"] ] ++
			[SEmptyLine] ++
			outputvarsZ3
		model_string = unlines $ map (render.pretty) model
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
	printLogV 1 $ "solveTraceM " ++ show traceid ++ " ..."
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
			let name_id = internalIdent (name ++ "_" ++ show i) in (name_id,CVar name_id (undefNode,ty) ⩵ expr,(name_id,ty))

		tyenv = createTyEnv trace ++ debug_tyenv

	(model_string,mb_sol) <- makeAndSolveZ3ModelM
		traceid
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

int a[10];
a[2] = 7;
a[2] = a[2] + 1;

-}
