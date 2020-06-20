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
import Text.Printf
import Text.Regex.TDFA
import Numeric (readHex)

{-
import Interfaces.FZSolutionParser
import qualified Interfaces.MZAST as MZAST
import Interfaces.MZPrinter
import Interfaces.MZinHaskell
import qualified Interfaces.MZBuiltIns as MZB
-}

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

intSize = 32
longIntSize = 64

solveIt = True
showOnlySolutions = True
don'tShowTraces = True
checkSolutions = solveIt && True
returnval_var_name = "return_val"
outputVerbosity = 1
floatTolerance = 1e-7 :: Float

z3FilePath = "C:\\z3-4.8.8-x64-win\\bin\\z3.exe"

_UNROLLING_DEPTH = 32

analyzerPath = "analyzer"
logFile = analyzerPath </> "log.txt"

printLog :: (MonadIO m) => String -> m ()
printLog text = liftIO $ do
	putStrLn text
	appendFile logFile (text++"\n")

printLogV :: (MonadIO m) => Int -> String -> m ()
printLogV verbosity text = when (verbosity>=outputVerbosity) $ printLog text

showLine :: Trace -> String
showLine trace = unlines $ map show (filter isnotbuiltin trace)

show_solution _ Nothing = "No solution"
show_solution _ (Just (_,_,[],_)) = "Empty solution"
show_solution funname (Just v@(_,_,solution,_)) = unlines [ show solution, showTestVector funname v ]

main = do
	-- when there is an error, we'd like to have *all* output till then
	hSetBuffering stdout NoBuffering

	gcc:filename:funname:opts <- getArgs >>= return . \case
--		[] -> "gcc" : (analyzerPath++"\\test.c") : "g" : [] --["-writeAST","-writeGlobalDecls"]
--		[] -> "gcc" : (analyzerPath++"\\myfp-bit.c") : "_fpdiv_parts" : [] --"-writeAST","-writeGlobalDecls"]
--		[] -> "gcc" : (analyzerPath++"\\branchtest.c") : "f" : ["-writeTree"] --["-writeAST","-writeGlobalDecls"]
--		[] -> "gcc" : (analyzerPath++"\\iftest.c") : "f" : [] --["-writeAST","-writeGlobalDecls"]
--		[] -> "gcc" : (analyzerPath++"\\deadtest.c") : "f" : [] --["-writeAST","-writeGlobalDecls"]
--		[] -> "gcc" : (analyzerPath++"\\whiletest.c") : "f" : [] --["-writeAST","-writeGlobalDecls"]
--		[] -> "gcc" : (analyzerPath++"\\ptrtest_flat.c") : "f" : ["-writeAST"]
		[] -> "gcc" : (analyzerPath++"\\ptrtest.c") : "f" : [] --["-writeAST"]
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
	
						printLog $ "\n####### FINAL RESULT #######\n"
	
						forM_ testvectors $ \ (traceid,trace,(model_string,mb_solution)) -> do
							case not showOnlySolutions || maybe False (not.null.(\(_,_,b,_)->b)) mb_solution of
								False -> return ()
								True -> printLog $ unlines $ mbshowtraces (
									[ "","=== TRACE " ++ show traceid ++ " ========================","<leaving out builtins...>" ] ++
									[ showLine trace ] ++
									[ "",
									"--- MODEL " ++ show traceid ++ " -------------------------",
									model_string,
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
	
						when (full_coverage && not (null deaths)) $ error "full coverage but deaths!"
						when (not full_coverage && null deaths) $ error "not full_coverage and no deaths!"
	
						printLog $ case full_coverage of
							False -> "FAIL, there are coverage gaps!"
							True  -> "OK, we have full branch coverage."

showEnv :: Env -> String
showEnv env = "{\n    " ++ intercalate " ,\n    " (map (render.pretty) env) ++ "\n    }"

showTestVector :: String -> (Env,Env,Solution,Maybe CExpr) -> String
showTestVector funname (env,ret_env,solution,mb_retval) = funname ++ " ( " ++ intercalate " , " (map showarg env) ++ " )" ++
	" = " ++ maybe "<NO_RETURN>" (const $ (intercalate " " $ map showarg ret_env)) mb_retval
	where
	showarg :: EnvItem -> String
	showarg (oldident,(newident,_)) =
		identToString oldident ++ " = " ++ case lookup (identToString newident) solution of
			Nothing           -> "DONT_CARE"
			Just (BoolVal b)  -> show b
			Just (IntVal i)   -> show i
			Just (FloatVal f) -> show f

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
	TraceOr traces  -> "OR\n"  ++ showlist traces
	TraceAnd traces -> "AND\n" ++ showlist traces
	te              -> show te ++ "\n" ++ showTrace ind trace
	where
	indent i = concat $ replicate i ":   "
	showlist traces = indent (ind+1) ++ "[\n" ++ showitems traces ++ indent (ind+1) ++ "]\n"
	showitems traces = intercalate (indent (ind+2) ++ ",\n") (map (showTrace (ind+2)) traces)

type ResultData = (String,Maybe (Env,Env,Solution,Maybe CExpr))
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

	param_env <- createInterfaceM $ for (map getVarDecl funparamdecls) $ \ (VarDecl (VarName srcident _) _ ty) -> (srcident,ty)
	printLogV 1 $ "param_env = " ++ showEnv param_env
	
	let decls = map (NewDeclaration .snd) (reverse param_env ++ glob_env)

	trace <- unfoldTracesM True (param_env:[glob_env]) decls [ defs ++ [ CBlockStmt body ] ]
	when ("-writeTree" ∈ opts) $ liftIO $ writeFile (filename ++ "_tree" <.> "html") $ traceToHTMLString trace
	printLogV 1 $ "\n********** TRACE ***********\n" ++ showTrace 0 trace
	printLogV 1 $ "****************************\n"

	runStateT (analyzeTreeM opts ret_type param_env [] [] trace) ([],Set.empty,Set.empty)

type Location = (Int,Int)

showLocation :: Location -> String
showLocation (l,c) = "line " ++ show l ++ ", col " ++ show c

type AnalyzeTreeM a = StateT ([TraceAnalysisResult],Set.Set Branch,Set.Set Branch) CovVecM a
-- Unfolds the tree to all execution paths, collecting the solutions and promoting them upwards.

analyzeTreeM :: [String] -> Type -> Env -> [Int] -> [TraceElem] -> Trace -> AnalyzeTreeM Bool

analyzeTreeM opts ret_type param_env traceid res_line [] = do
	when True $ do --(not don'tShowTraces) $ do
		printLogV 1 $ "=== TRACE " ++ show traceid ++ " ========================\n<leaving out builtins...>\n"
		printLogV 1 $ showLine res_line
	
	res_trace <- lift $ elimInds res_line
	when (not don'tShowTraces) $ do
		printLogV 1 $ "\n=== TRACE after elimInds " ++ show traceid ++ " =========\n<leaving out builtins...>\n"
		printLogV 1 $ showLine res_trace
	
	res_trace' <- lift $ elimAssignmentsM res_trace
	when (not don'tShowTraces) $ do
		printLogV 1 $ "\n--- TRACE after elimAssignmentsM " ++ show traceid ++ " -----------\n<leaving out builtins...>\n"
		printLogV 1 $ showLine res_trace'

	res_trace'' <- lift $ substIndM [] (map fst $ createTyEnv res_trace') res_trace'
	when (not don'tShowTraces) $ do
		printLogV 1 $ "\n--- TRACE after substIndM " ++ show traceid ++ " -----------\n<leaving out builtins...>\n"
		printLogV 1 $ showLine res_trace''

	resultdata@(model_string,mb_solution) <- lift $ solveTraceM ret_type param_env traceid res_trace''
	when (not don'tShowTraces) $ printLogV 1 $ "\n--- MODEL " ++ show traceid ++ " -------------------------\n" ++ model_string
	funname <- lift $ gets funNameCVS
	printLogV 1 $ "\n--- SOLUTION " ++ show traceid ++ " ----------------------\n" ++ show_solution funname mb_solution

	startend <- lift $ gets funStartEndCVS
	let visible_trace = Set.fromList $ concatMap to_branch res_line
		where
		to_branch cond@(Condition b _) | is_visible_traceelem startend cond =
			[ (if b then Then else Else) (lineColNodeInfo cond) ]
		to_branch _ = []

	let traceanalysisresult :: TraceAnalysisResult = (traceid,res_line,resultdata)
	case is_solution traceanalysisresult of
		False -> do
			printLogV 1  $ "### FALSE : " ++ show traceid ++ " no solution!"
			modify $ \ (tas,covered,alls) -> (tas,covered,Set.union visible_trace alls)
			return False
		True  -> do
			printLogV 1  $ "### TRUE : " ++ show traceid ++ " Is Solution"
			lift $ checkSolutionM traceid resultdata
			modify $ \ (tas,covered,alls) -> case visible_trace `Set.isSubsetOf` covered of
				False -> (traceanalysisresult:tas,Set.union visible_trace covered,Set.union visible_trace alls)
				True  -> (tas,covered,alls)
			return True

analyzeTreeM opts ret_type param_env traceid res_line (TraceOr traces : rest) = case rest of
	[] -> do
		printLogV 1 $ "### analyzeTreeM : TraceOr " ++ show traceid ++ " ..."
		try_traces (zip [1..] traces)
			where
			try_traces :: [(Int,Trace)] -> AnalyzeTreeM Bool
			try_traces [] = return False
			try_traces ((i,trace):rest) = do
				let traceid' = (traceid++[i])
				success <- analyzeTreeM opts ret_type param_env traceid' res_line trace
				case success of
					True -> do
						printLogV 1  $ "### analyzeTreeM : TraceOr " ++ show traceid' ++ " returned TRUE"
						return True
					False -> do
						printLogV 1  $ "### analyzeTreeM : TraceOr " ++ show traceid' ++ " returned FALSE, trying the rest..."
						try_traces rest
	_ -> error $ "analyzeTreeM: TraceOr not last element in " ++ showTrace 1 res_line

analyzeTreeM opts ret_type param_env traceid res_line (TraceAnd traces : rest) = case rest of
	[] -> do
		printLogV 1  $ "### analyzeTreeM : TraceAnd " ++ show traceid ++ " ..."
		results <- forM (zip [1..] traces) $ \ (i,trace) ->
			analyzeTreeM opts ret_type param_env (traceid++[i]) res_line trace
		return $ all (==True) results
	_ -> error $ "analyzeTreeM: TraceAnd not last element in " ++ showTrace 1 res_line

analyzeTreeM opts ret_type param_env traceid res_line (te:rest) = do
	analyzeTreeM opts ret_type param_env traceid (te:res_line) rest

is_solution :: TraceAnalysisResult -> Bool
is_solution (_,_,(_,Just (_,_,solution,_))) = not $ null solution
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

createInterfaceM :: [(Ident,Type)] -> CovVecM [EnvItem]
createInterfaceM ty_env = concatForM ty_env $ \ (srcident,ty) -> do
--	let VarDecl (VarName srcident _) _ ty = getVarDecl decl
	elimTypeDefsM ty >>= create_interfaceM (CVar srcident (nodeInfo srcident))

	where

	create_interfaceM :: CExpr -> Type -> CovVecM [EnvItem]
	create_interfaceM expr ty = elimTypeDefsM ty >>= \case

		-- STRUCT* p
		PtrType (DirectType (TyComp (CompTypeRef sueref _ _)) _ _) _ _ -> prepend_plainvar $ do
			member_ty_s <- getMembersM sueref
			concatForM member_ty_s $ \ (m_ident,m_ty) -> create_interfaceM (CMember expr m_ident True (nodeInfo expr)) m_ty

		-- ty* p
		PtrType target_ty _ _ -> prepend_plainvar $ do
			create_interfaceM (CUnary CIndOp expr (nodeInfo expr)) target_ty

		-- STRUCT expr
		DirectType (TyComp (CompTypeRef sueref _ _)) _ _ -> prepend_plainvar $ do
			member_ty_s <- getMembersM sueref
			concatForM member_ty_s $ \ (m_ident,m_ty) -> do
				create_interfaceM (CMember expr m_ident False (nodeInfo expr)) m_ty

		-- direct-type expr where direct-type is no struct/union
		ty'@(DirectType _ _ _) -> do
			let srcident = mkIdentWithCNodePos expr (lValueToVarName expr)
			return [ (srcident,(srcident,ty')) ]

		ty' ->
			error $ "create_interfaceM " ++ (render.pretty) expr ++ " " ++ (render.pretty) ty' ++ " not implemented"

		where

		prepend_plainvar :: CovVecM [EnvItem] -> CovVecM [EnvItem]
		prepend_plainvar rest_m = do
			let srcident = internalIdent $ lValueToVarName expr
			rest <- rest_m
			return $ (srcident,(srcident,ty)) : rest

-- Just unfold the traces
unfoldTracesM :: Bool -> [Env] -> Trace -> [[CBlockItem]] -> CovVecM Trace
unfoldTracesM toplevel envs trace cbss = do
	cbss_txt <- case cbss of
		[] -> return "[]"
		(l : _) -> return $ "[ " ++ (intercalate " , " (map (render.pretty) l)) ++ " ] : _"
	res <- unfoldTraces1M toplevel envs trace cbss
	return res

unfoldTraces1M :: Bool -> [Env] -> Trace -> [[CBlockItem]] -> CovVecM Trace
unfoldTraces1M toplevel envs trace ((CBlockStmt stmt : rest) : rest2) = case stmt of

	CLabel _ cstat _ _ -> unfoldTracesM toplevel envs trace ((CBlockStmt cstat : rest) : rest2)

	CCompound _ cbis _ -> unfoldTracesM toplevel ([]:envs) trace (cbis : (rest : rest2))

	CIf cond then_stmt mb_else_stmt ni -> do
		then_trace <- transids TraceOr cond trace $ \ (cond',trace') -> do
			unfoldTracesM toplevel envs (Condition True cond' : trace') ( (CBlockStmt then_stmt : rest) : rest2 )
		else_trace <- transids TraceOr cond trace $ \ (cond',trace') -> do
			let not_cond = Condition False (CUnary CNegOp cond' (nodeInfo cond'))
			case mb_else_stmt of
				Nothing        -> unfoldTracesM toplevel envs (not_cond : trace') ( rest : rest2 )
				Just else_stmt -> unfoldTracesM toplevel envs (not_cond : trace') ( (CBlockStmt else_stmt : rest) : rest2 )
		return [ (if toplevel then TraceAnd else TraceOr) [then_trace,else_trace] ]

	CReturn Nothing _ -> return trace
	CReturn (Just ret_expr) _ -> do
		transids undefined ret_expr trace $ \ (ret_expr',trace') -> do
			return $ Return ret_expr' : trace'

	CExpr (Just cass@(CAssign assignop lexpr assigned_expr ni)) _ -> do
		transids TraceOr assigned_expr' trace $ \ (assigned_expr'',trace') -> do
			transids (error "more than one transid results for lexpr!") lexpr trace' $ \ (lexpr',trace'') -> do
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

	-- That's cheating: Insert condition into trace
	CGotoPtr cond _ -> unfoldTracesM toplevel envs (Condition True cond : trace) ( rest : rest2 )

 	CWhile cond body False ni -> do
		unrolleds <- forM [0 .. _UNROLLING_DEPTH] $ \ n ->
			unfoldTracesM toplevel envs trace ((unroll n ++ rest) : rest2 )
		return [ TraceOr unrolleds ]

		where

		unroll :: Int -> [CBlockItem]
		unroll n = concat ( replicate n [ CBlockStmt (CGotoPtr cond undefNode), CBlockStmt body ] ) ++
			[ CBlockStmt $ CGotoPtr (CUnary CNegOp cond undefNode) undefNode ]

{-
 	CWhile cond0 body False ni -> do

	-- get every assignment in the body trace that
	-- assigns exactly once in every body trace to a variable V mentioned in the condition.
	-- if there is exactly one such assignment to V,

 		translateExprM toplevel envs cond0 >>= \case
 			[(cond,[])] -> do
				let
					-- get all variables used in the condition and make sure there is no function call in it
					cond_idents = nub $ everything (++) (mkQ [] searchvar) cond where
						searchvar :: CExpr -> [Ident]
						searchvar (CVar ident _) = [ ident ]
						searchvar (CCall _ _ _) = error $ "while condition " ++ (render.pretty) cond ++ " contains function call!"
						searchvar _ = []

				-- unfold body to all body traces and filter for all Assignments to variables from the condition
 				body_trace <- unfoldTracesM toplevel envs [] [[CBlockStmt body]]
 				let
 					body_traces = flattenTrace [] (reverse body_trace)
 					body_traces_ass = map (concatMap from_ass) body_traces where
 						from_ass (Assignment a@(CVar i _) b) | i ∈ cond_idents = [(a,b)]
 						from_ass _ = []
--				printLog $ "body_traces_ass = "
--				forM_ body_traces_ass $ \ bta -> printLog $ intercalate " , " (map (\(a,b) -> "(" ++ (render.pretty) a ++ " = " ++ (render.pretty) b ++ ")") bta)

				-- Filter for all assignments that occur exactly once in every body trace
				let
					body_assigns = foldl1 intersect (map (exists_once) body_traces_ass)
					exists_once l = filter (\ e -> length (filter (==e) l) == 1) l
--				printLog $ "body_assigns = "
--				printLog $ intercalate " , " $ map (\(a,b) -> "(" ++ (render.pretty) a ++ " = " ++ (render.pretty) b ++ ")") body_assigns

				n <- case body_assigns of
					[ (ass_var@(CVar ass_ident _),ass_expr) ] -> case ass_expr of
						CBinary CSubOp (CVar ident _) cconst@(CConst _) _ | ident==ass_ident -> do
							let
								n_name = "n_loopings"
								n_ident = internalIdent n_name
								n_var = CVar n_ident undefNode
								modelpath = analyzerPath </> n_name ++ show (lineColNodeInfo cond) ++ ".smtlib2"
								n_type = case lookup ass_ident (map snd $ concat envs) of
									Nothing -> error $ "CWhile: Could not find type of " ++ (render.pretty) ass_var
									Just ty -> ty
							(model_string,mb_sol) <- makeAndSolveZ3ModelM
								((n_ident,n_type) : map snd (concat envs))
								[
									CBinary CGeqOp n_var (CConst $ CIntConst (cInteger 1) undefNode) undefNode,
									substituteBy ass_var (CBinary CSubOp ass_var (CBinary CMulOp n_var cconst undefNode) undefNode) cond,
									CUnary CNegOp (substituteBy ass_var
										(CBinary CSubOp ass_var (CBinary CMulOp (CBinary CAddOp n_var
										(CConst $ CIntConst (cInteger 1) undefNode) undefNode) cconst undefNode) undefNode) cond) undefNode
								]
								[ SExpr [SLeaf "minimize",SLeaf n_name] ]
								[n_ident]
								modelpath
--							printLog $ "Model is\n" ++ model_string
							case mb_sol of
								Nothing -> error $ "Found no solution for " ++ modelpath
								Just sol@[(_,MInt n)] -> do
									printLog $ "Found looping n solution " ++ show sol
									return n
								_ -> error $ "n_looping: Strange mb_sol=" ++ show mb_sol
						_ -> error $ "CWhile: assignment " ++ (render.pretty) ass_ident ++ " := " ++ (render.pretty) ass_expr ++ " not implemented!"
					other -> error $ "body contains not exactly one assignment of a variable from the condition " ++ (render.pretty) cond ++ ":\n" ++
						unlines (map (\(ass_var,_) -> (render.pretty) ass_var) other)

				unfoldTracesM toplevel envs trace ((replicate n (CBlockStmt body) ++ rest) : rest2 )
		
			_ -> error $ "while condition " ++ (render.pretty) cond0 ++ " at " ++ (showLocation.lineColNodeInfo) cond0 ++ " contains a function call!"
-}
	_ -> error $ "unfoldTracesM " ++ (render.pretty) stmt ++ " not implemented yet"

	where
	
	transids :: ([Trace] -> TraceElem) -> CExpr -> Trace -> ((CExpr,Trace) -> CovVecM Trace) -> CovVecM Trace
	transids compose expr trace cont = do
		additional_expr_traces :: [(CExpr,Trace)] <- translateExprM envs expr
		conts :: [Trace] <- forM additional_expr_traces $ \ (expr',trace') -> do
			cont (expr',trace'++trace)
		case conts of
			[] -> error $ "transids Strange: conts empty!"
			[e] -> return e
			_ -> return [ compose conts ]

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

translateExprM :: [Env] -> CExpr -> CovVecM [(CExpr,Trace)]
translateExprM envs expr = do
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
--		printLog $ "##### extract_traces_rets " ++ showTrace 0 (reverse funtrace) ++ "\n"
		let funtraces = for (flattenTrace [] (reverse funtrace)) $ \case
			Return retexpr : tr -> (tr,retexpr)
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

data Z3_Type = Z3_BitVector Int Bool | Z3_Float | Z3_Bool
	deriving (Show,Eq,Ord)

type Constraint = CExpr

expr2SExpr :: TyEnv -> Expr -> SExpr
expr2SExpr tyenv expr = expr2sexpr (infer_type expr) (insert_eq0 False expr)

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

	expr2sexpr :: Maybe Z3_Type -> CExpr -> SExpr
	expr2sexpr cur_ty expr = case expr of
		CUnary CPlusOp expr _ -> expr2sexpr cur_ty expr
		CUnary op expr _ -> SExpr [ SLeaf op_str , expr2sexpr cur_ty expr ] where
			op_str = case op of
				CMinOp  -> "bvneg"
				CCompOp -> "bvnot"
				CNegOp  -> "not"
				_       -> error $ "expr2sexpr " ++ (render.pretty) op ++ " should not occur!"
		CBinary CNeqOp expr1 expr2 _ -> expr2sexpr cur_ty $ CUnary CNegOp (CBinary CEqOp expr1 expr2 undefNode) undefNode
		CBinary op expr1 expr2 _ -> SExpr [ op_sexpr , expr2sexpr subtype1 expr1 , expr2sexpr subtype2 expr2 ] where
			(op_sexpr,subtype1,subtype2) = case op of
				CMulOp -> (SLeaf "bvmul",cur_ty,cur_ty)
				CDivOp -> (SLeaf "bvdiv",cur_ty,cur_ty)
				CRmdOp -> (SLeaf $ unSigned "bvurem" "bvsrem",cur_ty,cur_ty)
				CAddOp -> (SLeaf "bvadd",cur_ty,cur_ty)
				CSubOp -> (SLeaf "bvsub",cur_ty,cur_ty)
				CShlOp -> (SLeaf $ unSigned "bvlshl" "bvashl",cur_ty,cur_ty)
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
--				other -> error $ "unSigned " ++ (render.pretty) expr1 ++ " " ++ (render.pretty) expr2 ++ " yielded " ++ show other

		CVar ident _ -> SLeaf $ (render.pretty) ident
		CConst cconst -> SLeaf $ case cconst of
			CIntConst intconst _ -> let i = getCInteger intconst in
				case cur_ty of
					Just (Z3_BitVector size _) -> printf "#x%*.*x" (size `div` 4) (size `div` 4) i
					Nothing -> printf "#x%*.*x" (32 `div` 4 :: Int) (32 `div` 4 :: Int) i
					_ -> error $ "expr2SExpr: cur_ty " ++ show cur_ty
			_ -> (render.pretty) cconst
		cmember@(CMember _ _ _ _) -> error $ "expr2SExpr " ++ (render.pretty) cmember ++ " should not occur!"
		ccall@(CCall _ _ _) -> error $ "expr2SExpr " ++ (render.pretty) ccall ++ " should not occur!"
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
	DirectType tyname _ _ -> case tyname of
		TyIntegral intty -> case intty of
			TyBool   -> Z3_Bool
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
		TyFloating _ -> Z3_Float
		TyEnum _ -> Z3_BitVector intSize False
		TyComp _ -> Z3_BitVector 16 False
		_ -> error $ "ty2Z3Type " ++ (render.pretty) ty ++ " not implemented!"
	PtrType _ _ _ -> Z3_BitVector 16 False
	_ -> error $ "ty2Z3Type " ++ (render.pretty) ty ++ " should not occur!"

ty2SExpr :: Type -> SExpr
ty2SExpr ty = case ty2Z3Type ty of
	Z3_BitVector size _ -> SExpr [ SLeaf "_", SLeaf "BitVec", SLeaf (show size) ]
	Z3_Float            -> SLeaf "Real"
	Z3_Bool             -> SLeaf "Bool"

type Solution = [(String,SolutionVal)]

data SolutionVal = IntVal Int | BoolVal Bool | FloatVal Float
instance Eq SolutionVal where
	IntVal i1   == IntVal i2   = i1==i2
	BoolVal b1  == BoolVal b2  = b1==b2
	FloatVal f1 == FloatVal f2 = f2-f1 <= floatTolerance

instance Show SolutionVal where
	show (IntVal i) = show i
	show (BoolVal b) = show b
	show (FloatVal f) = show f

makeAndSolveZ3ModelM :: TyEnv -> [CExpr] -> [SExpr] -> [Ident] -> String -> CovVecM (String,Maybe Solution)
makeAndSolveZ3ModelM tyenv constraints additional_sexprs output_idents modelpathfile = do
	let
		constraints_vars = nub $ everything (++) (mkQ [] searchvar) constraints where
			searchvar :: CExpr -> [Ident]
			searchvar (CVar ident _) = [ ident ]
			searchvar _ = []

		varsZ3 = for (filter ((∈ (constraints_vars ++ output_idents)).fst) tyenv) $ \ (ident,ty) -> SExpr [ SLeaf "declare-const", SLeaf (identToString ident), ty2SExpr ty ]
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

	liftIO $ writeFile modelpathfile model_string
	printLog $ "Running model " ++ takeFileName modelpathfile ++ "..."
	(_,output,_) <- liftIO $ withCurrentDirectory (takeDirectory modelpathfile) $ do
		readProcessWithExitCode z3FilePath ["-smt2","parallel.enable=true",takeFileName modelpathfile] ""
	printLog output
	case lines output of
		"unsat"   : _ -> return (model_string,Nothing)
		"unknown" : _ -> return (model_string,Nothing)
		"sat" : rest -> do
			sol_params <- forM output_idents $ \ ident -> do
				let is = identToString ident
				case (unlines rest) =~ ("^\\(\\(" ++ is ++ " ([^\\)]+)\\)\\)$") :: (String,String,String,[String]) of
					(_,_,_,[val_string]) -> case lookup ident tyenv of
						Nothing -> error $ "Parsing z3 output: Could not find type of " ++ is
						Just ty -> return (is, case ty2Z3Type ty of
							Z3_BitVector size signed -> let
								'#':'x':hexdigits = val_string
								[(i :: Integer,"")] = readHex hexdigits
								in
								IntVal $ case signed of
									False -> fromIntegral i
									True  -> fromIntegral $ if i < 2^(size-1) then i else i - 2^size
							Z3_Float -> FloatVal (read val_string :: Float)
							Z3_Bool -> BoolVal $ fromJust $ lookup val_string [("true",True),("false",False)] )

					_ -> error $ "Parsing z3 output: Could not find " ++ is
			return (model_string,Just sol_params)
		_ -> error $ "Execution of " ++ z3FilePath ++ " failed:\n" ++ output


solveTraceM :: Type -> Env -> [Int] -> Trace -> CovVecM ResultData
solveTraceM ret_type param_env traceid trace = do
	let
		tracename = show traceid
		param_names = map (fst.snd) param_env
		mb_ret_val = case last trace of
			Return ret_expr -> Just ret_expr
			_               -> Nothing

		returnval_ident = internalIdent returnval_var_name
		trace' = trace ++ case mb_ret_val of
			Nothing -> []
			Just ret_expr -> [
				Condition undefined $ CBinary CEqOp (CVar returnval_ident (nodeInfo returnval_ident)) ret_expr (nodeInfo ret_expr),
				NewDeclaration (returnval_ident,ret_type) ]

		constraints = concatMap traceitem2constr trace' where
		traceitem2constr (Condition _ expr) = [expr]
		traceitem2constr _ = []
		
		tyenv = createTyEnv trace'

	ret_env <- createInterfaceM [(internalIdent returnval_var_name,ret_type)]

	(model_string,mb_sol) <- makeAndSolveZ3ModelM
		tyenv
		constraints
		(for param_names $ \ name -> SExpr [SLeaf "minimize",SLeaf (identToString name)])
		( maybe [] (const [returnval_ident]) mb_ret_val ++ param_names )
		(analyzerPath </> "model_" ++ tracename ++ ".smtlib2")

	return (model_string,case mb_sol of
		Nothing -> Nothing
		Just sol -> Just (param_env,ret_env,sol,mb_ret_val))


--type ResultData = (String,Maybe (Env,Solution,Maybe CExpr))

checkSolutionM :: [Int] -> ResultData -> CovVecM ResultData
checkSolutionM _ resultdata | not checkSolutions = return resultdata
checkSolutionM traceid resultdata@(_,Nothing) = do
	printLog $ "No solution to check for " ++ show traceid
	return resultdata
checkSolutionM traceid resultdata@(_,Just (_,_,[],_)) = do
	printLog $ "Empty solution cannot be checked for " ++ show traceid
	return resultdata
checkSolutionM traceid resultdata@(_,Just (env,ret_env,solution,Just res_expr)) = do
	printLogV 1 $ "checkSolution env = " ++ showEnv env
	srcfilename <- gets srcFilenameCVS
	Just filename <- gets checkExeNameCVS
	absolute_filename <- liftIO $ makeAbsolute srcfilename
	let
		args = concat $ for env $ \ (_,(newident,ty)) -> case ty of
			DirectType _ _ _ -> case lookup (identToString newident) solution of
				Nothing -> ["99"]
				Just v -> [show v]
			PtrType target_ty _ _ -> ["65000"]
			ty -> error $ "checkSolutionM args: type " ++ (render.pretty) ty ++ " not implemented!"
	printLogV 1 $ "checkSolution args = " ++ show args
	(exitcode,stdout,stderr) <- liftIO $ withCurrentDirectory (takeDirectory absolute_filename) $ do
		readProcessWithExitCode (takeFileName filename) args ""
	case exitcode of
		ExitFailure _ -> error $ "Execution of " ++ filename ++ " failed:\n" ++ stdout ++ stderr
		ExitSuccess -> do
			let
				outputs = words $ last $ lines stdout
				-- get all solution vars that are in the ret_env
				ret_solution = filter ((∈ map (identToString.fst) ret_env).fst) solution
			when (length ret_env /= length outputs || length outputs /= length ret_solution) $
				error $ "checkSolutionM: lengths of ret_env, solution, outputs differ:\n" ++
					"ret_env = " ++ showEnv ret_env ++ "\n" ++
					"ret_solution = " ++ show ret_solution ++ "\n" ++
					"outputs = " ++ show outputs ++ "\n"
			forM_ (zip3 ret_env outputs ret_solution) $ \ ((sourceident,(ident,ty)),s,(ident_s,predicted_result)) -> do
				when (identToString ident /= ident_s) $
					error $ "checkSolutionM: ident=" ++ identToString ident ++ " and ident_s=" ++ ident_s ++ " mismatch"
				case ty of
					PtrType _ _ _ -> return ()
					_ -> do
						let exec_result = case ty of
							DirectType (TyIntegral TyBool) _ _  -> BoolVal $ (read s :: Int) == 0
							DirectType (TyIntegral _) _ _       -> IntVal (read s)
							DirectType (TyFloating TyFloat) _ _ -> FloatVal (read s)
							DirectType (TyEnum _) _ _           -> IntVal (read s)
							_ -> error $ "checkSolutionM: parsing type " ++ (render.pretty) ty ++ " of " ++ ident_s ++ " not implemented!"
						when (exec_result /= predicted_result) $ do
							let txt = "ERROR in " ++ show traceid ++ " exec_val=" ++ show exec_result ++ " /= predicted_result=" ++ show predicted_result
							printLog txt
							error txt

			printLog $ "checkSolutionM " ++ show traceid ++ " OK."
			return resultdata
