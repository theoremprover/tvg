{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE
	PackageImports,RecordWildCards,FunctionalDependencies,MultiParamTypeClasses,
	QuasiQuotes,UnicodeSyntax,LambdaCase,ScopedTypeVariables,TupleSections,
	TypeSynonymInstances,FlexibleInstances,FlexibleContexts,StandaloneDeriving,
	DeriveDataTypeable,DeriveGeneric,PatternGuards,UndecidableInstances,Rank2Types #-}

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
import Language.C.Analysis.DefTable
import Language.C.Analysis.TypeUtils
import Language.C.Analysis.TypeConversions
import Language.C.Analysis.TravMonad
import Language.C.Analysis.SemRep
import Language.C.Analysis.Export
import Language.C.Syntax.Ops
import Language.C.System.GCC
import "language-c-quote" Language.C.Quote.GCC
import qualified Text.PrettyPrint.Mainland as PPM
import qualified Text.PrettyPrint.Mainland.Class as PPMC
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State -- .Strict
import qualified Data.Set as Set
import Data.Set.Unicode
import Prelude.Unicode ((∧),(∨))
import Text.Printf
import Text.Regex.TDFA
import Text.Regex.TDFA.String
import Numeric (readHex,readInt)
import Data.Either
import Control.Monad.IO.Class (liftIO,MonadIO)
import Data.Generics
--import qualified GHC.Generics as GHCG
import qualified Data.Map.Strict as Map
import Text.PrettyPrint
import Data.Time.LocalTime
import Data.Foldable
import Data.List
import Data.Maybe
import System.IO
import Data.Char

-- This is for conversion of Z3 floats to Haskell Floating Point
import Data.Word (Word32,Word64)
import Data.Array.ST (newArray,readArray,MArray,STUArray)
import Data.Array.Unsafe (castSTUArray)
import GHC.ST (runST,ST)

import DataTree
import GlobDecls
import Logging

type Trace = [TraceElem]
type ResultData = (String,Maybe (Env,Env,Solution))
type TraceAnalysisResult = ([Int],Trace,Set.Set Branch,ResultData)
type UnfoldTracesRet = Either [Trace] Bool
type SolveFunRet = (Bool,([TraceAnalysisResult],Set.Set Branch))


main :: IO ()
main = do
	-- when there is an error, we'd like to have *all* output till then
	hSetBuffering stdout NoBuffering

	gcc:filename:funname:opts <- getArgs >>= return . \case
--		[] -> "gcc" : (analyzerPath++"\\test.c") : "_Dtest" : [] --["-writeAST","-writeGlobalDecls"]
--		[] -> "gcc" : (analyzerPath++"\\uniontest.c") : "f" : [] --["-writeAST","-writeGlobalDecls"]
--		[] -> "gcc" : (analyzerPath++"\\OscarsChallenge\\sin\\xdtest.c") : "_Dtest" : ["-writeModels"] --["-writeAST","-writeGlobalDecls"]
		[] -> "gcc" : (analyzerPath++"\\OscarsChallenge\\sin\\oscar.c") : "_Sinx" : [] --"-writeAST","-writeGlobalDecls"]
--		[] -> "gcc" : (analyzerPath++"\\conditionaltest.c") : "f" : ["-writeModels"] --["-writeAST","-writeGlobalDecls"]
--		[] -> "gcc" : (analyzerPath++"\\floattest.c") : "f" : [] --,"-exportPaths" "-writeAST","-writeGlobalDecls"]
--		[] -> "gcc" : (analyzerPath++"\\decltest.c") : "f" : [] --,"-exportPaths" "-writeAST","-writeGlobalDecls"]
--		[] -> "gcc" : (analyzerPath++"\\myfp-bit_mul.c") : "_fpmul_parts" : [] --,"-exportPaths" "-writeAST","-writeGlobalDecls"]
--		[] -> "gcc" : (analyzerPath++"\\myfp-bit_mul.c") : "_fpdiv_parts" : [] --"-writeAST","-writeGlobalDecls"]
--		[] -> "gcc" : (analyzerPath++"\\arraytest.c") : "f" : ["-writeModels"] --"-writeAST","-writeGlobalDecls"]
--		[] -> "gcc" : (analyzerPath++"\\fortest.c") : "f" : [] --"-writeAST","-writeGlobalDecls"]
--		[] -> "gcc" : (analyzerPath++"\\iffuntest.c") : "f" : [] --["-writeAST","-writeGlobalDecls"]
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

	getZonedTime >>= return.(++"\n").show >>= writeFile logFileTxt

	let parseit = case takeExtension filename `elem` [".i"] of
		True  -> parseCFilePre filename
		False -> parseCFile (newGCC gcc) Nothing [] filename
	parseit >>= \case
		Left err -> error $ show err
		Right translunit -> do
			when ("-writeAST" `elem` opts) $
				writeFile (filename <.> "ast.html") $ genericToHTMLString translunit
			case runTrav_ $ do
					res <- analyseAST translunit
					deftable <- getDefTable
					return (res,deftable)
				of
				Left errs -> putStrLn "ERRORS:" >> forM_ errs print
				Right ((globdecls,deftable),soft_errors) -> do
					when (not $ null soft_errors) $ putStrLn "Soft errors:" >> forM_ soft_errors print
					when ("-writeGlobalDecls" `elem` opts) $
						writeFile (filename <.> "globdecls.html") $ globdeclsToHTMLString globdecls

					(every_branch_covered,s) <- runStateT covVectorsM $
						CovVecState globdecls 1 translunit filename Nothing funname undefined gcc opts
							Nothing ([],Set.empty) Set.empty intialStats Nothing Nothing deftable (-1)
					let
						(testvectors_rev,covered) = analysisStateCVS s
						testvectors = reverse testvectors_rev
						alls = allCondPointsCVS s

					printLog 0 "\n"

					let deaths = Set.toList $ alls ∖ covered

					printLog 0 $ "\n###### FINAL RESULT #######\n\n"

					printLog 0 $ show (statsCVS s) ++ "\n"

					forM_ testvectors $ \ (traceid,trace,branches,(model_string,mb_solution)) -> do
						case not showOnlySolutions || maybe False (not.null.(\(_,_,b)->b)) mb_solution of
							False -> return ()
							True -> printLog 0 $ unlines $ mbshowtraces (
								[ "","=== TRACE " ++ show traceid ++ " ========================","<leaving out builtins...>" ] ++
								[ showLine trace ] ++
								[ "",
								"--- MODEL " ++ show traceid ++ " -------------------------",
								model_string,
								"" ]) ++
								[ "--- SOLUTION " ++ show traceid ++ " ----------------------",
								show_solution funname mb_solution,
								""]
								where
								mbshowtraces ts = if showTraces then ts else []

					printLog 0 $ "All decision points : \n"
					let decisionpoints = Set.toList alls
					case null decisionpoints of
						True  -> printLog 0 "<none>\n"
						False -> forM_ decisionpoints $ \ decisionpoint ->
							printLog 0 $ "    " ++ show decisionpoint ++ "\n"

					printLog 0 $ "\n===== SUMMARY =====\n\n"

					forM_ testvectors $ \ (traceid,trace,branches,(model,Just v)) -> do
						printLog 0 $ "Test Vector " ++ show traceid ++ " covering " ++ ": \n"
						forM_ branches $ \ branch -> printLog 0 $ "    " ++ show branch ++ "\n"
						printLog 0 $ "\n    " ++ showTestVector funname v ++ "\n"
					forM_ deaths $ \ branch -> do
						printLog 0 $ "DEAD " ++ show branch ++ "\n\n"

					printLog 0 $ "Full path coverage: " ++ show every_branch_covered ++ "\n\n"
					when (every_branch_covered && not (null deaths)) $ myErrorIO "Every branch covered but deaths!"

					printLog 0 $ case null deaths of
						False -> "FAIL, there are coverage gaps!\n"
						True  -> "OK, we have full branch coverage.\n"
					
					createHTMLLog

for :: [a] -> (a -> b) -> [b]
for = flip map

concatForM = flip concatMapM

{-
once :: MonadPlus m => GenericM m -> GenericM m
once f x = f x `mplus` gmapMo (once f) x
-}

once :: MonadPlus m => GenericM m -> GenericM m
once f x = f x `mplus` gmapMo (once f) x

------------------------

fastMode = True

outputVerbosity = if fastMode then 1 else 2
logFileVerbosity = if fastMode then 0 else 10

roundingMode = "roundNearestTiesToEven"
intType = integral TyInt :: Type
charType = integral TyChar :: Type
ptrType to_ty = PtrType to_ty noTypeQuals noAttributes :: Type

showInitialTrace = False && not fastMode
solveIt = True
showModels = False && not fastMode
showOnlySolutions = True
showTraces = True && not fastMode
showFinalTrace = True && not fastMode
checkSolutions = solveIt && False
returnval_var_name = "return_val"
floatTolerance = 1e-7 :: Float
doubleTolerance = 1e-10 :: Double
showBuiltins = False
logToFile = True
logToHtml = True && not fastMode
mainFileName = "main.c"
printTypes = True

mAX_UNROLLS = 3
uNROLLING_STRATEGY = [0..mAX_UNROLLS]

cutOffs = False
sizeConditionChunks = 8

z3FilePath = "C:\\z3-4.8.8-x64-win\\bin\\z3.exe"

analyzerPath = "analyzer"
logFile = analyzerPath </> "log"
logFileTxt = logFile <.> "txt"
logFileHtml = logFile <.> "html"

------------------------

compileHereM :: [String] -> String -> String -> CovVecM (String,String)
compileHereM args filename src = do
	liftIO $ writeFile filename src
	gcc <- gets compilerCVS
	runHereM (takeDirectory filename) gcc args

runHereM :: String -> String -> [String] -> CovVecM (String,String)
runHereM rundir exefilename args = liftIO $ withCurrentDirectory rundir $ do
	(retcode,sout,serr) <- readProcessWithExitCode (takeFileName exefilename) args ""
	case retcode of
		ExitFailure exitcode -> myErrorIO $
			"ExitCode " ++ show exitcode ++ " of runHereM " ++ exefilename ++ "\n" ++ sout ++ serr
		ExitSuccess -> return (sout,serr)

data IntSizes = IntSizes { intSize::Int, longSize::Int, longLongSize::Int } deriving (Read,Show)

find_out_sizes_name = "find_out_sizes" :: String

find_out_sizesM :: CovVecM IntSizes
find_out_sizesM = do
	let
		srcfilename = find_out_sizes_name <.> "c"
		exefilename = find_out_sizes_name <.> "exe"
	compileHereM ["-o",exefilename,srcfilename] srcfilename (PPM.prettyCompact $ PPMC.ppr find_out_sizes_src)
	(sizes_s,"") <- runHereM (takeDirectory srcfilename) exefilename []
	let sizes = read sizes_s
	printLogV 1 $ show sizes
	return sizes
	where
	incl_stdio = "#include <stdio.h>"
	find_out_sizes_src = [cunit|
$esc:incl_stdio

int main(void)
{
    printf("IntSizes { intSize=%i, longSize=%i, longLongSize=%i }\n",sizeof(int)*8,sizeof(long int)*8,sizeof(long long)*8);
    return 0;
}
|]

-- Z3 does not accept identifiers starting with an underscore, so we prefix these with an "a"
safeZ3IdentifierPrefix = 'a'

----------------
-- This is just for convenience in the logWrapper combinator...

class (Show a) => LogRender a where
	ren :: a -> String
instance {-# OVERLAPPABLE #-} (Show a) => LogRender a where
	ren a = show a
instance LogRender String where
	ren s = s
instance (LogRender a) => LogRender (Maybe a) where
	ren Nothing = "Nothing"
	ren (Just a) = "Just " ++ ren a
instance {-# OVERLAPPABLE #-} (LogRender a,LogRender b) => LogRender (a,b) where
	ren (a,b) = "(" ++ ren a ++ "," ++ ren b ++ ")"
instance {-# OVERLAPPABLE #-} (LogRender a) => LogRender [a] where
	ren as = "[" ++ intercalate "," (map ren $ take 3 as) ++ "]"
instance (LogRender a,LogRender b) => LogRender (Either a b) where
	ren (Left a) = "Left " ++ ren a
	ren (Right b) = "Right " ++ ren b
instance LogRender EnvItem where
	ren a = (render.pretty) a
instance LogRender TyEnvItem where
	ren a = (render.pretty) a
instance LogRender CExpr where
	ren a = (render.pretty) a
instance LogRender CExprWithType where
	ren a = (render.pretty) a
instance LogRender Type where
	ren a = (render.pretty) a
instance LogRender CBlockItem where
	ren a = (render.pretty) a

-------------------

printLog :: Int -> String -> IO ()
printLog verbosity text = do
	when (verbosity<=outputVerbosity) $ putStrLn text
	when (logToFile && verbosity<=logFileVerbosity) $ appendFile logFileTxt text

printLogM :: Int -> String -> CovVecM ()
printLogM verbosity text = do
	indent <- gets logIndentCVS
	let ind_prefix = concat (replicate indent indentPrefix)
	let ind_text = unlines $ map (ind_prefix++) $ lines text
	liftIO $ printLog verbosity ind_text

printLogV :: Int -> String -> CovVecM ()
printLogV verbosity text = printLogM verbosity text

createHTMLLog :: IO ()
createHTMLLog = do
	log <- readFile logFileTxt
	writeHTMLLog logFileHtml log

myErrorIO :: forall a . String -> IO a
myErrorIO txt = do
	printLog 0 txt
	createHTMLLog
	error txt
	
myError :: forall a . String -> CovVecM a
myError txt = do
	printLogM 0 txt
	liftIO $ createHTMLLog
	error txt

indentLog :: Int -> CovVecM ()
indentLog d = modify $ \ s -> s { logIndentCVS = logIndentCVS s + d }

logWrapper :: (LogRender a) => Int -> [String] -> CovVecM a -> CovVecM a
logWrapper verbosity _ m | outputVerbosity < verbosity = m
logWrapper verbosity args m = do
	indentLog 1
	printLogV verbosity $ intercalate " " args
	ret <- m
	printLogV verbosity $ "RESULT = " ++ ren ret
	indentLog (-1)
	return ret

showLine :: Trace -> String
showLine trace = unlines $ map show (filter isnotbuiltin trace)

show_solution _ Nothing = "No solution"
show_solution _ (Just (_,_,[])) = "Empty solution"
show_solution funname (Just v@(_,_,solution)) = unlines $ map show solution ++ [ showTestVector funname v ]

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
			Nothing  -> "DONT_CARE"
			Just val -> show val

data CovVecState = CovVecState {
	globDeclsCVS     :: GlobalDecls,
	newNameIndexCVS  :: Int,
	translUnitCVS    :: CTranslUnit,
	srcFilenameCVS   :: String,
	checkExeNameCVS  :: Maybe String,
	funNameCVS       :: String,
	funStartEndCVS   :: (Location,Location),
	compilerCVS      :: String,
	optsCVS          :: [String],
	paramEnvCVS      :: Maybe [(EnvItem,CExprWithType)],
	analysisStateCVS :: ([TraceAnalysisResult],Set.Set Branch),
	allCondPointsCVS :: Set.Set Branch,
	statsCVS         :: Stats,
	retEnvCVS        :: Maybe [(EnvItem,CExprWithType)],
	sizesCVS         :: Maybe IntSizes,
	defTableCVS      :: DefTable,
	logIndentCVS     :: Int
	}

data Stats = Stats {
	cutoffTriesS :: Int, cutoffsS :: Int, numTracesS :: Int,
	numSolutionS :: Int, numNoSolutionS :: Int }
	deriving (Show)
intialStats = Stats 0 0 0 0 0
incCutoffTriesM :: CovVecM ()
incCutoffTriesM = modify $ \ s -> s { statsCVS = (statsCVS s) { cutoffTriesS = cutoffTriesS (statsCVS s) + 1 } }
incCutoffsM :: CovVecM ()
incCutoffsM = modify $ \ s -> s { statsCVS = (statsCVS s) { cutoffsS = cutoffsS (statsCVS s) + 1 } }
incNumNoSolutionM = modify $ \ s -> s { statsCVS = (statsCVS s) { numNoSolutionS = numNoSolutionS (statsCVS s) + 1 } }
incNumSolutionM = modify $ \ s -> s { statsCVS = (statsCVS s) { numSolutionS = numSolutionS (statsCVS s) + 1 } }
incNumTracesM :: CovVecM ()
incNumTracesM = modify $ \ s -> s { statsCVS = (statsCVS s) { numTracesS = numTracesS (statsCVS s) + 1 } }
printStatsM :: CovVecM ()
printStatsM = gets statsCVS >>= (printLogV 2) . show

type CovVecM = StateT CovVecState IO

data TraceElem =
	Assignment CExprWithType CExprWithType |
	Condition (Maybe Bool) CExprWithType |
	NewDeclaration (Ident,Type) |
	Return CExprWithType |
	DebugOutput String CExprWithType
	deriving Data

data Branch = Then Location | Else Location
	deriving Eq
-- Order decision points first by location, and second by the constructor (Then/Else)
instance Ord Branch where
	Then b1 <= Then b2 = b1 <= b2
	Then b1 <= Else b2 = if b1==b2 then True else b1 < b2
	Else b1 <= Then b2 = if b1==b2 then False else b1 < b2
	Else b1 <= Else b2 = b1 <= b2
instance Show Branch where
	show (Then loc) = "Then branch at " ++ showLocation loc
	show (Else loc) = "Else branch at " ++ showLocation loc

branchLocation :: Branch -> Location
branchLocation (Then loc) = loc
branchLocation (Else loc) = loc

instance CNode TraceElem where
	nodeInfo (Assignment lexpr _)       = extractNodeInfo lexpr
	nodeInfo (Condition _ expr)         = extractNodeInfo expr
	nodeInfo (NewDeclaration (ident,_)) = nodeInfo ident
	nodeInfo (Return expr)              = extractNodeInfo expr
	nodeInfo (DebugOutput _ _)          = undefNode


instance Pretty CExprWithType where
	pretty (CBinary op expr1 expr2 (_,ty)) = prettyCE (pretty op) [pretty expr1,pretty expr2] ty
	pretty (CCast decl expr (_,ty)) = prettyCE (lparen <> (text $ show ty) <> rparen) [pretty expr] ty
	pretty (CUnary op expr (_,ty)) = prettyCE (pretty op) [pretty expr] ty
	pretty (CMember expr ident deref (_,ty)) = prettyCE (text "")
		[pretty expr,text (if deref then "->" else ".") <+> pretty ident] ty
	pretty (CVar ident (_,ty)) = pretty ident <+> prettyType ty
	pretty (CConst constant) = ( text $ case constant of
		CIntConst cint _ -> show $ getCInteger cint
		CCharConst cchar _ -> show $ getCChar cchar
		CFloatConst cfloat _ -> show cfloat
		CStrConst cstr _ -> show $ getCString cstr ) <+>
			prettyType ((snd.annotation) constant)
	pretty (CIndex arrexpr indexexpr (_,ty)) = pretty arrexpr <> brackets (pretty indexexpr) <+> prettyType ty
	pretty (CAssign op lexpr assexpr (_,ty)) = prettyCE (pretty lexpr <+> pretty op) [pretty assexpr] ty
	pretty (CCall fun args (_,ty)) = prettyCE (text "Call " <+> pretty fun) (map pretty args) ty
	pretty (CCond cond (Just true_expr) false_expr (_,ty)) = prettyCE (pretty cond <+> text "?")
		[ pretty true_expr, text ":", pretty false_expr ] ty
	pretty other = error $ "instance Pretty CExprWithType not implemented for " ++ show other

prettyType ty = case printTypes of
	True -> text "::" <+> text (show ty)
	False -> Text.PrettyPrint.empty

prettyCE head subs _ | not printTypes = parens $ case subs of
	[sub] -> head <+> sub
	[sub1,sub2] -> sub1 <+> head <+> sub2
	_ -> head <+> hsep subs

prettyCE head subs ty =
	lparen <+> head $+$
	(nest 4 $ vcat subs) $+$
	rparen <+> prettyType ty

instance (Pretty a) => Pretty [a] where
	pretty xs = brackets $ hcat $ punctuate comma (map pretty xs)

instance Show TraceElem where
	show te = ( case te of
		Assignment lvalue expr   -> "ASSN " ++ (render.pretty) lvalue ++ " = " ++ (render.pretty) expr
		Condition mb_b expr      -> "COND " ++ maybe "" (\b->if b then "(THEN) " else "(ELSE) ") mb_b ++ (render.pretty) expr
		NewDeclaration (lval,ty) -> "DECL " ++ (render.pretty) lval ++ " :: " ++ (render.pretty) ty
		Return exprs             -> "RET  " ++ (render.pretty) exprs
		DebugOutput varname expr -> "DBGOUT " ++ varname ++ " " ++ (render.pretty) expr
		) ++ "  (" ++ (showLocation.lineColNodeInfo) te ++ ")"

showTrace :: Trace -> String
showTrace trace = unlines $ concatMap show_te trace where
	show_te te | showBuiltins || isnotbuiltin te = [show te]
	show_te _ = []

covVectorsM :: CovVecM Bool
covVectorsM = logWrapper 2 [ren "covVectorsM"] $ do
	sizes <- find_out_sizesM
	modify $ \ s -> s { sizesCVS = Just sizes }

	funname <- gets funNameCVS
	globdecls <- gets ((Map.elems).gObjs.globDeclsCVS)
	glob_env <- concatMapM declaration2EnvItemM globdecls
	let
		def2stmt :: IdentDecl -> CovVecM [CBlockItem]
		def2stmt (EnumeratorDef (Enumerator ident const_expr _ ni)) = do
			return [ CBlockStmt $ CVar ident (nodeInfo ident) ≔ const_expr ]
		def2stmt (ObjectDef (ObjDef (VarDecl (VarName ident _) _ ty) (Just initializer) ni)) = do
			ty' <- elimTypeDefsM ty
			cinitializer2blockitems (CVar ident ni) ty' initializer
		def2stmt _ = return []
	-- creates the assignment statements from the global context
	defs <- concatMapM def2stmt globdecls

	fundef@(FunDef (VarDecl _ _ (FunctionType (FunType ret_type funparamdecls False) _)) body fundef_ni) <-
		lookupFunM (builtinIdent funname)
	ret_type' <- elimTypeDefsM ret_type

	let srcident = internalIdent returnval_var_name
	z3_ret_type' <- ty2Z3Type ret_type'
	ret_env_exprs <- createInterfaceFromExprM (CVar srcident (nodeInfo srcident,z3_ret_type')) ret_type'
	modify $ \ s -> s { retEnvCVS = Just ret_env_exprs }
	
	-- Go through the body of the function and determine all decision points
	let condition_points = Set.fromList (
		everything (++) (mkQ [] searchcondpoint) body ++
		everything (++) (mkQ [] searchexprcondpoint) body ) 
		where
		searchcondpoint :: CStat -> [Branch]
		searchcondpoint (CWhile cond _ _ _)        = [ Then (lineColNodeInfo cond), Else (lineColNodeInfo cond) ]
		searchcondpoint ccase@(CCase _ _ _)        = [ Then (lineColNodeInfo ccase) ]
		searchcondpoint cdefault@(CDefault stmt _) = [ Then (lineColNodeInfo cdefault) ]
		searchcondpoint (CFor _ (Just cond) _ _ _) = [ Then (lineColNodeInfo cond), Else (lineColNodeInfo cond) ]
		searchcondpoint (CFor _ Nothing _ _ _)     = error $ "searchcondpoint: for(_,,_) not implemented!"
		searchcondpoint (CIf cond _ _ _)           = [ Then (lineColNodeInfo cond), Else (lineColNodeInfo cond) ]
		searchcondpoint _                          = []
		
		searchexprcondpoint :: CExpr -> [Branch]
		searchexprcondpoint (CCond cond _ _ _)     = [ Then (lineColNodeInfo cond), Else (lineColNodeInfo cond) ]
		searchexprcondpoint _                      = []

	modify $ \ s -> s { allCondPointsCVS = condition_points }
	
	let
		srcfilename cnode | isNoPos (posOf cnode) = Nothing
		srcfilename cnode | isBuiltinPos (posOf cnode) = Nothing
		srcfilename cnode | isInternalPos (posOf cnode) = Nothing
		srcfilename cnode = Just $ posFile $ posOf cnode
		is_in_src_file identdecl = srcfilename identdecl == srcfilename fundef
		fun_lc = lineColNodeInfo fundef_ni
		next_lc = case sort $ filter (> lineColNodeInfo fundef_ni) $ map lineColNodeInfo $ filter is_in_src_file globdecls of
			[] -> (maxBound,maxBound,-1)
			next : _ -> next
	modify $ \ s -> s { funStartEndCVS = (fun_lc,next_lc) }

	let formal_params = for (map getVarDecl funparamdecls) $ \ (VarDecl (VarName srcident _) _ ty) -> (srcident,ty)
	ext_decls <- createDeclsM formal_params
	(param_env_exprs,(arraydecl_env,arrayitem_conds)) <- createInterfaceM formal_params
	modify $ \ s -> s { paramEnvCVS = Just param_env_exprs }
	let param_env = map fst param_env_exprs
	printLogV 8 $ "param_env = " ++ showEnv param_env

	let
		decls = arrayitem_conds ++ map (NewDeclaration . snd) (arraydecl_env ++ reverse param_env ++ glob_env)

	when checkSolutions $ do
		filename <- gets srcFilenameCVS
		let
			chkexefilename = replaceExtension mainFileName "exe"
			mainfilename = replaceFileName filename mainFileName
		srcabsfilename <- liftIO $ makeAbsolute $ mainfilename

		charness <- createCHarness ret_type formal_params (takeFileName filename) funname ext_decls
		compileHereM ["-Wno-builtin-declaration-mismatch","-Wno-int-conversion","-Wno-incompatible-pointer-types",
			"-o",chkexefilename,mainFileName] srcabsfilename charness
		modify $ \ s -> s { checkExeNameCVS = Just chkexefilename }

	Right every_branch_covered <- unfoldTracesM ret_type' True 0 ((arraydecl_env++param_env):[glob_env]) decls [ (defs ++ [ CBlockStmt body ],False) ]
	return every_branch_covered

harnessAST incl argdecls funcall print_retval1 print_retval2 = [cunit|
$esc:incl_stdio
$esc:incl_stdlib

int solver_pragma(int x,...) { return 1; }
void solver_debug(void* x) { }

$esc:incl


int main(int argc, char* argv[])
{
    int i=1;

	$escstm:argdecls

	$escstm:print_retval1
	$escstm:funcall
	$escstm:print_retval2
	
	return 0;
}
|]
	where
	incl_stdio = "#include <stdio.h>"
	incl_stdlib = "#include <stdlib.h>"

envItemNotPtrType :: EnvItem -> Bool
envItemNotPtrType (_,(_,PtrType _ _ _)) = False
envItemNotPtrType _ = True

createCHarness :: Type -> [(Ident,Type)] -> String -> String -> [String] -> CovVecM String
createCHarness orig_rettype formal_params filename funname extdecls = do
	Just retenvexprs0 <- gets retEnvCVS
	Just param_env_exprs0 <- gets paramEnvCVS
	let
		retenvexprs = filter (envItemNotPtrType.fst) retenvexprs0
		param_env_exprs = filter (envItemNotPtrType.fst) param_env_exprs0
		argexprs = map (\ (_,cexpr) -> (render.pretty) (fmap fst cexpr)) param_env_exprs
		incl_srcfilename = "#include \"" ++ filename ++ "\""

		funcall = (render.pretty) orig_rettype ++ " " ++ returnval_var_name ++ " = " ++
			funname ++ "(" ++ intercalate "," (map ((render.pretty).fst) formal_params) ++ ");"

	argformats <- mapM type_format_string $ map (snd.snd.fst) param_env_exprs	
	let
		argvals = intercalate ", " $ for (zip argexprs argformats) $ \ (argname,argformat) -> argname ++ " = " ++ argformat

	ret_formatss <- mapM type_format_string $ map (snd.snd.fst) retenvexprs
	let
		ret_vals = for retenvexprs $ \ (_,cexpr) -> (render.pretty) (fmap fst cexpr)

		print_retval1 = "printf(\"" ++ funname ++ "(" ++ argvals ++ ") = \\n\"," ++
			(if null argexprs then "" else (intercalate ", " argexprs)) ++ ");"
		print_retval2 = "printf(\"" ++
			intercalate " " ret_formatss ++ "\\n\", " ++
			intercalate ", " ret_vals ++ ");"
	return $ PPM.prettyCompact $ PPMC.ppr $ harnessAST incl_srcfilename (unlines extdecls) funcall print_retval1 print_retval2

-- Create declarations for the function under test in the C test harness
createDeclsM :: [(Ident,Type)] -> CovVecM [String]
createDeclsM formal_params = do
	concatForM formal_params $ \ (ident,ty) -> create_decls (CVar ident undefNode) ty ty False []

	where

	-- ty is the non-dereferenced type used for pretty printing,
	-- deref_ty is the dereferenced type that is to be analyzed further
	create_decls expr ty deref_ty all_declared decls = case deref_ty of

		TypeDefType (TypeDefRef ident _ _) _ _ -> do
			ty' <- lookupTypeDefM ident
			create_decls expr ty ty' all_declared decls

		-- case: deref_ty = <ty>* ptr
		-- int *      :: a  ->  { int PTR_a; int * a = & PTR_a; }
		-- struct S * :: s  ->  { struct S PTR_s; struct S * s = & PTR_s; }
		PtrType target_ty _ _ -> do
			let
				subexpr = CUnary CIndOp expr undefNode
				subexpr_varname = lValueToVarName subexpr
			create_decls subexpr target_ty target_ty True $ decls ++ [
				-- if we encounter a pointer type, we have to declare it anyway.
				(render.pretty) target_ty ++ " " ++ subexpr_varname ++ ";",
				(render.pretty) ty ++ " " ++ lval_varname ++ " = &" ++ subexpr_varname ++ ";" ]

		-- case: deref_ty = STRUCT expr
		DirectType (TyComp (CompTypeRef sueref _ _)) _ _ -> do
			member_ty_s <- getMembersM sueref
			res_decls <- concatForM member_ty_s $ \ (m_ident,m_ty) -> do
				create_decls (CMember expr m_ident False undefNode) m_ty m_ty True []
			let decl = case all_declared of
				True  -> []
				False -> [ (render.pretty) ty ++ " " ++ lval_varname ++ ";" ]
			return $ decls ++ decl ++ res_decls

		-- case: deref_ty = <direct-type> where <direct-type> is no struct/union.
		DirectType _ _ _ -> do
			let decl = case all_declared of
				False -> [ (render.pretty) ty ++ " " ++ (render.pretty) expr ++ ";" ]
				True -> []
			tyfs <- type_format_string ty
			return $ decls ++ decl ++
				[ "sscanf(argv[i++],\"" ++ tyfs ++ "\",&(" ++ (render.pretty) expr ++ "));" ]

		ArrayType elem_ty (ArraySize False (CConst (CIntConst cint _))) _ _ -> do
			let
				(CVar (Ident _ _ _) _) = expr
				arr_size = getCInteger cint
				arr_decl = (render.pretty) elem_ty ++ " " ++ (render.pretty) expr ++ "[" ++ show arr_size ++ "];"
			arr_decls <- concatForM [0..(arr_size - 1)] $ \ i -> do
				ii <- ⅈ i
				create_decls (CIndex expr ii undefNode) elem_ty elem_ty True []
			return $ decls ++ [arr_decl] ++ arr_decls

		_ -> myError $ "createDeclsM " ++ (render.pretty) expr ++ " " ++ show ty ++ " not implemented"

		where
		
		lval_varname = lValueToVarName expr

type_format_string :: Type -> CovVecM String
type_format_string ty = do
	Just IntSizes{..} <- gets sizesCVS
	(z3ty,_) <- ty2Z3Type ty
	return $ "%" ++ case z3ty of
		Z3_Float                                        -> "f"
		Z3_Double                                       -> "lf"
		Z3_LDouble                                      -> "Lf"
		Z3_BitVector size unsigned | size==intSize      -> if unsigned then "u" else "i"
		Z3_BitVector size unsigned | size==longSize     -> "l" ++ if unsigned then "u" else "i"
		Z3_BitVector size unsigned | size==longLongSize -> "ll" ++ if unsigned then "u" else "i"
		Z3_BitVector size unsigned                      -> if unsigned then "u" else "i"
		Z3_Ptr _                                        -> "p"
		_ -> error $ "type_format_string " ++ (render.pretty) ty ++ " not implemented"

type Location = (Int,Int,Int)
locationToName :: Location -> String
locationToName (l,c,len) = show l ++ "_" ++ show c ++ "_" ++ show len

showLocation :: Location -> String
showLocation (l,c,len) = "line " ++ show l ++ ", col " ++ show c ++ ", len " ++ show len

-- In case of a cutoff, mb_ret_type is Nothing.
analyzeTraceM :: Maybe Type -> [TraceElem] -> CovVecM Bool
analyzeTraceM mb_ret_type res_line = logWrapper 2 [ren "analyzeTraceM",ren mb_ret_type,ren res_line,ren "traceid=",ren traceid] $ do
	incNumTracesM
	printStatsM	
	printLogV 1 $ "===== ANALYZING TRACE " ++ show traceid ++ " ================================="

	opts <- gets optsCVS
	when ("-exportPaths" `elem` opts) $ liftIO $ do
		writeFile (analyzerPath </> "models" </> "path_" ++ show traceid <.> ".c") $ unlines $ concat $ for trace $ \case
			Assignment lexpr assexpr -> [ (render.pretty) lexpr ++ " = " ++ (render.pretty) assexpr ++ " ;" ]
			NewDeclaration (ident,ty) -> [ "(" ++ (render.pretty) ty ++ ") " ++ (render.pretty) ident ++ " ;" ]
			Return expr -> [ "return " ++ (render.pretty) expr ++ " ;" ]
			_ -> []
	
	trace' <-		
		showtraceM showInitialTrace "Initial" return trace >>=
		showtraceM showTraces "elimInds"             elimInds >>=
		showtraceM showTraces "1. simplifyTraceM"    simplifyTraceM >>=
		showtraceM showTraces "1. elimAssignmentsM"  elimAssignmentsM >>=
		showtraceM showTraces "elimArrayAssignsM"    elimArrayAssignsM >>=
		showtraceM showTraces "2. elimAssignmentsM"  elimAssignmentsM >>=
		showtraceM showTraces "2. simplifyTraceM"    simplifyTraceM >>=
		showtraceM showTraces "addUnionConstraintsM" addUnionConstraintsM >>=
		showtraceM showTraces "createSymbolicVarsM"  createSymbolicVarsM

	either_resultdata <- solveTraceM mb_ret_type traceid trace'
	case either_resultdata of
		Left solvable -> return solvable
		Right resultdata@(model_string,mb_solution) -> do
--			when showTraces $ printLog $ "\n--- Result of " ++ show traceid ++ " : \n"
			funname <- gets funNameCVS
			printLogV 1 $ "--- Result of TRACE " ++ show traceid ++ " ----------------------\n" ++
				show_solution funname mb_solution ++ "\n"
		
			startend <- gets funStartEndCVS
			printLogV 2 $ "startend =" ++ show startend
			let visible_trace = Set.fromList $ concatMap to_branch res_line
				where
				is_visible_traceelem :: (Location,Location) -> CExprWithType -> Bool
				is_visible_traceelem (start,end) expr = start <= lc && lc < end where
					lc = lineColNodeInfo $ extractNodeInfo expr
				to_branch (Condition (Just b) cond) | is_visible_traceelem startend cond =
					[ (if b then Then else Else) (lineColNodeInfo $ extractNodeInfo cond) ]
				to_branch _ = []
			printLogV 2 $ "visible_trace =\n" ++ unlines (map show $ Set.toList visible_trace)
		
			let
				traceanalysisresult :: TraceAnalysisResult = (traceid,res_line,visible_trace,resultdata)
				solved = is_solution traceanalysisresult
			case solved of
				False -> do
					incNumNoSolutionM
					return ()
				True  -> do
					incNumSolutionM
					when (checkSolutions && isJust mb_ret_type) $ checkSolutionM traceid resultdata >> return ()
					modify $ \ s -> s { analysisStateCVS = let (tas,covered) = analysisStateCVS s in
						-- Are all the decision points are already covered?
						-- If yes, this trace does not contribute to full coverage...
						case visible_trace ⊆ covered of
							False -> (traceanalysisresult:tas,visible_trace ∪ covered)
							True  -> (tas,covered) }
			printLogV 10 $ "*** " ++ show traceid ++ " is " ++ show solved
			return solved

	where
	
	trace = reverse res_line
	traceid = trace2traceid trace

	showtraceM cond stage combinator trace = do
		trace' <- combinator trace
		when cond $ do
			printLogV 5 $ "\n--- TRACE after " ++ stage ++ " " ++ show traceid ++ " -----------\n" ++
				if showBuiltins then "" else "<leaving out builtins...>\n"
			printLogV 5 $ showLine trace'
		return trace'

trace2traceid trace = concatMap extract_conds trace where
	extract_conds (Condition (Just b) _) = [ if b then 1 else 2 ]
	extract_conds _ = []

is_solution :: TraceAnalysisResult -> Bool
is_solution (_,_,_,(_,Just (_,_,solution))) = not $ null solution
is_solution _ = False

lineColNodeInfo :: (CNode a) => a -> Location
lineColNodeInfo cnode = if isSourcePos pos_te then (posRow pos_te,posColumn pos_te,fromJust $ lengthOfNode ni) else (-1,-1,-1)
	where
	ni = nodeInfo cnode
	pos_te = posOfNode ni

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

-- Cannot use "deriving Eq" because we have to ignore the NodeInfo for the desired equality...
instance Eq (CConstant a) where
	(CIntConst   c1 _) == (CIntConst   c2 _) = c1==c2
	(CCharConst  c1 _) == (CCharConst  c2 _) = c1==c2
	(CFloatConst c1 _) == (CFloatConst c2 _) = c1==c2
	(CStrConst   c1 _) == (CStrConst   c2 _) = c1==c2

lValueToVarName :: (Show a) => CExpression a -> String
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
lValueToVarName (CIndex arr index _) = lValueToVarName arr ++ "_INDEX_" ++ lValueToVarName index
lValueToVarName lval = error $ "lValueToVarName " ++ show lval ++ " not implemented!"

type TyEnvItem = (Ident,Type)
instance Pretty TyEnvItem where
	pretty (idnew,ty) = pretty idnew <+> text " :: " <+> pretty ty
type EnvItem = (Ident,TyEnvItem)
instance Pretty EnvItem where
	pretty (idold,tyenvitem) = pretty idold <+> text " |-> " <+> pretty tyenvitem
type Env = [EnvItem]

dumpEnvs :: [Env] -> String
dumpEnvs envs = unlines $ for envs $ \ env -> "[ " ++
	(concat $ intersperse ", " $ for env $ \ (a,(at,ty)) -> (render.pretty) a ++ "->" ++ (render.pretty) at) ++ " ]"

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

decl2TypeM :: CDecl -> CovVecM Type
decl2TypeM decl = do
	deftable <- gets defTableCVS
	case runTrav_ (withDefTable (\_->((),deftable)) >> myAnalyseTypeDecl decl) of
		Right (ty,[]) -> return ty
		Right (ty,_) -> return ty
		Left errs -> myError $ show errs
	where
	-- taken from Language/C/Analysis/DeclAnalysis.hs, added proper handling of initializers
	myAnalyseTypeDecl :: (MonadTrav m) => CDecl -> m Type
	myAnalyseTypeDecl (CDecl declspecs declrs node) = case declrs of
		[] -> analyseTyDeclr (CDeclr Nothing [] Nothing [] node)
		[(Just declr,_,Nothing)] -> analyseTyDeclr declr
		where
		analyseTyDeclr (CDeclr _ derived_declrs Nothing attrs _declrnode) = do
			canonTySpecs <- canonicalTypeSpec typespecs
			t <- tType True node (map CAttrQual (attrs++attrs_decl) ++ typequals) canonTySpecs derived_declrs []
			case nameOfNode node of
				Just n -> withDefTable (\dt -> (t, insertType dt n t))
				Nothing -> return t
			where
			(storagespec, attrs_decl, typequals, typespecs, funspecs, alignspecs) = partitionDeclSpecs declspecs
		analyseTyDeclr other = error $ "analyseTyDeclr " ++ show other

type2Decl :: Ident -> NodeInfo -> Type -> CDecl
type2Decl ident ni ty = CDecl (map CTypeSpec typespecs)
	[(Just $ CDeclr (Just ident) derivdeclrs Nothing [] ni,Nothing,Nothing)] ni
	where
	(typespecs,derivdeclrs) = ty2specs ty
	ty2specs ty = case ty of
		DirectType tyname _ _ -> (case tyname of
			TyVoid -> [CVoidType ni]
			TyIntegral inttype -> case inttype of
				TyBool -> [CBoolType ni]
				TyChar -> [CCharType ni]
				TySChar -> [CSignedType ni,CCharType ni]
				TyUChar -> [CUnsigType ni,CCharType ni]
				TyShort -> [CShortType ni]
				TyUShort -> [CUnsigType ni,CShortType ni]
				TyInt -> [CIntType ni]
				TyUInt -> [CUnsigType ni,CIntType ni]
				TyInt128 -> [CInt128Type ni]
				TyUInt128 -> [CUnsigType ni,CInt128Type ni]
				TyLong -> [CLongType ni]
				TyULong -> [CUnsigType ni,CLongType ni]
				TyLLong -> [CLongType ni,CLongType ni]
				TyULLong -> [CUnsigType ni,CLongType ni,CLongType ni]
			TyFloating floattype -> case floattype of
				TyFloat -> [CFloatType ni]
				TyDouble -> [CDoubleType ni]
				TyLDouble -> [CLongType ni,CDoubleType ni]
				TyFloatN i b -> [CFloatNType i b ni]
			TyComplex floattype -> case floattype of
				TyFloat -> [CComplexType ni]
				TyDouble -> [CDoubleType ni,CComplexType ni]
				TyLDouble -> [CLongType ni,CDoubleType ni,CComplexType ni]
				TyFloatN i b -> [CFloatNType i b ni,CComplexType ni]
			TyComp (CompTypeRef sueref comptykind _) ->
				[CSUType (CStruct (comptykind2structtag comptykind) (sueref2mbident sueref) Nothing [] ni) ni]
			TyEnum (EnumTypeRef sueref _) -> [CEnumType (CEnum (sueref2mbident sueref) Nothing [] ni) ni]
			,[])
			where
			comptykind2structtag StructTag = CStructTag
			comptykind2structtag UnionTag = CUnionTag
			sueref2mbident (NamedRef ident) = Just ident
			sueref2mbident (AnonymousRef _) = Nothing

		PtrType target_ty _ _ -> let (typespecs,derivdeclrs) = ty2specs target_ty in
			(typespecs,derivdeclrs++[CPtrDeclr [] ni])

		ArrayType elem_ty arrsize _ _ -> ( typespecs, derivdeclrs ++ [ CArrDeclr [] (arraysize2carraysize arrsize) ni ] )
			where
			(typespecs,derivdeclrs) = ty2specs elem_ty
			arraysize2carraysize (UnknownArraySize is_starred) = CNoArrSize is_starred
			arraysize2carraysize (ArraySize is_static sizeexpr) = CArrSize is_static sizeexpr

		TypeDefType (TypeDefRef ident refd_type ni) _ _ -> ([CTypeDef ident ni],[])


lookupTagM :: SUERef -> CovVecM TagDef
lookupTagM ident = do
	tags <- gets (gTags.globDeclsCVS)
	case Map.lookup ident tags of
		Just tagdef -> return tagdef
		Nothing -> myError $ "Tag " ++ (show ident) ++ " not found"

getMemberTypeM :: Type -> Ident -> CovVecM Type
getMemberTypeM ty member = do
	ty' <- elimTypeDefsM ty
	case ty' of
		DirectType (TyComp (CompTypeRef sueref _ _)) _ _ -> do
			mem_tys <- getMembersM sueref
			case lookup member mem_tys of
				Nothing -> myError $ "getMemberTypeM: Could not find member " ++ (render.pretty) member ++ " in " ++ (render.pretty) ty
				Just mem_ty -> elimTypeDefsM mem_ty
		other -> myError $ "getMemberTypeM " ++ (render.pretty) ty' ++ "\n    " ++ (render.pretty) other ++
			" not implemented!"

getMembersM :: SUERef -> CovVecM [(Ident,Type)]
getMembersM sueref = do
	CompDef (CompType _ _ memberdecls _ _) <- lookupTagM sueref
	forM memberdecls $ \ (MemberDecl (VarDecl (VarName ident _) _ ty) Nothing _) -> do
		return (ident,ty)

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
	return $ [ (srcident,(srcident,ty')) ]

mkIdentWithCNodePos :: (CNode cnode) => cnode -> String -> Ident
mkIdentWithCNodePos cnode name = mkIdent (posOfNode $ nodeInfo cnode) name (Name 9999)


-- Takes an identifier and a type, and creates env item(s) from that.

identTy2EnvItemM :: Ident -> Type -> CovVecM [EnvItem]
identTy2EnvItemM srcident@(Ident _ i ni) ty = do
	ty' <- elimTypeDefsM ty
	new_var_num <- gets newNameIndexCVS
	-- strictly monotone newNameIndexCVS assures uniqueness of new identifiers:
	modify $ \ s -> s { newNameIndexCVS = newNameIndexCVS s + 1 }
	let
		name_prefix = identToString srcident
		-- a dollar sign is not allowed in a C identifier, but Z3 allows for it.
		-- By inserting one in new names we avoid unlucky name collisions with already existing names in the source file
		newident = Ident (name_prefix ++ "$" ++ show new_var_num) i ni
	return $ [ (srcident,(newident,ty')) ]


-- From a list of identifiers and types (i.e. the signature of the function to be analyzed),
-- create a list of EnvItems (representing the declarations) and CExprs.
-- the returned string list is the list of declarations/definitions for the C test harness

createInterfaceM :: [(Ident,Type)] -> CovVecM ([(EnvItem,CExprWithType)],([EnvItem],[TraceElem]))
createInterfaceM ty_env = runStateT cifes_m ([],[])
	where
	cifes_m :: CIFE
	cifes_m = do
		res <- forM ty_env $ \ tyenvitem@(srcident,ty) -> do
			ty' <- lift $ elimTypeDefsM ty
			case ty' of
				ArrayType _ _ _ _ -> modify $ \ (envitems,traceitems) -> ((srcident,tyenvitem):envitems,traceitems)
				_ -> return ()
			z3ty' <- lift $ ty2Z3Type ty'
			createInterfaceFromExpr_WithEnvItemsM (CVar srcident (nodeInfo srcident,z3ty')) ty
		return $ concat res

createInterfaceFromExprM :: CExprWithType -> Type -> CovVecM [(EnvItem,CExprWithType)]
createInterfaceFromExprM expr ty = evalStateT (createInterfaceFromExpr_WithEnvItemsM expr ty) ([],[])

type CIFE = StateT ([EnvItem],[TraceElem]) CovVecM [(EnvItem,CExprWithType)]

createInterfaceFromExpr_WithEnvItemsM :: CExprWithType -> Type -> CIFE
createInterfaceFromExpr_WithEnvItemsM expr ty = do
	lift $ printLogV 20 $ "### createInterfaceFromExpr_WithEnvItemsM " ++ (render.pretty) expr ++ " " ++ (render.pretty) ty
	z3_ty <- lift $ ty2Z3Type ty
	lift $ printLogV 20 $ "###                                z3type = " ++ show z3_ty
	ty' <- lift $ elimTypeDefsM ty
	case ty' of

		-- STRUCT* p
		PtrType (DirectType (TyComp (CompTypeRef sueref _ _)) _ _) _ _ -> prepend_plainvar ty' $ do
			member_ty_s <- lift $ getMembersM sueref
			ress <- forM member_ty_s $ \ (m_ident,m_ty) -> do
				z3_m_ty <- lift $ ty2Z3Type m_ty
				createInterfaceFromExpr_WithEnvItemsM (CMember expr m_ident True (extractNodeInfo expr,z3_m_ty)) m_ty
			return $ concat ress
		-- ty* p
		PtrType target_ty _ _ -> do
			z3_target_ty <- lift $ ty2Z3Type target_ty
			prepend_plainvar ty' $
				createInterfaceFromExpr_WithEnvItemsM (CUnary CIndOp expr (extractNodeInfo expr,z3_target_ty)) target_ty

		-- STRUCT expr
		DirectType (TyComp (CompTypeRef sueref _ _)) _ _ -> do
			member_ty_s <- lift $ getMembersM sueref
			ress <- forM member_ty_s $ \ (m_ident,m_ty) -> do
				z3_m_ty <- lift $ ty2Z3Type m_ty
				createInterfaceFromExpr_WithEnvItemsM (CMember expr m_ident False (extractNodeInfo expr,z3_m_ty)) m_ty
			return $ concat ress

		-- direct-type expr where direct-type is no struct/union
		DirectType _ _ _ -> prepend_plainvar ty' $ return []

		ArrayType elem_ty (ArraySize False (CConst (CIntConst cint _))) _ _ -> do
			elem_ty' <- lift $ elimTypeDefsM elem_ty
			let
				arr_size = getCInteger cint
				CVar (Ident _ _ _) (ni,_) = expr    -- Just to be sure...
			ress <- forM [0..(getCInteger cint - 1)] $ \ i -> do
				elem_ty2 <- lift $ ty2Z3Type elem_ty'
				intty <- lift $ ty2Z3Type intType
				let
					arrayelemexpr = CIndex expr (CConst $ CIntConst (cInteger i) (undefNode,intty)) (ni,elem_ty2)
					arrayelem_var = CVar (internalIdent $ lValueToVarName arrayelemexpr) (ni,elem_ty2)
					eqcond = Condition Nothing $ CBinary CEqOp arrayelem_var arrayelemexpr (ni,_BoolTypes)
				modify $ \ (envitems,traceitems) -> ( envitems,traceitems ++ [eqcond] )
				createInterfaceFromExpr_WithEnvItemsM arrayelemexpr elem_ty'
			return $ concat ress

		_ -> lift $ myError $ "createInterfaceFromExprM " ++ (render.pretty) expr ++ " " ++
			(render.pretty) ty' ++ " not implemented"

		where
	
		srcident = internalIdent $ lValueToVarName expr

		prepend_plainvar :: Type -> CIFE -> CIFE
		prepend_plainvar ty' rest_m = do
			rest1 <- rest_m
			return $ (((srcident,(srcident,ty')),expr) : rest1)

unfoldTracesM ret_type toplevel forks envs trace cbss =
	logWrapper 2 [ren "unfoldTracesM",ren ret_type,ren toplevel,ren forks,ren envs,ren trace,'\n':ren cbss] $ do
		(if forks > 0 && forks `mod` sizeConditionChunks == 0 then maybe_cutoff else id) $
			unfoldTraces1M ret_type toplevel forks envs trace cbss
		where
		maybe_cutoff :: CovVecM UnfoldTracesRet -> CovVecM UnfoldTracesRet
		maybe_cutoff cont | cutOffs = do
			incCutoffTriesM
			printLogV 1 $ "******* Probing for CutOff in depth " ++ show (length trace) ++ " ..."
			analyzeTraceM Nothing trace >>= \case
				False -> do
					printLogV 1 $ "******** Cutting off !"
					incCutoffsM
					return $ Right False
				True  -> do
					printLogV 1 $ "******** Continuing..."
					cont
		maybe_cutoff cont = cont

unfoldTraces1M :: Type -> Bool -> Int -> [Env] -> Trace -> [([CBlockItem],Bool)] -> CovVecM UnfoldTracesRet
unfoldTraces1M ret_type toplevel forks envs trace bstss@((cblockitems@(CBlockStmt stmt0 : rest),breakable) : rest2) =
	logWrapper 2 [ren "unfoldTraces1M",ren ret_type,ren toplevel,ren forks,ren envs,ren trace,'\n':ren cblockitems] $
	case stmt0 of

		CCompound _ cbis _ -> unfoldTracesM ret_type toplevel forks ([]:envs) trace ((cbis,False) : (rest,breakable) : rest2)
	
		_ -> do
			{-
				if the stmt is not a compound,
				search for all CConds in the expression,
				replacing ...( a ? b : c )... by
				<T> condexpr$10_12;
				if(a) condexpr$10_12 = b; else condexpr$10_12 = c;
				...condexpr$10_12...
			-}
			let
				to_condexpr :: CExpr -> StateT [CBlockItem] CovVecM CExpr
				to_condexpr ccond@(CCond cond (Just true_expr) false_expr ni) = do
					let
						var_ident = internalIdent $ "condexpr$" ++ locationToName (lineColNodeInfo ccond)
						var = CVar var_ident ni
					ccond' <- lift $ annotateTypesAndCastM envs ccond Nothing
					let
						(_,ty) = extractTypes ccond'
						-- Isn't there a QuasiQuoter for language-c?
						cbis = [
							CBlockDecl $ type2Decl var_ident ni ty,
							CBlockStmt $ CIf cond (var ≔ true_expr) (Just $ var ≔ false_expr) ni
							]
					modify ( cbis ++ )
					lift $ printLogV 20 $ "### Found CCond: " ++ ren ccond
					-- Replace the condexpr by the new variable "var"
					return var
				to_condexpr expr = mzero
			-- Only replace the topmost CCond (in order to handle recursive CConds properly)
			(stmt,add_cbis) <- runStateT ((once (mkMp to_condexpr) stmt0) `mplus` (return stmt0)) []
			printLogV 20 $ "#### stmt    = " ++ (render.pretty) stmt
			printLogV 20 $ "#### add_cbis= " ++ ren add_cbis

			case stmt of
				_ | not (null add_cbis) -> unfoldTracesM ret_type toplevel forks envs trace (((add_cbis ++ [CBlockStmt stmt] ++ rest),breakable) : rest2)

				CLabel _ cstat _ _ -> unfoldTracesM ret_type toplevel forks envs trace (((CBlockStmt cstat : rest, breakable)) : rest2)

				CSwitch condexpr (CCompound [] cbis _) switch_ni -> do
					let
						cond_ni = nodeInfo condexpr
						cond_var_ident = mkIdentWithCNodePos condexpr $ "cond_" ++ (locationToName $ lineColNodeInfo condexpr)
						-- we have to evaluate the switch'ed expression only once, and in the beginning,
						-- since there could be side effects in it! (May God damn them...)
						cond_var = CVar cond_var_ident cond_ni
			
						-- Go through all the switch's "case"s and "default"s...
						collect_stmts :: [CBlockItem] -> [CBlockItem]
						collect_stmts [] = []
						collect_stmts (CBlockStmt (CDefault _ default_ni) : _ : _) = error $ ren default_ni ++ " : " ++
							"collect_stmts: the case when 'default' is not the last item in the switch is not implemented"
						-- if we have a "default", insert a "goto 1", which will later be translated into "Condition (Just True) 1"
						-- and append the default statement
						collect_stmts [ CBlockStmt (CDefault stmt default_ni) ] = [
							CBlockStmt $ CGotoPtr (CConst $ CIntConst (cInteger 1) default_ni) undefNode, CBlockStmt stmt ]
						-- if we have a "case <expr>: stmt", insert "if (expr==cond_var) { stmt; rest } else <recurse_collect_stmts>"
						collect_stmts (CBlockStmt (CCase caseexpr stmt case_ni) : rest) = [
							CBlockStmt $ CIf (CBinary CEqOp cond_var caseexpr case_ni)
							(CCompound [] (CBlockStmt stmt : filtercases rest) undefNode)
							(Just $ CCompound [] (collect_stmts rest) undefNode) undefNode ]
							where
							-- Eliminate the case/default "prefixes" from a statement list.
							-- append a "break;" in order to pop from the break stack after the switch
							filtercases :: [CBlockItem] -> [CBlockItem]
							filtercases cbis = for cbis $ \case
								CBlockStmt (CCase _ stmt _)  -> CBlockStmt stmt
								CBlockStmt (CDefault stmt _) -> CBlockStmt stmt
								cbi -> cbi
			
			
						-- if it was neither a "case" or "default", skip it.
						collect_stmts (_:rest) = collect_stmts rest
			
						-- This is the whole switch, rewritten as nested if-then-elses.
						case_replacement = collect_stmts cbis
			
					unfoldTracesM ret_type toplevel (forks+1) ([]:envs) trace (
						(CBlockDecl (CDecl [CTypeSpec $ CLongType cond_ni]
							[(Just $ CDeclr (Just cond_var_ident) [] Nothing [] cond_ni,
							Just $ CInitExpr condexpr cond_ni, Nothing)] cond_ni) :
						case_replacement,True) :
						(rest,breakable) : rest2 )
			
				CBreak ni -> do
					-- The scope that break reaches is the successor of the first "breakable" scope
					let
						drop_after_true (_:l1s) ((_,False):l2s) = drop_after_true l1s l2s
						drop_after_true (_:l1s) ((l2,True):l2s) = (l1s,l2s)
						(new_envs,new_bstss) = drop_after_true envs bstss
					printLogV 20 $ "### CBreak at " ++ (showLocation.lineColNodeInfo) ni ++ " dropped " ++ show (length envs - length new_envs) ++ " envs"
					printLogV 20 $ "### new_envs = \n" ++ dumpEnvs envs
					printLogV 20 $ "### length new_envs  = " ++ show (length new_envs)
					printLogV 20 $ "### length new_bstss = " ++ show (length new_bstss)
					unfoldTracesM ret_type toplevel forks new_envs trace new_bstss
			
				CIf cond then_stmt mb_else_stmt ni -> do
					let then_trace_m forks' real_cond = transids real_cond (Just _BoolTypes) trace $ \ (cond',trace') -> do
						unfoldTracesM ret_type toplevel forks' envs (Condition (Just True) cond' : trace') ( (CBlockStmt then_stmt : rest,breakable) : rest2 )
					let else_trace_m forks' real_cond = transids (CUnary CNegOp real_cond (annotation real_cond)) (Just _BoolTypes) trace $ \ (ncond',trace') -> do			
						printLogV 20 $ "### real_cond = " ++ (render.pretty) real_cond
						printLogV 20 $ "### ncond'    = " ++ (render.pretty) ncond'
						let not_cond = Condition (Just False) ncond'
						case mb_else_stmt of
							Nothing        -> unfoldTracesM ret_type toplevel forks' envs (not_cond : trace') ( (rest,breakable) : rest2 )
							Just else_stmt -> unfoldTracesM ret_type toplevel forks' envs (not_cond : trace') ( (CBlockStmt else_stmt : rest,breakable) : rest2 )
					case recognizeAnnotation cond of
						-- 12 is a wildcard in the choice list
						-- if the condition has been reached more often than the pragma list specifies, it is a wildcard
						(real_cond,Just (ns,num_reached)) | length ns > num_reached && ns!!num_reached /= 12 -> do
							printLogV 2 $ "Recognized IF annotation " ++ show (ns!!num_reached) ++ " to " ++ (render.pretty) real_cond ++
								" (reached " ++ show num_reached ++ " times)"
							case ns!!num_reached of
								1 -> then_trace_m forks real_cond
								2 -> else_trace_m forks real_cond
						(real_cond,_) -> do		
							either_then <- then_trace_m (forks+1) real_cond
							either_else <- else_trace_m (forks+1) real_cond
							return $ case (either_then,either_else) of
								(Left then_traces,Left else_traces) -> Left $ then_traces ++ else_traces
								(Right then_success,Right else_success) -> case toplevel of
									False -> Right $ then_success || else_success
									True  -> Right $ then_success && else_success
			
				CReturn Nothing _ | toplevel -> analyzeTraceM (Just ret_type) trace >>= return.Right
				CReturn Nothing _            -> return $ Left [trace]
			
				CReturn (Just ret_expr) _ -> do
					z3_ret_type <- ty2Z3Type ret_type
					transids ret_expr (Just z3_ret_type) trace $ \ (ret_expr',trace') -> do
						case toplevel of
							False -> return $ Left [ Return ret_expr' : trace' ]
							True  -> do
								Just ret_var_expr <- gets retEnvCVS
								ret_env_expr <- createInterfaceFromExprM ret_expr' ret_type
								when (length ret_var_expr /= length ret_env_expr) $ error "unfoldTraces1M CReturn: length ret_var_expr /= length ret_env_expr !"
								ret_trace <- concatForM (zip ret_var_expr ret_env_expr) $
									\ ( ((_,(ret_var_ident,ret_var_ty)),_) , (_,ret_member_expr)) -> do
										z3_ret_var_ty <- ty2Z3Type ret_var_ty
										let ret_val_cond = CBinary CEqOp
											(CVar ret_var_ident (nodeInfo ret_var_ident,z3_ret_var_ty))
											ret_member_expr
											(undefNode,_BoolTypes)
										return [ Condition Nothing ret_val_cond, NewDeclaration (ret_var_ident,ret_var_ty) ]
								analyzeTraceM (Just ret_type) (Return ret_expr' : (ret_trace ++ trace'))
									>>= return.Right
			
				CExpr (Just (CCall (CVar (Ident "solver_debug" _ _) _) args ni)) _ -> do
					dbgouts <- forM args $ \ arg -> do
						expr' <- transcribeExprM envs Nothing arg
						return $ DebugOutput ("solver_debug_" ++ lValueToVarName expr') expr'
					unfoldTracesM ret_type toplevel forks envs (reverse dbgouts ++ trace) ((rest,breakable):rest2)
			
				CExpr (Just (CAssign assignop lexpr assigned_expr ni)) _ -> do
					transids (CAssign CAssignOp lexpr assigned_expr' ni) Nothing trace $
						\ (CAssign CAssignOp lexpr' assigned_expr'' _,trace') -> do
							unfoldTracesM ret_type toplevel forks envs (Assignment lexpr' assigned_expr'' : trace') ((rest,breakable):rest2)
					where
					assigned_expr' = case assignop of
						CAssignOp -> assigned_expr
						ass_op    -> CBinary (assignBinop ass_op) lexpr assigned_expr ni
			
				CExpr (Just (CUnary unaryop expr ni_op)) ni | unaryop `elem` (map fst unaryops) -> do
					ii <- ⅈ 1
					let stmt' = CExpr (Just $ CAssign assignop expr ii ni) ni
					unfoldTracesM ret_type toplevel forks envs trace ( (CBlockStmt stmt' : rest,breakable) : rest2 )
					where
					Just assignop = lookup unaryop unaryops
					unaryops = [ (CPreIncOp,CAddAssOp),(CPostIncOp,CAddAssOp),(CPreDecOp,CSubAssOp),(CPostDecOp,CSubAssOp) ]
			
				CExpr (Just expr) _ -> do
					myError $ "unfoldTraces: " ++ (render.pretty) stmt ++ " not implemented yet."
			
				-- That's cheating: Insert condition into trace (for loop unrolling and switch) via GOTO
				CGotoPtr cond ni -> do
					transids cond (Just _BoolTypes) trace $ \ (cond',trace') -> do
						unfoldTracesM ret_type toplevel forks envs (Condition (Just $ isUndefNode ni) cond' : trace') ( (rest,breakable) : rest2 )
			
				CWhile cond body False ni -> do
					(mb_unrolling_depth,msg) <- infer_loopingsM cond body
					printLogV 1 msg
					unroll_loopM $ case mb_unrolling_depth of
						Nothing -> uNROLLING_STRATEGY
						Just ns -> ns
			
					where
			
					unroll_loopM :: [Int] -> CovVecM UnfoldTracesRet
					unroll_loopM depths = do
						ress <- forM depths $ \ depth ->
							unfoldTracesM ret_type toplevel forks ([]:envs) trace ((unroll cond depth,True) : (rest,breakable) : rest2 )
						return $ case toplevel of
							False -> Left $ concat $ lefts ress
							True  -> Right $ any id $ rights ress
			
					unroll :: CExpr -> Int -> [CBlockItem]
					unroll while_cond n = 
						concat ( replicate n [ CBlockStmt (CGotoPtr while_cond undefNode), CBlockStmt body ] ) ++
						[ CBlockStmt $ CGotoPtr (not_c while_cond) ni ]
			
				-- Express the for loop as a bisimular while loop
				CFor (Right decl) mb_cond mb_inc_expr stmt ni -> do
					ii <- ⅈ 1
					let
						body_stmt = CWhile (maybe ii id mb_cond) while_body False ni
						while_body = CCompound [] ( CBlockStmt stmt :
							maybe [] (\ expr -> [ CBlockStmt $ CExpr (Just expr) (nodeInfo expr) ]) mb_inc_expr) (nodeInfo stmt)
						stmt' = CCompound [] [ CBlockDecl decl, CBlockStmt body_stmt ] ni
					unfoldTracesM ret_type toplevel forks envs trace ((CBlockStmt stmt' : rest,breakable) : rest2)
			
				_ -> myError $ "unfoldTracesM " ++ (render.pretty) stmt ++ " not implemented yet"
		
			where
		
			recognizeAnnotation :: CExpr -> (CExpr,Maybe ([Int],Int))
			recognizeAnnotation (CBinary CLndOp (CCall (CVar (Ident "solver_pragma" _ _) _) args _) real_cond ni) =
				-- set the NodeInfo in real_cond to the original NodeInfo of the *whole* condition that includes the solver_annotation
				-- otherwise, it will be reported as uncovered (have in mind: all branching points are determined before the analysis starts!)
				(amap (const ni) real_cond,Just (map arg2int args,num_reached)) where
					num_reached = length $ filter is_this_cond trace where
						is_this_cond (Condition _ c) = extractNodeInfo c == ni
						is_this_cond _ = False
					arg2int (CConst (CIntConst (CInteger i _ _) _)) = fromIntegral i
			recognizeAnnotation real_cond = (real_cond,Nothing)
		
			infer_loopingsM :: CExpr -> CStat -> CovVecM (Maybe [Int],String)
			infer_loopingsM cond0 body = do
				case recognizeAnnotation cond0 of
					(real_cond,Just (ns,_)) -> return (Just ns,"Recognized LOOP annotation to " ++ (render.pretty) cond0)
					(real_cond,Nothing) -> do
						let default_ns = [0,1,2]
		--				return (Just default_ns,"No annotation, trying " ++ show default_ns)
		
						translateExprM envs real_cond (Just _BoolTypes) >>= \case
							[(cond,[])] -> do
								let
									-- get all variables used in the condition
									cond_idents = fvar cond
								-- unfold body to all body traces and filter for all Assignments to variables from the condition
								Left body_traces <- unfoldTracesM ret_type False forks envs [] [([CBlockStmt body],False)]
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
									[ (counter_var@(CVar ass_ident (_,(ass_ident_z3ty,_))),ass_expr) ] -> do
										let
											is_ass_to_ass_var (Assignment (CVar ident _) _) | ident==ass_ident = True
											is_ass_to_ass_var _ = False
										case filter is_ass_to_ass_var trace of
											[] -> return (Nothing,"infer_loopingsM: There is no assignment to the loop counter " ++ (render.pretty) counter_var ++ " prior to the loop")
											ass@(Assignment _ i_0) : _ | null (fvar i_0)-> do
												inttypes <- ty2Z3Type intType
												printLogV 2 $ "last assignment to loop counter is " ++ show ass
												let i_n :: CExprWithType -> CovVecM CExprWithType = case ass_expr of
													-- for all binops where the following holds (Linearity?):
													-- i_n = i_(n-1) `binop` c  =>  i_n = i_0 `binop` c
													CBinary binop (CVar ident _) cconst@(CConst _) _ | ident == ass_ident ∧ binop `elem` [CSubOp,CAddOp,CShrOp,CShlOp] -> \ n_var → do
														return $ CBinary binop i_0 (n_var ∗ cconst) (undefNode,inttypes)
													_ -> error $ "infer_loopingsM: assignment " ++ (render.pretty) ass_ident ++ " := " ++ (render.pretty) ass_expr ++ " not implemented!"
												let
													n_name = "n$loopings"
													n_ident = internalIdent n_name
													n_var = CVar n_ident (undefNode,inttypes)
													modelpath = analyzerPath </> n_name ++ show (lineColNodeInfo $ extractNodeInfo cond) ++ ".smtlib2"
												n_types <- case lookup ass_ident (envs2tyenv envs) of
													Nothing -> myError $ "infer_loopingsM: Could not find type of " ++ (render.pretty) counter_var
													Just ty -> ty2Z3Type ty
												i_n_n_var <- i_n n_var
												i_0 <- ⅈ 0
												i_1 <- ⅈ 1
												i_n_n_var_minus_1 <-  i_n $ n_var − i_1
												i_n_0 <- i_n i_0
												tyenv <- tyEnvFromTraceM trace
												(model_string,mb_sol) <- makeAndSolveZ3ModelM
													[]
													((n_ident,ass_ident_z3ty) : tyenv)
													(let
														cond_n       = (counter_var `substituteBy` i_n_n_var) cond
														cond_nminus1 = (counter_var `substituteBy` i_n_n_var_minus_1) cond
														cond_0       = (counter_var `substituteBy` i_n_0) cond
														in
														map (Condition Nothing) [
															n_var ⩾ i_0,
															not_c cond_0  ⋏  n_var ⩵ i_0
																⋎
																cond_nminus1 ⋏ n_var ⩾ i_1 ⋏ not_c cond_n
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
		
			-- mb_ty is Nothing if the result type of expr is not known, i.e. no casting necessary.
			transids :: CExpr -> Maybe Types -> Trace -> ((CExprWithType,Trace) -> CovVecM UnfoldTracesRet) -> CovVecM UnfoldTracesRet
			transids expr mb_ty trace cont = logWrapper 5 ["transids",ren expr,ren mb_ty,ren trace,"<cont>"] $ do
				printLogV 20 $ "### transids " ++ (render.pretty) expr
				additional_expr_traces :: [(CExprWithType,Trace)] <- translateExprM envs expr mb_ty
				printLogV 20 $ "### -> additional_expr_traces = " ++ (render.pretty) (map fst additional_expr_traces)
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

unfoldTraces1M ret_type toplevel forks (env:envs) trace ( (cblockitem@(CBlockDecl decl@(CDecl typespecs triples _)) : rest, breakable) : rest2 ) =
	logWrapper 2 [ren "unfoldTraces1M",ren ret_type,ren toplevel,ren forks,ren envs,ren trace,'\n':ren cblockitem] $ do
		ty <- decl2TypeM decl
		new_env_items <- forM triples $ \case
			(Just (CDeclr (Just ident) derivdeclrs _ _ ni),mb_init,Nothing) -> do
				newenvitems <- identTy2EnvItemM ident ty
				let newdecls = map (NewDeclaration . snd) newenvitems
				initializers <- case mb_init of
					Nothing -> return []
					Just initializer -> cinitializer2blockitems (CVar ident ni) ty initializer
				return (newenvitems,newdecls,initializers)
			triple -> myError $ "unfoldTracesM: triple " ++ show triple ++ " not implemented!"
		let (newenvs,newitems,initializerss) = unzip3 $ reverse new_env_items
		unfoldTracesM ret_type toplevel forks ((concat newenvs ++ env) : envs) (concat newitems ++ trace) ((concat initializerss ++ rest,breakable):rest2)

unfoldTraces1M ret_type toplevel forks (_:restenvs) trace (([],_):rest2) = do
	unfoldTracesM ret_type toplevel forks restenvs trace rest2

unfoldTraces1M ret_type False _ envs trace [] = return $ Left [trace]
unfoldTraces1M ret_type True  _ _    trace [] = analyzeTraceM (Just ret_type) trace >>= return.Right

unfoldTraces1M _ _ _ _ _ ((cbi:_,_):_) = myError $ "unfoldTracesM " ++ (render.pretty) cbi ++ " not implemented yet."


-- The following  definitions are only for CExpr's, since CExprWithType's also need correct type information, i.e.
-- casting n'stuff...

infix 4 ⩵
(⩵) :: CExprWithType -> CExprWithType -> CExprWithType
a ⩵ b = CBinary CEqOp a b (annotation a)

infix 4 !⩵
(!⩵) :: CExpression a -> CExpression a -> CExpression a
a !⩵ b = not_c $ CBinary CEqOp a b (annotation a)

infix 4 ⩾
(⩾) :: CExprWithType -> CExprWithType -> CExprWithType
a ⩾ b = CBinary CGeqOp a b (annotation a)

infixr 3 ⋏
(⋏) :: CExprWithType -> CExprWithType -> CExprWithType
a ⋏ b = CBinary CLndOp a b (annotation a)

infixr 2 ⋎
(⋎) :: CExprWithType -> CExprWithType -> CExprWithType
a ⋎ b = CBinary CLorOp a b (annotation a)

infixr 7 ∗
(∗) :: CExprWithType -> CExprWithType -> CExprWithType
a ∗ b = CBinary CMulOp a b (annotation a)

infixr 6 −
(−) :: CExprWithType -> CExprWithType -> CExprWithType
a − b = CBinary CSubOp a b (annotation a)

not_c :: CExpression a -> CExpression a
not_c e = CUnary CNegOp e (annotation e)

class CreateInt a where
	ⅈ :: Integer -> CovVecM a
instance CreateInt CExpr where
	ⅈ i = return $ CConst $ CIntConst (cInteger i) undefNode
instance CreateInt CExprWithType where
	ⅈ i = do
		inttype <- ty2Z3Type intType
		return $ CConst $ CIntConst (cInteger i) (undefNode,inttype)

infix 1 ≔
(≔) :: CExpression a -> CExpression a -> CStatement a
ass ≔ expr = CExpr (Just $ CAssign CAssignOp ass expr (annotation ass)) (annotation expr)


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
		CInitExpr expr ni_init -> return [ CBlockStmt $ lexpr ≔ expr ]
		CInitList initlist ni_init -> case ty of
			DirectType (TyComp (CompTypeRef sueref _ _)) _ _ -> do
				memberidentstypes <- getMembersM sueref
				concatForM (zip initlist memberidentstypes) $ \case
					(([],initializer),(memberident,memberty)) -> do
						memberty' <- elimTypeDefsM memberty
						cinitializer2blockitems (CMember lexpr memberident False (nodeInfo memberident)) memberty' initializer
					_ -> myError $ "cinitializer2blockitems DirectType: CPartDesignators not implemented yet in\n" ++ (render.pretty) ty
			ArrayType elem_ty _ _ _ -> concatForM (zip [0..] initlist) $ \ (i,(partdesigs,cinitializer)) -> do
				case partdesigs of
					[] -> do
						ii <- ⅈ i
						cinitializer2blockitems (CIndex lexpr ii ni_init) elem_ty cinitializer
					_ -> myError $ "cinitializer2blockitems ArrayType: CPartDesignators not implemented yet in\n" ++ (render.pretty) ty
			_ -> myError $ "cinitializer2blockitems: " ++ (render.pretty) ty ++ " at " ++ (show $ nodeInfo lexpr) ++ " not implemented!"

showFullLocation :: (CNode a) => a -> String
showFullLocation cnode = (posFile $ posOfNode $ nodeInfo cnode) ++ " : " ++ (showLocation.lineColNodeInfo) cnode

-- Creates an CExprWithType from a CExpr
transcribeExprM :: [Env] -> Maybe Types -> CExpr -> CovVecM CExprWithType
transcribeExprM envs mb_target_ty expr = do
	vars' <- renameVars envs expr
	annotateTypesAndCastM envs vars' mb_target_ty
	where
	-- Renames Variables to unique names, looking up their unique name (wíth a number suffix)
	
	-- The renaming should be reflected in the type, smth. like
	-- newtype IsRenamed = IsRenamed NodeInfo
	-- type CExprRenamed = CExpression IsRenamed
	--
	-- But the above would be over-engineering, since renameVars is only used here.
	renameVars :: [Env] -> CExpr -> CovVecM CExpr
	renameVars envs expr = everywhereM (mkM subst_var) expr where
		subst_var :: CExpr -> CovVecM CExpr
		subst_var (CVar ident ni) = case lookup ident (concat envs) of
			Just (ident',_) -> return $ CVar ident' ni
			Nothing -> myError $ " in subst_var: Could not find " ++ (render.pretty) ident ++
				" when renaming " ++ (render.pretty) expr ++ " at " ++ showFullLocation expr ++ "\n" ++
				"env = \n" ++ envToString (concat envs)
		subst_var expr = return expr


-- Translates all identifiers in an expression to fresh ones,
-- and expands function calls. Translates to CExprWithType's.
-- It needs to keep the original NodeInfos, because of the coverage information which is derived from the original source tree.
translateExprM :: [Env] -> CExpr -> Maybe Types -> CovVecM [(CExprWithType,Trace)]
translateExprM envs expr0 mb_target_ty = logWrapper 5 ["translateExprM","<envs>",ren expr0,ren mb_target_ty] $ do
	-- extract a list of all calls from the input expression expr0
	-- (including fun-identifier, the arguments, and NodeInfo)
	let
		to_call :: CExpr -> StateT [(Ident,[CExpr],NodeInfo)] CovVecM CExpr
		to_call ccall@(CCall funexpr args ni) = case funexpr of
			CVar (Ident "__builtin_expect" _ _) _ -> return $ head args
			CVar (Ident "solver_pragma" _ _) _ -> lift $ ⅈ 1
			CVar funident _ -> do
				modify ( (funident,args,ni) : )
				-- Replace the call by a placeholder with the same NodeInfo
				lift $ printLogV 20 $ "Found call " ++ ren funident ++ " at " ++ ren ni
				return ccall
			_  -> lift $ myError $ "is_call: found call " ++ (render.pretty) funexpr
		to_call expr = return expr
	(expr,calls::[(Ident,[CExpr],NodeInfo)]) <- runStateT (everywhereM (mkM to_call) expr0) []

	-- construct all possible traces in called (sub-)functions and return them together with the returned expression.
	-- NodeInfo is the position of the call, [(CExprWithType,Trace)] is the list of possible
	-- return expressions together with their trace
	funcalls_traces :: [(NodeInfo,[(CExprWithType,Trace)])] <- forM calls $ \ (funident,args,ni) -> do
		FunDef (VarDecl _ _ (FunctionType (FunType ret_ty paramdecls False) _)) body _ <- lookupFunM funident
		expanded_params_args <- expand_params_argsM paramdecls args
		printLogV 20 $ "body = " ++ (render.pretty) body
		printLogV 20 $ "expanded_params_args = " ++ show expanded_params_args
		-- β-reduction of the arguments:
		let body' = replace_param_with_arg expanded_params_args body
		printLogV 20 $ "body'= " ++ (render.pretty) body'
		Left funtraces <- unfoldTracesM ret_ty False 0 envs [] [ ([ CBlockStmt body' ],False) ]
		forM_ funtraces $ \ tr -> printLogV 20 $ "funtrace = " ++ showTrace tr
		let funtraces_rets = concat $ for funtraces $ \case
			(Return retexpr : tr) -> [(retexpr,tr)]
			tr -> error $ "funcalls_traces: trace of no return:\n" ++ showTrace tr
		return (ni,funtraces_rets)

	expr' <- transcribeExprM envs mb_target_ty expr

	printLogV 20 $ "creating combinations..."
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

	-- iterate over all possible traces in a called (sub-)function and concatenate their traces 
	create_combinations :: CExprWithType -> Trace -> [(NodeInfo,[(CExprWithType,Trace)])] -> CovVecM [(CExprWithType,Trace)]
	create_combinations expr trace [] = do
		return [(expr,trace)]
	-- replace the place-holder in the expr with the return expression of each sub-function's trace,
	-- concatenating all possibilities (but it does not matter which one, since we only fully cover the top level function)
	create_combinations expr trace ((tes_ni,tes):rest) = do
		concatForM tes $ \ (ret_expr,fun_trace) -> do
			let
				-- substitute the function call by the return expression
				expr' = everywhere (mkT subst_ret_expr) expr where
					subst_ret_expr :: CExprWithType -> CExprWithType
					subst_ret_expr (CCall _ _ (ni,_)) | tes_ni == ni = ret_expr
					subst_ret_expr expr = expr
--			printLog $ "fun_trace=" ++ show fun_trace
			create_combinations expr' (fun_trace++trace) rest

-- Substitutes an expression x by y everywhere in d
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
	elim_indsM :: Trace -> Trace -> CovVecM Trace
	elim_indsM res_trace [] = return res_trace
	elim_indsM res_trace (ti@(Assignment ptr@(CVar ptr_ident _) expr) : rest) = do
		case extractType ptr of
			PtrType _ _ _ -> elim_indsM (cancel_ind_adrs $ substituteBy ptr expr res_trace) rest
			_ -> elim_indsM (ti : res_trace) rest
	elim_indsM res_trace (ti : rest) = elim_indsM (ti : res_trace) rest

	cancel_ind_adrs :: Trace -> Trace
	cancel_ind_adrs trace = everywhere (mkT cancel_ind_adr) trace
		where
		cancel_ind_adr :: CExprWithType -> CExprWithType
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
	-- Skip assignments to array elements
	foldtraceM result (ass@(Assignment (CIndex _ _ _) _) : rest) = foldtraceM (ass : result) rest
	foldtraceM result (Assignment lvalue expr : rest) = foldtraceM (substituteBy lvalue expr result) rest
	foldtraceM result (traceitem : rest) = foldtraceM (traceitem:result) rest

-- eliminate assignments to arrays, replacing them by a new array declaration
-- and a condition that a_n+1 = store a_n ... ...
{-
... f ( int a[3], ...)  =>  ... f ( int a_INDEX_0, int a_INDEX_1, int a_INDEX_2, ... )
-}
-- trace is in the right order.
elimArrayAssignsM :: Trace -> CovVecM Trace
elimArrayAssignsM trace = evalStateT elim_arr_assnsM Map.empty
	where
	elim_arr_assnsM :: StateT (Map.Map Ident Int) CovVecM Trace
	elim_arr_assnsM = do
		ls <- forM trace $ \case
			Assignment (CIndex var index_expr index_ni) ass_expr -> case var of
				CVar ident var_ni -> do
					counters <- get
					i <- case Map.lookup ident counters of
						Nothing -> do
							modify $ Map.insert ident 2
							return 1
						Just i -> do
							modify $ Map.adjust (+1) ident
							return i
					let
						newident = internalIdent $ identToString ident ++ "$" ++ show i
						newvar   = CVar newident var_ni
					return $ [
						NewDeclaration (newident,extractType var) ,
						Assignment (CIndex newvar index_expr index_ni) ass_expr,
						Assignment var newvar ]
				other -> lift $ myError $ "elim_arr_assnsM: not a variable in CIndex: " ++ (render.pretty) other
			other -> return [other]
		return $ concat ls

-- Simplify:
-- *(&x)  ~> x
-- &s->m  ~> s.m
-- (*p).m ~> p->m
-- (A)a   ~> a  if a::A
-- (A*)x  ~> x::(A*)

simplifyTraceM :: Trace -> CovVecM Trace
simplifyTraceM trace = everywhereM (mkM simplify) trace where
	simplify :: CExprWithType -> CovVecM CExprWithType
	simplify (CUnary CIndOp (CUnary CAdrOp expr _) _) = return expr
	simplify (CMember (CUnary CAdrOp s _) member True ni) = return $ CMember s member False ni
	simplify (CMember (CUnary CIndOp p _) member False ni) = return $ CMember p member True ni
	simplify (CCast _ expr (_,(z3ty,_))) | extractZ3Type expr == z3ty = return expr
-- This one:
	simplify (CCast _ expr (_,tys@(Z3_Ptr _,_))) = return $ amap (\(ni,_)->(ni,tys)) expr
	simplify expr = return expr


addUnionConstraintsM :: Trace -> CovVecM Trace
addUnionConstraintsM trace = do
	-- collect all CMembers
	let members = everything (++) (mkQ [] search_members) trace
		where
		search_members :: CExprWithType -> [CExprWithType]
		search_members cmember@(CMember _ _ _ _) = [cmember]
		search_members _ = []

	-- filter the ones referring to a union and extract exprs and members from the CMember
	union_members <- concatForM members $ \case
		cmember@(CMember cvar@(CVar ident _) member True  _)
			| Z3_Ptr (Z3_Compound sueref UnionTag) <- extractZ3Type cvar ->
				return [(sueref,ident,member,cmember)]
		cmember@(CMember cvar@(CVar ident _) member False _)
			| Z3_Compound sueref UnionTag <- extractZ3Type cvar ->
				return [(sueref,ident,member,cmember)]
		cmember -> return []

	let unique_members = nubBy (\ (_,ident1,member1,_) (_,ident2,member2,_) -> ident1==ident2 && member1==member2) union_members

	printLogV 2 $ "unique_members=\n" ++ unlines (map (\(_,ident,member,_)->show (ident,member)) unique_members)

	return trace

-- Create symbolic vars for leftover expressions

createSymbolicVarsM :: Trace -> CovVecM Trace
createSymbolicVarsM trace = create_symbolic_vars [] (map fst $ createTyEnv trace) trace
	where
	create_symbolic_vars :: Trace -> [Ident] -> Trace -> CovVecM Trace
	create_symbolic_vars res_trace _ [] = return $ reverse res_trace
	create_symbolic_vars res_trace new_idents (ti : rest) = do
		(ti',add_tis) <- runStateT (everywhereM (mkM createsymvar_m) ti) []
		create_symbolic_vars (ti' : (map NewDeclaration add_tis) ++ res_trace) (map fst add_tis ++ new_idents) rest
		where
		
		create_var :: CExprWithType -> Type -> StateT [(Ident,Type)] CovVecM CExprWithType
		create_var expr ty = do
			let newident = mkIdentWithCNodePos (extractNodeInfo expr) $ lValueToVarName expr
			when (not $ newident `elem` new_idents) $
				modify ((newident,ty) : )
			return $ CVar newident (annotation expr)
	
		createsymvar_m :: CExprWithType -> StateT [(Ident,Type)] CovVecM CExprWithType
	
		createsymvar_m expr@(CUnary CIndOp cvar@(CVar ptr_ident _) ni) = do
			let PtrType ty _ _ = extractType cvar
			create_var expr ty
	
		--  for ptr->member   create    p1_ARROW_member :: member_type
		createsymvar_m expr@(CMember cvar@(CVar ptr_ident _) member True _) = do
			let PtrType sue_ty _ _ = extractType cvar
			member_ty <- lift $ getMemberTypeM sue_ty member
			create_var expr member_ty
	
		--  for a.member   create    a_DOT_member :: member_type
		createsymvar_m expr@(CMember (CVar a_ident _) member False ni) = do
			return $ CVar (mkIdentWithCNodePos (extractNodeInfo expr) $ lValueToVarName expr) ni
	
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

type Types = (Z3_Type,Type)

-- TODO: Avoid overlapping instance here
instance {-# OVERLAPPING #-} Ord Types where
	(t1,_) <= (t2,_) = t1 <= t2

-- TODO: Avoid overlapping instance here
instance {-# OVERLAPPING #-} Eq Types where
	(t1,_) == (t2,_) = t1 == t2

-- TODO: Avoid overlapping instance here
instance {-# OVERLAPPING #-} Show Types where
	show (t1,_) = show t1

_BoolTypes = (Z3_Bool,intType) :: Types
_IntTypesM :: CovVecM Types
_IntTypesM = ty2Z3Type intType

extractTypes :: CExprWithType -> Types
extractTypes = snd.annotation

extractZ3Type :: CExprWithType -> Z3_Type
extractZ3Type = fst.extractTypes

extractType :: CExprWithType -> Type
extractType = snd.extractTypes

extractNodeInfo :: CExprWithType -> NodeInfo
extractNodeInfo = fst.annotation

type NodeInfoWithType = (NodeInfo,Types)
type CExprWithType = CExpression NodeInfoWithType

-- adds Z3 types and Language.C.Type's to the annotation that the expressions have
-- also inserts implicit casts
-- if mb_target_ty is Nothing, the result CExprWithType will not be casted to the mb_target_ty type.
annotateTypesAndCastM :: [Env] -> CExpr -> Maybe Types -> CovVecM CExprWithType
annotateTypesAndCastM envs cexpr mb_target_ty = logWrapper 2 ["annotateTypesAndCastM","<env>",ren cexpr,ren mb_target_ty] $ do
	cexpr' <- annotate_types cexpr
	let ret = case mb_target_ty of
		Just target_ty -> mb_cast target_ty cexpr'
		_ -> cexpr'
	printLogV 20 $ "\n# cexpr = " ++ (showLocation.lineColNodeInfo) cexpr
	printLogV 20 $ "# annotateTypesAndCastM [envs] " ++ (render.pretty) cexpr
	printLogV 20 $ "# target_ty = " ++ show mb_target_ty
	printLogV 20 $ "#   cexpr' = " ++ (render.pretty) cexpr'
	printLogV 20 $ "# ==>\n" ++ (render.pretty) ret ++ "\n"

	return $ ret

	where

	tyenv = envs2tyenv envs

	annotate_types :: CExpr -> CovVecM CExprWithType

	-- Get rid of CNeqOp now, later in expr2sexpr it will be more difficult...
	annotate_types (CBinary CNeqOp expr1 expr2 ni) = annotate_types $ expr1 !⩵ expr2
	annotate_types (CBinary binop expr1 expr2 ni) = do
		expr1' <- annotate_types expr1
		expr2' <- annotate_types expr2
		let
			common_ty = case binop `elem` [CLndOp,CLorOp] of
				False -> max (extractTypes expr1') (extractTypes expr2')
				True  -> _BoolTypes
			result_ty = case binop `elem` [CLndOp,CLorOp,CLeOp,CGrOp,CLeqOp,CGeqOp,CEqOp,CNeqOp] of
				True  -> _BoolTypes
				False -> common_ty
		return $ CBinary binop (mb_cast common_ty expr1') (mb_cast common_ty expr2') (ni,result_ty)

	annotate_types ccast@(CCast decl expr ni) = do
		ty1' <- decl2TypeM decl
		printLogV 20 $ "XXX  decl2TypeM ( " ++ (render.pretty) decl ++ ") = " ++ show ty1'
		ty2' <- elimTypeDefsM ty1'
		printLogV 20 $ "XXX  elimTypeDefsM ( " ++ (render.pretty) ty1' ++ ") = " ++ show ty2'
		ty' <- ty2Z3Type ty2'
		printLogV 20 $ "XXX  ty2Z3Type ( " ++ (render.pretty) ty2' ++ ") = " ++ show ty'
		expr' <- annotate_types expr
		printLogV 20 $ "XXX  annotate_types " ++ (render.pretty) expr ++ " = " ++ (render.pretty) expr'
		printLogV 20 $ "XXX  mb_cast " ++ show ty' ++ " " ++ (render.pretty) expr' ++ " = " ++ (render.pretty) (mb_cast ty' expr')
		return $ mb_cast ty' expr'

	annotate_types cunary@(CUnary unop expr ni) = do
		printLogV 20 $ "### annotate_types " ++ (render.pretty) cunary
		expr' <- annotate_types expr
		printLogV 20 $ "### expr' = " ++ (render.pretty) expr'
		let (arg_ty,result_ty) = case (unop,extractTypes expr') of
			(CNegOp, _)                                -> (_BoolTypes,_BoolTypes)
			(CAdrOp, ty@(z3ty,cty))                    -> (ty,(Z3_Ptr z3ty,ptrType cty))
			(CIndOp, ty@(Z3_Ptr z3ty,PtrType cty _ _)) -> (ty,(z3ty,cty))
			(CIndOp, _) -> error $ "annotate_types: argument type of " ++ (render.pretty) cunary ++ " is no Ptr!"
			(_,      ty)                               -> (ty,ty)
		printLogV 20 $ "### (arg_ty,result_ty) = " ++ show (arg_ty,result_ty)
		let erg = CUnary unop (mb_cast arg_ty expr') (ni,result_ty)
		printLogV 20 $ "### erg = " ++ (render.pretty) erg
		return erg

	annotate_types (CCond cond_expr (Just then_expr) else_expr ni) = do
		cond_expr' <- annotate_types cond_expr
		then_expr' <- annotate_types then_expr
		else_expr' <- annotate_types else_expr
		let common_ty = max (extractTypes then_expr') (extractTypes else_expr')
		return $ CCond (mb_cast _BoolTypes cond_expr')
			(Just $ mb_cast common_ty then_expr') (mb_cast common_ty else_expr') (ni,common_ty)

	annotate_types cvar@(CVar ident ni) = case lookup ident tyenv of
		Nothing -> myError $ "Could not find " ++ (render.pretty) ident ++ " in " ++ showTyEnv tyenv
		Just ty -> do
			var_ty <- elimTypeDefsM ty >>= ty2Z3Type
			return $ CVar ident (ni,var_ty)

	annotate_types (CConst ctconst) = case ctconst of
		CIntConst cint@(CInteger _ _ flags) ni -> do
			z3_ty <- ty2Z3Type $ integral (getIntType flags)
			return $ CConst $ CIntConst cint (ni,z3_ty)
		CFloatConst cfloat@(CFloat s) ni       -> do
			z3_ty <- ty2Z3Type $ floating (getFloatType s)
			return $ CConst $ CFloatConst cfloat (ni,z3_ty)
		CCharConst cchar@(CChar _ False) ni    -> do
			z3_ty <- ty2Z3Type $ charType
			return $ CConst $ CCharConst cchar (ni,z3_ty)
		CStrConst cstr ni                      -> do
			z3_ty <- ty2Z3Type $ ptrType charType
			return $ CConst $ CStrConst cstr (ni,z3_ty)

	annotate_types (CAssign assign_op lexpr ass_expr ni) = do
		lexpr' <- annotate_types lexpr
		let (_,lexpr_ty) = annotation lexpr'
		ass_expr' <- annotate_types ass_expr
		return $ CAssign assign_op lexpr' (mb_cast lexpr_ty ass_expr') (ni,lexpr_ty)

	annotate_types (CMember pexpr member_ident True ni) = do
		pexpr' <- annotate_types pexpr
		let PtrType objty _ _ = extractType pexpr' 
		mem_ty <- getMemberTypeM objty member_ident
		z3_mem_ty <- ty2Z3Type mem_ty
		return $ CMember pexpr' member_ident True (ni,z3_mem_ty)
	annotate_types (CMember pexpr member_ident False ni) = do
		pexpr' <- annotate_types pexpr
		mem_ty <- getMemberTypeM (extractType pexpr') member_ident
		z3_mem_ty <- ty2Z3Type mem_ty
		return $ CMember pexpr' member_ident False (ni,z3_mem_ty)

	-- dummy, used when inferring types when constructing CStmt's
	annotate_types (CCall funexpr args ni) = do
		funexpr' <- annotate_types funexpr
		let (_,(Z3_Fun z3_retty _ _,FunctionType (FunType ret_ty _ _) _)) = annotation funexpr'
		args' <- forM args annotate_types
		return $ CCall funexpr' args' (ni,(z3_retty,ret_ty))

	annotate_types (CIndex arr_expr ix ni) = do
		arr_expr' <- annotate_types arr_expr
		let (Z3_Array elemty _,ArrayType elemcty _ _ _) = extractTypes arr_expr'
		ix' <- annotate_types ix
		intty <- _IntTypesM
		return $ CIndex arr_expr' (mb_cast intty ix') (ni,(elemty,elemcty))

	annotate_types other = myError $ "annotate_types " ++ (render.pretty) other ++ " not implemented"

	mb_cast :: (Z3_Type,Type) -> CExprWithType -> CExprWithType
	mb_cast to_ty cexpr = case (extractZ3Type cexpr,fst to_ty) of
		( ty1, ty2 ) | ty1==ty2 -> cexpr
		( Z3_BitVector size_from _, Z3_BitVector size_to _ ) | size_from == size_to -> cexpr
		_ -> cast cexpr to_ty
		where
		cast :: CExprWithType -> Types -> CExprWithType
		-- The NodeInfo of the CDecl will contain the cast target type (we do not want to convert to DeclSpecs...)
		cast cexpr to_ty = CCast (CDecl [] [] to_anno) cexpr to_anno
			where
			to_anno = (extractNodeInfo cexpr,to_ty)


type Constraint = TraceElem

expr2SExpr :: Constraint -> CovVecM SExpr
expr2SExpr expr = expr2sexpr expr

	where

	make_intconstant :: String -> Types -> Integer -> CovVecM SExpr
	make_intconstant _ (Z3_BitVector size _,_) const | size `mod` 4 == 0 =
		return $ SLeaf (printf "#x%*.*x" (size `div` 4) (size `div` 4) const)
	make_intconstant from types const = myError $ "make_intconstant " ++ from ++ " " ++ show types ++ " " ++ show const

	expr2sexpr :: Constraint -> CovVecM SExpr
	expr2sexpr (Condition _ cexpr) = do
		printLogV 2 $ "expr2sexpr " ++ (render.pretty) cexpr
		expr2sexpr' cexpr
		
	-- Assignment to an array member
	expr2sexpr (Assignment (CIndex var@(CVar ident _) indexexpr _) ass_expr) = do
		var_s <- expr2sexpr' var
		index_s <- expr2sexpr' indexexpr
		ass_s <- expr2sexpr' ass_expr
		let [array_expr] = everything (++) (mkQ [] search_arrays) ass_expr where
			search_arrays :: CExprWithType -> [CExprWithType]
			search_arrays (CIndex var@(CVar _ _) _ _) = [var]
			search_arrays _ = []
		arr_s <- expr2sexpr' array_expr
		return $ SExpr [ SLeaf "=", var_s, SExpr [ SLeaf "store", arr_s, index_s, ass_s ] ]

	-- Turns a CExprWithType into an SExpr
	expr2sexpr' :: CExprWithType -> CovVecM SExpr

	expr2sexpr' expr = case expr of

		CIndex arr_expr@(CVar _ _) index_expr _ ->
			SExpr <$> sequence [ pure $ SLeaf "select", expr2sexpr' arr_expr, expr2sexpr' index_expr ]

		-- CNeqOp was resolved while annotateTypes
		CBinary binop expr1 expr2 _ ->
			SExpr <$> sequence [ pure $ SLeaf op_sexpr, expr2sexpr' expr1, expr2sexpr' expr2 ]
				where
				op_ty = extractZ3Type expr1
				op_sexpr = case binop of
					CMulOp -> bitVectorTy op_ty "bvmul" ("fp.mul " ++ roundingMode)
					CDivOp -> bitVectorTy op_ty "bvdiv" ("fp.div " ++ roundingMode)
					CAddOp -> bitVectorTy op_ty "bvadd" ("fp.add " ++ roundingMode)
					CSubOp -> bitVectorTy op_ty "bvsub" ("fp.sub " ++ roundingMode)
					CRmdOp -> bitVectorTy op_ty (unSignedTy op_ty "bvurem" "bvsrem") "fp.rem"
					CShlOp -> unSignedTy op_ty "bvshl" "bvshl"
					CShrOp -> unSignedTy op_ty "bvlshr" "bvashr"
					CAndOp -> "bvand"
					COrOp  -> "bvor"
					CXorOp -> "bvxor"
					CLndOp -> "and"
					CLorOp -> "or"
					CLeOp  -> bitVectorTy op_ty (unSignedTy op_ty "bvult" "bvslt") "fp.lt"
					CGrOp  -> bitVectorTy op_ty (unSignedTy op_ty "bvugt" "bvsgt") "fp.gt"
					CLeqOp -> bitVectorTy op_ty (unSignedTy op_ty "bvule" "bvsle") "fp.leq"
					CGeqOp -> bitVectorTy op_ty (unSignedTy op_ty "bvuge" "bvsge") "fp.geq"
					CEqOp  -> bitVectorTy op_ty "=" "fp.eq"
					other  -> error $ "op_sexpr " ++ (render.pretty) binop ++ " not implemented!"

		cconst@(CConst ctconst) -> case ctconst of
			CIntConst intconst (_,ty)  -> make_intconstant ((render.pretty) cconst) ty (getCInteger intconst)
			CCharConst cchar _         -> return $ SLeaf $ (render.pretty) cconst
			CFloatConst (CFloat f_s) (_,ty) -> return $ SExpr [ SLeaf "fp", SLeaf ("#b"++s1), SLeaf ("#b"++s2), SLeaf ("#b"++s3) ]
				where
				show_bin :: (Integral a,PrintfArg a) => Int -> a -> String
				show_bin l i = printf "%0*.*b" l l i
				(s1,s2,s3) = case fst ty of
					Z3_Float  -> (take 1 val,take 8 $ drop 1 val,take 23 $ drop 9 val) where
						val = show_bin 32 (floatToWord $ read f_s)
					Z3_Double -> (take 1 val,take 11 $ drop 1 val,take 52 $ drop 12 val) where
						val = show_bin 64 (doubleToWord $ read f_s)
					Z3_LDouble -> error "long double is not supported"
 
			CStrConst cstr _           -> return $ SLeaf $ (render.pretty) cconst

		CVar ident _ -> return $ SLeaf $ (render.pretty) ident

		CUnary CPlusOp subexpr _ -> expr2sexpr' subexpr
		CUnary op subexpr _ -> SExpr <$> sequence
			[ pure $ SLeaf op_str, expr2sexpr' subexpr ]
			where
			op_str = case op of
				CMinOp  -> bitVectorTy (extractZ3Type subexpr) "bvneg" "fp.neg"
				CCompOp -> "bvnot"
				CNegOp  -> "not"
				_ -> error $ "expr2sexpr " ++ (render.pretty) op ++ " should not occur!"

		castexpr@(CCast _ subexpr (_,to_ty)) -> do
			sexpr <- expr2sexpr' subexpr
			let from_ty = extractTypes subexpr
			case (fst from_ty,fst to_ty) of

				-- SAMECAST: identity
				( ty1, ty2 ) | ty1==ty2 -> return sexpr

				-- Casting signed to unsigned or vice versa with same size: No cast needed (Z3 interprets it)
				( Z3_BitVector size_from _, Z3_BitVector size_to _ ) | size_from==size_to -> return sexpr

				-- Casting from Bool
				( Z3_Bool, Z3_BitVector size_from _ ) -> do
					ic1 <- make_intconstant ((render.pretty) castexpr) to_ty 1
					ic0 <- make_intconstant ((render.pretty) castexpr) to_ty 0
					return $ SExpr [ SLeaf "ite", sexpr, ic1, ic0 ]

				-- Casting to Bool
				( Z3_BitVector size_from _ , Z3_Bool ) -> do
					ic <- make_intconstant ((render.pretty) castexpr) from_ty 0
					return $ SExpr [ SLeaf "not", SExpr [ SLeaf "=", sexpr, ic ]]

				-- DOWNCAST: extract bits (modulo)
				( Z3_BitVector size_from _, Z3_BitVector size_to _ ) | size_from > size_to -> 
					return $ SExpr [ SExpr [ SLeaf "_", SLeaf "extract", SLeaf (show $ size_to - 1), SLeaf "0"], sexpr ]
		
				-- UPCAST signed (to signed or unsigned): extend sign bit
				( Z3_BitVector size_from False, Z3_BitVector size_to _ ) | size_from < size_to ->
					return $ SExpr [ SExpr [ SLeaf "_", SLeaf "sign_extend", SLeaf $ show (size_to-size_from) ], sexpr ] 
		
				-- UPCAST unsigned (to signed or unsigned): extend with zeros
				( Z3_BitVector size_from True, Z3_BitVector size_to _ ) | size_from < size_to ->
					return $ SExpr [ SExpr [ SLeaf "_", SLeaf "zero_extend", SLeaf $ show (size_to-size_from) ], sexpr ]

				( Z3_Double, Z3_Float ) -> return $ SExpr [ SExpr [ SLeaf "_", SLeaf "to_fp", SLeaf "8", SLeaf "24" ],
					SLeaf roundingMode, sexpr ]

				( Z3_Float, Z3_Double ) -> return $ SExpr [ SExpr [ SLeaf "_", SLeaf "to_fp", SLeaf "11", SLeaf "53" ],
					SLeaf roundingMode, sexpr ]

				(from_ty,to_ty) -> error $ "expr2sexpr cast: " ++ show from_ty ++ " => " ++ show to_ty ++ " in " ++
					(render.pretty) castexpr ++ " " ++ " not implemented!"

		ccond@(CCond cond (Just then_expr) else_expr _) -> do
			myError $ "expr2sexpr CCond should not appear: " ++ (render.pretty) ccond
{-
			SExpr <$> sequence [
				pure $ SLeaf "ite",
				expr2sexpr' cond,
				expr2sexpr' then_expr,
				expr2sexpr' else_expr ]
-}

		cmember@(CMember _ _ _ _) -> myError $ "expr2sexpr of member " ++ (render.pretty) cmember ++ " should not occur!"
	
		ccall@(CCall _ _ _) -> myError $ "expr2sexpr of call " ++ (render.pretty) ccall ++ " should not occur!"

		other -> myError $ "expr2SExpr " ++ (render.pretty) other ++ " not implemented" 

		where

		unSignedTy ty unsigned signed = case ty of
			Z3_BitVector _ is_unsigned -> if is_unsigned then unsigned else signed
			Z3_Bool -> unsigned
			_ -> error $ "unSignedTy " ++ (render.pretty) expr ++ " is no bitvector!"
		bitVectorTy ty bv fp = case ty of
			Z3_Float -> fp
			Z3_Double -> fp
			_ -> bv
--			_ -> error $ "bitVectorTy for " ++ show operator_ty ++ " not implemented!"


data Z3_Type =
	Z3_Unit |   -- The proper type-theoretical name for C's void is "1" (i.e. "unit")
	Z3_Bool |
-- Z3_BitVector Int (is*Un*signed::Bool), hence
-- the derived ordering intentionally coincides with the type cast ordering :-)
	Z3_BitVector Int Bool |
	Z3_Float |
	Z3_Double |
	Z3_LDouble |
	Z3_Ptr Z3_Type |
	Z3_Array Z3_Type (Maybe Integer) |
	Z3_Compound SUERef CompTyKind |
	Z3_Fun Z3_Type [Z3_Type] Bool |
	Z3_FunIncomplete Z3_Type |
	Z3_VaList |
	Z3_Any
	deriving (Show,Eq,Ord,Data)

ty2Z3Type :: Type -> CovVecM (Z3_Type,Type)
ty2Z3Type ty = do
	Just IntSizes{..} <- gets sizesCVS
	z3_ty <- case ty of
		DirectType tyname _ attrs -> case tyname of
			TyVoid -> return Z3_Unit
			TyIntegral intty    -> do
				sizeofintty <- sizeofIntTy ty
				return $ Z3_BitVector sizeofintty $ intty `elem` [TyChar,TyUChar,TyUShort,TyUInt,TyULong,TyULLong]
			TyFloating floatingty -> return $ case floatingty of
				TyFloat   -> Z3_Float
				TyDouble  -> Z3_Double
				TyLDouble -> Z3_LDouble
			TyEnum _            -> return $ Z3_BitVector intSize True
			TyComp (CompTypeRef sueref comptykind _) -> return $ Z3_Compound sueref comptykind
			TyBuiltin TyVaList  -> return $ Z3_VaList
			TyBuiltin TyAny     -> return $ Z3_Any
			_ -> myError $ "ty2Z3Type " ++ (render.pretty) ty ++ " not implemented!"
		PtrType target_ty _ _ -> Z3_Ptr <$> ty2Z3TypeOnly target_ty
		ArrayType elem_ty arraysize _ _ ->
			Z3_Array <$> ty2Z3TypeOnly elem_ty <*> pure ( case arraysize of
				ArraySize _ (CConst (CIntConst cint _)) -> Just $ getCInteger cint
				_                                       -> Nothing )
		TypeDefType (TypeDefRef _ ty _) _ _ -> ty2Z3TypeOnly ty
		FunctionType (FunTypeIncomplete ret_type) _ -> Z3_FunIncomplete <$> ty2Z3TypeOnly ret_type
		FunctionType (FunType ret_type funparamdecls is_variadic) _ -> do
			let arg_types = for (map getVarDecl funparamdecls) $ \ (VarDecl _ _ ty) -> ty
			Z3_Fun <$> ty2Z3TypeOnly ret_type <*> mapM ty2Z3TypeOnly arg_types <*> pure is_variadic
--		_ -> error $ "ty2Z3Type " ++ (render.pretty) ty ++ " should not occur!"
	return (z3_ty,ty)

ty2Z3TypeOnly :: Type -> CovVecM Z3_Type
ty2Z3TypeOnly ty = ty2Z3Type ty >>= return.fst

sizeofIntTy :: Type -> CovVecM Int
sizeofIntTy ty@(DirectType tyname _ attrs) = do
	Just IntSizes{..} <- gets sizesCVS
	return $ case tyname of
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
			(TyLong,[])     -> longSize
			(TyULong,[])    -> longSize
			(TyLLong,[])    -> longLongSize
			(TyULLong,[])   -> longLongSize
			other           -> error $ "sizeofIntTy " ++ show other ++ " not implemented!"
		other -> error $ "sizeofIntTy: " ++ (render.pretty) ty ++ " is not an Integral type"
	where
	to_mode (Attr (Ident "mode" _ _) [CVar (Ident mode _ _) _] _) = mode
	to_mode attr = error $ "attrs2modes: unknown attr " ++ (render.pretty) attr

z3Ty2SExpr :: Z3_Type -> CovVecM SExpr
z3Ty2SExpr ty = case ty of
	Z3_BitVector size _      -> return $ SExpr [ SLeaf "_", SLeaf "BitVec", SLeaf (show size) ]
	Z3_Float                 -> return $ SLeaf "Float32"
	Z3_Double                -> return $ SLeaf "Float64"
	Z3_LDouble               -> return $ SLeaf "Float128"
	Z3_Bool                  -> return $ SLeaf "Bool"
	Z3_Ptr _                 -> return $ SExpr [ SLeaf "_", SLeaf "BitVec", SLeaf (show 1) ]
	Z3_Array elem_ty mb_size -> do
		(z3_inttype,_) <- _IntTypesM
		inttysexpr <- z3Ty2SExpr z3_inttype
		elem_ty_sexpr <- z3Ty2SExpr elem_ty
		return $ SExpr [ SLeaf "Array", inttysexpr, elem_ty_sexpr ]
	other                    -> myError $ "z3Ty2SExpr " ++ show other ++ " should not occur!"

type Solution = [(String,SolutionVal)]

data SolutionVal = IntVal Int | FloatVal Float | DoubleVal Double | PtrVal
instance Eq SolutionVal where
	IntVal i1    == IntVal i2    = i1==i2
	PtrVal       == PtrVal       = True
	FloatVal f1  == FloatVal f2  = abs (f2-f1) <= floatTolerance
	DoubleVal f1 == DoubleVal f2 = abs (f2-f1) <= doubleTolerance

instance Show SolutionVal where
	show (IntVal i)    = show i
	show (FloatVal f)  = show f
	show (DoubleVal f) = show f
	show PtrVal        = "<SOME_PTR>"

makeAndSolveZ3ModelM :: [Int] -> [(Ident,Z3_Type)] -> [Constraint] -> [SExpr] -> [Ident] -> String -> CovVecM (String,Maybe Solution)
makeAndSolveZ3ModelM traceid z3tyenv constraints additional_sexprs output_idents modelpathfile = do
	opts <- gets optsCVS
	let  -- prefix a "a_" for identifiers starting with underscore (Z3 does not like leading underscores...)
		(a_constraints,a_output_idents) = everywhere (mkT prefix_a) (constraints,output_idents) where
			prefix_a :: Ident -> Ident
			prefix_a (Ident s@('_':_) i ni) = Ident (safeZ3IdentifierPrefix:s) i ni
			prefix_a ident = ident
	printLogV 2 $ "output_idents = " ++ showIdents output_idents
	let
		constraints_vars = nub $ concat $ for a_constraints $ \case
			Condition _ expr -> fvar expr
			Assignment lexpr@(CIndex _ _ _) ass_expr -> fvar lexpr ++ fvar ass_expr
	printLogV 2 $ "constraints_vars = " ++ showIdents constraints_vars

	varsZ3 :: [SCompound] <- forM (filter ((`elem` (constraints_vars ++ a_output_idents)).fst) z3tyenv) $ \ (ident,ty) ->
		SExprLine <$> (SOnOneLine <$> (SExpr <$> sequence [ pure (SLeaf "declare-const"), pure (SLeaf (identToString ident)), z3Ty2SExpr ty ]))
	constraintsZ3 :: [SCompound] <- concatForM a_constraints $ \ constraint -> do
		assert_sexpr <- expr2SExpr constraint
		return $ [ SEmptyLine,
			SComment "----------------------------------------------" ] ++
			(map SComment $ lines $ show constraint) ++
			[ SComment "----------------------------------------------",
			SExprLine $ SExpr [SLeaf "assert", assert_sexpr] ]
	let
		outputvarsZ3 = for a_output_idents $ \ ident -> SExprLine $ SOnOneLine $
			SExpr [SLeaf "get-value", SExpr [ SLeaf $ identToString ident ] ]
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
	when showModels $ printLogM 0 $ "Model " ++ takeFileName modelpathfile ++ " =\n" ++ model_string_linenumbers
	printLogV 2 $ "Running model " ++ takeFileName modelpathfile ++ "..."
	(_,output,_) <- liftIO $ withCurrentDirectory (takeDirectory modelpathfile) $ do
		readProcessWithExitCode z3FilePath ["-smt2","-in","parallel.enable=true"] model_string
	printLogV 2 output
	case lines output of
		"unsat"   : _ -> return (model_string_linenumbers,Nothing)
		"unknown" : _ -> return (model_string_linenumbers,Nothing)
		"sat" : rest -> do
			sol_params <- forM (zip a_output_idents rest) $ \ (ident,line) -> do
				let is = escapeDollars $ identToString ident
				case line =~ ("\\(\\(" ++ is ++ " ([^\\)]+)\\)\\)") :: (String,String,String,[String]) of
					(_,_,_,[val_string]) -> case lookup ident z3tyenv of
						Nothing -> myError $ "Parsing z3 output: Could not find type of " ++ is
						Just ty -> return (is, case ty of
							Z3_BitVector size unsigned -> let
								'#':'x':hexdigits = val_string
								[(i :: Integer,"")] = readHex hexdigits
								in
								IntVal $ case unsigned of
									True -> fromIntegral i
									False  -> fromIntegral $ if i < 2^(size-1) then i else i - 2^size
							Z3_Float -> FloatVal $ parseFloating_fb val_string
							Z3_Double -> DoubleVal $ parseFloating_fb val_string
							Z3_LDouble -> error $ "long double is not supported"
							Z3_Ptr _ -> PtrVal
							other -> error $ "case ty2Z3Type " ++ show other ++ " not implemented" )
					_ -> myError $ "Parsing z3 output: Could not find " ++ is
			return (model_string_linenumbers,Just sol_params)
		_ -> myError $ "Execution of " ++ z3FilePath ++ " failed:\n" ++ output ++ "\n\n" ++
			"Model is\n" ++ model_string_linenumbers

escapeDollars :: String -> String
escapeDollars s = concat $ for s $ \case
	'$' -> "\\$"
	c -> [c]

{- ((_ to_fp eb sb) RoundingMode (_ FloatingPoint mb nb) (_ FloatingPoint eb sb))
  -  Float32 is a synonym for (_ FloatingPoint  8  24)
  -  Float64 is a synonym for (_ FloatingPoint 11  53)
-}
class FB_Lengths a b | a -> b where
	fb_lengths :: a -> (a,(Int,Int),(b->a),(a->b))

-- floatToWord/doubleToWord currently unused in the class
instance FB_Lengths Float Word32 where
	fb_lengths a = (a,(8,24),wordToFloat,floatToWord)

instance FB_Lengths Double Word64 where
	fb_lengths a = (a,(11,53),wordToDouble,doubleToWord)

wordToFloat :: Word32 -> Float
wordToFloat x = runST (fb_cast x)
floatToWord :: Float -> Word32
floatToWord x = runST (fb_cast x)
wordToDouble :: Word64 -> Double
wordToDouble x = runST (fb_cast x)
doubleToWord :: Double -> Word64
doubleToWord x = runST (fb_cast x)

{-# INLINE fb_cast #-}
fb_cast :: (MArray (STUArray s) a (ST s), MArray (STUArray s) b (ST s)) => a -> ST s b
fb_cast x = newArray (0::Int,0) x >>= castSTUArray >>= flip readArray 0

parseFloating_fb :: (Eq b,RealFloat a,FB_Lengths a b,Num b) => String -> a
parseFloating_fb s = case s of
	_ | "(_ NaN "  `isPrefixOf` s ->  0.0 / 0.0
	_ | "(_ +oo "  `isPrefixOf` s ->  1.0 / 0.0
	_ | "(_ -oo "  `isPrefixOf` s -> -1.0 / 0.0
	_ | "(+ zero " `isPrefixOf` s ->  0.0
	_ | "(- zero " `isPrefixOf` s -> -0.0
	_ -> f
	where
	-- Thats a funny idea: Forwarding the return type to fb_lengths' argument, so Haskell can infer the type a in order
	-- to determine which instance of FB_Lengths we have.
	-- Has someone done something like that already?
	(f,(l2,l3),from_word,_) = fb_lengths $ from_word $ sign * (2^(l2+l3-1)) + expo * (2^(l3-1)) + mantissa
	(sign,expo,mantissa) = case s =~ ("fp #([b|x][0-9a-f]+) #([b|x][0-9a-f]+) #([b|x][0-9a-f]+)") :: (String,String,String,[String]) of
		(_,_,_,[s1,s2,s3]) -> (parse_lit s1, parse_lit s2, parse_lit s3)
	parse_lit :: (Num a,Eq a) => String -> a
	parse_lit (c:s) = i
		where
		[(i,"")] = case c of
			'b' -> readInt 2 (`elem` "01") (\ c -> ord c - ord '0') s
			'x' -> readHex s


-- In case of a cutoff, mb_ret_type is Nothing.
solveTraceM :: Maybe Type -> [Int] -> Trace -> CovVecM (Either Bool ResultData)
solveTraceM mb_ret_type traceid trace = do
	printLogV 2 $ "solveTraceM " ++ show traceid ++ " ..."
	let
		tracename = show traceid
	Just retval_env  <- case mb_ret_type of
		Nothing  -> return $ Just []
		Just ret_type -> gets retEnvCVS
	Just param_env_exprs <- gets paramEnvCVS
	let
		param_env = map fst param_env_exprs
		param_names = map (fst.snd) param_env
		ret_names   = map (fst.snd.fst) retval_env
		constraints = concatMap traceitem2constr trace where
		traceitem2constr constraint@(Condition _ _) = [constraint]
		traceitem2constr constraint@(Assignment (CIndex _ _ _) _) = [constraint]
		traceitem2constr _ = []
		debug_outputs = concatMap is_debug_output trace where
			is_debug_output (DebugOutput name expr) = [(name,expr)]
			is_debug_output _ = []
		(debug_idents,debug_constraints,debug_tyenv) = unzip3 $ for (zip [1..] debug_outputs) $ \ (i,(name,expr)) ->
			let name_id = internalIdent (name ++ "_" ++ show i) in
			(name_id,Condition Nothing $ CBinary CEqOp (CVar name_id (annotation expr)) expr (annotation expr),(name_id,extractZ3Type expr))

	tyenv1 <- tyEnvFromTraceM trace

	(model_string,mb_sol) <- makeAndSolveZ3ModelM
		traceid
		(tyenv1 ++ debug_tyenv)
		(constraints ++ debug_constraints)
		(concat $ for param_env_exprs $ \ ((_,(name,_)),expr) -> case extractZ3Type expr of
			Z3_Float -> []
			Z3_Double -> []
			Z3_LDouble -> []
			_ -> [ SExpr [SLeaf "minimize",SLeaf (identToString name)] ])
		(param_names ++ ret_names ++ debug_idents)
		(analyzerPath </> "models" </> "model_" ++ tracename ++ ".smtlib2")

	return $ case mb_ret_type of
		Nothing -> Left $ isJust mb_sol
		Just _ -> Right (model_string,case mb_sol of
			Nothing -> Nothing
			Just sol -> Just (param_env,map fst retval_env,sol))


tyEnvFromTraceM :: Trace -> CovVecM [(Ident,Z3_Type)]
tyEnvFromTraceM trace = forM (createTyEnv trace) $ \ (e,t) -> do
	z3_t <- ty2Z3TypeOnly t
	return (e,z3_t)

checkSolutionM :: [Int] -> ResultData -> CovVecM ResultData
checkSolutionM _ resultdata | not checkSolutions = return resultdata
checkSolutionM traceid resultdata@(_,Nothing) = do
	printLogM 2 $ "No solution to check for " ++ show traceid
	return resultdata
checkSolutionM traceid resultdata@(_,Just (_,_,[])) = do
	printLogM 2 $ "Empty solution cannot be checked for " ++ show traceid
	return resultdata
checkSolutionM traceid resultdata@(_,Just (param_env0,ret_env0,solution)) = do
	let
		param_env = filter envItemNotPtrType param_env0
		ret_env = filter envItemNotPtrType ret_env0
	printLogV 8 $ "checkSolution param_env =\n" ++ showEnv param_env
	printLogV 8 $ "checkSolution ret_env =\n" ++ showEnv ret_env
	srcfilename <- gets srcFilenameCVS
	Just filename <- gets checkExeNameCVS
	absolute_filename <- liftIO $ makeAbsolute srcfilename
	let
		args = concat $ for param_env $ \ (_,(newident,ty)) -> case ty of
			DirectType _ _ _ -> case lookup (identToString newident) solution of
				Just v -> [show v]
			ty -> error $ "checkSolutionM args: type " ++ (render.pretty) ty ++ " not implemented!"
	printLogV 2 $ "checkSolution args = " ++ show args
	(stdout,stderr) <- runHereM (takeDirectory absolute_filename) (takeFileName filename) args
	printLogV 2 $ "stdout=\n" ++ stdout ++ "\n------------" 

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
					DirectType (TyIntegral _) _ _       -> IntVal $ read s
					DirectType (TyFloating floatty) _ _ -> case floatty of
						TyFloat  -> FloatVal  $ read s
						TyDouble -> DoubleVal $ read s
					DirectType (TyEnum _) _ _           -> IntVal $ read s
					_ -> error $ "checkSolutionM: parsing type " ++ (render.pretty) ty ++ " of " ++ ident_s ++ " not implemented!"
				when (exec_result /= predicted_result) $ do
					let txt = "ERROR in " ++ show traceid ++ " for " ++ ident_s ++ " : exec_val=" ++ show exec_result ++ " /= predicted_result=" ++ show predicted_result
					myError txt

	printLogV 2 $ "checkSolutionM " ++ show traceid ++ " OK."
	return resultdata
