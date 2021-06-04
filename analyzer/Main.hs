{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE
	PackageImports,RecordWildCards,FunctionalDependencies,MultiParamTypeClasses,
	QuasiQuotes,UnicodeSyntax,LambdaCase,ScopedTypeVariables,TupleSections,
	TypeSynonymInstances,FlexibleInstances,FlexibleContexts,StandaloneDeriving,
	DeriveDataTypeable,DeriveGeneric,PatternGuards,UndecidableInstances,Rank2Types,NamedFieldPuns #-}

-- https://rise4fun.com/Z3/tutorial/strategies

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
import Control.Monad.Trans.State.Lazy
import Prelude.Unicode ((∧),(∨))
import Text.Printf
import Text.Regex.TDFA
import Text.Regex.TDFA.String
import Numeric (readHex,readInt)
import Data.Either
import Data.Ord (comparing)
import Control.Monad.IO.Class (liftIO,MonadIO)
import Data.Generics
import qualified Data.Map.Strict as Map
import Text.PrettyPrint
import Data.Time
import Data.Time.LocalTime (diffLocalTime)
import Data.Foldable
import Data.List
import Data.Maybe
import System.IO
import Data.Char
import Data.Numbers.FloatingHex (showHFloat)

-- This is for conversion of Z3 floats to Haskell Floating Point
import Data.Word (Word32,Word64)
import Data.Bits
import Data.Array.ST (newArray,readArray,MArray,STUArray)
import Data.Array.Unsafe (castSTUArray)
import GHC.ST (runST,ST)

import DataTree
import GlobDecls
import Logging

--------------

fastMode = True

z3TimeoutSecs :: Maybe Int = Just $ 2*60

reachFixedTrace :: Maybe [Int] = Nothing --Just [2,1,2,2,1,1,3,2,1,2,2,1,2,1]

subfuncovOpt = "-subfuncov"
noHaltOnVerificationErrorOpt = "-nohalt"
noIndentLogOpt = "-noindentlog"
findModeOpt = "-findmode"
minimizeOpt = "-minimize"
branchCovOpt = "-branchcov"
htmlLogOpt = "-htmllog"
showModelsOpt = "-showmodels"
writeModelsOpt = "-writemodels"
cutoffsOpt = "-cutoffs"
noLoopInferenceOpt = "-noloopinference"

main :: IO ()
main = do
	-- when there is an error, we'd like to have *all* output till then
	hSetBuffering stdout NoBuffering

	starttime <- printDateTime
	writeFile logFileTxt (show starttime ++ "\n\n")
	writeFile solutionsFile (show starttime ++ "\n\n")

	gcc:funname:opts_filenames <- getArgs >>= return . \case
		[] → "gcc" : "roundf_test" : (map ((analyzerPath++"\\")++) ["tvg_roundf_test.c"]) ++ [noHaltOnVerificationErrorOpt,cutoffsOpt,subfuncovOpt]
--		[] → "gcc" : "roundf" : (map ((analyzerPath++"\\knorr\\dinkum\\")++) ["tvg_roundf.c"]) ++ [noHaltOnVerificationErrorOpt,cutoffsOpt,subfuncovOpt]
--		[] → "gcc" : "ceilf" : (map ((analyzerPath++"\\knorr\\dinkum\\")++) ["tvg_ceilf.i"]) ++ [noHaltOnVerificationErrorOpt,cutoffsOpt,subfuncovOpt]
--		[] → "gcc" : "fmin" : (map ((analyzerPath++"\\knorr\\dinkum\\")++) ["tvg_fmax.c"]) ++ [subfuncovOpt]
--		[] → "gcc" : "sqrtf" : (analyzerPath++"\\test.c") : [subfuncovOpt,writeModelsOpt,noIndentLogOpt] --["-writeGlobalDecls"]
--		[] → "gcc" : "_FDscale" : (analyzerPath++"\\test.c") : [cutoffsOpt] --["-writeGlobalDecls"]

--		[] → "gcc" : "_FDtest" : (map ((analyzerPath++"\\knorr\\dinkum\\")++) ["tvg_fabsf.c"]) ++ [htmlLogOpt,showModelsOpt,writeModelsOpt]
--		[] → "gcc" : "f" : (analyzerPath++"\\switchtest.c") : [htmlLogOpt,writeModelsOpt,showModelsOpt,noLoopInferenceOpt] --"-writeAST","-writeGlobalDecls"]
--		[] → "gcc" : "_fpdiv_parts" : (analyzerPath++"\\myfp-bit_mul.c") : [cutoffsOpt,htmlLogOpt,writeModelsOpt] --"-writeAST","-writeGlobalDecls"]

--		[] → "gcc" : "sqrtf" : (analyzerPath++"\\sqrtf.c") : [subfuncovOpt,writeModelsOpt] --["-writeGlobalDecls"]
--		[] → "gcc" : "_FDunscale" : (analyzerPath++"\\test.c") : [noHaltOnVerificationErrorOpt,showModelsOpt,writeModelsOpt,subfuncovOpt,noIndentLogOpt,cutoffsOpt] --["-writeGlobalDecls"]

--		[] → "gcc" : "f" : (analyzerPath++"\\sideffectstest.c") : [writeModelsOpt] --["-writeGlobalDecls"]
--		[] → "gcc" : "_FDnorm" : (analyzerPath++"\\test.c") : [writeModelsOpt,subfuncovOpt] --["-writeGlobalDecls"]
--		[] → "gcc" : "_FDnorm" : (map ((analyzerPath++"\\knorr\\dinkum\\")++) ["tvg_sqrtf.c"]) ++ ["-writemodels"]
--		[] → "gcc" : "_Sinx" : (analyzerPath++"\\OscarsChallenge\\sin\\oscar.i") : [cutoffsOpt] --"-writeAST","-writeGlobalDecls"]
--		[] → "gcc" : "f" : (analyzerPath++"\\nesttest.c") : [writeModelsOpt,"-writeAST"] --["-writeGlobalDecls"]

--		[] → "gcc" : "f" : (analyzerPath++"\\loopmcdctest.c") : ["-writemodels"] --["-writeAST","-writeGlobalDecls"]

--		[] → "gcc" : "_FDint" : (map ((analyzerPath++"\\knorr\\dinkum\\")++) ["tvg_roundf.c"]) ++ ["-writemodels",noIndentLogOpt]
--		[] → "gcc" : "__udiv6432" : (analyzerPath++"\\knorr\\libgcc\\tvg_udiv6432.c") : ["-writemodels"]

--		[] → "gcc" : "_FDint" : (analyzerPath++"\\knorr\\dinkum\\tvg_xfdint.c") : [showModelsOpt,writeModelsOpt]
--		[] → "gcc" : "_FDtest" : (map ((analyzerPath++"\\knorr\\dinkum\\")++) ["tvg_fabsf.c"]) ++ [writeModelsOpt,subfuncovOpt,noIndentLogOpt]
--		[] → "gcc" : "fabsf" : (map ((analyzerPath++"\\knorr\\dinkum\\")++) ["tvg_fabsf.c"]) ++ ["-writemodels",noIndentLogOpt]

--		[] → "gcc" : "f" : (analyzerPath++"\\mcdctest.c") : ["-writemodels"] --["-writeAST","-writeGlobalDecls"]
--		[] → "gcc" : "_Dtest" : (analyzerPath++"\\xdtest.c") : ["-writemodels"]

--		[] → "gcc" : "f" : (analyzerPath++"\\covsubfuntest.c") : [] --["-writeGlobalDecls"]
--		[] → "gcc" : "f" : (analyzerPath++"\\arraytest3.c") : ["-writemodels"] --"-writeAST","-writeGlobalDecls"]
--		[] → "gcc" : "f" : (analyzerPath++"\\checkvarsdefinedtest.c") : ["-writemodels"] --["-writeAST","-writeGlobalDecls"]
--		[] → "gcc" : "f" : (analyzerPath++"\\commatest.c") : []

--		[] → "gcc" : "f" : (analyzerPath++"\\arraytest.c") : ["-writemodels"] --"-writeAST","-writeGlobalDecls"]

--		[] → "gcc" : "_Dtest" : (analyzerPath++"\\knorr\\dinkum\\xdtest.i") : ["-writemodels",noHaltOnVerificationErrorOpt]
--		[] → "gcc" : "f" : (analyzerPath++"\\arraytest2.c") : ["-MCDC","-writemodels"] --"-writeAST","-writeGlobalDecls"]
--		[] → "gcc" : "f" : (analyzerPath++"\\mcdcsubfunctiontest.c") : [subfuncovOpt] --["-writeAST","-writeGlobalDecls"]
--		[] → "gcc" : "sqrtf" : (analyzerPath++"\\knorr\\libgcc") : []
--		[] → "gcc" : "f" : (analyzerPath++"\\uniontest.c") : [] --["-writeAST","-writeGlobalDecls"]
--		[] → "gcc" : "_Dtest" : (analyzerPath++"\\OscarsChallenge\\sin\\xdtest.c") : ["-writemodels"] --["-writeAST","-writeGlobalDecls"]
--		[] → "gcc" : "f" : (analyzerPath++"\\conditionaltest.c") : ["-writemodels"] --["-writeAST","-writeGlobalDecls"]
--		[] → "gcc" : "f" : (analyzerPath++"\\floattest.c") : [] --,"-exportPaths" "-writeAST","-writeGlobalDecls"]
--		[] → "gcc" : "f" : (analyzerPath++"\\decltest.c") : [] --,"-exportPaths" "-writeAST","-writeGlobalDecls"]
--		[] → "gcc" : "_fpmul_parts" : (analyzerPath++"\\myfp-bit_mul.c") : []
--		[] → "gcc" : "f" : (analyzerPath++"\\fortest.c") : [] --"-writeAST","-writeGlobalDecls"]
--		[] → "gcc" : "f" : (analyzerPath++"\\iffuntest.c") : [] --["-writeAST","-writeGlobalDecls"]
--		[] → "gcc" : "_fpdiv_parts" : (analyzerPath++"\\whiletest2.c") : [] --"-writeAST","-writeGlobalDecls"]
--		[] → "gcc" : "f" : (analyzerPath++"\\branchtest.c") : ["-writeTree"] --["-writeAST","-writeGlobalDecls"]
--		[] → "gcc" : "f" : (analyzerPath++"\\iftest.c") : [] --["-writeAST","-writeGlobalDecls"]
--		[] → "gcc" : "f" : (analyzerPath++"\\deadtest.c") : [] --["-writeAST","-writeGlobalDecls"]
--		[] → "gcc" : "f" : (analyzerPath++"\\whiletest.c") : ["-writemodels"] --["-writeAST","-writeGlobalDecls"]
--		[] → "gcc" : "f" : (analyzerPath++"\\ptrtest_flat.c") : ["-writeAST"]
--		[] → "gcc" : "f" : (analyzerPath++"\\ptrtest.c") : [] --["-writeAST"]
--		[] → "gcc" : "g" : (analyzerPath++"\\assigntest.c") : [] --["-writeAST","-writeGlobalDecls"]
--		[] → "gcc" : "g" : (analyzerPath++"\\ptrrettest.c") : [] --["-writeAST","-writeGlobalDecls"]
		args → args

	let
		opts = filter ("-" `isPrefixOf`) opts_filenames
		filenames = opts_filenames \\ opts

		parse_filearg :: [String] → IO [CExtDecl]
		parse_filearg filenames = concatForM filenames $ \ filename → do
			isdir <- doesDirectoryExist filename
			case isdir of
				True → do
					files <- listDirectory filename
					parse_filearg $ map (filename</>) files
				False → case takeExtension filename `elem` [".c",".i"] of
					False → return []
					True → do
						putStr $ "\rParsing " ++ filename ++ "...                                           "
						ast <- case takeExtension filename `elem` [".i"] of
							True  → parseCFilePre filename
							False → parseCFile (newGCC gcc) Nothing [] filename
						case ast of
							Left err → myErrorIO $ show err
							Right translunit@(CTranslUnit extdecls _) → do
								when ("-writeAST" `elem` opts) $
									writeFile (filename <.> "ast.html") $ genericToHTMLString translunit
								return extdecls

	let init_msg =
		"Compiler: " ++ gcc ++ "\n" ++
		"Function: " ++ funname ++ "\n" ++
		"Source files: " ++ show filenames ++ "\n" ++
		"Options: " ++ show opts
	printToSolutions init_msg
	printLog 0 $ init_msg

	extdecls <- parse_filearg filenames

{-
	let
		dumpdeclr (CDeclr (Just ident) _ _ _ _) = (render.pretty) ident
		dump = for extdecls $ \case
			CDeclExt (CDecl _ declrs ni) → ("CDeclExt " ++ intercalate " / " (for declrs $ \ (Just declr,_,_) → dumpdeclr declr),show ni)
			CFDefExt (CFunDef _ declr argdecls _ ni) → ("CFDefExt " ++ dumpdeclr declr,show ni)
			CAsmExt cstringlit ni → ("AsmExt " ++ (render.pretty) cstringlit,show ni)
	printLog 0 $ unlines $ map (\(s,l)→s ++ " at " ++ l) $ sort dump
-}
	let translunit = CTranslUnit (nubBy same_ext_decl extdecls) undefNode
		where
		same_ext_decl :: CExtDecl → CExtDecl → Bool
		same_ext_decl extdecl1 extdecl2 = (render.pretty) extdecl1 == (render.pretty) extdecl2

	when checkSolutions $ writeFile allFileName $ (render.pretty) translunit

	printLog 0 "\rAnalyzing the merged AST...                                    "
	case runTrav_ $ do
		res <- analyseAST translunit
		deftable <- getDefTable
		return (res,deftable) of
		Left errs → do
			let errs_msg = "ERRORS:\n" ++ unlines (map show errs)
			myErrorIO $ errs_msg
		Right ((globdecls,deftable),soft_errors) → do
			when (not $ null soft_errors) $ putStrLn "Soft errors:" >> forM_ soft_errors print
			when ("-writeGlobalDecls" `elem` opts) $
				writeFile "globdecls.html" $ globdeclsToHTMLString globdecls

			let coveragekind = if branchCovOpt `elem` opts then Branch_Coverage else MCDC_Coverage
			(every_branch_covered,s) <- runStateT covVectorsM $
				CovVecState globdecls 1 allFileName Nothing funname [] gcc opts
					Nothing ([],[]) [] intialStats Nothing Nothing deftable (-1) coveragekind 0 [] starttime starttime

			let
				(testvectors,covered) = analysisStateCVS s

				alls = allCondPointsCVS s

			printLog 0 "\n"

			let deaths = alls \\ covered

			printLog 0 $ "\n###### FINAL RESULT #######\n\n"

			printLog 0 $ show (statsCVS s) ++ "\n"

			forM_ testvectors $ \ (traceid,trace,branches,(model_string,mb_solution)) → do
				case not showOnlySolutions || maybe False (not.null.(\(_,_,b)→b)) mb_solution of
					False → return ()
					True → printLog 0 $ unlines $ mbshowtraces (
						[ "","=== TRACE " ++ show traceid ++ " ========================","<leaving out builtins...>" ] ++
						[ showLine (reverse trace) ] ++
						[ "",
						"--- MODEL " ++ show traceid ++ " -------------------------",
						model_string,
						"" ]) ++
						[ "--- SOLUTION " ++ show traceid ++ " ----------------------",
						show_solution funname mb_solution,
						""]
						where
						mbshowtraces ts = if showTraces then ts else []

			printLog 0 $ "\n===== SUMMARY =====\n\n"
			
			printCondPoints alls

			printLog 0 $ "\nCoverage to reach: " ++ show coveragekind ++ "\n"

			forM_ testvectors $ \ (traceid,trace,branches,(model,Just v)) → do
				printLog 0 $ "Test Vector " ++ show traceid ++ " covering "
				forM_ branches $ \ branch → printLog 0 $ "    " ++ showBranch branch
				printLog 0 $ "\n    " ++ showTestVector funname v ++ "\n"
			forM_ deaths $ \ branch → do
				let msg = "\n--------------------------\n\nDEAD " ++ showBranch branch ++ "\n"
				printLog 0 msg
				printToSolutions msg

			printLog 0 $ "Full path coverage: " ++ show every_branch_covered ++ "\n"
			when (every_branch_covered && not (null deaths)) $ printLog 0 $ "There is unreachable code, in spite of full path coverage."

			let
				errs = verificationErrsCVS s
				coverage_kind_txt = show coveragekind
				msg = "\n================================\n\n" ++ case (errs>0) of
					True → "VERIFICATION ERRORS: " ++ show errs ++ "\n"
					False → case null deaths of
						False → "FAIL, there are " ++ coverage_kind_txt ++ " gaps!\n"
						True  → "OK, we have full " ++ coverage_kind_txt ++ ".\n"
			printToSolutions msg
			printLog 0 msg

			(_,total_time_s) <- diffToCurrentTime starttime
			printLog 0 $ "\nTotal run time: " ++ total_time_s

			when (htmlLogOpt `elem` opts) $ do
				printConsole 0 "Creating HTML Log..."
				createHTMLLog

			printConsole 0 $ "\nEnd."

outputVerbosity = if fastMode then 1 else 2
logFileVerbosity = if fastMode then 0 else 10

mAX_REN_LIST_LENGTH = 3

showInitialTrace = True && not fastMode
showOnlySolutions = True
showTraces = True && not fastMode
showFinalTrace = True && not fastMode
checkSolutions = True
returnval_var_name = "return_val"
floatTolerance = 1e-7 :: Float
doubleTolerance = 1e-10 :: Double
showBuiltins = False
logToFile = not fastMode
mainFileName = "main.c"
printTypes = False
printLocations = False
errorModelPath = analyzerPath </> "models"

mAX_UNROLLS = 3
uNROLLING_STRATEGY = [0..mAX_UNROLLS]

sizeConditionChunks = 4

-------------

type Trace = [TraceElem]
type ResultData = (String,Maybe (Env,Env,Solution))
type TraceAnalysisResult = ([Int],Trace,[Branch],ResultData)
type UnfoldTracesRet = Either [(Int,Progress,[Env],Trace)] Bool
type SolveFunRet = (Bool,([TraceAnalysisResult],[Branch]))

for :: [a] → (a → b) → [b]
for = flip map

concatForM :: (Monad m) => [a] → (a → m [b]) → m [b]
concatForM = flip concatMapM

{-
once :: MonadPlus m => GenericM m → GenericM m
once f x = f x `mplus` gmapMo (once f) x
-}

solverFindMagicString = "solver_find() encountered!"

isOptionSet :: String → CovVecM Bool
isOptionSet optname = do
	opts <- gets optsCVS
	return $ optname `elem` opts

whenOptionSet :: String → Bool → CovVecM () → CovVecM ()
whenOptionSet opt_s target action = do
	opt_set <- isOptionSet opt_s
	case (opt_set && target) || (not opt_set && not target) of
		True  → action
		False → return ()

------------------------

roundingMode = "roundNearestTiesToEven"

intType = integral TyInt
uLongType = integral TyULong
uLongLongType = integral TyULLong
charType = integral TyChar
ptrType to_ty = PtrType to_ty noTypeQuals noAttributes
floatType = DirectType (TyFloating TyFloat) noTypeQuals noAttributes
doubleType = DirectType (TyFloating TyDouble) noTypeQuals noAttributes

sizeToIntTypeM :: Int -> CovVecM Type
sizeToIntTypeM size = do
	Just MachineSpec{..} <- gets machineSpecCVS
	case size of
		8                     → return $ integral TyUChar
		16                    → return $ integral TyUShort
		32 | intSize==32      → return $ integral TyUInt
		64 | intSize==64      → return $ integral TyUInt
		64 | longSize==64     → return $ integral TyULong
		64 | longLongSize==64 → return $ integral TyULLong
		other                 → myError $ "sizeToIntTypeM " ++ show other ++ " : No integral type available for this size!"

z3FilePath = "C:\\z3\\bin\\z3.exe"

analyzerPath = "analyzer"
logFile = analyzerPath </> "log"
logFileTxt = logFile <.> "txt"
logFileHtml = logFile <.> "html"
allFileName = analyzerPath </> "all" <.> "c"
solutionsFile = analyzerPath </> "solutions.txt"

------------------------

compileHereM :: [String] → String → String → CovVecM (String,String)
compileHereM args filename src = do
	liftIO $ writeFile filename src
	gcc <- gets compilerCVS
	runHereM (takeDirectory filename) gcc args

runHereM :: String → String → [String] → CovVecM (String,String)
runHereM rundir exefilename args = liftIO $ withCurrentDirectory rundir $ do
	(retcode,sout,serr) <- readProcessWithExitCode (takeFileName exefilename) args ""
	case retcode of
		ExitFailure exitcode → myErrorIO $
			"ExitCode " ++ show exitcode ++ " of runHereM " ++ exefilename ++ "\n" ++ sout ++ serr
		ExitSuccess → return (sout,serr)

data Endianness = Little | Big deriving (Read,Show)
data MachineSpec = MachineSpec {
	intSize::Int, longSize::Int, longLongSize::Int,
	endianness::Endianness
	} deriving (Read,Show)

find_out_sizes_name = "find_out_sizes" :: String

find_out_sizesM :: CovVecM MachineSpec
find_out_sizesM = do
	let
		srcfilename = find_out_sizes_name <.> "c"
		exefilename = find_out_sizes_name <.> "exe"
	compileHereM ["-o",exefilename,srcfilename] srcfilename (PPM.prettyCompact $ PPMC.ppr find_out_sizes_src)
	(sizes_s,"") <- runHereM "." exefilename []
	let sizes = read sizes_s
	printLogV 1 $ show sizes
	return sizes
	where
	incl_stdio = "#include <stdio.h>"

	find_out_sizes_src = [cunit|
$esc:incl_stdio

int main(void)
{
    char* little = "Little";
    char* big = "Big";
    char* endianness;
    unsigned char arr[2];
    *((unsigned short *)arr) = 255;
    // big endian means MSB is stored at smallest address.
    if(arr[0]==255 && arr[1]==0) endianness=little; else { if(arr[0]==0 && arr[1]==255) endianness=big; else
    	{ printf ("ERROR: Could not determine Endianness!\n"); return(1); } }
    printf("MachineSpec { intSize=%i, longSize=%i, longLongSize=%i, endianness=%s }\n",
        sizeof(int)*8,sizeof(long int)*8,sizeof(long long)*8,endianness);
    return 0;
}
|]

-- Z3 does not accept identifiers starting with an underscore, so we prefix these with an "a"
safeZ3IdentifierPrefix = 'a'

----------------
-- This is just for convenience in the logWrapper combinator...

class (Show a) => LogRender a where
	ren :: a → String
instance LogRender [Env] where
	ren (e1:_) = "<envs>"
instance LogRender [([Env],CExprWithType,Trace)] where
	ren _ = "<translateexpr result>"
instance {-# OVERLAPPABLE #-} (Show a) => LogRender a where
	ren a = show a
instance LogRender String where
	ren s = s
instance (LogRender a) => LogRender (Maybe a) where
	ren Nothing  = "Nothing"
	ren (Just a) = "Just " ++ ren a
instance {-# OVERLAPPABLE #-} (LogRender a,LogRender b) => LogRender (a,b) where
	ren (a,b) = "(" ++ ren a ++ "," ++ ren b ++ ")"
instance {-# OVERLAPPABLE #-} (LogRender a) => LogRender [a] where
	ren as | length as > mAX_REN_LIST_LENGTH = "[ " ++ intercalate "," (map ren $ take mAX_REN_LIST_LENGTH as) ++ " ... ]"
	ren as = "[ " ++ intercalate "," (map ren as) ++ " ]"
instance (LogRender a,LogRender b) => LogRender (Either a b) where
	ren (Left a)  = "Left " ++ ren a
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
instance LogRender Branch where
	ren branch = show branch

-------------------

printLog :: Int → String → IO ()
printLog verbosity text = do
	printLogInd verbosity text (text++"\n")

printConsole :: (MonadIO m) => Int → String → m ()
printConsole verbosity text = do
	when (verbosity<=outputVerbosity) $ liftIO $ putStr text

printLogInd :: Int → String → String → IO ()
printLogInd verbosity text ind_text = do
	printConsole verbosity (text++"\n")
	when (logToFile && verbosity<=logFileVerbosity) $ appendFile logFileTxt ind_text

printLogM :: Int → String → CovVecM ()
printLogM verbosity text = do
	indent <- gets logIndentCVS
	let ind_prefix = concat (replicate indent indentPrefix)
	ind_text <- isOptionSet noIndentLogOpt >>= \case
		False → return $ unlines $ map (ind_prefix++) $ lines text
		True → return text
	liftIO $ printLogInd verbosity text ind_text

printLogV :: Int → String → CovVecM ()
printLogV verbosity text = printLogM verbosity text

printDateTime :: IO LocalTime
printDateTime = do
	current_time <- getCurrentTime
	time_zone <- getCurrentTimeZone
	let local_time = utcToLocalTime time_zone current_time
	printConsole 0 $ show local_time ++ "\n"
	return local_time

diffToCurrentTime :: LocalTime -> IO (LocalTime,String)
diffToCurrentTime last_time = do
	current_time <- printDateTime
	timezone <- getCurrentTimeZone
	return (current_time, formatTime defaultTimeLocale "%2H:%2M:%2S" (diffLocalTime current_time last_time))

printDateTimeM :: Int -> CovVecM ()
printDateTimeM verbosity = do
	last_time <- gets lastTimeCVS
	(current_time,duration_s) <- liftIO $ diffToCurrentTime last_time
	printLogV verbosity $ "Last Duration (hour:min:sec): " ++ duration_s
	modify $ \ s -> s { lastTimeCVS = current_time }
	return ()

createHTMLLog :: IO ()
createHTMLLog = do
	log <- readFile logFileTxt
	writeHTMLLog logFileHtml log

myErrorIO :: forall a . String → IO a
myErrorIO txt = do
	printLog 0 txt
	createHTMLLog
	error txt

myError :: forall a . String → CovVecM a
myError txt = do
	printLogM 0 txt
	liftIO $ createHTMLLog
	error txt

indentLog :: Int → CovVecM ()
indentLog d = modify $ \ s → s { logIndentCVS = logIndentCVS s + d }

logWrapper :: (LogRender a) => [String] → CovVecM a → CovVecM a
logWrapper args m = do
	let verbosity = 10
	indentLog 1
	printLogV verbosity $ intercalate " " args
	ret <- m
--	printLogV verbosity $ "RESULT = " ++ ren ret
	indentLog (-1)
	return ret

showLine :: Trace → String
showLine trace = unlines $ map show (filter isnotbuiltin trace)

show_solution _ Nothing = "No solution"
show_solution _ (Just (_,_,[])) = "Empty solution"
show_solution funname (Just v) = unlines [ showTestVector funname v ]

showEnv :: Env → String
showEnv ϵ = "{\n    " ++ intercalate " ,\n    " (map (render.pretty) ϵ) ++ "\n    }"

showTyEnv :: TyEnv → String
showTyEnv tyenv = "{\n    " ++ intercalate " ,\n    " (map (render.pretty) tyenv) ++ "\n    }"

showIdents :: [Ident] → String
showIdents idents = "[ " ++ intercalate ", " (map (render.pretty) idents) ++ " ]"

showTestVector :: String → (Env,Env,Solution) → String
showTestVector funname (ϵ,ret_env,solution) = funname ++ " ( " ++ intercalate " , " (map showarg ϵ) ++ " )" ++
	"\n    = " ++ intercalate " , " (map showarg ret_env)
	where
	showarg :: EnvItem → String
	showarg (oldident,(newident,_)) =
		identToString oldident ++ " = " ++ case lookup (identToString newident) solution of
			Nothing  → "DONT_CARE"
			Just val → show val


data CovVecState = CovVecState {
	globDeclsCVS     :: GlobalDecls,
	newNameIndexCVS  :: Int,
	srcFilenameCVS   :: String,
	checkExeNameCVS  :: Maybe String,
	funNameCVS       :: String,
	funStartEndCVS   :: [(Location,Location)],
	compilerCVS      :: String,
	optsCVS          :: [String],
	paramEnvCVS      :: Maybe [(EnvItem,CExprWithType)],
	analysisStateCVS :: ([TraceAnalysisResult],[Branch]),
	allCondPointsCVS :: [Branch],
	statsCVS         :: Stats,
	retEnvCVS        :: Maybe [(EnvItem,CExprWithType)],
	machineSpecCVS   :: Maybe MachineSpec,
	defTableCVS      :: DefTable,
	logIndentCVS     :: Int,
	coverageKindCVS  :: CoverageKind,
	verificationErrsCVS :: Int,
	progressCVS      :: [(Int,Int)],
	startTimeCVS     :: LocalTime,
	lastTimeCVS      :: LocalTime
	}

data Stats = Stats {
	cutoffTriesS :: Int, cutoffsS :: Int, numTracesS :: Int,
	numSolutionS :: Int, numNoSolutionS :: Int }
	deriving (Show)
intialStats = Stats 0 0 0 0 0
incCutoffTriesM :: CovVecM ()
incCutoffTriesM = modify $ \ s → s { statsCVS = (statsCVS s) { cutoffTriesS = cutoffTriesS (statsCVS s) + 1 } }
incCutoffsM :: CovVecM ()
incCutoffsM = modify $ \ s → s { statsCVS = (statsCVS s) { cutoffsS = cutoffsS (statsCVS s) + 1 } }
incNumNoSolutionM = modify $ \ s → s { statsCVS = (statsCVS s) { numNoSolutionS = numNoSolutionS (statsCVS s) + 1 } }
incNumSolutionM = modify $ \ s → s { statsCVS = (statsCVS s) { numSolutionS = numSolutionS (statsCVS s) + 1 } }
incNumTracesM :: CovVecM ()
incNumTracesM = modify $ \ s → s { statsCVS = (statsCVS s) { numTracesS = numTracesS (statsCVS s) + 1 } }
printStatsM :: Int -> CovVecM ()
printStatsM verbosity = gets statsCVS >>= (printLogV verbosity) . show

type CovVecM = StateT CovVecState IO

data AssignmentKind = Normal CExprWithType | ArrayUpdate Ident Ident Types CExprWithType
	deriving Data
instance Pretty AssignmentKind where
	pretty (Normal expr) = pretty expr
	pretty (ArrayUpdate ident1 ident2 _ index) = pretty ident1 <> text "->" <> pretty ident2 <> brackets (pretty index)

data TraceElem =
	Assignment AssignmentKind CExprWithType |
	-- Left is Identity, Right is C-Equality
	Condition (Either (CExprWithType,CExprWithType) (Branch,CExprWithType)) |
	NewDeclaration (Ident,Type) |
	Return CExprWithType |
	DebugOutput String CExprWithType |
	SolverFind
	deriving Data

data Branch = NoBranch | Branch { branchLocation::Location, numBranch::Int, isElseBranch::Bool, nameBranch::String }
	deriving (Data,Show,Read)
-- Order decision points first by location, then by number, and last by the direction (Then/Else)

-- For equality for Branches, the branch's name should be irrelevant (it is only given for documentation purposes)
instance Eq Branch where
	(Branch loc1 num1 iselse1 _) == (Branch loc2 num2 iselse2 _) = loc1==loc2 && num1==num2 && iselse1==iselse2
	NoBranch == NoBranch = True
	_ == _ = False
-- Ord instance is needed for Set.Set difference
instance Ord Branch where
	(Branch loc1 num1 iselse1 _) <= (Branch loc2 num2 iselse2 _) =
		loc1<loc2 || ((loc1==loc2) && (num1<num2 || ((num1==num2) && (iselse1<=iselse2))))
	NoBranch <= _ = True
showBranch (Branch loc num is_else name) = (if is_else then "Else" else "Then") ++ " branch " ++ show num ++ " " ++ show name ++ " at " ++ showLocation loc
showBranch NoBranch = "NoBranch"

makeCondBranchName cond = (render.pretty) cond ++ " ? _ : _"
makeIfBranchName cond = "if(" ++ (render.pretty) cond ++ ")"
makeForWhileBranchName cond = "for/while(" ++ (render.pretty) cond ++ ")"
makeDefaultBranchName = "default"
makeCaseBranchName cond = "case " ++ (render.pretty) cond

printCondPoints :: (MonadIO m) => [Branch] → m ()
printCondPoints decisionpoints = liftIO $ do
	printLog 0 $ "All decision point outcomes:"
	case null decisionpoints of
		True  → printLog 0 "<none>"
		False → forM_ decisionpoints $ \ decisionpoint →
			printLog 0 $ "    " ++ showBranch decisionpoint


data CoverageKind = Branch_Coverage | MCDC_Coverage
	deriving (Show,Eq)

data MCDC_Branch = MCDC_Branch { nameMCDCB::String, resultMCDCB::Bool, condMCDCB::CExpr }
instance Show MCDC_Branch where
	show (MCDC_Branch n r c) = n ++ " = " ++ show r ++ " (" ++ (render.pretty) c ++ ")"

createMCDCTables :: CExpr → [MCDC_Branch]
createMCDCTables expr = case expr of
	CBinary binop expr1 expr2 _ | binop `elem` [CLorOp,CLndOp] →
		[ MCDC_Branch ("(" ++ name1 ++ (render.pretty) binop ++ to_dontcare name2 ++ ")") shortcut cond1 |
			MCDC_Branch name1 result1 cond1 <- t1, result1 == shortcut,
			MCDC_Branch name2 _ _ <- take 1 t2 ] ++
		[ MCDC_Branch ("(" ++ name1 ++ (render.pretty) binop ++ name2 ++ ")") result2 (cond1 ⋏ cond2) |
			MCDC_Branch name1 result1 cond1 <- take 1 t1_not_shortcut,
			MCDC_Branch name2 result2 cond2 <- t2 ] ++
		[ MCDC_Branch ("(" ++ name1 ++ (render.pretty) binop ++ name2 ++ ")") result2 (cond1 ⋏ cond2) |
			MCDC_Branch name1 result1 cond1 <- drop 1 t1_not_shortcut,
			MCDC_Branch name2 result2 cond2 <- take 1 t2_not_shortcut ]
		where
		shortcut = case binop of
			CLorOp -> True
			CLndOp -> False
		(t1,t2) = (createMCDCTables expr1,createMCDCTables expr2)
		t1_not_shortcut = filter ((== (not shortcut)).resultMCDCB) t1
		t2_not_shortcut = filter ((== (not shortcut)).resultMCDCB) t2
		to_dontcare s = for s $ \case
			'F'   -> '_'
			'T'   -> '_'
			other -> other
	expr → [ MCDC_Branch "T" True expr, MCDC_Branch "F" False (not_ expr) ]

setNodeInfo :: NodeInfo → CExpr → CExpr
setNodeInfo ni expr = amap (const ni) expr

createBranches :: (CExpr → String) → CExpr → CovVecM [(Branch,CExpr)]
createBranches name_creator cond = do
	let (real_cond,_) = extractAnnotation cond
 	gets coverageKindCVS >>= \case
 		Branch_Coverage → return [
 			(Branch (lineColNodeInfo cond) 1 False "", set_ni real_cond),
 			(Branch (lineColNodeInfo cond) 2 True "", set_ni $ not_ real_cond) ]
		MCDC_Coverage   → return $ case createMCDCTables real_cond of
			[ MCDC_Branch name1 result1 bcond1, MCDC_Branch name2 result2 bcond2 ] → [
				(Branch (lineColNodeInfo cond) 1 (not result1) (name_creator real_cond),set_ni bcond1),
				(Branch (lineColNodeInfo cond) 2 (not result2) (name_creator real_cond),set_ni bcond2) ]
			mcdctable → for (zip [1..] mcdctable) $ \ (i,MCDC_Branch name result bcond) →
				(Branch (lineColNodeInfo cond) i (not result) (name ++ " in " ++ name_creator real_cond),set_ni bcond)
	where
	-- set the condition's NodeInfo to
	set_ni = setNodeInfo (nodeInfo cond)

instance CNode TraceElem where
	nodeInfo (Assignment kind _)        = case kind of
		Normal lexpr                     → extractNodeInfo lexpr
		ArrayUpdate (Ident _ _ ni) _ _ _ → ni
	nodeInfo (Condition (Left (_,expr))) = extractNodeInfo expr
	nodeInfo (Condition (Right (_,expr))) = extractNodeInfo expr
	nodeInfo (NewDeclaration (ident,_)) = nodeInfo ident
	nodeInfo (Return expr)              = extractNodeInfo expr
	nodeInfo (DebugOutput _ _)          = undefNode
	nodeInfo SolverFind                 = undefNode


instance Pretty CExprWithType where
	pretty (CBinary op expr1 expr2 (_,ty)) = prettyCE (pretty op) [pretty expr1,pretty expr2] ty
	pretty (CCast _ expr (_,ty)) = prettyCE (lparen <> pretty (fst ty) <> rparen) [pretty expr] ty
	pretty (CUnary op expr (_,ty)) = prettyCE (pretty op) [pretty expr] ty
	pretty (CMember expr ident deref (_,ty)) = prettyCE (text "")
		[pretty expr,text (if deref then "->" else ".") <+> pretty ident] ty
	pretty (CVar ident (_,ty)) = pretty ident <+> prettyType ty
	pretty (CConst constant) = ( text $ case constant of
		CIntConst cint _ → show $ getCInteger cint
		CCharConst cchar _ → show $ getCChar cchar
		CFloatConst cfloat _ → show cfloat
		CStrConst cstr _ → show $ getCString cstr ) <+>
			prettyType ((snd.annotation) constant)
	pretty (CIndex arrexpr indexexpr (_,ty)) = pretty arrexpr <> brackets (pretty indexexpr) <+> prettyType ty
	pretty (CAssign op lexpr assexpr (_,ty)) = prettyCE (pretty lexpr <+> pretty op) [pretty assexpr] ty
	pretty (CCall fun args (_,ty)) = prettyCE (text "Call " <+> pretty fun) (map pretty args) ty
	pretty (CCond cond (Just true_expr) false_expr (_,ty)) = prettyCE (pretty cond <+> text "?")
		[ pretty true_expr, text ":", pretty false_expr ] ty
	pretty other = error $ "instance Pretty CExprWithType not implemented for " ++ show other

prettyType ty = case printTypes of
	True → text "::" <+> pretty (fst ty)
	False → Text.PrettyPrint.empty

prettyCE head subs _ | not printTypes = parens $ case subs of
	[sub] → head <+> sub
	[sub1,sub2] → sub1 <+> head <+> sub2
	_ → head <+> hsep subs

prettyCE head subs ty =
	lparen <+> head $+$
	(nest 4 $ vcat subs) $+$
	rparen <+> prettyType ty

instance (Pretty a) => Pretty [a] where
	pretty xs = brackets $ hcat $ punctuate comma (map pretty xs)

instance Show TraceElem where
	show te = ( case te of
		Assignment lexpr expr    → "ASSN " ++ (render.pretty) lexpr ++ " = " ++ (render.pretty) expr
		Condition (Left (varexpr,expr)) → "COND " ++ (render.pretty) varexpr ++ " = " ++ (render.pretty) expr
		Condition (Right (branch,expr)) → "COND " ++ showBranch branch ++ " " ++ (render.pretty) expr
		NewDeclaration (lval,ty) → "DECL " ++ (render.pretty) lval ++ " :: " ++ (render.pretty) ty
		Return exprs             → "RET  " ++ (render.pretty) exprs
		DebugOutput varname expr → "DBGOUT " ++ varname ++ " " ++ (render.pretty) expr
		SolverFind               → "SOLVER_FIND"
		) ++ if printLocations then "  (" ++ (showLocation.lineColNodeInfo) te ++ ")" else ""

showTrace :: Trace → String
showTrace trace = unlines $ concatMap show_te trace where
	show_te te | showBuiltins || isnotbuiltin te = [show te]
	show_te _ = []

covVectorsM :: CovVecM Bool
covVectorsM = logWrapper [ren "covVectorsM"] $ do
	printDateTimeM 0

	sizes <- find_out_sizesM
	modify $ \ s → s { machineSpecCVS = Just sizes }

	funname <- gets funNameCVS
	globdecls <- gets ((Map.elems).gObjs.globDeclsCVS)
	glob_env <- concatMapM declaration2EnvItemM globdecls
	let
		def2stmt :: IdentDecl → CovVecM [CBlockItem]
		def2stmt (EnumeratorDef (Enumerator ident const_expr _ ni)) = do
			return [ CBlockStmt $ CVar ident (nodeInfo ident) ≔ const_expr ]
		def2stmt (ObjectDef (ObjDef (VarDecl (VarName ident _) _ ty) (Just initializer) ni)) = do
			ty' <- elimTypeDefsM ty
			cinitializer2blockitems (CVar ident ni) ty' initializer
		def2stmt _ = return []
	-- creates the assignment statements from the global context
	defs <- concatMapM def2stmt globdecls

	let funname_ident = builtinIdent funname
	fundef@(FunDef (VarDecl _ _ (FunctionType (FunType ret_type funparamdecls False) _)) body fundef_ni) <-
		lookupFunM funname_ident
	ret_type' <- elimTypeDefsM ret_type

	let srcident = internalIdent returnval_var_name
	z3_ret_type' <- ty2Z3Type ret_type'
	ret_env_exprs <- createInterfaceFromExprM (CVar srcident (nodeInfo srcident,z3_ret_type')) ret_type'
	modify $ \ s → s { retEnvCVS = Just ret_env_exprs }

	-- Go through the body of the function and determine all decision points
	let
		fun_lc fundef = lineColNodeInfo (nodeInfo fundef)
		next_lc fundef = case sort $ filter (> lineColNodeInfo (nodeInfo fundef)) $ map lineColNodeInfo $ {-filter (is_in_src_file fundef)-} globdecls of
			[] → (maxBound,maxBound,-1)
			next : _ → next

		liftandfst :: CovVecM [(Branch,CExpr)] → StateT [Branch] CovVecM [Branch]
		liftandfst createbranches = do
			branchesexprs <- lift createbranches
			return $ map fst branchesexprs
		searchcondpoint :: CStat → StateT [Branch] CovVecM CStat
		searchcondpoint stmt = do
			add_branches <- case stmt of
				CSwitch _ (CCompound _ cbis _) _ → return $ collectcases 1 cbis
					where
					collectcases _ [] = []
					collectcases i ( CBlockStmt (CCase cond casestmt@(CCase _ _ _) caseni) : rest) =
						collectcases i $ CBlockStmt (CCase cond (CExpr Nothing undefNode) caseni) : (CBlockStmt casestmt) : rest
					collectcases i ( CBlockStmt ccase@(CCase caseexpr _ _) : rest ) =
						Branch (lineColNodeInfo ccase) i False (makeCaseBranchName caseexpr) : collectcases (i+1) rest
					collectcases i ( CBlockStmt cdefault@(CDefault _ _) : _ ) =
						[ Branch (lineColNodeInfo cdefault) i False makeDefaultBranchName ]
					collectcases i (_:rest) = collectcases i rest
				CSwitch _ _ _            → error $ "searchcondpoint: Strange Switch " ++ (render.pretty) stmt
				CWhile cond _ _ _        → liftandfst $ createBranches makeForWhileBranchName cond
				CFor _ (Just cond) _ _ _ → liftandfst $ createBranches makeForWhileBranchName cond
				CFor _ Nothing _ _ _     → error $ "searchcondpoint: for(_,Nothing,_) not implemented!"
				CIf cond _ _ _           → liftandfst $ createBranches makeIfBranchName cond
				_                        → return []
			modify (add_branches++)
			return stmt

		searchexprcondpoint :: CExpr → StateT [Branch] CovVecM CExpr
		searchexprcondpoint expr = do
			subfuncov <- lift $ isOptionSet subfuncovOpt
			add_branches <- case expr of
				CCond cond _ _ _ → liftandfst $ createBranches makeCondBranchName cond
				-- Recurse on allpoints_in_body if called functions should be covered as well
				CCall (CVar funident _) _ _ | subfuncov && not ( (identToString funident) `elem` ["solver_pragma","solver_error","solver_find"] ) → do
					lift $ allpoints_in_body funident
				_ → return []
			modify (add_branches++)
			return expr

		allpoints_in_body :: Ident → CovVecM [Branch]
		allpoints_in_body (Ident fun_ident _ _) | any (`isPrefixOf` fun_ident) ["solver_debug","solver_error","solver_find","solver_pragma"] = return []
		allpoints_in_body funident = do
			fundef@(FunDef (VarDecl _ _ (FunctionType (FunType _ _ _) _)) body fundef_ni) <- lookupFunM funident
			modify $ \ s → s { funStartEndCVS = (fun_lc fundef,next_lc fundef) : funStartEndCVS s }
			condpoints <- execStateT (everywhereM (mkM searchcondpoint) body) []
			exprcondpoints <- execStateT (everywhereM (mkM searchexprcondpoint) body) []
			return $ condpoints ++ exprcondpoints
	allpoints <- allpoints_in_body (builtinIdent funname)
	modify $ \ s → s { allCondPointsCVS = nub allpoints }

	let
		formal_params = for (map getVarDecl funparamdecls) $ \ (VarDecl (VarName srcident _) _ ty) → (srcident,ty)
		actual_args = for formal_params $ \ (srcident,ty) → case ty of
			DirectType (TyFloating TyFloat)  _ _ → printf "u2f(%s)" ((render.pretty) srcident)
			DirectType (TyFloating TyDouble) _ _ → printf "u2d(%s)" ((render.pretty) srcident)
			_ → (render.pretty) srcident
	ext_decls <- createDeclsM formal_params
	(param_env_exprs,(arraydecl_env,arrayitem_conds)) <- createInterfaceM formal_params
	modify $ \ s → s { paramEnvCVS = Just param_env_exprs }
	let param_env = map fst param_env_exprs
	printLogV 20 $ "param_env = " ++ showEnv param_env

	let
		decls = arrayitem_conds ++ map (NewDeclaration . snd) (arraydecl_env ++ reverse param_env ++ glob_env)

	when checkSolutions $ do
		filename <- gets srcFilenameCVS
		let
			chkexefilename = replaceExtension mainFileName "exe"
			mainfilename = replaceFileName filename mainFileName
		srcabsfilename <- liftIO $ makeAbsolute $ mainfilename

		charness <- createCHarness ret_type actual_args (takeFileName filename) funname ext_decls
		compileHereM ["-Wno-builtin-declaration-mismatch","-Wno-int-conversion","-Wno-incompatible-pointer-types",
			"-o",chkexefilename,mainFileName] srcabsfilename charness
		modify $ \ s → s { checkExeNameCVS = Just chkexefilename }

	Right every_branch_covered <- unfoldTracesM [] (Just ret_type') True 0 [] ((arraydecl_env++param_env):[glob_env]) decls [ (defs ++ [ CBlockStmt body ],False) ]
	printStatsM 0
	printDateTimeM 0
	return every_branch_covered

harnessAST incl argdecls funcall print_retval1 print_retval2 = [cunit|
int __attribute__((__cdecl__)) printf(const char *,...);
int __attribute__((__cdecl__)) sscanf(const char *,const char *,...) ;

$esc:uns_float
$esc:uns_double
float u2f(unsigned long int u) { float_conv.uint_val = u; return (float_conv.float_val); }
double u2d(unsigned long long int u) { double_conv.ulong_val = u; return (double_conv.double_val); }
unsigned long f2u(float f) { float_conv.float_val = f; return (float_conv.uint_val); }
unsigned long long int d2u(double f) { double_conv.double_val = f; return (double_conv.ulong_val); }

int solver_pragma(int x,...) { return 1; }
void solver_debug_Float(char* s,float x) { printf("DEBUG_VAL Float %s = %g = 0x%lx\n",s,x,f2u(x)); }
void solver_debug_Double(char* s,double x) { printf("DEBUG_VAL Double %s = %g = 0x%llx\n",s,x,d2u(x)); }
void solver_debug_UByte(char* s,unsigned char x) { printf("DEBUG_VAL UByte %s = %hhi = 0x%hhx\n",s,x,x); }
void solver_debug_Short(char* s,short x) { printf("DEBUG_VAL Short %s = %hi = 0x%x\n",s,x,x); }
void solver_debug_UShort(char* s,unsigned short x) { printf("DEBUG_VAL UShort %s = %hu = 0x%hx\n",s,x,x); }
void solver_debug_UInt(char* s,unsigned int x) { printf("DEBUG_VAL UInt %s = %u = 0x%x\n",s,x,x); }
void solver_debug_Int(char* s,int x) { printf("DEBUG_VAL_Int %s = %i = 0x%lx\n",s,x,x); }
void solver_debug_ULong(char* s,unsigned long x) { printf("DEBUG_VAL ULong %s = %lu = 0x%lx\n",s,x,x); }
void solver_debug_Long(char* s,long x) { printf("DEBUG_VAL Long %s = %li = 0x%lx\n",s,x,x); }
void solver_debug_ULongLong(char* s,unsigned long long x) { printf("DEBUG_VAL ULongLong %s = %llu = 0x%llx\n",s,x,x); }
void solver_debug_LongLong(char* s,long long x) { printf("DEBUG_VAL LongLong %s = %lli = 0x%llx\n",s,x,x); }

void solver_find() { printf($esc:solverfindstr); }
void solver_error() {}

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
	solverfindstr = show $ solverFindMagicString ++ "\n"
	uns_float = "union { float float_val; unsigned long int uint_val; } float_conv;"
	uns_double = "union { double double_val; unsigned long long int ulong_val; } double_conv;"

envItemNotPtrType :: EnvItem → Bool
envItemNotPtrType (_,(_,PtrType _ _ _)) = False
envItemNotPtrType _ = True

createCHarness :: Type → [String] → String → String → [String] → CovVecM String
createCHarness orig_rettype fun_args filename funname extdecls = do
	Just retenvexprs0 <- gets retEnvCVS
	Just param_env_exprs0 <- gets paramEnvCVS
	let
		param_env_exprs = filter (envItemNotPtrType.fst) param_env_exprs0
		retenvexprs = filter (envItemNotPtrType.fst) retenvexprs0
		argexprs = for param_env_exprs $ \ (_,cexprwithty) → funcallargwrapper cexprwithty
		incl_srcfilename = "#include \"" ++ filename ++ "\""

		funcall = (render.pretty) orig_rettype ++ " " ++ returnval_var_name ++ " = " ++
			funname ++ "(" ++ intercalate "," fun_args ++ ");"

	argformats <- mapM type_format_string $ map (snd.snd.fst) param_env_exprs
	let
		argvals = intercalate ", " $ for (zip argexprs argformats) $
			\ (argname,argformat) → argname ++ " = " ++ argformat

	ret_formatss <- mapM type_format_string $ map (snd.snd.fst) retenvexprs
	let
		ret_vals = for retenvexprs $ \ (_,cexprwithty) → ty2argwrapper cexprwithty
		param_exprs = for param_env_exprs $ \ (_,cexprwithty) → (render.pretty) $ fmap fst cexprwithty
		print_retval1 = "printf(\"" ++ funname ++ "(" ++ argvals ++ ") = \\n\"," ++
			(if null argexprs then "" else (intercalate ", " param_exprs)) ++ ");"
		print_retval2 = "printf(\"" ++
			intercalate " " ret_formatss ++ "\\n\", " ++
			intercalate ", " ret_vals ++ ");"
	return $ PPM.prettyCompact $ PPMC.ppr $ harnessAST incl_srcfilename (unlines extdecls) funcall print_retval1 print_retval2

	where

	funcallargwrapper cexprwithty = case extractType cexprwithty of
		DirectType (TyFloating TyFloat)  _ _ → printf "u2f(%s)" ((render.pretty) (fmap fst cexprwithty))
		DirectType (TyFloating TyDouble) _ _ → printf "u2d(%s)" ((render.pretty) (fmap fst cexprwithty))
		_ → (render.pretty) (fmap fst cexprwithty)
	ty2argwrapper cexprwithty = case extractType cexprwithty of
		DirectType (TyFloating TyFloat)  _ _ → printf "f2u(%s)" ((render.pretty) (fmap fst cexprwithty))
		DirectType (TyFloating TyDouble) _ _ → printf "d2u(%s)" ((render.pretty) (fmap fst cexprwithty))
		_ → (render.pretty) (fmap fst cexprwithty)

-- Create declarations for the function under test in the C test harness
createDeclsM :: [(Ident,Type)] → CovVecM [String]
createDeclsM formal_params = do
	concatForM formal_params $ \ (ident,ty) → create_decls (CVar ident undefNode) ty ty False []

	where

	-- ty is the non-dereferenced type used for pretty printing,
	-- deref_ty is the dereferenced type that is to be analyzed further
	create_decls expr ty deref_ty all_declared decls = logWrapper ["create_decls",ren expr,ren deref_ty,ren all_declared,ren decls] $ case deref_ty of

		TypeDefType (TypeDefRef ident _ _) _ _ → do
			ty' <- lookupTypeDefM ident
			create_decls expr ty ty' all_declared decls

		-- case: deref_ty = <ty>* ptr
		-- int *      :: a  →  { int PTR_a; int * a = & PTR_a; }
		-- struct S * :: s  →  { struct S PTR_s; struct S * s = & PTR_s; }
		PtrType target_ty _ _ → do
			let
				subexpr = CUnary CIndOp expr undefNode
				subexpr_varname = lValueToVarName subexpr
			create_decls (CVar (internalIdent subexpr_varname) undefNode) target_ty target_ty True $ decls ++ [
				-- if we encounter a pointer type, we have to declare it anyway.
				(render.pretty) target_ty ++ " " ++ subexpr_varname ++ ";",
				(render.pretty) ty ++ " " ++ lval_varname ++ " = &" ++ subexpr_varname ++ ";" ]

		-- case: deref_ty = STRUCT/UNION expr
		DirectType (TyComp (CompTypeRef sueref kind _)) _ _ → do
			member_ty_s <- getMembersM sueref
			-- If it is a union, only take the first member
			res_decls <- concatForM ((if kind==UnionTag then take 1 else id) member_ty_s) $ \ (m_ident,m_ty) → do
				create_decls (CMember expr m_ident False undefNode) m_ty m_ty True []
			let decl = case all_declared of
				True  → []
				False → [ (render.pretty) ty ++ " " ++ lval_varname ++ ";" ]
			return $ decls ++ decl ++ res_decls

		-- case: deref_ty = <direct-type> where <direct-type> is no struct/union.
		DirectType tyname _ _ → do
			mb_fixed_floating_ty <- case ty of
				DirectType (TyFloating _) _ _ → sizeofTy ty >>= sizeToIntTypeM
				_ → return ty
			let decl = case all_declared of
				False → [ (render.pretty) mb_fixed_floating_ty ++ " " ++ (render.pretty) expr ++ ";" ]
				True → []
			tyfs <- type_format_string ty
			return $ decls ++ decl ++
				[ "if(sscanf(argv[i++],\"" ++ tyfs ++ "\",&(" ++ (render.pretty) expr ++ "))!=1) return 1;" ]

		ArrayType elem_ty (ArraySize False arr_size_expr) _ _ → do
			arr_size <- evalConstExprM arr_size_expr
			let
				(CVar (Ident _ _ _) _) = expr
				arr_decl = case all_declared of
					False → [(render.pretty) elem_ty ++ " " ++ (render.pretty) expr ++ "[" ++ show arr_size ++ "];"]
					True → []
			arr_decls <- concatForM [0..(arr_size - 1)] $ \ i → do
				ii <- ⅈ i
				create_decls (CIndex expr ii undefNode) elem_ty elem_ty True []
			return $ decls ++ arr_decl ++ arr_decls

		_ → myError $ "createDeclsM " ++ (render.pretty) expr ++ " " ++ show ty ++ " not implemented"

		where

		lval_varname = lValueToVarName expr

evalConstExprM :: CExpr → CovVecM Integer
evalConstExprM expr = eval_const_expr expr
	where
	eval_const_expr (CConst (CIntConst cint _)) = return $ getCInteger cint
	eval_const_expr (CBinary binop expr1 expr2 _) = op <$> (eval_const_expr expr1) <*> (eval_const_expr expr2) where
		Just op = lookup binop [(CMulOp,(*)),(CDivOp,div),(CAddOp,(+)),(CSubOp,(-)),(CRmdOp,mod)]
	eval_const_expr (CUnary unop expr _) = op <$> (eval_const_expr expr) where
		Just op = lookup unop [(CPlusOp,id),(CMinOp,negate)]
--	TODO: eval_const_expr (CSizeofExpr expr _) = do
	eval_const_expr (CSizeofType decl _) = do
		size <- decl2TypeM decl >>= sizeofTy
		-- round up the number of bytes, if necessary
		return $ div (fromIntegral size + 7) 8
	eval_const_expr other = myError $ "eval_const_expr " ++ (render.pretty) other ++ " impossible or not implemented!"

type_format_string :: Type → CovVecM String
type_format_string ty = do
	Just MachineSpec{..} <- gets machineSpecCVS
	(z3ty,_) <- ty2Z3Type ty
	return $ "%" ++ case z3ty of
		Z3_Float                                        → "lx"
		Z3_Double                                       → "llx"
		Z3_BitVector   16 unsigned                      → "h" ++ if unsigned then "u" else "i"
		Z3_BitVector size unsigned | size==intSize      → if unsigned then "u" else "i"
		Z3_BitVector size unsigned | size==longSize     → "l" ++ if unsigned then "u" else "i"
		Z3_BitVector size unsigned | size==longLongSize → "ll" ++ if unsigned then "u" else "i"
		Z3_Ptr _                                        → "p"
		_ → error $ "type_format_string " ++ (render.pretty) ty ++ " not implemented"

type Location = (Int,Int,Int)
locationToName :: Location → String
locationToName (l,c,len) = show l ++ "_" ++ show c ++ "_" ++ show len

showLocation :: Location → String
showLocation (l,c,len) = "line " ++ show l ++ ", col " ++ show c ++ ", len " ++ show len

printToSolutions :: (MonadIO m) => String → m ()
printToSolutions msg = liftIO $ appendFile solutionsFile msg

type Progress = [(Int,Int)]

printProgressM :: Progress → CovVecM ()
printProgressM progress = do
	printLogV 20 $ printf "Current progress = %s" (show progress)
	printLogV 1 $ printf "Current depth = %i" (length progress)
	printLogV 1 $ printf "Current estimated total number of traces: %i" (product $ map snd progress) 
	printLogV 1 $ printf "Progress: %.1f %%" (100.0 * (pct $ reverse progress))
	where
	pct :: Progress → Float
	pct [] = 0.0
	pct ((cur,tot):rest) = (fromIntegral cur / fromIntegral tot) + (1.0 / fromIntegral tot) * pct rest

-- In case of a cutoff, mb_ret_type is Nothing.
analyzeTraceM :: Maybe Type → Progress → [TraceElem] → CovVecM Bool
analyzeTraceM mb_ret_type progress res_line = logWrapper [ren "analyzeTraceM",ren mb_ret_type,ren res_line,ren "traceid=",ren traceid] $ do
	printLogV 1 $ "\n"
{-
	case reachFixedTrace of
		Just fixedtrace | not (traceid `isPrefixOf` fixedtrace) -> do
			printConsole 1 $ "\r " ++ show traceid ++ " no prefix of " ++ show fixedtrace ++ "                   "
			return False
		_ -> do
-}
	printStatsM 0
	printDateTimeM 0
	(traceanalysisresults,_) <- gets analysisStateCVS
	-- check if the trace was already analyzed (this can happen because of unfolding loops containing a return).
	case traceid `elem` (map (\(tid,_,_,_)->tid) traceanalysisresults) of
		True -> do
			myError $ "Trace " ++ show traceid ++ " already analyzed!"
			-- Since the trace is in analysisStateCVS, it must have been solvable (see below,
			-- otherwise it wouldn't have been added to analysisStateCVS)
			return True
		False -> do
			findmode <- isOptionSet findModeOpt
			let
				found_finds = any is_solverfind trace
				is_solverfind :: TraceElem → Bool
				is_solverfind SolverFind = True
				is_solverfind _ = False
			case findmode && not found_finds of
				True → do
					printLogV 1 $ "Did not find solver_find, cutting off."
					return False
				False → do
					incNumTracesM
					printProgressM progress
					printLogV 1 $ "===== ANALYZING TRACE " ++ show traceid ++ " ================================="

					opts <- gets optsCVS
					when ("-exportPaths" `elem` opts) $ liftIO $ do
						writeFile (errorModelPath </> "path_" ++ show traceid <.> ".c") $ unlines $ concat $ for trace $ \case
							Assignment lexpr assexpr → [ (render.pretty) lexpr ++ " = " ++ (render.pretty) assexpr ++ " ;" ]
							NewDeclaration (ident,ty) → [ "(" ++ (render.pretty) ty ++ ") " ++ (render.pretty) ident ++ " ;" ]
							Return expr → [ "return " ++ (render.pretty) expr ++ " ;" ]
							_ → []

					showtraceM showInitialTrace "Initial" return trace >>=
						showtraceM showTraces "elimInds"                 elimInds >>=
						showtraceM showTraces "1. simplifyTraceM"        simplifyTraceM >>=
--								showtraceM showTraces "elimArrayAssignsM"        elimArrayAssignsM >>=
						showtraceM showTraces "sequenceArraysM"          sequenceArraysM >>=
						showtraceM showTraces "elimAssignmentsM"         elimAssignmentsM >>=
						showtraceM showTraces "2. simplifyTraceM"        simplifyTraceM >>=
						showtraceM showFinalTrace "createSymbolicVarsM"  createSymbolicVarsM >>=
						solveTraceM mb_ret_type traceid >>= \case

							Left solvable → return solvable
							Right resultdata@(model_string,mb_solution) → do
								funname <- gets funNameCVS
								let show_solution_msg = show_solution funname mb_solution ++ "\n"
								printLogV 1 $ "--- Result of TRACE " ++ show traceid ++ " ----------------------\n\n" ++ show_solution_msg
								case mb_solution of
									Nothing → do
										incNumNoSolutionM
										return False
									Just (_,_,[]) → myError $ "Empty solution: \n" ++ show_solution_msg
									Just solution → do
										incNumSolutionM
										printToSolutions $ "\n\n---- Trace " ++ show traceid ++ " -----------------------------------\n\n"
										printToSolutions show_solution_msg

										startends <- gets funStartEndCVS
										printLogV 20 $ "startends = " ++ show startends
										let visible_trace = concatMap to_branch trace
											where
											is_visible_branch :: [(Location,Location)] → Location → Bool
											is_visible_branch locs lc = any (\(start,end) → start <= lc && lc < end) locs
											to_branch (Condition (Right (branch,_))) | is_visible_branch startends (branchLocation branch) = [ branch ]
											to_branch _ = []

										let cov_branches = "\tDECISION POINTS (IN CONTROL FLOW ORDER):\n" ++ unlines (map (("\t"++).showBranch) visible_trace)
										printLogV 1 cov_branches
										printToSolutions cov_branches

										when (checkSolutions && isJust mb_ret_type) $ checkSolutionM traceid resultdata >> return ()

										(tas,covered) <- gets analysisStateCVS
										all_branches <- gets allCondPointsCVS
										let
											traceanalysisresult :: TraceAnalysisResult = (traceid,res_line,visible_trace,resultdata)
											nub_visible_trace = nub visible_trace
											excess_branches = nub_visible_trace \\ all_branches
										when (not $ null excess_branches) $ do
											printCondPoints all_branches
											myError $ "Branches " ++ show excess_branches ++ " are covered but are not in all_branches!"
										-- Are all the decision points are already covered? They can't, probably...?
										-- If yes, this trace does not contribute to full coverage...
										when (not $ null (nub_visible_trace \\ covered)) $
											modify $ \ s → s { analysisStateCVS = (tas++[traceanalysisresult],nub_visible_trace `union` covered) }
										return True
			
		where

		trace = reverse res_line
		traceid :: [Int] = trace2traceid trace

		showtraceM cond stage combinator trace = do
			trace' <- combinator trace
			when cond $ do
				printLogV 5 $ "\n--- TRACE after " ++ stage ++ " " ++ show traceid ++ " -----------\n" ++
					if showBuiltins then "" else "<leaving out builtins...>\n"
				printLogV 5 $ showLine trace'
			return trace'

-- move backwards on the trace, till a assignment to a to_be_defined variable is reached.
-- then, remove the to_be_defined variable from the to_be_defined variable set, and add all variables
-- that are in the assigned expr.
checkVarsDefined :: Maybe Type → Trace → CovVecM (Maybe String)
checkVarsDefined mb_ret_type trace = do
--	printLogV 0 $ "Trace for checkVarsDefined:\n" ++ showTrace trace
	(param_env_exprs,param_env,param_names,ret_names) <- getArgRetNames mb_ret_type
	let
		Return ret_expr : resttrace = reverse trace
		ret_vars = fvar ret_expr
	-- find out all variables that are undefined in the trace, and also not parameters
	return Nothing {- TODO
	return $ case collect_undef_vars (fvar ret_expr) resttrace \\ param_names of
		[] → Nothing
		undef_vars → Just $ unlines $ "Undefined Variables:" :
			(for undef_vars $ \ var →printf "\tVariable %s is undefined" ((render.pretty) var))
-}

	where

	collect_undef_vars :: [Ident] → Trace → [Ident]

	collect_undef_vars should_be_defined_vars [] = should_be_defined_vars

	collect_undef_vars should_be_defined_vars (Assignment (Normal (CVar defd_ident _)) assd_expr : resttrace) |
		defd_ident `elem` should_be_defined_vars =
			collect_undef_vars (nub $ (should_be_defined_vars \\ [defd_ident]) ++ assd_expr_vars) resttrace
			where
			assd_expr_vars = fvar assd_expr

{-
	-- assume assignments to arrays are defining all elements...
	collect_undef_vars should_be_defined_vars (Assignment lexpr assd_expr : resttrace) =
		collect_undef_vars should_be_defined_vars resttrace
-}

	collect_undef_vars should_be_defined_vars (_ : resttrace) =
		collect_undef_vars should_be_defined_vars resttrace

trace2traceid trace = concatMap extract_conds trace where
	extract_conds (Condition (Right (branch,_))) = [ numBranch branch ]
	extract_conds _ = []

lineColNodeInfo :: (CNode a) => a → Location
lineColNodeInfo cnode = if isSourcePos pos_te then (posRow pos_te,posColumn pos_te,fromJust $ lengthOfNode ni) else (-1,-1,-1)
	where
	ni = nodeInfo cnode
	pos_te = posOfNode ni

lookupFunM :: Ident → CovVecM FunDef
lookupFunM ident = do
	funs <- gets (gObjs.globDeclsCVS)
	case Map.lookup ident funs of
		Just (FunctionDef fundef) → return fundef
		Just other → myError $ "lookupFunM " ++ (render.pretty) ident ++ " yielded " ++ (render.pretty) other
		Nothing → myError $ "Function " ++ (show ident) ++ " not found"

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

lValueToVarName :: (Show a) => CExpression a → String
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
			Nothing → error $ "binop2str " ++ (render.pretty) binop ++ " not implemented"
			Just s → s
lValueToVarName (CConst (CIntConst cint _)) = (if i<0 then "m" else "") ++ show (abs i) where
	i = getCInteger cint
lValueToVarName (CIndex arr index _) = lValueToVarName arr ++ "_INDEX_" ++ lValueToVarName index
-- HACK: Abusing lValueToVarName for turning an expression into a String represenation (used with new array names)
lValueToVarName (CCast _ expr _) = lValueToVarName expr
lValueToVarName lval = error $ "lValueToVarName " ++ show lval ++ " not implemented!"

type TyEnvItem = (Ident,Type)
instance Pretty TyEnvItem where
	pretty (idnew,ty) = pretty idnew <+> text " :: " <+> pretty ty
type EnvItem = (Ident,TyEnvItem)
instance Pretty EnvItem where
	pretty (idold,tyenvitem) = pretty idold <+> text " |-> " <+> pretty tyenvitem
type Env = [EnvItem]

dumpEnvs :: [Env] → String
dumpEnvs ϵs = unlines $ for ϵs $ \ ϵ → "[ " ++
	(concat $ intersperse ", " $ for ϵ $ \ (a,(at,ty)) → (render.pretty) a ++ "->" ++ (render.pretty) at) ++ " ]"

envToString :: Env → String
envToString ϵ = unlines $ map (render.pretty) $ filter (isnotbuiltinIdent.fst) ϵ

lookupTypeDefM :: Ident → CovVecM Type
lookupTypeDefM ident = do
	typedefs <- gets (gTypeDefs.globDeclsCVS)
	case Map.lookup ident typedefs of
		Just (TypeDef _ ty attrs _) → return $ case ty of
			DirectType tyname tyquals tyattrs    → DirectType tyname tyquals (tyattrs++attrs)
			PtrType ty tyquals tyattrs           → PtrType ty tyquals (tyattrs++attrs)
			ArrayType ty size tyquals tyattrs    → ArrayType ty size tyquals (tyattrs++attrs)
			FunctionType ty tyattrs              → FunctionType ty (tyattrs++attrs)
			TypeDefType tydefref tyquals tyattrs → TypeDefType tydefref tyquals (tyattrs++attrs)
		Nothing → myError $ "TypeDef " ++ (show ident) ++ " not found"

envs2tyenv :: [Env] → TyEnv
envs2tyenv ϵs = map snd $ concat ϵs

decl2TypeM :: CDecl → CovVecM Type
decl2TypeM decl = do
	deftable <- gets defTableCVS
	case runTrav_ (withDefTable (\_→((),deftable)) >> myAnalyseTypeDecl decl) of
		Right (ty,_) → return ty
		Left errs → myError $ show errs
	where
	-- taken from Language/C/Analysis/DeclAnalysis.hs, added proper handling of initializers
	myAnalyseTypeDecl :: (MonadTrav m) => CDecl → m Type
	myAnalyseTypeDecl (CDecl declspecs declrs node) = case declrs of
		[] → analyseTyDeclr (CDeclr Nothing [] Nothing [] node)
		(Just declr,_,Nothing):_ → analyseTyDeclr declr
		where
		analyseTyDeclr (CDeclr _ derived_declrs Nothing attrs _declrnode) = do
			canonTySpecs <- canonicalTypeSpec typespecs
			t <- tType True node (map CAttrQual (attrs++attrs_decl) ++ typequals) canonTySpecs derived_declrs []
			case nameOfNode node of
				Just n → withDefTable (\dt → (t, insertType dt n t))
				Nothing → return t
			where
			(storagespec, attrs_decl, typequals, typespecs, funspecs, alignspecs) = partitionDeclSpecs declspecs
		analyseTyDeclr other = error $ "analyseTyDeclr " ++ show other

type2Decl :: Ident → NodeInfo → Type → Maybe CExpr → CDecl
type2Decl ident ni ty mb_init_expr = CDecl (map CTypeSpec typespecs)
	[(Just $ CDeclr (Just ident) derivdeclrs Nothing [] ni,mb_initializer,Nothing)] ni
	where
	mb_initializer = fmap (\ expr -> CInitExpr expr undefNode) mb_init_expr
	(typespecs,derivdeclrs) = ty2specs ty
	ty2specs ty = case ty of
		DirectType tyname _ _ → (case tyname of
			TyVoid → [CVoidType ni]
			TyIntegral inttype → case inttype of
				TyBool → [CBoolType ni]
				TyChar → [CCharType ni]
				TySChar → [CSignedType ni,CCharType ni]
				TyUChar → [CUnsigType ni,CCharType ni]
				TyShort → [CShortType ni]
				TyUShort → [CUnsigType ni,CShortType ni]
				TyInt → [CIntType ni]
				TyUInt → [CUnsigType ni,CIntType ni]
				TyInt128 → [CInt128Type ni]
				TyUInt128 → [CUnsigType ni,CInt128Type ni]
				TyLong → [CLongType ni]
				TyULong → [CUnsigType ni,CLongType ni]
				TyLLong → [CLongType ni,CLongType ni]
				TyULLong → [CUnsigType ni,CLongType ni,CLongType ni]
			TyFloating floattype → case floattype of
				TyFloat → [CFloatType ni]
				TyDouble → [CDoubleType ni]
				TyLDouble → [CLongType ni,CDoubleType ni]
				TyFloatN i b → [CFloatNType i b ni]
			TyComplex floattype → case floattype of
				TyFloat → [CComplexType ni]
				TyDouble → [CDoubleType ni,CComplexType ni]
				TyLDouble → [CLongType ni,CDoubleType ni,CComplexType ni]
				TyFloatN i b → [CFloatNType i b ni,CComplexType ni]
			TyComp (CompTypeRef sueref comptykind _) →
				[CSUType (CStruct (comptykind2structtag comptykind) (sueref2mbident sueref) Nothing [] ni) ni]
			TyEnum (EnumTypeRef sueref _) → [CEnumType (CEnum (sueref2mbident sueref) Nothing [] ni) ni]
			,[])
			where
			comptykind2structtag StructTag = CStructTag
			comptykind2structtag UnionTag = CUnionTag
			sueref2mbident (NamedRef ident) = Just ident
			sueref2mbident (AnonymousRef _) = Nothing

		PtrType target_ty _ _ → let (typespecs,derivdeclrs) = ty2specs target_ty in
			(typespecs,derivdeclrs++[CPtrDeclr [] ni])

		ArrayType elem_ty arrsize _ _ → ( typespecs, derivdeclrs ++ [ CArrDeclr [] (arraysize2carraysize arrsize) ni ] )
			where
			(typespecs,derivdeclrs) = ty2specs elem_ty
			arraysize2carraysize (UnknownArraySize is_starred) = CNoArrSize is_starred
			arraysize2carraysize (ArraySize is_static sizeexpr) = CArrSize is_static sizeexpr

		TypeDefType (TypeDefRef ident refd_type ni) _ _ → ([CTypeDef ident ni],[])


lookupTagM :: SUERef → CovVecM TagDef
lookupTagM ident = do
	tags <- gets (gTags.globDeclsCVS)
	case Map.lookup ident tags of
		Just tagdef → return tagdef
		Nothing → myError $ "Tag " ++ (show ident) ++ " not found"

getMemberTypeM :: Type → Ident → CovVecM Type
getMemberTypeM ty member = do
	ty' <- elimTypeDefsM ty
	case ty' of
		DirectType (TyComp (CompTypeRef sueref _ _)) _ _ → do
			mem_tys <- getMembersM sueref
			case lookup member mem_tys of
				Nothing → myError $ "getMemberTypeM: Could not find member " ++ (render.pretty) member ++ " in " ++ (render.pretty) ty
				Just mem_ty → elimTypeDefsM mem_ty
		other → myError $ "getMemberTypeM " ++ (render.pretty) ty' ++ "\n    " ++ (render.pretty) other ++
			" not implemented!"

getMembersM :: SUERef → CovVecM [(Ident,Type)]
getMembersM sueref = do
	CompDef (CompType _ _ memberdecls _ _) <- lookupTagM sueref
	forM memberdecls $ \ (MemberDecl (VarDecl (VarName ident _) _ ty) Nothing _) → do
		return (ident,ty)

elimTypeDefsM :: Type → CovVecM Type
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

declaration2EnvItemM :: Declaration decl => decl → CovVecM [EnvItem]
declaration2EnvItemM decl = do
	let VarDecl (VarName srcident@(Ident srcname i ni) _) _ ty = getVarDecl decl
	ty' <- elimTypeDefsM ty
	return $ [ (srcident,(srcident,ty')) ]

mkIdentWithCNodePos :: (CNode cnode) => cnode → String → Ident
mkIdentWithCNodePos cnode name = mkIdent (posOfNode $ nodeInfo cnode) name (Name 9999)


newNameM :: CovVecM Int
newNameM = do
	new_var_num <- gets newNameIndexCVS
	-- strictly monotone newNameIndexCVS assures uniqueness of new identifiers:
	modify $ \ s → s { newNameIndexCVS = newNameIndexCVS s + 1 }
	return new_var_num

-- Takes an identifier and a type, and creates envitem(s) from that.

identTy2EnvItemM :: Ident → Type → CovVecM [EnvItem]
identTy2EnvItemM srcident@(Ident _ i ni) ty = do
	ty' <- elimTypeDefsM ty
	new_var_num <- newNameM
	let
		name_prefix = identToString srcident
		-- a dollar sign is not allowed in a C identifier, but Z3 allows for it.
		-- By inserting one in new names we avoid unlucky name collisions with already existing names in the source file
		newident_name = name_prefix ++ "$" ++ show new_var_num
		newident = Ident newident_name i ni

	-- also create envitems for structure/union members (recursively)
	-- (without giving new names)
		recurse_over_members new_ident new_ty = case new_ty of
			DirectType (TyComp (CompTypeRef sueref _ _)) _ _ → do
				members_tys <- getMembersM sueref
				concatForM members_tys $ \ (member_ident,member_ty) → do
					member_ty' <- elimTypeDefsM member_ty
					let
						newident = internalIdent $
							lValueToVarName $ CMember (CVar new_ident undefNode) member_ident False undefNode
					recs <- recurse_over_members newident member_ty'
					return $ (newident,(newident,member_ty')) : recs
			_ → return []
	newenvitems <- recurse_over_members newident ty'

	return $ newenvitems ++ [ (srcident,(newident,ty')) ]

type CIFE = StateT ([EnvItem],[TraceElem]) CovVecM [(EnvItem,CExprWithType)]

-- From a list of identifiers and types (i.e. the signature of the function to be analyzed),
-- create a list of EnvItems (representing the declarations) and CExprs.
createInterfaceM :: [(Ident,Type)] → CovVecM ([(EnvItem,CExprWithType)],([EnvItem],[TraceElem]))
createInterfaceM ty_env = runStateT cifes_m ([],[])
	where
	cifes_m :: CIFE
	cifes_m = do
		res <- forM ty_env $ \ tyenvitem@(srcident,ty) → do
			ty' <- lift $ elimTypeDefsM ty
			case ty' of
				ArrayType _ _ _ _ → modify $ \ (envitems,traceitems) → ((srcident,tyenvitem):envitems,traceitems)
				_ → return ()
			z3ty' <- lift $ ty2Z3Type ty'
			createInterfaceFromExpr_WithEnvItemsM (CVar srcident (nodeInfo srcident,z3ty')) ty
		return $ concat res

createInterfaceFromExprM :: CExprWithType → Type → CovVecM [(EnvItem,CExprWithType)]
createInterfaceFromExprM expr ty = evalStateT (createInterfaceFromExpr_WithEnvItemsM expr ty) ([],[])

createInterfaceFromExpr_WithEnvItemsM :: CExprWithType → Type → CIFE
createInterfaceFromExpr_WithEnvItemsM expr ty = do
	lift $ printLogV 20 $ "### createInterfaceFromExpr_WithEnvItemsM " ++ (render.pretty) expr ++ " " ++ (render.pretty) ty
	z3_ty <- lift $ ty2Z3Type ty
	lift $ printLogV 20 $ "###                                z3type = " ++ show z3_ty
	ty' <- lift $ elimTypeDefsM ty
	case ty' of

		-- STRUCT/UNION* p
		PtrType (DirectType (TyComp (CompTypeRef sueref kind _)) _ _) _ _ → prepend_plainvar ty' $ do
			union_constraints <- lift $ eqConstraintsM expr ty'
			modify $ \ (envitems,traceitems) -> (envitems,union_constraints++traceitems)
			member_ty_s <- lift $ getMembersM sueref
			ress <- forM ((if kind==UnionTag then take 1 else id) member_ty_s) $ \ (m_ident,m_ty) → do
				z3_m_ty <- lift $ ty2Z3Type m_ty
				createInterfaceFromExpr_WithEnvItemsM (CMember expr m_ident True (extractNodeInfo expr,z3_m_ty)) m_ty
			return $ concat ress

		-- ty* p
		PtrType target_ty _ _ → do
			z3_target_ty <- lift $ ty2Z3Type target_ty
			prepend_plainvar ty' $
				createInterfaceFromExpr_WithEnvItemsM (CUnary CIndOp expr (extractNodeInfo expr,z3_target_ty)) target_ty

		-- STRUCT/UNION expr
		DirectType (TyComp (CompTypeRef sueref kind _)) _ _ → do
			union_constraints <- lift $ eqConstraintsM expr ty'
			modify $ \ (envitems,traceitems) -> (envitems,union_constraints++traceitems)
			member_ty_s <- lift $ getMembersM sueref
			-- if it is a union, only take the first element
			ress <- forM ((if kind==UnionTag then take 1 else id) member_ty_s) $ \ (m_ident,m_ty) → do
				z3_m_ty <- lift $ ty2Z3Type m_ty
				createInterfaceFromExpr_WithEnvItemsM (CMember expr m_ident False (extractNodeInfo expr,z3_m_ty)) m_ty
			return $ concat ress

		-- direct-type expr where direct-type is no struct/union
		DirectType _ _ _ → prepend_plainvar ty' $ return []

		ArrayType elem_ty (ArraySize False constexpr) _ _ → do
			arr_size <- lift $ evalConstExprM constexpr
			elem_ty' <- lift $ elimTypeDefsM elem_ty
			ress <- forM [0..(arr_size - 1)] $ \ i → do
				elem_ty2 <- lift $ ty2Z3Type elem_ty'
				intty <- lift $ ty2Z3Type intType
				let
					ni = extractNodeInfo expr
					arrayelemexpr = CIndex expr (CConst $ CIntConst (cInteger i) (undefNode,intty)) (ni,elem_ty2)
					arrayelem_var = CVar (internalIdent $ lValueToVarName arrayelemexpr) (ni,elem_ty2)
					eqcond = Condition $ Left (arrayelem_var,arrayelemexpr)
				modify $ \ (envitems,traceitems) → ( envitems,traceitems ++ [eqcond] )
				createInterfaceFromExpr_WithEnvItemsM arrayelemexpr elem_ty'
			return $ concat ress

		_ → lift $ myError $ "createInterfaceFromExprM " ++ (render.pretty) expr ++ " " ++
			(render.pretty) ty' ++ " not implemented"

		where

		srcident = internalIdent $ lValueToVarName expr

		prepend_plainvar :: Type → CIFE → CIFE
		prepend_plainvar ty' rest_m = do
			rest1 <- rest_m
			return $ (((srcident,(srcident,ty')),expr) : rest1)

makeCompound :: [CBlockItem] → CStat
makeCompound cstats = CCompound [] cstats undefNode

wrapGoto :: Either (CExpr,CExpr) (Branch,CExpr) → CBlockItem
--wrapGoto (mb_branch,cond) = CBlockStmt $ CGotoPtr (CBinary CEqOp cond (CConst $ CStrConst (CString (show mb_branch) True) undefNode) undefNode) undefNode
wrapGoto (Right (branch,cond)) = CBlockStmt $ CGotoPtr (CBinary CEqOp cond (CConst $ CStrConst (CString (show branch) True) undefNode) undefNode) undefNode
wrapGoto (Left (varexpr,cond)) = CBlockStmt $ CGotoPtr (CBinary CNeqOp varexpr cond undefNode) undefNode
unwrapGoto :: CExpr → ((CExprWithType → TraceElem),CExpr)
unwrapGoto (CBinary CEqOp cond (CConst (CStrConst (CString s _) _)) _) = (make_cond,cond)
	where
	make_cond cond' = Condition (Right (branch,cond'))
	branch = case reads s of
		[(branch,"")] → branch
		[] → error $ "unwrapGoto: reads error for " ++ s

unwrapGoto x = error $ "unwrapGoto " ++ show x

forkUnfoldTraces :: Int → Progress → Bool → [a] → (Int → Progress → a → CovVecM UnfoldTracesRet) → CovVecM UnfoldTracesRet
forkUnfoldTraces forks progress toplevel l m = do
{-
	l' <- case reachFixedTrace of
		Nothing -> return l
		Just fixedtrace | length l == 1 -> return l
		Just fixedtrace -> do
			printLogV 0 $ "### fixedtrace = " ++ show fixedtrace
			printLogV 0 $ "### forks = " ++ show forks
			printLogV 0 $ "### length l = " ++ show (length l)
			return [ l !! ((fixedtrace!!forks)-1) ]
-}
	forM (zip [0..] l) cont >>= \ results → case partitionEithers results of
		(left_results,[]) → return $ Left $ concat left_results
		([],successes) → do
			subfuncov <- isOptionSet subfuncovOpt
			return $ Right $ case subfuncov || toplevel of
				-- Do not cover in subfunctions, and we are in a subfunction
				False -> or successes
				-- We should cover in subfunctions, or on toplevel
				True  -> and successes
	where
	cont (i,a) = m forks' (progress'f i) a
	alternatives = length l
	(forks',progress'f) = case alternatives > 1 of
		False -> (forks,const progress)
		True  -> (forks+1,\ i -> (i,alternatives):progress)

extractAnnotation :: CExpr → (CExpr,Maybe [Int])
extractAnnotation (CBinary CLndOp (CCall (CVar (Ident "solver_pragma" _ _) _) args _) real_cond ni) =
	(setNodeInfo ni real_cond,Just $ map arg2int args)
	where
	arg2int (CConst (CIntConst (CInteger i _ _) _)) = fromIntegral i
extractAnnotation cond = (cond,Nothing)

expandAssignRightSide :: CExpr → CExpr
expandAssignRightSide (CAssign assignop lexpr assigned_expr ni) = case assignop of
	CAssignOp → assigned_expr
	ass_op    → CBinary (assignBinop ass_op) lexpr assigned_expr ni

recognizeAnnotation :: CExpr → Trace → CovVecM (CExpr,Maybe ([Int],Int))
recognizeAnnotation cond trace = do
	let (real_cond,mb_args) = extractAnnotation cond
	-- set the NodeInfo in real_cond to the original NodeInfo of the *whole* condition that includes the solver_annotation
	-- otherwise, it will be reported as uncovered (have in mind: all branching points are determined before the analysis starts!)
	return ( real_cond, case mb_args of
		Nothing   → Nothing
		Just args → Just (args,num_reached) )
	where
	is_this_cond (Condition (Right (_,c))) = extractNodeInfo c == nodeInfo cond
	is_this_cond _ = False
	num_reached = length $ filter is_this_cond trace

createBranchesWithAnno :: CExpr → (CExpr → String) → Trace → CovVecM [(Branch,CExpr)]
createBranchesWithAnno cond makebranchname trace = do
	(real_cond,mb_annotation) <- recognizeAnnotation cond trace
	all_branches <- createBranches makebranchname real_cond
	branches_to_choose <- case mb_annotation of
		-- 12 is a wildcard in the choice list
		-- if the condition has been reached more often than the pragma list specifies, it is a wildcard
		Just (ns,num_reached) | length ns > num_reached && ns!!num_reached /= 12 → do
			printLogV 1 $ "\nRecognized annotation " ++ show (ns!!num_reached) ++ " to " ++ (render.pretty) real_cond ++
				" (reached " ++ show num_reached ++ " times)"
			return [ all_branches !! (ns!!num_reached - 1) ]
		_ → return all_branches
	return branches_to_choose

type LabelEnv = [(Ident,Trace → Int → CovVecM UnfoldTracesRet)]

unfoldTracesM :: LabelEnv → Maybe Type → Bool → Int → Progress → [Env] → Trace → [([CBlockItem],Bool)] → CovVecM UnfoldTracesRet
unfoldTracesM labelϵ ret_type toplevel forks progress ϵs trace cbss = do
--	logWrapper [ren "unfoldTracesM",ren ret_type,ren toplevel,ren forks,ren ϵs,ren trace,'\n':ren cbss] $ do
		(if forks > 0 && forks `mod` sizeConditionChunks == 0 then maybe_cutoff else id) $ do
			unfoldTraces1M labelϵ ret_type toplevel forks progress ϵs trace cbss
		where
		maybe_cutoff :: CovVecM UnfoldTracesRet → CovVecM UnfoldTracesRet
		maybe_cutoff cont = isOptionSet cutoffsOpt >>= \case
			False → cont
			True  → do
				incCutoffTriesM
				printLogV 1 $ "******* Probing for CutOff in depth " ++ show (length trace) ++ " ..."
				analyzeTraceM Nothing progress trace >>= \case
					False → do
						printLogV 1 $ "******** Cutting off !"
						incCutoffsM
						printStatsM 2
						return $ case toplevel of
							True  → Right False
							False → Left []
					True  → do
						printLogV 1 $ "******** Continuing..."
						cont

unfoldTraces1M :: LabelEnv → Maybe Type → Bool → Int → Progress → [Env] → Trace → [([CBlockItem],Bool)] → CovVecM UnfoldTracesRet
unfoldTraces1M labelϵ mb_ret_type toplevel forks progress ϵs trace bstss@((CBlockStmt stmt : rest,breakable) : rest2) =
	logWrapper [ren "unfoldTraces1M",ren "<labelenv>",ren mb_ret_type,ren toplevel,ren forks,ren $ take 2 ϵs,ren trace,'\n':ren bstss] $ do
--		printLogV 0 $ showTrace trace
		case stmt of

			CCompound _ cbis _ → unfoldTracesM labelϵ mb_ret_type toplevel forks progress ([]:ϵs) trace ((cbis,False) : (rest,breakable) : rest2)

			CLabel labelident cstat _ _ → do
				let jumphere_m = \ trace forks → unfoldTracesM ((labelident,jumphere_m):labelϵ) mb_ret_type toplevel forks progress ϵs trace (((CBlockStmt cstat : rest, breakable)) : rest2)
				jumphere_m trace forks

			CSwitch condexpr0 (CCompound [] cbis _) switch_ni → do
				(condexpr,mb_anno) <- recognizeAnnotation condexpr0 trace
				ctrue <- ⅈ 1
				_1 <- ⅈ 1
				_2 <- ⅈ 2
				let
					cond_ni = nodeInfo condexpr
					cond_var_ident = mkIdentWithCNodePos condexpr $ "cond_" ++ (locationToName $ lineColNodeInfo condexpr)
					-- we have to evaluate the switch'ed expression only once, and in the beginning,
					-- since there could be side effects in it! (May God damn them...)
					cond_var = CVar cond_var_ident cond_ni

					-- Go through all the switch's "case"s and "default"s...
					collect_stmts :: Int → CExpr → [CBlockItem] → [CBlockItem]

					collect_stmts _ _ [] = []

					collect_stmts branchnum notconds (CBlockStmt cdefault@(CDefault def_stmt default_ni) : rest) =
						-- This is to explicitly cover the "default" branch (located at the "default" keyword).
						wrapGoto (Right (Branch (lineColNodeInfo cdefault) branchnum False makeDefaultBranchName,notconds)) :
							CBlockStmt def_stmt :
							( for rest $ \case
								CBlockStmt (CCase _ stmt _)  → default_not_last_err
								CBlockStmt (CDefault stmt _) → default_not_last_err
								cbi → cbi )
								where
								default_not_last_err = error $ ren default_ni ++ " : " ++
									"collect_stmts: the case when 'default' is not the last item in the switch is not implemented"

					-- flatten if the stmt of the case is the next case
					collect_stmts branchnum notconds (CBlockStmt ccase@(CCase caseexpr subcase@(CCase _ _ _) case_ni) : rest) =
						collect_stmts branchnum notconds (
							CBlockStmt (CCase caseexpr (CExpr Nothing undefNode) case_ni) : CBlockStmt subcase : rest )

					-- if we have a "case <expr>: stmt", insert "if (expr==cond_var) { stmt; rest } else <recurse_collect_stmts>"
					collect_stmts branchnum notconds (CBlockStmt ccase@(CCase caseexpr stmt case_ni) : rest) = [
						CBlockStmt $ CIf if_cond_mb_with_anno
							(makeCompound $
								wrapGoto (Right (Branch (lineColNodeInfo ccase) branchnum False ("case " ++ (render.pretty) caseexpr),cond)) :
								CBlockStmt stmt :
								filtercases rest)
							(Just $ makeCompound $ collect_stmts (branchnum+1) notconds' rest)
							undefNode ]
						where
						cond = cond_var ⩵ caseexpr
						notconds' = notconds ⋏ not_ cond
						-- Eliminate the case/default "wrappers" from a statement list.
						filtercases :: [CBlockItem] → [CBlockItem]
						filtercases cbis = for cbis $ \case
							CBlockStmt (CCase _ stmt _)  → CBlockStmt stmt
							CBlockStmt (CDefault stmt _) → CBlockStmt stmt
							cbi → cbi
						if_cond_mb_with_anno = case mb_anno of
							Just (args,num_reached) →
								CBinary CLndOp (CCall (CVar (internalIdent "solver_pragma") undefNode) case_args undefNode)
									pure_cond (nodeInfo pure_cond)
								where
								case_args = for (filter (>=branchnum) args) $ \ arg -> if arg==branchnum then _1 else _2
							_ → pure_cond
							where
							-- undefNode
							pure_cond = CBinary CEqOp cond_var caseexpr case_ni

					-- if it was neither a "case" or "default", skip it.
					collect_stmts branchnum notconds (_:rest) = collect_stmts branchnum notconds rest

					-- This is the whole switch, rewritten as nested if-then-elses.
					case_replacement = collect_stmts 1 ctrue cbis

				unfoldTracesM labelϵ mb_ret_type toplevel (forks+1) progress ([]:ϵs) trace (
					(CBlockDecl (CDecl [CTypeSpec $ CLongType cond_ni]
						[(Just $ CDeclr (Just cond_var_ident) [] Nothing [] cond_ni,
						Just $ CInitExpr condexpr cond_ni, Nothing)] cond_ni) :
					case_replacement,True) :
					(rest,breakable) : rest2 )

			CBreak ni → do
				-- The scope that break reaches is the successor of the first "breakable" scope
				let
					drop_after_true (_:l1s) ((_,False):l2s) = drop_after_true l1s l2s
					drop_after_true (_:l1s) ((_,True):l2s) = return (l1s,l2s)
					drop_after_true l1s l2s = myError $ "drop_after_true " ++ ren l1s ++ " " ++ ren l2s
				(new_envs,new_bstss) <- drop_after_true ϵs bstss
				unfoldTracesM labelϵ mb_ret_type toplevel forks progress new_envs trace new_bstss

			CIf cond then_stmt mb_else_stmt ni → do
				branches_to_follow <- createBranchesWithAnno cond makeIfBranchName trace
				forkUnfoldTraces forks progress toplevel branches_to_follow $ \ forks' progress' (branch,branch_cond) → do
					let
						-- Only insert a condition if the CIf was not generated by TVG (for translation of while, e.g.)
						branchcbi = if ni==undefNode then [] else [wrapGoto $ Right (branch,branch_cond)]
						cbstmts = branchcbi ++ case isElseBranch branch of
							False → [CBlockStmt then_stmt]
							True  → case mb_else_stmt of
								Nothing → []
								Just else_stmt → [CBlockStmt else_stmt]
					unfoldTracesM labelϵ mb_ret_type toplevel forks' progress' ϵs trace ( (cbstmts ++ rest,breakable) : rest2 )

			CReturn Nothing _ → case toplevel of
				False -> return $ Left [(forks,progress,ϵs,trace)]
				True  -> analyzeTraceM mb_ret_type progress trace >>= return.Right

			CReturn (Just ret_expr) _ | Just ret_type <- mb_ret_type → do
				z3_ret_type <- ty2Z3Type ret_type
				transids ret_expr (Just z3_ret_type) trace forks progress $ \ forks' progress' (envs',ret_expr',trace') → do
					case toplevel of
						False → return $ Left [ (forks',progress',envs',Return ret_expr' : trace') ]
						True  → do
							Just ret_var_expr <- gets retEnvCVS
							ret_env_expr <- createInterfaceFromExprM ret_expr' ret_type
							when (length ret_var_expr /= length ret_env_expr) $ error "unfoldTraces1M CReturn: length ret_var_expr /= length ret_env_expr !"
							ret_trace <- concatForM (zip ret_var_expr ret_env_expr) $
								\ ( ((_,(ret_var_ident,ret_var_ty)),_) , (_,ret_member_expr)) → do
									z3_ret_var_ty <- ty2Z3Type ret_var_ty
									return [ Condition (Left ((CVar ret_var_ident (nodeInfo ret_var_ident,z3_ret_var_ty)),ret_member_expr)), NewDeclaration (ret_var_ident,ret_var_ty) ]
							analyzeTraceM mb_ret_type progress' (Return ret_expr' : (ret_trace ++ trace'))
								>>= return.Right

			CExpr (Just (CCall (CVar (Ident is _ _) _) [_,expr] ni)) _ | "solver_debug" `isPrefixOf` is → do
				expr' <- transcribeExprM ϵs Nothing expr
				let dbgout = DebugOutput ("solver_debug_" ++ lValueToVarName expr') expr'
				unfoldTracesM labelϵ mb_ret_type toplevel forks progress ϵs (dbgout : trace) ((rest,breakable):rest2)

			CExpr (Just (CCall (CVar (Ident "solver_find" _ _) _) _ _)) _ → do
				unfoldTracesM labelϵ mb_ret_type toplevel forks progress ϵs (SolverFind:trace) ((rest,breakable):rest2)

			CExpr (Just (CCall cvar@(CVar (Ident "solver_error" _ _) _) _ _)) _ → do
				myError $ "solver_error() reached at " ++ showLocation (lineColNodeInfo cvar)

			CExpr (Just cass@(CAssign _ lexpr _ ni)) _ → do
				lexpr' <- transcribeExprM ϵs Nothing lexpr
				transids (expandAssignRightSide cass) (Just $ extractTypes lexpr') trace forks progress $ \ forks' progress' (ϵs',assigned_expr,trace') →
					unfoldTracesM labelϵ mb_ret_type toplevel forks' progress' ϵs'
						(Assignment (Normal lexpr') assigned_expr : trace') ((rest,breakable):rest2)

			CExpr (Just (CUnary unaryop expr ni_op)) ni | unaryop `elem` (map fst unaryops) → do
				ii <- ⅈ 1
				let stmt' = CExpr (Just $ CAssign assignop expr ii ni) ni
				unfoldTracesM labelϵ mb_ret_type toplevel forks progress ϵs trace ( (CBlockStmt stmt' : rest,breakable) : rest2 )
				where
				Just assignop = lookup unaryop unaryops
				unaryops = [ (CPreIncOp,CAddAssOp),(CPostIncOp,CAddAssOp),(CPreDecOp,CSubAssOp),(CPostDecOp,CSubAssOp) ]

			CExpr (Just expr) _ → do
				transids expr Nothing trace forks progress $ \ forks' progress' (ϵs',_,trace') → do
					unfoldTracesM labelϵ mb_ret_type toplevel forks' progress' ϵs' trace' ( (rest,breakable) : rest2 )

			CExpr Nothing _ → unfoldTracesM labelϵ mb_ret_type toplevel forks progress ϵs trace ( (rest,breakable) : rest2 )

			CGoto ident _ → do
				case lookup ident labelϵ of
					-- label already encountered, execute memoized action
					Just m  → m trace forks
					Nothing → search_label ϵs ( (rest,breakable) : rest2 )
						where
						search_label ϵs cbiss@( (CBlockStmt (CLabel labelident cstat _ _) : rest,breakable) : rest2 ) | labelident == ident =
							unfoldTracesM labelϵ mb_ret_type toplevel forks progress ϵs trace cbiss
						search_label ϵs ( (_ : rest,breakable) : rest2 ) = search_label ϵs ( (rest,breakable) : rest2 )
						search_label (_:envs) ( ([],_) : rest2 ) = search_label ϵs rest2
						search_label [] [] = error $ "search_label: " ++ (render.pretty) ident ++ " not found!"

			-- That's cheating: Insert condition into trace (for loop unrolling and switch) via GOTO
			CGotoPtr wrapped ni → do
				let (make_cond,cond) = unwrapGoto wrapped
				transids cond (Just _BoolTypes) trace forks progress $ \ forks' progress' (ϵs',cond',trace') → do
					unfoldTracesM labelϵ mb_ret_type toplevel forks' progress' ϵs' (make_cond cond' : trace') ( (rest,breakable) : rest2 )

			CWhile cond body False ni → do
				(mb_unrolling_depths,msg) <- infer_loopingsM cond body
				printLogV 1 $ msg
				createBranches makeForWhileBranchName cond >>= unroll_loopM ( case mb_unrolling_depths of
					Nothing → uNROLLING_STRATEGY
					Just ns → ns )

				where

				unroll_loopM :: [Int] → [(Branch,CExpr)] → CovVecM UnfoldTracesRet
				unroll_loopM depths branch_exprs = do
					forkUnfoldTraces forks progress toplevel depths $ \ forks' progress' depth → do
						forkUnfoldTraces forks' progress' toplevel (unroll depth) $ \ forks'' progress'' cbi_trace → do
							unfoldTracesM labelϵ mb_ret_type toplevel forks'' progress'' ([]:ϵs) trace ( (cbi_trace,True) : (rest,breakable) : rest2 )

					where

					(else_branches,then_branches) = partition (isElseBranch.fst) branch_exprs

					unroll :: Int → [[CBlockItem]]
					unroll 0 = for else_branches $ \ (branch,cond) →[ wrapGoto $ Right (branch,cond) ]
					unroll n = for then_branches $ \ (branch,cond) →
						concatMap (\ l → wrapGoto (Right (branch,cond)) : CBlockStmt body : l) $ unroll (n-1)

			-- Express the for loop as a bisimular while loop
			CFor precond mb_cond mb_inc_expr stmt ni → do
				ii <- ⅈ 1
				let
					cbis = case precond of
						Right decl → [CBlockDecl decl]
						Left mb_expr → [ CBlockStmt $ CExpr mb_expr undefNode ]
					while_body = makeCompound $ CBlockStmt stmt :
						maybe [] (\ expr → [ CBlockStmt $ CExpr (Just expr) (nodeInfo expr) ]) mb_inc_expr
					body_stmt = CWhile (maybe ii id mb_cond) while_body False ni
				unfoldTracesM labelϵ mb_ret_type toplevel forks progress ϵs trace ((cbis ++ [CBlockStmt body_stmt] ++ rest,breakable) : rest2)

			_ → myError $ "unfoldTracesM " ++ (render.pretty) stmt ++ " not implemented yet"

		where

		infer_loopingsM :: CExpr → CStat → CovVecM (Maybe [Int],String)
		infer_loopingsM cond0 body = logWrapper [ren "infer_loopingsM",ren cond0,'\n':ren body] $ do
			rec_anno <- recognizeAnnotation cond0 trace
			isOptionSet noLoopInferenceOpt >>= \case
				True -> return (Nothing,"\nLoop inference switched off.")
				False -> case rec_anno of
					(real_cond,Just (ns,_)) → do
						printLogV 20 $ "\n### Loop anno = " ++ show ns
						return (Just ns,"\nRecognized LOOP annotation to " ++ (render.pretty) cond0)
					(real_cond,Nothing) → do
		--				let default_ns = [0,1,2]
		--				return (Just default_ns,"No annotation, trying " ++ show default_ns)
	
						translateExprM labelϵ ϵs toplevel real_cond (Just _BoolTypes) trace 0 [] >>= \case
							[(_,_,_,cond,_)] → do
								let
									-- get all variables used in the condition
									cond_idents = fvar cond
								-- unfold body to all body traces and filter for all Assignments to variables from the condition
								Left body_traces <- unfoldTracesM labelϵ mb_ret_type False forks progress ϵs [] [([CBlockStmt body],True)]
								let
									body_traces_ass = map (concatMap from_ass) $ map (\(_,_,_,tr) -> tr) body_traces where
										from_ass (Assignment (Normal a@(CVar i _)) b) | i `elem` cond_idents = [(a,b)]
										from_ass _ = []
								printLogV 20 $ "body_traces_ass =\n" ++
									(unlines $ for body_traces_ass $ \ bta →
										intercalate " , " (map (\(a,b) → "(" ++ (render.pretty) a ++ " = " ++ (render.pretty) b ++ ")") bta))
	
								-- Filter for all assignments that occur exactly once in every body trace
								let
									body_assigns = foldl1 intersect (map (exists_once) body_traces_ass)
									exists_once l = filter (\ e → length (filter (==e) l) == 1) l
								printLogV 2 $ "body_assigns = \n" ++
									intercalate " , " (map (\(a,b) → "(" ++ (render.pretty) a ++ " = " ++ (render.pretty) b ++ ")") body_assigns)
	
								case body_assigns :: [(CExprWithType,CExprWithType)] of
									[ (counter_var@(CVar ass_ident (_,(ass_ident_z3ty,_))),ass_expr) ] → do
										let
											is_ass_to_ass_var (Assignment (Normal (CVar ident _)) _) | ident==ass_ident = True
											is_ass_to_ass_var _ = False
										case filter is_ass_to_ass_var trace of
											[] → return (Nothing,"infer_loopingsM: There is no assignment to the loop counter " ++ (render.pretty) counter_var ++ " prior to the loop")
											ass@(Assignment _ i_0) : _ | null (fvar i_0) → do
												inttypes <- ty2Z3Type intType
												printLogV 2 $ "last assignment to loop counter is " ++ show ass
												let i_n :: CExprWithType → CovVecM CExprWithType = case ass_expr of
													-- for all binops where the following holds (Linearity?):
													-- i_n = i_(n-1) `binop` c  =>  i_n = i_0 `binop` c
													CBinary binop (CVar ident _) cconst@(CConst _) _ | ident == ass_ident ∧ binop `elem` [CSubOp,CAddOp,CShrOp,CShlOp] → \ n_var → do
														return $ CBinary binop i_0 (n_var ∗ cconst) (undefNode,inttypes)
													_ → error $ "infer_loopingsM: assignment " ++ (render.pretty) ass_ident ++ " := " ++ (render.pretty) ass_expr ++ " not implemented!"
												let
													n_name = "n$loopings"
													n_ident = internalIdent n_name
													n = CVar n_ident (undefNode,inttypes)
													modelpath = analyzerPath </> n_name ++ show (lineColNodeInfo $ extractNodeInfo cond) ++ ".smtlib2"
												n_types <- case lookup ass_ident (envs2tyenv ϵs) of
													Nothing → myError $ "infer_loopingsM: Could not find type of " ++ (render.pretty) counter_var
													Just ty → ty2Z3Type ty
												i_n_n_var <- i_n n
												_0 <- ⅈ 0
												_1 <- ⅈ 1
												i_n_n_var_minus_1 <-  i_n $ n − _1
												i_n_0 <- i_n _0
												tyenv <- tyEnvFromTraceM trace
												(model_string,mb_sol) <- makeAndSolveZ3ModelM
													[]
													((n_ident,ass_ident_z3ty) : tyenv)
													(let
														cond_n       = (counter_var `substituteBy` i_n_n_var) cond
														cond_nminus1 = (counter_var `substituteBy` i_n_n_var_minus_1) cond
														cond_0       = (counter_var `substituteBy` i_n_0) cond
														in
														map (\ cond -> Condition $ Right (NoBranch,cond) ) [
															n ⩾ _0,
															not_ cond_0  ⋏  n ⩵ _0
																⋎
																cond_nminus1 ⋏ n ⩾ _1 ⋏ not_ cond_n
														])
													[ SExpr [SLeaf "minimize",SLeaf n_name] ]
													[n_ident]
													modelpath
												return $ case mb_sol of
													Nothing                 → (Nothing,"Found no solution for " ++ modelpath)
													Just sol@[(_,IntVal n)] → (Just [n], "Found looping solution n = " ++ show sol)
													_                       → (Nothing,"n_looping: Strange mb_sol=" ++ show mb_sol)
											ass → return (Nothing,"infer_loopingsM: " ++ show ass ++ " is not assigning a constant.")
	
									other → return (Nothing,"body contains not exactly one assignment of a variable from the condition " ++ (render.pretty) cond ++ ":\n" ++
										unlines (map (\(ass_var,_) → (render.pretty) ass_var) other))
	
							_ → do
								return (Nothing,"condition " ++ (render.pretty) cond0 ++ " at " ++ (showLocation.lineColNodeInfo) cond0 ++ " contains a function call!")

		-- mb_ty is Nothing if the result type of expr is not known, i.e. no casting necessary.
		transids :: CExpr → Maybe Types → Trace → Int → Progress → (Int → Progress → ([Env],CExprWithType,Trace) → CovVecM UnfoldTracesRet) → CovVecM UnfoldTracesRet
		transids expr mb_ty trace forks progress cont = logWrapper ["transids",ren expr,ren mb_ty,ren trace,"<cont>"] $ do
			additional_envs_expr_traces :: [(Int,Progress,[Env],CExprWithType,Trace)] <-
				translateExprM labelϵ ϵs toplevel expr mb_ty trace forks progress
			forkUnfoldTraces forks progress toplevel additional_envs_expr_traces $ \ forks' progress' (trans_forks,trans_progress,ϵs',expr',trace') → do
				diff_progress <- case stripPrefix (reverse progress) (reverse trans_progress) of
					Just diff_p -> return diff_p
					Nothing -> do
						printLogV 0 $ "progress = " ++ show progress
						printLogV 0 $ "trans_progress = " ++ show trans_progress
						myError "diff_progress"
				cont (forks'+trans_forks-forks) (reverse diff_progress ++ progress') (ϵs',expr',trace')

unfoldTraces1M labelϵ mb_ret_type toplevel forks progress ϵs@(ϵ:restϵs) trace ( (cblockitem@(CBlockDecl decl@(CDecl typespecs triples _)) : rest, breakable) : rest2 ) =
	logWrapper [ren "unfoldTraces1M CBlockDecl",ren mb_ret_type,ren toplevel,ren forks,ren $ take 2 ϵs,ren trace,'\n':ren cblockitem] $ do
		ty <- decl2TypeM decl
		new_env_items <- forM triples $ \case
			(Just (CDeclr (Just ident) derivdeclrs _ _ ni),mb_init,Nothing) → do
				newenvitems <- identTy2EnvItemM ident ty
				let
					newdecls = map (NewDeclaration . snd) newenvitems
				tys <- ty2Z3Type ty
				let
					Just (ident',_) = lookup ident newenvitems
				eq_constraints <- eqConstraintsM (CVar ident' (undefNode,tys)) ty
				printLogV 20 $ "XXX eq_constraints=\n" ++ showTrace eq_constraints
				initializers <- case mb_init of
					Nothing → return []
					Just initializer → cinitializer2blockitems (CVar ident ni) ty initializer
				return (newenvitems,eq_constraints++newdecls,initializers)
			triple → myError $ "unfoldTracesM: triple " ++ show triple ++ " not implemented!"
		let (newϵs,newitems,initializerss) = unzip3 $ reverse new_env_items
		unfoldTracesM labelϵ mb_ret_type toplevel forks progress ((concat newϵs ++ ϵ) : restϵs) (concat newitems ++ trace) ((concat initializerss ++ rest,breakable):rest2)

unfoldTraces1M labelϵ mb_ret_type toplevel@False forks progress ϵs trace cbss@[([],_)] =
	logWrapper [ren "unfoldTraces1M END_NOT_TOPLEVEL",ren mb_ret_type,ren toplevel,ren forks,ren $ take 2 ϵs,ren trace,'\n':ren cbss] $ do
		return $ Left [(forks,progress,ϵs,trace)]

unfoldTraces1M labelϵ mb_ret_type toplevel forks progress ϵs@(_:restenvs) trace cbss@(([],_):rest2) =
	logWrapper [ren "unfoldTraces1M LEAVE_SCOPE",ren mb_ret_type,ren toplevel,ren forks,ren $ take 2 ϵs,ren trace,'\n':ren cbss] $ do
		unfoldTracesM labelϵ mb_ret_type toplevel forks progress restenvs trace rest2

unfoldTraces1M _ mb_ret_type True _ progress _ trace [] = analyzeTraceM mb_ret_type progress trace >>= return.Right
-- This case only appears during loop length inference with breaks in the path
unfoldTraces1M _ mb_ret_type False forks progress ϵs trace [] = return $ Left [(forks,progress,ϵs,trace)]

unfoldTraces1M labelϵ mb_ret_type toplevel forks progress ϵs trace bstss =
	logWrapper [ren "unfoldTraces1M",ren "<labelenv>",ren mb_ret_type,ren toplevel,ren forks,ren $ take 2 ϵs,ren trace,'\n':ren bstss] $ do
		myError $ "unfoldTraces1M not fully implemented (see above)"

eqConstraintsM :: CExprWithType -> Type -> CovVecM [TraceElem]
eqConstraintsM expr ty = do
	ty' <- elimTypeDefsM ty
	printLogV 20 $ "eqConstraintsM " ++ (render.pretty) expr ++ " " ++ (render.pretty) ty'
	case ty' of
		PtrType (DirectType (TyComp (CompTypeRef sueref UnionTag _)) _ _) _ _ → union_eq_constraints sueref True
		DirectType (TyComp (CompTypeRef sueref UnionTag _)) _ _ → union_eq_constraints sueref False
{-
		DirectType (TyFloating floattype) _ _ → do
			bv_ty <- sizeofTy ty' >>= sizeToIntTypeM
			bv_z3ty <- ty2Z3Type bv_ty
			let bv_ident = internalIdent $ makeFloatBVVarName (lValueToVarName expr)
			return [
				Condition $ Left (CVar bv_ident (undefNode,bv_z3ty) , expr ) ,
				NewDeclaration (bv_ident,bv_ty)
				]
-}
		_ -> return []
	where

	union_eq_constraints :: SUERef -> Bool -> CovVecM [TraceElem]
	union_eq_constraints sueref is_ptr = do
		((ident_0,member_ty_0) : ident_types) <- getMembersM sueref
		ident_0_ty <- ty2Z3Type member_ty_0
		forM ident_types $ \ (ident_i,member_ty_i) -> do
			ident_i_ty <- ty2Z3Type member_ty_i
			return $ Assignment (Normal $ CMember expr ident_0 is_ptr (undefNode,ident_0_ty)) $
				CCast (CDecl [] [] (undefNode,ident_0_ty))
					( CMember expr ident_i is_ptr (undefNode,ident_i_ty) )
					(undefNode,ident_0_ty)
{-
			return $ Condition $ Left
				(CMember expr ident_0 is_ptr (undefNode,ident_0_ty),
				CCast (CDecl [] [] (undefNode,ident_0_ty))
					( CMember expr ident_i is_ptr (undefNode,ident_i_ty) )
					(undefNode,ident_0_ty))
-}

infix 4 ⩵
(⩵) :: CExpression a → CExpression a → CExpression a
a ⩵ b = CBinary CEqOp a b (annotation a)

infix 4 !⩵
(!⩵) :: CExpression a → CExpression a → CExpression a
a !⩵ b = not_ $ CBinary CEqOp a b (annotation a)

infix 4 ⩾
(⩾) :: CExprWithType → CExprWithType → CExprWithType
a ⩾ b = CBinary CGeqOp a b (annotation a)

infixr 3 ⋏
(⋏) :: CExpression a → CExpression a → CExpression a
a ⋏ b = CBinary CLndOp a b (annotation a)

infixr 2 ⋎
(⋎) :: CExpression a → CExpression a → CExpression a
a ⋎ b = CBinary CLorOp a b (annotation a)

infixr 7 ∗
(∗) :: CExprWithType → CExprWithType → CExprWithType
a ∗ b = CBinary CMulOp a b (annotation a)

infixr 6 −
(−) :: CExprWithType → CExprWithType → CExprWithType
a − b = CBinary CSubOp a b (annotation a)

not_ :: CExpression a → CExpression a
not_ e = CUnary CNegOp e (annotation e)

class CreateInt a where
	ⅈ :: Integer → CovVecM a
instance CreateInt CExpr where
	ⅈ i = return $ CConst $ CIntConst (cInteger i) undefNode
instance CreateInt CExprWithType where
	ⅈ i = do
		inttype <- ty2Z3Type intType
		return $ CConst $ CIntConst (cInteger i) (undefNode,inttype)

infix 1 ≔
(≔) :: CExpression a → CExpression a → CStatement a
ass ≔ expr = CExpr (Just $ CAssign CAssignOp ass expr (annotation ass)) (annotation expr)


fvar :: CExprWithType → [Ident]
fvar expr = nub $ everything (++) (mkQ [] searchvar) (everywhere (mkT delete_attrs) expr)
	where

	delete_attrs :: CAttribute NodeInfoWithType → CAttribute NodeInfoWithType
	delete_attrs (CAttr ident _ a) = CAttr ident [] a

	searchvar :: CExpression NodeInfoWithType → [Ident]
	searchvar (CVar ident _) = [ ident ]
	searchvar _ = []

cinitializer2blockitems :: CExpr → Type → CInit → CovVecM [CBlockItem]
cinitializer2blockitems lexpr ty initializer = logWrapper ["cinitializer2blockitems",ren lexpr,ren ty,ren initializer] $ do
	case initializer of
		CInitExpr expr ni_init → do
			let cbi = CBlockStmt $ lexpr ≔ expr
			return [ cbi ]
		CInitList initlist ni_init → do
			ty' <- elimTypeDefsM ty
			case ty' of
				DirectType (TyComp (CompTypeRef sueref _ _)) _ _ → do
					memberidentstypes <- getMembersM sueref
					concatForM (zip initlist memberidentstypes) $ \case
						(([],initializer),(memberident,memberty)) → do
							memberty' <- elimTypeDefsM memberty
							cinitializer2blockitems (CMember lexpr memberident False (nodeInfo memberident)) memberty' initializer
						_ → myError $ "cinitializer2blockitems DirectType: CPartDesignators not implemented yet in\n" ++ (render.pretty) ty
				ArrayType elem_ty _ _ _ → do
					concatForM (zip [0..] initlist) $ \ (i,(partdesigs,cinitializer)) → do
						case partdesigs of
							[] → do
								ii <- ⅈ i
								cinitializer2blockitems (CIndex lexpr ii ni_init) elem_ty cinitializer
							_ → myError $ "cinitializer2blockitems ArrayType: CPartDesignators not implemented yet in\n" ++ (render.pretty) ty
				_ → myError $ "cinitializer2blockitems: " ++ (render.pretty) ty' ++ " at " ++ (show $ nodeInfo lexpr) ++ " not implemented!"

showFullLocation :: (CNode a) => a → String
showFullLocation cnode = (posFile $ posOfNode $ nodeInfo cnode) ++ " : " ++ (showLocation.lineColNodeInfo) cnode

-- Creates an CExprWithType from a CExpr
transcribeExprM :: [Env] → Maybe Types → CExpr → CovVecM CExprWithType
transcribeExprM ϵs mb_target_ty expr = do
	vars' <- renameVars ϵs expr
	annotateTypesAndCastM ϵs vars' mb_target_ty
	where
	-- Renames Variables to unique names, looking up their unique name (wíth a number suffix)

	-- The renaming should be reflected in the type, smth. like
	-- newtype IsRenamed = IsRenamed NodeInfo
	-- type CExprRenamed = CExpression IsRenamed
	--
	-- But the above would be over-engineering, since renameVars is only used here.
	renameVars :: [Env] → CExpr → CovVecM CExpr
	renameVars ϵs expr = everywhereM (mkM subst_var) expr where
		subst_var :: CExpr → CovVecM CExpr
		subst_var (CVar ident ni) = case lookup ident (concat ϵs) of
			Just (ident',_) → return $ CVar ident' ni
			Nothing → myError $ " in subst_var: Could not find " ++ (render.pretty) ident ++
				" when renaming " ++ (render.pretty) expr ++ " at " ++ showFullLocation expr ++ "\n" ++
				"env = \n" ++ envToString (concat ϵs)
		subst_var expr = return expr

	-- adds Z3 types and Language.C.Type's to the annotation that the expressions have
	-- also inserts implicit casts
	-- if mb_target_ty is Nothing, the result CExprWithType will not be casted to the mb_target_ty type.
	annotateTypesAndCastM :: [Env] → CExpr → Maybe Types → CovVecM CExprWithType
	annotateTypesAndCastM ϵs cexpr mb_target_ty = logWrapper ["annotateTypesAndCastM",ren $ take 1 ϵs,ren cexpr,ren mb_target_ty] $ do
		cexpr' <- annotate_types cexpr
		return $ case mb_target_ty of
			Just target_ty → mb_cast target_ty cexpr'
			_ → cexpr'

		where

		tyenv = envs2tyenv ϵs

		annotate_types :: CExpr → CovVecM CExprWithType

		-- Get rid of CNeqOp now, later in expr2sexpr it will be more difficult...
		annotate_types (CBinary CNeqOp expr1 expr2 ni) = annotate_types $ amap (const ni) (expr1 !⩵ expr2)
		annotate_types (CBinary binop expr1 expr2 ni) = do
			expr1' <- annotate_types expr1
			expr2' <- annotate_types expr2
			let
				common_ty = case binop `elem` [CLndOp,CLorOp] of
					False → max (extractTypes expr1') (extractTypes expr2')
					True  → _BoolTypes
				result_ty = case binop `elem` [CLndOp,CLorOp,CLeOp,CGrOp,CLeqOp,CGeqOp,CEqOp,CNeqOp] of
					True  → _BoolTypes
					False → common_ty
			return $ CBinary binop (mb_cast common_ty expr1') (mb_cast common_ty expr2') (ni,result_ty)

		annotate_types ccast@(CCast decl expr ni) = do
			mb_cast <$> (decl2TypeM decl >>= elimTypeDefsM >>= ty2Z3Type) <*> annotate_types expr

		annotate_types cunary@(CUnary unop expr ni) = do
			expr' <- annotate_types expr
			let (arg_ty,result_ty) = case (unop,extractTypes expr') of
				(CNegOp, _)                                → (_BoolTypes,_BoolTypes)
				(CAdrOp, ty@(z3ty,cty))                    → (ty,(Z3_Ptr z3ty,ptrType cty))
				(CIndOp, ty@(Z3_Ptr z3ty,PtrType cty _ _)) → (ty,(z3ty,cty))
				(CIndOp, _) → error $ "annotate_types: argument type of " ++ (render.pretty) cunary ++ " is no Ptr!"
				(_,      ty)                               → (ty,ty)
			let erg = CUnary unop (mb_cast arg_ty expr') (ni,result_ty)
			return erg

		annotate_types (CCond cond_expr (Just then_expr) else_expr ni) = do
			cond_expr' <- annotate_types cond_expr
			then_expr' <- annotate_types then_expr
			else_expr' <- annotate_types else_expr
			let common_ty = max (extractTypes then_expr') (extractTypes else_expr')
			return $ CCond (mb_cast _BoolTypes cond_expr')
				(Just $ mb_cast common_ty then_expr') (mb_cast common_ty else_expr') (ni,common_ty)

		annotate_types cvar@(CVar ident ni) = case lookup ident tyenv of
			Nothing → myError $ "Could not find " ++ (render.pretty) ident ++ " in " ++ showTyEnv tyenv
			Just ty → do
				var_ty <- elimTypeDefsM ty >>= ty2Z3Type
				return $ CVar ident (ni,var_ty)

		annotate_types (CConst ctconst) = case ctconst of
			CIntConst cint@(CInteger _ _ flags) ni → do
				z3_ty <- ty2Z3Type $ integral (getIntType flags)
				return $ CConst $ CIntConst cint (ni,z3_ty)
			CFloatConst cfloat@(CFloat s) ni       → do
				z3_ty <- ty2Z3Type $ floating (getFloatType s)
				return $ CConst $ CFloatConst cfloat (ni,z3_ty)
			CCharConst cchar@(CChar _ False) ni    → do
				z3_ty <- ty2Z3Type $ charType
				return $ CConst $ CCharConst cchar (ni,z3_ty)
			CStrConst cstr ni                      → do
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

		mb_cast :: (Z3_Type,Type) → CExprWithType → CExprWithType
		mb_cast to_ty cexpr = case (extractZ3Type cexpr,fst to_ty) of
			( ty1, ty2 ) | ty1==ty2 → cexpr
			( Z3_BitVector size_from _, Z3_BitVector size_to _ ) | size_from == size_to → cexpr
			_ → cast cexpr to_ty
			where
			cast :: CExprWithType → Types → CExprWithType
			-- The NodeInfo of the CDecl will contain the cast target type (we do not want to convert to DeclSpecs...)
			cast cexpr to_ty = CCast (CDecl [] [] to_anno) cexpr to_anno
				where
				to_anno = (extractNodeInfo cexpr,to_ty)

type CallOrTernaryIfs = [Either (Ident,[CExpr],NodeInfo) [[CBlockItem]]]

scanExprM :: [Env] → CExpr → Maybe Types → Trace → CovVecM (CExpr,CallOrTernaryIfs)
scanExprM ϵs expr0 mb_target_ty trace = logWrapper ["scanExprM",ren ϵs,ren expr0,ren mb_target_ty,ren trace] $ do
	let
		to_call_or_ternaryifs :: CExpr → StateT CallOrTernaryIfs CovVecM CExpr

		-- extract a list of all calls from the input expression expr0
		-- (including fun-identifier, the arguments, and NodeInfo)
		to_call_or_ternaryifs ccall@(CCall funexpr args ni) = case funexpr of
			CVar (Ident "__builtin_expect" _ _) _ → return $ head args
			CVar (Ident "__builtin_clz" _ _) _ → return ccall
			CVar (Ident "solver_find" _ _) _ → lift $ ⅈ 1
			-- Keep the solver_pragmas in as they are (bottom-up search!), so they can be found in CCond's, e.g.
			CVar (Ident "solver_pragma" _ _) _ → return ccall
			CVar funident _ → do
				modify ( Left (funident,args,ni) : )
				return ccall
			_  → lift $ myError $ "is_call: found call " ++ (render.pretty) funexpr

		{-
			search for all CConds in the expression,
			replacing ...( a ? b : c )... by
			<T> condexpr$10_12;
			if(a) condexpr$10_12 = b; else condexpr$10_12 = c;
			...condexpr$10_12...
		-}
		to_call_or_ternaryifs ccond@(CCond cond (Just true_expr) false_expr ni) = do
			let
				var_ident = internalIdent $ "condexpr$" ++ locationToName (lineColNodeInfo ccond)
				var = CVar var_ident ni

			branches_to_follow <- lift $ createBranchesWithAnno cond makeCondBranchName trace

--			transcribeExprM :: [Env] → Maybe Types → CExpr → CovVecM CExprWithType
			true_expr_wt <- lift $ transcribeExprM ϵs Nothing true_expr
			let
				decl = CBlockDecl $ type2Decl var_ident ni (extractType true_expr_wt) Nothing
			cbiss <- forM branches_to_follow $ \ (branch,branch_cond) → do
				return $ decl : wrapGoto (Right (branch,branch_cond)) : case isElseBranch branch of
					False → [ CBlockStmt $ var ≔ true_expr  ]
					True  → [ CBlockStmt $ var ≔ false_expr ]
			modify ( Right cbiss : )
			-- Replace the ccond by the new variable "var"
			return var

		to_call_or_ternaryifs (CComma exprs _) = do
			let
				all_but_last_exprs = reverse $ tail $ reverse exprs
			modify ( ( Right [
				map CBlockStmt $ map (\ e → CExpr (Just e) undefNode) all_but_last_exprs ] : ) )
			return $ last exprs

		to_call_or_ternaryifs cass@(CAssign _ lexpr _ _) = do
			modify ( (Right [ [ CBlockStmt $ CExpr (Just cass) undefNode ] ] ) : )
			return lexpr

		to_call_or_ternaryifs expr = return expr

	-- important: everywhereM works bottom-up, which is crucial for the sequence of side effects in an expression!
	runStateT (everywhereM (mkM to_call_or_ternaryifs) expr0) []

createCombinationsM :: LabelEnv → [Env] → Bool → (CExpr,CallOrTernaryIfs) → Maybe Types → Trace → Int → Progress → CovVecM [(Int,Progress,[Env],CExprWithType,Trace)]
createCombinationsM labelϵ ϵs toplevel (expr,call_or_ternaryifs) mb_target_ty trace forks progress = do
	-- reverse the list of side effect code, since it was constructed bottom-up using the cons operator ":"
	create_combinations ϵs expr trace [] forks progress (reverse call_or_ternaryifs)

	where

	-- β-reduction
	replace_param_with_arg :: [(Ident,CExpr)] → CStat → CStat
	replace_param_with_arg iexprs stmt = foldl (\ stmt' (ident,cexpr) →
		substituteBy (CVar ident undefNode) cexpr stmt') stmt iexprs

	-- construct all possible traces ending in the called (sub-)functions and return them together with the returned expression,
	-- concatenating all possibilities.
	-- also substitutes ternary ifs
	create_combinations :: [Env] → CExpr → Trace → [(NodeInfo,CExprWithType)] → Int → Progress → CallOrTernaryIfs → CovVecM [(Int,Progress,[Env],CExprWithType,Trace)]

	-- callOrTernaryIfs empty
	create_combinations ϵs expr trace subs forks progress [] = do
		expr' <- transcribeExprM ϵs mb_target_ty expr
		let
			-- substitute all function calls by the respective return expressions
			expr_substituted = foldr substitute expr' subs

			substitute (call_ni,retexpr) expr = everywhere (mkT subst_ret_expr) expr where
				subst_ret_expr :: CExprWithType → CExprWithType
				subst_ret_expr (CCall _ _ (ni,_)) | call_ni == ni = retexpr
				subst_ret_expr expr = expr
		return [(forks,progress,ϵs,expr_substituted,trace)]

	-- Handle a function call
	-- "trace" is the whole trace up to here, not just the function body's trace!
	create_combinations ϵs expr trace subs forks progress (Left (funident,args,call_ni) : rest) = do
		FunDef (VarDecl _ _ (FunctionType (FunType ret_ty paramdecls False) _)) body _ <- lookupFunM funident
		expanded_params_args <- expand_params_argsM paramdecls args
		-- β-reduction of the arguments:
		let
			body' = replace_param_with_arg expanded_params_args body
		Left ϵs_funtraces <- unfoldTracesM labelϵ (Just ret_ty) False 0 progress ϵs trace [ ([ CBlockStmt body' ],False) ]
		funtraces_rets :: [(Int,Progress,[Env],CExprWithType,Trace)] <- forM ϵs_funtraces $ \case
			(forks',progress',envs',(Return retexpr : tr)) → return (forks',progress',envs',retexpr,tr)
			-- if we have a "trace of no return", we assume return(99);
			-- (not return (0) just to cause trouble in case of undefined behaviour, since 0 is taken randomly as default sometimes...)
			(forks',progress',envs',tr) → ⅈ 99 >>= return.(forks',progress',envs',,tr)
		concatForM funtraces_rets $ \ (forks',progress',_,retexpr,funtrace_trace) → do
			create_combinations ϵs expr funtrace_trace ((call_ni,retexpr):subs) forks' progress' rest

		where

		-- From the list of ParamDecls, extract the identifiers from the declarations and pair them with the argument
		expand_params_argsM ::  [ParamDecl] → [CExpr] → CovVecM [(Ident,CExpr)]
		expand_params_argsM paramdecls args = concatForM (zip paramdecls args) expandparam where
			expandparam :: (ParamDecl,CExpr) → CovVecM [(Ident,CExpr)]
			expandparam (paramdecl,arg) = do
				let VarDecl (VarName srcident _) _ arg_ty = getVarDecl paramdecl
				return [(srcident,arg)]

	-- Handle additional CBlockItems stemming from
	-- comma operator, assignments in expressions, or conditional expressions
	create_combinations ϵs expr trace subs forks progress (Right cbiss : rest) = do
		concatForM cbiss $ \ cbis → do
			-- TODO: toplevel=False is wrong here, could be a CCond in one of the expressions, or a return...
			Left ϵs_ternaryiftraces <- unfoldTracesM labelϵ Nothing False forks progress ϵs trace [ (cbis,False) ]
			concatForM ϵs_ternaryiftraces $ \ (forks',progress',ϵs',ternaryif_trace) → do
				create_combinations ϵs' expr ternaryif_trace subs forks' progress' rest

translateExprM :: LabelEnv → [Env] → Bool → CExpr → Maybe Types → Trace → Int → Progress → CovVecM [(Int,Progress,[Env],CExprWithType,Trace)]
translateExprM labelϵ ϵs toplevel expr0 mb_target_ty trace forks progress = logWrapper ["translateExprM",ren ϵs,ren toplevel,ren expr0,ren mb_target_ty] $ do
--	printLogV 0 $ showTrace trace
	scan_res <- scanExprM ϵs expr0 mb_target_ty trace
	createCombinationsM labelϵ ϵs toplevel scan_res mb_target_ty trace forks progress


-- Substitutes an expression x by y everywhere in d
-- Preserve the original node info of the replaced term
-- in order to keep the NodeInfos in conditions in sync with the pre-calculated locations
-- (this happens while beta-reducing function bodies)
substituteBy :: (Pretty (CExpression a),Typeable a,Data d) => CExpression a → CExpression a → d → d
substituteBy x y d = everywhere (mkT (substexpr x y)) d
	where
	substexpr :: (Pretty (CExpression a)) => CExpression a → CExpression a → CExpression a → CExpression a
	substexpr x y found_expr | (render.pretty) x == (render.pretty) found_expr =
		amap (const $ annotation found_expr) y
	substexpr _ _ found_expr = found_expr


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

{-
-- Add declarations for struct/union members
completeStructDeclsM :: Trace → CovVecM Trace
completeStructDeclsM trace = complete_decls [] (reverse trace)
	where
	complete_decls Trace → Trace → CovVecM Trace
	complete_decls res_trace [] = return res_trace
	complete_decls res_trace (NewDeclaration (ident,ty) : rest) = case ty of
		DirectType (TyComp (CompTypeRef sueref comptykind _)) _ _ → do
			member_ty_s <- getMembersM sueref
-}


elimInds :: Trace → CovVecM Trace
elimInds trace = elim_indsM [] $ reverse trace
	where
	elim_indsM :: Trace → Trace → CovVecM Trace
	elim_indsM res_trace [] = return res_trace
	elim_indsM res_trace (ti@(Assignment (Normal ptr@(CVar ptr_ident _)) expr) : rest) = do
		case extractType ptr of
			-- For assignments to a pointer ptr = expr, substitute ptr for expr  downwards in the trace,
			-- and cancel out */& operators in the resulting expressions
			PtrType _ _ _ → elim_indsM (cancel_ind_adrs $ substituteBy ptr expr res_trace) rest
			_ → elim_indsM (ti : res_trace) rest
	elim_indsM res_trace (ti : rest) = elim_indsM (ti : res_trace) rest

	cancel_ind_adrs :: Trace → Trace
	cancel_ind_adrs trace = everywhere (mkT cancel_ind_adr) trace
		where
		cancel_ind_adr :: CExprWithType → CExprWithType
		cancel_ind_adr (CUnary CIndOp (CUnary CAdrOp expr _) _) = expr
		cancel_ind_adr (CMember (CUnary CAdrOp obj _) member True ni) = CMember obj member False ni
		cancel_ind_adr expr = expr


-- FOLD TRACE BY SUBSTITUTING ASSIGNMENTS BACKWARDS

elimAssignmentsM :: Trace → CovVecM Trace
elimAssignmentsM trace = foldtraceM [] $ reverse trace
	where
	foldtraceM :: Trace → Trace → CovVecM Trace
	foldtraceM result [] = return result
	-- Skip assignments to array elements
	foldtraceM result (ass@(Assignment (Normal (CIndex _ _ _)) _) : rest) = foldtraceM (ass : result) rest
	foldtraceM result (ass@(Assignment (Normal lvalue) expr) : rest) = do
		foldtraceM (substituteBy lvalue expr result) rest
	foldtraceM result (traceitem : rest) = foldtraceM (traceitem:result) rest

{-
-- Eliminate assignments of arrays
elimArrayAssignsM :: Trace -> CovVecM Trace
elimArrayAssignsM trace = foldtraceM [] $ reverse trace
	where
	foldtraceM :: Trace → Trace → CovVecM Trace
	foldtraceM result [] = return result
	foldtraceM result (ass@(Assignment (Normal lvalue) expr) : rest) | Z3_Array _ _ <- extractZ3Type lvalue = do
		foldtraceM (substituteBy lvalue expr result) rest
	foldtraceM result (traceitem : rest) = foldtraceM (traceitem:result) rest
-}

-- eliminate assignments to arrays, replacing them by a new array declaration
-- and a condition that a_n+1 = store a_n ... ...
{-
... f ( int a[3], ...)  =>  ... f ( int a_INDEX_0, int a_INDEX_1, int a_INDEX_2, ... )
-}
-- trace is in the right order.
sequenceArraysM :: Trace → CovVecM Trace
sequenceArraysM trace = evalStateT elimarrassns Map.empty
	where
	elimarrassns :: StateT (Map.Map String Int) CovVecM Trace
	elimarrassns = do
		concatForM trace $ \case
			Assignment (Normal (CIndex arr_expr index_expr _)) ass_expr → do
				counters <- get
				let arr_name = lValueToVarName arr_expr
				i <- case Map.lookup arr_name counters of
					Nothing → do
						modify $ Map.insert arr_name 2
						return 1
					Just i → do
						modify $ Map.adjust (+1) arr_name
						return i
				let
					-- Three '$' denote the class of array names (to avoid name clashes)
					arr_types = extractTypes arr_expr
					new_arr_name = makeArrName arr_name i
					new_arr_ident = internalIdent new_arr_name
					new_arr = CVar new_arr_ident (undefNode,arr_types)
					(store_arr,store_arr_ident) = case i of
						1 → (new_arr,new_arr_ident)
						i → (CVar prev_ident (undefNode,arr_types),prev_ident) where
							prev_ident = internalIdent $ arr_name ++ "$$$" ++ show (i-1)
				return $ [
					NewDeclaration (new_arr_ident,extractType arr_expr) ,
					Assignment (ArrayUpdate store_arr_ident new_arr_ident arr_types index_expr) ass_expr ] ++
					-- Only insert the following condition when the new array is introduced (i.e. i==1)
					-- in order to logically connect the array with the argument
					( if i==1 then [ Condition (Left (arr_expr,new_arr)) ] else [] ) ++
					[ Assignment (Normal arr_expr) new_arr ]
			other → return [other]

-- Simplify:
-- *(&x)  ~> x
-- &s→m  ~> s.m
-- (*p).m ~> p→m
-- (A)a   ~> a  if a::A
-- (A*)x  ~> x::(A*)

simplifyTraceM :: Trace → CovVecM Trace
simplifyTraceM trace = everywhereM (mkM simplify) trace where
	simplify :: CExprWithType → CovVecM CExprWithType

	simplify (CUnary CIndOp (CUnary CAdrOp expr _) _) = return expr

	simplify (CMember (CUnary CAdrOp s _) member True ni) = return $ CMember s member False ni
	simplify (CMember (CUnary CIndOp p _) member False ni) = return $ CMember p member True ni

	-- Eliminate two consecutive pointer casts:  (A*)(B*)ptr ~> (A*)ptr
	simplify (CCast t1 (CCast _ expr (_,tys@(Z3_Ptr _,_))) ni@(_,(Z3_Ptr _,_))) = return $ CCast t1 expr ni

	-- Convert cast of ptr to union with member
	simplify (CMember (CCast dummy px (_,tys@(Z3_Ptr (Z3_Compound sueref UnionTag),_))) member_id True mem_ni) = do
		let (Z3_Ptr px_target_type,PtrType pxtt _ _) = extractTypes px
		return $ CCast dummy (CUnary CIndOp px (undefNode,(px_target_type,pxtt))) mem_ni

	-- Superfluous cast, because (A)b with b::A  can be simplified to b
	simplify (CCast _ expr (_,(z3ty,_))) | extractZ3Type expr == z3ty = return expr

	-- Eliminate pointer casts
--	simplify (CCast _ expr (_,tys@(Z3_Ptr _,_))) = return $ amap (\(ni,_)→(ni,tys)) expr

	simplify expr = return expr


-- Create symbolic vars for leftover expressions

createSymbolicVarsM :: Trace → CovVecM Trace
createSymbolicVarsM trace = create_symbolic_vars [] (map fst $ createTyEnv trace) trace
	where
	create_symbolic_vars :: Trace → [Ident] → Trace → CovVecM Trace
	create_symbolic_vars res_trace _ [] = return $ reverse res_trace
	create_symbolic_vars res_trace new_idents (ti : rest) = do
		(ti',add_tis) <- runStateT (everywhereM (mkM createsymvar_m) ti) []
		create_symbolic_vars (ti' : (map NewDeclaration add_tis) ++ res_trace) (map fst add_tis ++ new_idents) rest
		where

		create_var :: CExprWithType → Type → StateT [(Ident,Type)] CovVecM CExprWithType
		create_var expr ty = do
			let newident = mkIdentWithCNodePos (extractNodeInfo expr) $ lValueToVarName expr
			when (not $ newident `elem` new_idents) $
				modify ((newident,ty) : )
			return $ CVar newident (annotation expr)

		createsymvar_m :: CExprWithType → StateT [(Ident,Type)] CovVecM CExprWithType

		createsymvar_m expr@(CUnary CIndOp cvar@(CVar ptr_ident _) ni) = do
			let PtrType ty _ _ = extractType cvar
			create_var expr ty

		--  for ptr→member   create    p1_ARROW_member :: member_type
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

createTyEnv :: Trace → [TyEnvItem]
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

infix 4 ＝
(＝) :: SExpr → SExpr → SExpr
sexpr1 ＝ sexpr2 = SExpr [ SLeaf "=", sexpr1, sexpr2 ]


𝓈𝓉𝑜𝓇𝑒 :: SExpr → SExpr → SExpr → SExpr
𝓈𝓉𝑜𝓇𝑒 arr ix val = SExpr [ SLeaf "store", arr, ix, val ]

𝓈𝑒𝓁𝑒𝒸𝓉 :: SExpr → SExpr → SExpr
𝓈𝑒𝓁𝑒𝒸𝓉 arr ix = SExpr [ SLeaf "select", arr, ix ]

𝑒𝓍𝓉𝓇𝒶𝒸𝓉 :: Int → Int → SExpr → SExpr
𝑒𝓍𝓉𝓇𝒶𝒸𝓉 l r sexpr = SExpr [ SExpr [SLeaf "_", SLeaf "extract", SLeaf (show l), SLeaf (show r)], sexpr ]

_𝓉𝑜_𝒻𝓅 32 = SExpr [ SLeaf "_", SLeaf "to_fp", SLeaf "8" , SLeaf "24" ]
_𝓉𝑜_𝒻𝓅 64 = SExpr [ SLeaf "_", SLeaf "to_fp", SLeaf "11", SLeaf "53" ]
_𝓉𝑜_𝒻𝓅 128 = SExpr [ SLeaf "_", SLeaf "to_fp", SLeaf "15", SLeaf "113" ]

𝒶𝓈𝓈𝑒𝓇𝓉 sexpr = SExpr [ SLeaf "assert", sexpr ]

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

extractTypes :: CExprWithType → Types
extractTypes = snd.annotation

extractZ3Type :: CExprWithType → Z3_Type
extractZ3Type = fst.extractTypes

extractType :: CExprWithType → Type
extractType = snd.extractTypes

extractNodeInfo :: CExprWithType → NodeInfo
extractNodeInfo = fst.annotation

type NodeInfoWithType = (NodeInfo,Types)
instance {-# OVERLAPPING #-} Eq NodeInfoWithType where
	x == y = True

type CExprWithType = CExpression NodeInfoWithType


type Constraint = TraceElem

type SECovVecM = StateT [SExpr] CovVecM

makeArrName :: String → Int → String
makeArrName arr_name i = arr_name ++ "$$$" ++ show i

createAssertEq :: SExpr -> Int -> SExpr -> SExpr
createAssertEq lsexpr bvsize rsexpr = 𝒶𝓈𝓈𝑒𝓇𝓉 $ SExpr [ SLeaf "=", lsexpr, SExpr [ _𝓉𝑜_𝒻𝓅 bvsize, rsexpr ] ]

expr2SExpr :: Constraint → CovVecM (SExpr,[SExpr])
expr2SExpr expr = runStateT (expr2sexpr expr) []

	where

	make_intconstant :: Z3_Type → Int → SECovVecM SExpr
	make_intconstant (Z3_BitVector size _) const | size `mod` 4 == 0 =
		return $ SLeaf (printf "#x%*.*x" (size `div` 4) (size `div` 4) const)
	make_intconstant z3type const = lift $ myError $ "make_intconstant " ++ show z3type ++ " " ++ show const

	ident2sexpr ident = SLeaf $ (render.pretty) ident

	expr2sexpr :: Constraint → SECovVecM SExpr
	expr2sexpr (Condition (Left (varcexpr,cexpr))) = do
		varsexpr <- expr2sexpr' varcexpr
		sexpr <- expr2sexpr' cexpr
		return $ SExpr [ SLeaf "=", varsexpr, sexpr ]
	expr2sexpr (Condition (Right (_,cexpr))) = expr2sexpr' cexpr

	-- Assignment to an array member
	expr2sexpr (Assignment (ArrayUpdate store_arr_ident var_arr_ident _ index_expr) ass_expr) = do
		let var_arr_s = ident2sexpr var_arr_ident
		index_s <- expr2sexpr' index_expr
		ass_s <- expr2sexpr' ass_expr
		let store_arr_s = ident2sexpr store_arr_ident
		return $ var_arr_s ＝ 𝓈𝓉𝑜𝓇𝑒 store_arr_s index_s ass_s

	-- Turns a CExprWithType into an SExpr
	expr2sexpr' :: CExprWithType → StateT [SExpr] CovVecM SExpr

	expr2sexpr' expr = case expr of

		CIndex arr_expr index_expr _ → 𝓈𝑒𝓁𝑒𝒸𝓉 <$> expr2sexpr' arr_expr <*> expr2sexpr' index_expr

		-- CNeqOp was resolved while annotateTypes
		CBinary binop expr1 expr2 _ →
			SExpr <$> sequence [ pure $ SLeaf op_sexpr, expr2sexpr' expr1, expr2sexpr' expr2 ]
				where
				op_ty = extractZ3Type expr1
				op_sexpr = case binop of
					CMulOp → bitVectorTy op_ty "bvmul" ("fp.mul " ++ roundingMode)
					CDivOp → bitVectorTy op_ty "bvudiv" ("fp.div " ++ roundingMode)
					CAddOp → bitVectorTy op_ty "bvadd" ("fp.add " ++ roundingMode)
					CSubOp → bitVectorTy op_ty "bvsub" ("fp.sub " ++ roundingMode)
					CRmdOp → bitVectorTy op_ty (unSignedTy op_ty "bvurem" "bvsrem") "fp.rem"
					CShlOp → "bvshl"
					CShrOp → unSignedTy op_ty "bvlshr" "bvashr"
					CAndOp → "bvand"
					COrOp  → "bvor"
					CXorOp → "bvxor"
					CLndOp → "and"
					CLorOp → "or"
					CLeOp  → bitVectorTy op_ty (unSignedTy op_ty "bvult" "bvslt") "fp.lt"
					CGrOp  → bitVectorTy op_ty (unSignedTy op_ty "bvugt" "bvsgt") "fp.gt"
					CLeqOp → bitVectorTy op_ty (unSignedTy op_ty "bvule" "bvsle") "fp.leq"
					CGeqOp → bitVectorTy op_ty (unSignedTy op_ty "bvuge" "bvsge") "fp.geq"
					CEqOp  → bitVectorTy op_ty "=" "fp.eq"
					other  → error $ "op_sexpr " ++ (render.pretty) binop ++ " not implemented!"

		cconst@(CConst ctconst) → case ctconst of
			CIntConst intconst (_,(ty,_))   → make_intconstant ty (fromIntegral $ getCInteger intconst)
			CCharConst cchar _              → return $ SLeaf $ (render.pretty) cconst
			CFloatConst (CFloat f_s) (_,ty) → return $ SExpr [ SLeaf "fp", SLeaf ("#b"++s1), SLeaf ("#b"++s2), SLeaf ("#b"++s3) ]
				where
				readfloat s = case reads s of
					[(w,suffix)] | suffix `elem` ["F","f"] → w
					_ → error $ "error: readfloat " ++ show s
				show_bin :: (Integral a,PrintfArg a) => Int → a → String
				show_bin l i = printf "%0*.*b" l l i
				(s1,s2,s3) = case fst ty of
					Z3_Float  → (take 1 val,take 8 $ drop 1 val,take 23 $ drop 9 val) where
						val = show_bin 32 (floatToWord $ readfloat f_s)
					Z3_Double → (take 1 val,take 11 $ drop 1 val,take 52 $ drop 12 val) where
						val = show_bin 64 (doubleToWord $ readfloat f_s)
					Z3_LDouble → error "long double is not supported"

			CStrConst cstr _                → return $ SLeaf $ (render.pretty) cconst

		CVar ident _ → return $ ident2sexpr ident

		CUnary CPlusOp subexpr _ → expr2sexpr' subexpr
		CUnary op subexpr _ → SExpr <$> sequence
			[ pure $ SLeaf op_str, expr2sexpr' subexpr ]
			where
			op_str = case op of
				CMinOp  → bitVectorTy (extractZ3Type subexpr) "bvneg" "fp.neg"
				CCompOp → "bvnot"
				CNegOp  → "not"
				_ → error $ "expr2sexpr " ++ (render.pretty) op ++ " should not occur!"

		castexpr@(CCast _ subexpr (_,to_ty)) → do
			sexpr <- expr2sexpr' subexpr
			let from_ty = extractTypes subexpr
			case (fst from_ty,fst to_ty) of

				-- Identity cast
				( ty1, ty2 ) | ty1==ty2 → return sexpr

				-- Casting signed to unsigned or vice versa with same size: No cast needed (Z3 interprets it)
				( Z3_BitVector size_from _, Z3_BitVector size_to _ ) | size_from==size_to → return sexpr

				-- Casting from Bool
				( Z3_Bool, toty@(Z3_BitVector size_from _ )) → do
					ic1 <- make_intconstant toty 1
					ic0 <- make_intconstant toty 0
					return $ SExpr [ SLeaf "ite", sexpr, ic1, ic0 ]

				-- Casting to Bool
				( frty@(Z3_BitVector size_from _) , Z3_Bool ) → do
					ic <- make_intconstant frty 0
					return $ SExpr [ SLeaf "not", sexpr ＝ ic ]

				-- DOWNCAST: extract bits (modulo)
				( Z3_BitVector size_from _, Z3_BitVector size_to _ ) | size_from > size_to →
					return $ 𝑒𝓍𝓉𝓇𝒶𝒸𝓉 (size_to - 1) 0 sexpr

				-- UPCAST signed (to signed or unsigned): extend sign bit
				( Z3_BitVector size_from False, Z3_BitVector size_to _ ) | size_from < size_to →
					return $ SExpr [ SExpr [ SLeaf "_", SLeaf "sign_extend", SLeaf $ show (size_to-size_from) ], sexpr ]

				-- UPCAST unsigned (to signed or unsigned): extend with zeros
				( Z3_BitVector size_from True, Z3_BitVector size_to _ ) | size_from < size_to →
					return $ SExpr [ SExpr [ SLeaf "_", SLeaf "zero_extend", SLeaf $ show (size_to-size_from) ], sexpr ]

				( Z3_Float, Z3_BitVector size_to is_unsigned ) → do
					return $ SExpr [ SExpr [ SLeaf "_", fp_to, SLeaf (show size_to) ], SLeaf roundingMode, sexpr ]
					where
					fp_to = SLeaf $ if is_unsigned then "fp.to_ubv" else "fp.to_sbv"

				( Z3_Float, Z3_Double ) →
					return $ SExpr [ _𝓉𝑜_𝒻𝓅 32, SLeaf roundingMode, sexpr ]

				( Z3_Float, arr_ty@(Z3_Array (Z3_BitVector 16 True) _ )) → cast_fp2arr sexpr Z3_Float arr_ty

				( Z3_Double, arr_ty@(Z3_Array (Z3_BitVector 16 True) _ )) → cast_fp2arr sexpr Z3_Double arr_ty

				( Z3_LDouble, arr_ty@(Z3_Array (Z3_BitVector 16 True) _ )) → cast_fp2arr sexpr Z3_LDouble arr_ty

				(from_ty,to_ty) → lift $ myError $ "expr2sexpr cast: " ++ show from_ty ++ " => " ++ show to_ty ++ " in " ++
					(render.pretty) castexpr ++ " " ++ " at " ++ show (lineColNodeInfo $ extractNodeInfo castexpr) ++ " not implemented!"

			where

			cast_fp2arr :: SExpr → Z3_Type → Z3_Type → StateT [SExpr] CovVecM SExpr
			cast_fp2arr sexpr fp_ty arr_ty@(Z3_Array elem_ty _) = do
				elem_size <- lift $ sizeofZ3Ty elem_ty
				bv_size <- lift $ sizeofZ3Ty fp_ty
				let
					num_elems = div bv_size elem_size
				bv <- case subexpr of
					CVar ident _ -> return $ SLeaf $ makeFloatBVVarName (identToString ident)
					_ -> case elem_ty of
						Z3_BitVector _ True -> do
							(bv_cast,bv_cast_decl) <- new_var "fp2arr_cast_bv" (Z3_BitVector bv_size True)
							subsexpr <- expr2sexpr' subexpr
							let bv_cast_eq = createAssertEq subsexpr bv_size bv_cast
							modify ( ++ [bv_cast_decl,bv_cast_eq] )
							return bv_cast
						other -> lift $ myError $ "cast_fp2arr: elem_ty = " ++ show elem_ty ++ "\n at " ++ show (extractNodeInfo subexpr)

				(arr,arr_decl) <- new_var "arr" arr_ty
				(z3_inttype,_) <- lift $ _IntTypesM
				is <- forM [0..(num_elems-1)] $ make_intconstant z3_inttype
				Just MachineSpec{endianness} <- lift $ gets machineSpecCVS
				let addresses = map (\ (h,l) → 𝑒𝓍𝓉𝓇𝒶𝒸𝓉 h l bv) $
					(case endianness of Little → id; Big → reverse)
						[ ( (i+1)*elem_size-1 , i*elem_size ) | i <- [0..(num_elems-1)] ]

				modify ( ++ (
					arr_decl :
					map (\(i,address) → 𝒶𝓈𝓈𝑒𝓇𝓉 $ arr ＝ 𝓈𝓉𝑜𝓇𝑒 arr i address) (zip is addresses)) )

				return arr

		ccond@(CCond cond (Just then_expr) else_expr _) → do
			lift $ myError $ "expr2sexpr CCond should not appear: " ++ (render.pretty) ccond

		cmember@(CMember _ _ _ _) → lift $ myError $ "expr2sexpr of member " ++ (render.pretty) cmember ++ " should not occur!"

		CCall (CVar (Ident (_:"__builtin_clz") _ _) _) [arg] _ → do
			inttypes@(z3_inttype,_) <- lift $ _IntTypesM
			(n,n_decl) <- new_var "n_clz" z3_inttype
			let to_anno = (undefNode,inttypes)
			arg_sexpr <- expr2sexpr' $ CCast (CDecl [] [] to_anno) arg to_anno
			i_0 <- make_intconstant z3_inttype 0
			Just MachineSpec{..} <- lift $ gets machineSpecCVS
			sintsize <- make_intconstant z3_inttype intSize
			sintsize_minus_1 <- make_intconstant z3_inttype (intSize-1)
			let n_cond = 𝒶𝓈𝓈𝑒𝓇𝓉 $ SExpr [SLeaf "and", SExpr [SLeaf "bvlshr", arg_sexpr, SExpr [SLeaf "bvsub",sintsize,n]] ＝ i_0,
				SExpr [ SLeaf "bvugt", SExpr [SLeaf "bvlshr", arg_sexpr, SExpr [SLeaf "bvsub",sintsize_minus_1,n]], i_0 ] ]
			modify (++[n_decl,n_cond])
			return n

		ccall@(CCall _ _ _) → lift $ myError $ "expr2sexpr of call " ++ (render.pretty) ccall ++ " should not occur!"

		other → lift $ myError $ "expr2SExpr " ++ (render.pretty) other ++ " not implemented"

		where

		new_var :: String → Z3_Type → SECovVecM (SExpr,SExpr)
		new_var name z3ty = do
			n <- lift $ newNameM
			-- avoiding name clashes using "$$" for "temporary" variables
			let new_name = name ++ "$$" ++ show n
			decl <- lift $ declConst2SExpr new_name z3ty
			return (SLeaf new_name,decl)

		unSignedTy ty unsigned signed = case ty of
			Z3_BitVector _ is_unsigned → if is_unsigned then unsigned else signed
			Z3_Bool → unsigned
			_ → error $ "unSignedTy " ++ (render.pretty) expr ++ " is no bitvector!"
		bitVectorTy ty bv fp = case ty of
			Z3_Float → fp
			Z3_Double → fp
			_ → bv
--			_ → error $ "bitVectorTy for " ++ show operator_ty ++ " not implemented!"


bvPrefix = "bv$"
makeFloatBVVarName :: String → String
makeFloatBVVarName name = bvPrefix ++ name
stripBVPrefix :: Ident -> Maybe Ident
stripBVPrefix ident = case stripPrefix bvPrefix (identToString ident) of
	Nothing -> Nothing
	Just rest -> Just $ internalIdent rest


data Z3_Type =
	Z3_Unit |   -- The proper type-theoretical name for C's void is "1" (i.e. "unit", "()" in Haskell )
	Z3_Bool |
-- Z3_BitVector Int (is*Un*signed::Bool), hence
-- the derived ordering intentionally coincides with the type cast hierarchy :-)
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

instance Pretty Z3_Type where
	pretty (Z3_Compound sueref kind) = pretty kind <+> pretty sueref
	pretty (Z3_Ptr ty) = pretty ty <+> text "*"
	pretty (Z3_Array elem_ty mb_size) = pretty elem_ty <+> text "[" <> text (maybe "" show mb_size) <> text "]"
	pretty other = text $ show other

ty2Z3Type :: Type → CovVecM (Z3_Type,Type)
ty2Z3Type ty = do
	Just MachineSpec{..} <- gets machineSpecCVS
	z3_ty <- case ty of
		DirectType tyname _ attrs → case tyname of
			TyVoid → return Z3_Unit
			TyIntegral intty    → do
				sizeofintty <- sizeofTy ty
				return $ Z3_BitVector sizeofintty $ intty `elem` [TyChar,TyUChar,TyUShort,TyUInt,TyULong,TyULLong]
			TyFloating floatingty → return $ case floatingty of
				TyFloat   → Z3_Float
				TyDouble  → Z3_Double
				TyLDouble → Z3_LDouble
			TyEnum _            → return $ Z3_BitVector intSize True
			TyComp (CompTypeRef sueref comptykind _) → return $ Z3_Compound sueref comptykind
			TyBuiltin TyVaList  → return $ Z3_VaList
			TyBuiltin TyAny     → return $ Z3_Any
			_ → myError $ "ty2Z3Type " ++ (render.pretty) ty ++ " not implemented!"
		PtrType target_ty _ _ → Z3_Ptr <$> ty2Z3TypeOnly target_ty
		ArrayType elem_ty arraysize _ _ →
			Z3_Array <$> ty2Z3TypeOnly elem_ty <*> pure ( case arraysize of
				ArraySize _ (CConst (CIntConst cint _)) → Just $ getCInteger cint
				_                                       → Nothing )
		TypeDefType (TypeDefRef _ ty _) _ _ → ty2Z3TypeOnly ty
		FunctionType (FunTypeIncomplete ret_type) _ → Z3_FunIncomplete <$> ty2Z3TypeOnly ret_type
		FunctionType (FunType ret_type funparamdecls is_variadic) _ → do
			let arg_types = for (map getVarDecl funparamdecls) $ \ (VarDecl _ _ ty) → ty
			Z3_Fun <$> ty2Z3TypeOnly ret_type <*> mapM ty2Z3TypeOnly arg_types <*> pure is_variadic
	return (z3_ty,ty)

ty2Z3TypeOnly :: Type → CovVecM Z3_Type
ty2Z3TypeOnly ty = ty2Z3Type ty >>= return.fst

sizeofTy :: Type → CovVecM Int
sizeofTy ty@(DirectType tyname _ attrs) = do
	Just MachineSpec{..} <- gets machineSpecCVS
	return $ case tyname of
		TyIntegral intty → case (intty,concatMap to_mode attrs) of
			(TyChar,[])     → 8
			(TySChar,[])    → 8
			(TyUChar,[])    → 8
			(TyShort,[])    → 16
			(TyUShort,[])   → 16
			(TyInt,[])      → intSize
			(TyInt,["SI"])  → 32
			(TyInt,["DI"])  → 64
			(TyUInt,[])     → intSize
			(TyUInt,["SI"]) → 32
			(TyUInt,["DI"]) → 64
			(TyLong,[])     → longSize
			(TyULong,[])    → longSize
			(TyLLong,[])    → longLongSize
			(TyULLong,[])   → longLongSize
			other           → error $ "sizeofZ3Ty " ++ show other ++ " not implemented!"
		TyFloating floatty → case floatty of
			TyFloat   → 32
			TyDouble  → 64
			TyLDouble → 128
			other     → error $ "sizeofTy " ++ show other ++ " not implemented!"
		other → error $ "sizeofTy: " ++ (render.pretty) ty ++ " is not implemented!"
	where
	to_mode (Attr (Ident "mode" _ _) [CVar (Ident mode _ _) _] _) = [mode]
	to_mode (Attr (Ident "fardata" _ _) _ _) = []
	to_mode attr = error $ "attrs2modes: unknown attr " ++ (render.pretty) attr

sizeofZ3Ty :: Z3_Type → CovVecM Int
sizeofZ3Ty z3ty = case z3ty of
	Z3_BitVector size _ → return size
	Z3_Float   → sizeofTy $ DirectType (TyFloating TyFloat) noTypeQuals noAttributes
	Z3_Double  → sizeofTy $ DirectType (TyFloating TyDouble) noTypeQuals noAttributes
	Z3_LDouble → sizeofTy $ DirectType (TyFloating TyLDouble) noTypeQuals noAttributes

z3Ty2SExpr :: Z3_Type → CovVecM SExpr
z3Ty2SExpr ty = case ty of
	Z3_BitVector size _      → return $ SExpr [ SLeaf "_", SLeaf "BitVec", SLeaf (show size) ]
	Z3_Float                 → return $ SLeaf "Float32"
	Z3_Double                → return $ SLeaf "Float64"
	Z3_LDouble               → return $ SLeaf "Float128"
	Z3_Bool                  → return $ SLeaf "Bool"
	Z3_Ptr _                 → return $ SExpr [ SLeaf "_", SLeaf "BitVec", SLeaf (show 1) ]
	Z3_Array elem_ty mb_size → do
		(z3_inttype,_) <- _IntTypesM
		inttysexpr <- z3Ty2SExpr z3_inttype
		elem_ty_sexpr <- z3Ty2SExpr elem_ty
		return $ SExpr [ SLeaf "Array", inttysexpr, elem_ty_sexpr ]
	other                    → myError $ "z3Ty2SExpr " ++ show other ++ " should not occur!"

type Solution = [(String,SolutionVal)]

data SolutionVal = IntVal Int | FloatVal (Word32,Float) | DoubleVal (Word64,Double) | PtrVal
instance Show SolutionVal where
	show (IntVal i) = show i
	show (FloatVal (w,f)) | isNaN f = printf "NaN(0x%06x) = 0x%08x = %g" (w .&. (fromIntegral 0x7fffff)) w f
	show (FloatVal (w,f)) = printf "%s = 0x%08x = %g" (showHFloat f "") w f
	show (DoubleVal (w,d)) | isNaN d = printf "NaN(0x%013x) = 0x%016x = %g" (w .&. (fromIntegral 0xfffffffffffff)) w d
	show (DoubleVal (w,d)) = printf "%s = 0x%016x = %g" (showHFloat d "") w d
	show PtrVal = "<SOME_PTR>"
instance Eq SolutionVal where
	IntVal i1    == IntVal i2    = i1==i2
	PtrVal       == PtrVal       = True
	FloatVal (_,f1)  == FloatVal (_,f2) | isNaN f1 && isNaN f2 = True
	FloatVal (_,f1)  == FloatVal (_,f2) | isInfinite f1 && isInfinite f2 = True
	FloatVal (_,f1)  == FloatVal (_,f2) = abs (f2-f1) <= floatTolerance
	DoubleVal (_,f1) == DoubleVal (_,f2) | isNaN f1 && isNaN f2 = True
	DoubleVal (_,f1) == DoubleVal (_,f2) | isInfinite f1 && isInfinite f2 = True
	DoubleVal (_,f1) == DoubleVal (_,f2) = abs (f2-f1) <= doubleTolerance
{- Bitwise equality looks like that:
	FloatVal (w1,_)  == FloatVal (w2,_)  = w1==w2
	DoubleVal (w1,_) == DoubleVal (w2,_) = w1==w2
-}

declConst2SExpr :: String → Z3_Type → CovVecM SExpr
declConst2SExpr id_name ty = do
	ty_sexpr <- z3Ty2SExpr ty
	return $ SExpr [ SLeaf "declare-const", SLeaf id_name, ty_sexpr ]

prefix_a :: Ident → Ident
prefix_a (Ident s@('_':_) i ni) = Ident (safeZ3IdentifierPrefix:s) i ni
prefix_a ident = ident

makeAndSolveZ3ModelM :: [Int] → [(Ident,Z3_Type)] → [Constraint] → [SExpr] → [Ident] → String → CovVecM (String,Maybe Solution)
makeAndSolveZ3ModelM traceid z3tyenv0 constraints additional_sexprs output_idents0 modelpathfile = do
	-- collect all variables that appear in the constraints and assignments
	let
		constraints_vars = nub $ concat $ for constraints $ \case
			Condition (Left (varexpr,expr)) → fvar varexpr ++ fvar expr
			Condition (Right (_,expr)) → fvar expr
			Assignment (Normal lexpr@(CIndex _ _ _)) ass_expr → fvar lexpr ++ fvar ass_expr
			Assignment (ArrayUpdate ident1 ident2 _ index) ass_expr → [ident1,ident2] ++ fvar index ++ fvar ass_expr

	printLogV 20 $ "XXX output_idents0 = " ++ show (map (render.pretty) output_idents0)
	-- For all floats, replace the float-Varname by the bitvector_Varname "bv$<floatvar>"
	output_idents <- forM output_idents0 $ \ oid → case lookup oid z3tyenv0 of
		-- if ty is floating point, add bv$fp :: Z3_BitVector size to tyenv and replace fp by bv$fp in output_idents
		Just ty | ty `elem` [Z3_Float,Z3_Double,Z3_LDouble] → do
			-- only replace the identifier by bv$.. if it is in output_idents0
			case oid `elem` output_idents0 of
				True  → do
					let bvid = internalIdent $ makeFloatBVVarName (identToString oid)
					printLogV 20 $ "XXX replacing " ++ (render.pretty) oid ++ " by " ++ (render.pretty) bvid
					return bvid
				False → return oid
		_ → return oid

	-- prefix a "a_" for identifiers starting with underscore (Z3 does not like leading underscores...)
	-- in constraints and output_idents
	let
		(a_constraints_vars,a_constraints,a_output_idents,a_z3tyenv) =
			everywhere (mkT prefix_a) (constraints_vars,constraints,output_idents,z3tyenv0)

--	forM_ a_z3tyenv $ \ (ident,z3ty) -> printLogV 0 $ "### " ++ (render.pretty) ident ++ " :: " ++ show z3ty

--	forM_ a_constraints_vars $ \ v -> printLogV 0 $ "*** " ++ (render.pretty) v

	-- create declarations in the Z3 model for all variables from the z3tyenv that
	-- appear in the constraints and assignments, or
	-- are a_output_idents, or
	-- ?
	let all_idents = nub $ a_constraints_vars ++ a_output_idents ++ output_idents0

	bvs :: [((Ident,Z3_Type),SCompound)] <- concatForM all_idents $ \ ident → do
		case stripBVPrefix ident of
			-- Ignore it if ident = bv$..
			Just _ -> return []
			-- Create declaration and equality constraint for bv$<ident>
			Nothing -> do
				let Just ty = lookup ident a_z3tyenv
				case ty of
					ty | ty `elem` [Z3_Float,Z3_Double,Z3_LDouble] -> do
						let bvident = internalIdent $ makeFloatBVVarName $ identToString ident
						bv_size <- sizeofZ3Ty ty
						let eq_constraint = createAssertEq (SLeaf $ identToString ident) bv_size (SLeaf $ identToString bvident)
						return [((bvident,Z3_BitVector bv_size True),SExprLine $ SOnOneLine eq_constraint)]
					_ -> return []
	let
		(bvtyenv,eqconstraintsZ3) = unzip bvs
		z3tyenv = bvtyenv ++ a_z3tyenv

	let	create_decl (ident,_) = ident `elem` (all_idents ++ map fst bvtyenv)
	varsZ3 :: [SCompound] <- concatForM (filter create_decl z3tyenv) $ \ (ident,ty) → do
		printLogV 20 $ "XXX ident = " ++ (render.pretty) ident
		let varname = identToString ident
		decl <- declConst2SExpr varname ty
		return [ SExprLine (SOnOneLine decl) ]

	-- create Z3 constraints
	constraintsZ3 :: [SCompound] <- concatForM a_constraints $ \ constraint → do
		(assert_sexpr,add_sexprs) <- expr2SExpr constraint
		return $ [ SEmptyLine,
			SComment "----------------------------------------------" ] ++
			(map SComment $ lines $ show constraint) ++
			[ SComment "----------------------------------------------" ] ++
			map SExprLine add_sexprs ++
			[ SExprLine $ 𝒶𝓈𝓈𝑒𝓇𝓉 assert_sexpr]

	-- for all a_output_idents, create a (get-value ...) in the Z3 model 
	let
		outputvarsZ3 = for a_output_idents $ \ ident → SExprLine $ SOnOneLine $
			SExpr [SLeaf "get-value", SExpr [ SLeaf $ identToString ident ] ]

		model :: [SCompound] = [
			SComment $ show traceid,
			SEmptyLine,
			SExprLine $ SOnOneLine $ SExpr [SLeaf "set-option", SLeaf ":smt.relevancy", SLeaf "0"],
			SExprLine $ SOnOneLine $ SExpr [SLeaf "set-option", SLeaf ":produce-models", SLeaf "true"],
			SEmptyLine ] ++
			varsZ3 ++
			[SEmptyLine] ++
			eqconstraintsZ3 ++
			[SEmptyLine] ++
			constraintsZ3 ++
			[SEmptyLine] ++
			map (SExprLine . SOnOneLine) additional_sexprs ++
			[SEmptyLine] ++
			[ SExprLine $ SOnOneLine $ SExpr [SLeaf "apply",SExpr [SLeaf "then",SLeaf "simplify",SLeaf "solve-eqs"]] ] ++
			[ SExprLine $ SOnOneLine $ SExpr [SLeaf "check-sat"] ] ++
			[SEmptyLine] ++
			outputvarsZ3
		model_string = unlines $ map (render.pretty) model
		model_string_linenumbers = unlines $ map (\ (i,l) → show i ++ ": " ++ l) (zip [1..] (lines model_string))

	whenOptionSet writeModelsOpt True $ liftIO $ writeFile modelpathfile model_string
	whenOptionSet showModelsOpt True $ printLogM 0 $ "Model " ++ takeFileName modelpathfile ++ " =\n" ++ model_string_linenumbers
	printLogV 2 $ "Running model " ++ takeFileName modelpathfile ++ "..."
	let z3timeout_opt = case z3TimeoutSecs of
		Nothing -> []
		Just timeout_secs -> [ "-T:" ++ show timeout_secs ]
	(_,output,_) <- liftIO $ withCurrentDirectory (takeDirectory modelpathfile) $ do
		readProcessWithExitCode z3FilePath
			(z3timeout_opt ++ ["-smt2","-in","parallel.enable=true"])
			model_string
	let
		drop_prelude [] = []
		drop_prelude (l:ls) = case l of
			_ | l `elem` ["unsat","sat","unknown"] → l:ls
			_ | "(error " `isPrefixOf` l → l:ls
			_ | "timeout" `isPrefixOf` l → l:ls
			_ → drop_prelude ls
		dropped_output = drop_prelude $ lines output
	printLogV 0 $ "\nZ3 says:\n" ++ unlines dropped_output
	case dropped_output of
		"timeout" : _ → do
			let timeout_txt = "Timeout for " ++ show traceid
			printLogV 0 timeout_txt
			liftIO $ printToSolutions timeout_txt
			liftIO $ writeFile (errorModelPath </> "TIMEOUT_" ++ show traceid ++ ".smtlib2") model_string
			return (model_string_linenumbers,Nothing)
		"unsat"   : _ → return (model_string_linenumbers,Nothing)
		"unknown" : _ → return (model_string_linenumbers,Nothing)
		"sat" : rest → do
			sol_params <- forM (zip a_output_idents rest) $ \ (ident0,line) → do
				let
					ident_s = identToString ident0
					ident = case stripPrefix bvPrefix ident_s of
						Just ident_wo_prefix → internalIdent ident_wo_prefix
						Nothing → ident0
					is = escapeDollars $ identToString ident0
				case line =~ ("\\(\\(" ++ is ++ " ([^\\)]+)\\)\\)") :: (String,String,String,[String]) of
					(_,_,_,[val_string]) → case lookup ident z3tyenv of
						Nothing → myError $ "Parsing z3 output: Could not find type of " ++ (render.pretty) ident
						Just ty → return (identToString ident, case ty of
							Z3_BitVector size unsigned → let
								'#':'x':hexdigits = val_string
								[(i :: Integer,"")] = readHex hexdigits
								in
								IntVal $ case unsigned of
									True  → fromIntegral i
									False → fromIntegral $ if i < 2^(size-1) then i else i - 2^size
							Z3_Float → parseFloat val_string
							Z3_Double → parseDouble val_string
							Z3_LDouble → error $ "long double is not supported"
							Z3_Ptr _ → PtrVal
							other → error $ "case ty2Z3Type " ++ show other ++ " not implemented" )
					_ → myError $ "Parsing z3 output: Could not find " ++ is
			return (model_string_linenumbers,Just sol_params)
		_ → do
			let err_msg = "Execution of " ++ z3FilePath ++ " failed:\n" ++ output ++ "\n\n" ++ "Model is\n" ++ model_string_linenumbers
			printLogV 0 err_msg
			liftIO $ writeFile modelpathfile model_string
			whenOptionSet noHaltOnVerificationErrorOpt False $ myError err_msg
			return (model_string_linenumbers,Nothing)

escapeDollars :: String → String
escapeDollars s = concat $ for s $ \case
	'$' → "\\$"
	c → [c]

{- ((_ to_fp eb sb) RoundingMode (_ FloatingPoint mb nb) (_ FloatingPoint eb sb))
  -  Float32 is a synonym for (_ FloatingPoint  8  24)
  -  Float64 is a synonym for (_ FloatingPoint 11  53)
-}
wordToFloat :: Word32 → Float
wordToFloat x = runST (fb_cast x)
floatToWord :: Float → Word32
floatToWord x = runST (fb_cast x)
wordToDouble :: Word64 → Double
wordToDouble x = runST (fb_cast x)
doubleToWord :: Double → Word64
doubleToWord x = runST (fb_cast x)

{-# INLINE fb_cast #-}
fb_cast :: (MArray (STUArray s) a (ST s), MArray (STUArray s) b (ST s)) => a → ST s b
fb_cast x = newArray (0::Int,0) x >>= castSTUArray >>= flip readArray 0

parseFloat :: String → SolutionVal
parseFloat ('#':'x':s) = let [(w,"")] = readHex s in FloatVal (w,wordToFloat w)

parseDouble :: String → SolutionVal
parseDouble ('#':'x':s) = let [(w,"")] = readHex s in DoubleVal (w,wordToDouble w)

-- Find solutions for floating point constants close to 1.0 (in order to not produce mismatches due to rounding stuff)
fPMinimizer cons name = [ SExpr [SLeaf "minimize",SExpr [SLeaf "bvsub", SLeaf cons, SLeaf $ makeFloatBVVarName name ] ] ]

--getArgRetNames :: Maybe Type → CovVecM ()
getArgRetNames mb_ret_type = do
	Just retval_env  <- case mb_ret_type of
		Nothing  → return $ Just []
		Just ret_type → gets retEnvCVS
	Just param_env_exprs <- gets paramEnvCVS
	let
		param_env = map fst param_env_exprs
		param_names = map (fst.snd) param_env
		ret_names = map (fst.snd.fst) retval_env
	return (param_env_exprs,param_env,param_names,ret_names)

-- In case of a cutoff, mb_ret_type is Nothing.
solveTraceM :: Maybe Type → [Int] → Trace → CovVecM (Either Bool ResultData)
solveTraceM mb_ret_type traceid trace = do
	(param_env_exprs,param_env,param_names,ret_names) <- getArgRetNames mb_ret_type
	Just retval_env <- case mb_ret_type of
		Nothing  → return $ Just []
		Just ret_type → gets retEnvCVS
	let
		tracename = show traceid
		constraints = concatMap traceitem2constr trace where
		traceitem2constr constraint@(Condition _) = [constraint]
		traceitem2constr constraint@(Assignment (ArrayUpdate _ _ _ _) _) = [constraint]
		traceitem2constr (Return _) = []
		traceitem2constr SolverFind = []
		traceitem2constr (NewDeclaration _) = []
		traceitem2constr (DebugOutput _ _) = []
		traceitem2constr traceelem = error $ "traceitem2constr: There is a strange TraceElem left in the final trace: " ++ show traceelem
		debug_outputs = concatMap is_debug_output trace where
			is_debug_output (DebugOutput name expr) = [(name,expr)]
			is_debug_output _ = []
		(debug_idents,debug_constraints,debug_tyenv) = unzip3 $ for (zip [1..] debug_outputs) $ \ (i,(name,expr)) →
			let name_id = internalIdent (name ++ "_" ++ show i) in
			(name_id,Condition $ Left (CVar name_id (annotation expr),expr),(name_id,extractZ3Type expr))

	tyenv1 <- tyEnvFromTraceM trace
	minimize <- isOptionSet minimizeOpt

--makeAndSolveZ3ModelM :: [Int] → [(Ident,Z3_Type)] → [Constraint] → [SExpr] → [Ident] → String → CovVecM (String,Maybe Solution)
--makeAndSolveZ3ModelM traceid z3tyenv0 constraints additional_sexprs output_idents0 modelpathfile = do
	(model_string,mb_sol) <- makeAndSolveZ3ModelM
		traceid
		(tyenv1 ++ debug_tyenv)
		(constraints ++ debug_constraints)
		(if minimize then (concat $ for param_env_exprs $ \ ((_,(name,_)),expr) → case extractZ3Type expr of
			Z3_Float → fPMinimizer "#x00000001" (identToString name)
			Z3_Double → fPMinimizer "#x0000000000000001" (identToString name)
			Z3_LDouble → fPMinimizer "#x00000000000000000000000000000001" (identToString name)
			_ → [ SExpr [SLeaf "minimize",SLeaf (identToString name)] ]) else [])
		(param_names ++ ret_names ++ debug_idents)
		(analyzerPath </> "models" </> "model_" ++ tracename ++ ".smtlib2")

	return $ case mb_ret_type of
		Nothing → Left $ isJust mb_sol
		Just _ → Right (model_string,case mb_sol of
			Nothing → Nothing
			Just sol → Just (param_env,map fst retval_env,sol))


tyEnvFromTraceM :: Trace → CovVecM [(Ident,Z3_Type)]
tyEnvFromTraceM trace = forM (createTyEnv trace) $ \ (e,t) → do
	z3_t <- ty2Z3TypeOnly t
	return (e,z3_t)

checkSolutionM :: [Int] → ResultData → CovVecM ResultData
checkSolutionM _ resultdata | not checkSolutions = return resultdata
checkSolutionM traceid resultdata@(_,Nothing) = do
	printLogM 2 $ "No solution to check for " ++ show traceid
	return resultdata
checkSolutionM traceid resultdata@(_,Just (_,_,[])) = do
	printLogM 2 $ "Empty solution cannot be checked for " ++ show traceid
	return resultdata
checkSolutionM traceid resultdata@(model_string,Just (param_env0,ret_env0,solution)) = do
	let
		param_env = filter envItemNotPtrType param_env0
		ret_env = filter envItemNotPtrType ret_env0
	srcfilename <- gets srcFilenameCVS
	Just filename <- gets checkExeNameCVS
	absolute_filename <- liftIO $ makeAbsolute srcfilename
	args <- forM param_env $ \ (_,(newident,ty)) →case ty of
		DirectType _ _ _ → case lookup (identToString newident) solution of
			Just (FloatVal (w,_)) → do
				formats <- type_format_string floatType
				return $ printf formats w
			Just (DoubleVal (w,_)) → do
				formats <- type_format_string doubleType
				return $ printf formats w
			Just v → return $ show v
		ty → error $ "checkSolutionM args: type " ++ (render.pretty) ty ++ " not implemented!"
	printLogV 1 $ "checkSolution args = " ++ show args
	(stdout,stderr) <- runHereM (takeDirectory absolute_filename) (takeFileName filename) args
	printLogV 1 $ "stdout=\n" ++ stdout ++ "\n"
	
	findmode <- isOptionSet findModeOpt
	nohalt <- isOptionSet noHaltOnVerificationErrorOpt
	when (findmode && not (solverFindMagicString `isInfixOf` stdout)) $ do
		let errtxt = "\nsolver_find() not called!"
		printToSolutions errtxt
		modify $ \ s → s { verificationErrsCVS = verificationErrsCVS s + 1 }
		case nohalt of
			False → myError errtxt
			True → printLogV 0 $ errtxt

	let
		outputs = words $ last $ lines stdout
		-- get all solution vars that are in the ret_env
		ret_solution = filter ((`elem` (map (identToString.fst) ret_env)).fst) solution
	when (length ret_env /= length outputs || length outputs /= length ret_solution) $
		myError $ "checkSolutionM: lengths of ret_env, solution, outputs differ:\n" ++
			"ret_env = " ++ showEnv ret_env ++ "\n" ++
			"ret_solution = " ++ show ret_solution ++ "\n" ++
			"outputs = " ++ show outputs ++ "\n"
	oks <- forM (zip3 ret_env outputs ret_solution) $ \ ((sourceident,(ident,ty)),s,(ident_s,predicted_result)) → do
		when (identToString ident /= ident_s) $
			myError $ "checkSolutionM: ident=" ++ identToString ident ++ " and ident_s=" ++ ident_s ++ " mismatch"
		case ty of
			PtrType _ _ _ → return True
			DirectType (TyComp _) _ _ → return True
			_ → do
				let exec_result = case ty of
					DirectType (TyIntegral _) _ _       → IntVal $ read s
					DirectType (TyFloating floatty) _ _ → case floatty of
						TyFloat  → let [(w,"")] = readHex s in FloatVal (w,wordToFloat w)
						TyDouble → let [(w,"")] = readHex s in DoubleVal (w,wordToDouble w)
					DirectType (TyEnum _) _ _           → IntVal $ read s
					_ → error $ "checkSolutionM: parsing type " ++ (render.pretty) ty ++ " of " ++ ident_s ++ " not implemented!"
--				printLogV 0 $ "exec_result      = " ++ show exec_result
--				printLogV 0 $ "predicted_result = " ++ show predicted_result
				let check_OK = exec_result == predicted_result
				when (not check_OK) $ do
					let txt = "\ncheckSolutionM ERROR for " ++ ident_s ++ " : exec_val=" ++ show exec_result ++ " /= predicted_result=" ++ show predicted_result ++ "\n"
					printToSolutions txt
					printLogV 0 txt
					liftIO $ writeFile (errorModelPath </> "ERROR_" ++ show traceid <.> ".smtlib2") model_string
					modify $ \ s → s { verificationErrsCVS = verificationErrsCVS s + 1 }
					whenOptionSet noHaltOnVerificationErrorOpt False $ myError "Halting on verification errors."
				return check_OK
	let all_ok = all (==True) oks
	when all_ok $ do
		let all_ok_msg = "\ncheckSolutionM " ++ show traceid ++ " ok.\n"
		printLogV 1 all_ok_msg
		printToSolutions all_ok_msg

	return resultdata
