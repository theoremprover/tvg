{-# LANGUAGE RecordWildCards,LambdaCase,OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

{-
stack build :calltree-exe && stack exec calltree-exe -- "gcc" calltreetest.c calltreetest2.c calltree.h
-}

module Main where

import System.Environment
import System.Directory
import Data.List
import Control.Monad
import Control.Monad.IO.Class
import Language.C
import Language.C.Data.Ident
import Language.C.System.GCC
import Language.C.Pretty
import Data.Generics
import Control.Monad.Trans.State.Strict
import Data.Graph
import System.FilePath
import Data.Maybe
import System.IO
import Text.PrettyPrint

main = do
	args <- getArgs
	maini args

maini [] = putStrLn "USAGE (in tvg dir): stack exec :calltree-exe -- <GCC-EXE> {<SRCFILE>|<OPT>}*"

maini (gccexe:args) = do
	let (preprocess_args,other_args) = partition (\ arg -> any (`isPrefixOf` arg) ["-I","-D"]) args
	pot_filess <- forM other_args getsrcfiles
--	mapM_ print (concat pot_filess)
	callss <- forM (concat pot_filess) (handleSrcFile gccexe preprocess_args)
--	forM_ (concat callss) print
	let (callgraph,vertex2node,key2vertex) = graphFromEdges $ map (\(funname,mb_defsrcfile,calledfunnames) -> ((funname,mb_defsrcfile),funname,calledfunnames)) (concat callss)

	writeFile "graph.dot" $ unlines $ nub $ map (drawedge vertex2node) (edges callgraph)

	forM_ functions $ \ rootfunname -> do
		case key2vertex rootfunname of
			Nothing -> do
				putStrLn $ "??? Function " ++ rootfunname ++ " not found in source files!"
				writeFile ("calltree_out" </> (rootfunname ++ "_NOT_FOUND")) ""
			Just rootvertex -> do
				forM_ (reachable callgraph rootvertex) $ \ vertex -> do
					let
						calledfuns = map (\ v -> let (node,key,_) = vertex2node v in (node,key)) $ reachable callgraph vertex
						((sourcefun,mb_sourcefile),_,_) = vertex2node vertex
					createDirectoryIfMissing True ("calltree_out" </> rootfunname)
					writeFile ("calltree_out" </> rootfunname </> sourcefun <.> "csv") $ printCSV $ ["FUNCTIONS CALLED DIRECTLY OR INDIRECTLY","FUNCTION DEFINED IN","DIRECTLY CALLED BY"] :
						[ [f, maybe "<NOWHERE>" id mb_srcfile, concat $ intersperse ","
							[ caller | (callerv,calledv) <- edges callgraph, Just calledv == key2vertex fk, let ((caller,_),_,_) = vertex2node callerv ] ] | ((f,mb_srcfile),fk) <- calledfuns ]

	where
	drawedge vertex2node (callerv,calledv) = let
		((callername,_),_,_) = vertex2node callerv
		((calledname,_),_,_) = vertex2node calledv in
		callername ++ " -> " ++ calledname ++ ";"

getsrcfiles :: String -> IO [String]
getsrcfiles name = do
	isdir <- doesDirectoryExist name
	case isdir of
		False -> case any (`isSuffixOf` name) [".i.c",".c.i",".h"] of
			True -> return [name]
			False -> return []
		True -> do
			names <- getDirectoryContents name
			filess <- mapM getsrcfiles $ map (name </>) $ filter (not . (`elem` [".",".."])) names
			return $ concat filess

removeellipses path = do
	srcfiles <- getsrcfiles path
	forM_ srcfiles $ \ filename -> do
		f <- readFile filename
		let ls = lines f
		print $ length ls
		writeFile filename $ unlines $ filter (\ l -> not ("static void __tg_promote(...)" `isInfixOf` l) && not (" __attribute__((__overloadable__, __unavailable__));" `isInfixOf` l)) ls

functions = [
{-
	"abs","acos","acosf","acosh","acoshf","asin","asinf","asinh","asinhf","atan","atan2","atan2f",
	"atanf","atanh","atanhf","cbrt","cbrtf","ceil","ceilf","cos","cosf","cosh","coshf","exp","expm1f","exp2",
	"exp2f","expm1","expf","fabs","fabsf","fdim","fdimf","floor","floorf","fmaf","fmax","fmaxf","fmin","fminf",
	"fmod","fmodf","hypot","hypotf","ilogb","ilogbf","imaxabs","imaxdiv","ldexp","ldexpf","ldiv","llabs","lldiv",
	"log","log10","log10f","log1p","log1pf","log2","log2f","logb","logbf","logf","lrint","lrintf","lround",
	"lroundf","modf","modff","nextafter","nextafterf","pow","powf","remainder","remainderf","rint","rintf",
	"round","roundf","scalbln","scalblnf","scalbn","scalbnf","sin","sinf","sinh","sinhf","sqrt","sqrtf",
	"srand","tan","tanf","tanh","tanhf","trunc","truncf" ]
	-}
	"__aeabi_cdcmpeq","__aeabi_cdcmple","__aeabi_cdrcmple","__aeabi_cfcmpeq","__aeabi_cfcmple","__aeabi_cfrcmple","__aeabi_d2f","__aeabi_d2h",
	"__aeabi_d2h_alt","__aeabi_d2iz","__aeabi_d2lz","__aeabi_d2uiz","__aeabi_d2ulz","__aeabi_dadd","__aeabi_dcmpeq","__aeabi_dcmpge","__aeabi_dcmpgt",
	"__aeabi_dcmple","__aeabi_dcmplt","__aeabi_dcmpun","__aeabi_ddiv","__aeabi_dmul","__aeabi_drsub","__aeabi_dsub","__aeabi_f2d","__aeabi_f2h","__aeabi_f2h_alt",
	"__aeabi_f2iz","__aeabi_f2lz","__aeabi_f2uiz","__aeabi_f2ulz","__aeabi_fadd","__aeabi_fcmpeq","__aeabi_fcmpge","__aeabi_fcmpgt","__aeabi_fcmple","__aeabi_fcmplt",
	"__aeabi_fcmpun","__aeabi_fdiv","__aeabi_fmul","__aeabi_frsub","__aeabi_fsub","__aeabi_h2f","__aeabi_h2f_alt","__aeabi_i2d","__aeabi_i2f","__aeabi_idivmod",
	"__aeabi_idiv","__aeabi_idiv0","__aeabi_l2d","__aeabi_l2f","__aeabi_lasr","__aeabi_lcmp","__aeabi_ldivmod","__aeabi_ldiv0","__aeabi_llsl","__aeabi_llsr",
	"__aeabi_lmul","__aeabi_memclr","__aeabi_memclr4","__aeabi_memclr8","__aeabi_memcpy","__aeabi_memcpy4","__aeabi_memcpy8","__aeabi_memmove","__aeabi_memmove4",
	"__aeabi_memmove8","__aeabi_memset","__aeabi_memset4","__aeabi_memset8","__aeabi_ui2d","__aeabi_ui2f","__aeabi_uidiv","__aeabi_uidivmod","__aeabi_ul2d",
	"__aeabi_ul2f","__aeabi_ulcmp","__aeabi_uldivmod","__aeabi_uread4","__aeabi_uread8","__aeabi_uwrite4","__aeabi_uwrite8","abs","acos","acosf","acosh",
	"acoshf","add","addf","asin","asinf","asinh","asinhf","atan","atan2","atan2f","atanf","atanh","atanhf","cbrt","cbrtf","ceil","ceilf","cos","cosf",
	"cosh","coshf","divide","dividef","exp","expm1f","exp2","exp2f","expm1","expf","fabs","fabsf","fdim","fdimf","floor","floorf","fmaf","fmax","fmaxf",
	"fmin","fminf","fmod","fmodf","HUGE_VAL","hypot","hypotf","ilogb","ilogbf","imaxabs","imaxdiv","ldexp","ldexpf","ldiv","llabs","lldiv","log","log10",
	"log10f","log1p","log1pf","log2","log2f","logb","logbf","logf","lrint","lrintf","lround","lroundf","modf","modff","multiply","multiplyf","nextafter",
	"nextafterf","NULL","pow","powf","remainder","remainderf","rint","rintf","round","roundf","scalbln","scalblnf","scalbn","scalbnf","sin","sinf","sinh",
	"sinhf","size_t","sqrt","sqrtf","srand","subtract","subtractf","tan","tanf","tanh","tanhf","trunc","truncf" ]

printCSV ls = unlines $ map (concat . (intersperse ";")) ls

-- Returns a list of (funname,srcfile,[calledfunname1,...])
handleSrcFile gccexe preprocess_args srcfilename = do
	putStrLn $ "handling SrcFile " ++ srcfilename
	mb_ast <- parseCFile (newGCC gccexe) Nothing preprocess_args srcfilename
	case mb_ast of
		Left err -> error $ show err
		Right ctranslunit -> execStateT (everywhereM (mkM searchFunDefs) ctranslunit) []
			
-- For all function definitons in the current source file:
searchFunDefs :: CFunDef -> StateT [(String,Maybe FilePath,[String])] IO CFunDef
searchFunDefs cfundef@(CFunDef _ (CDeclr (Just (Ident funname _ _)) _ _ _ _) _ stmt ni) = do
	calledfunnames <- execStateT (everywhereM (mkM searchFunCalls) stmt) []
	modify ((funname,fileOfNode ni,nub calledfunnames) : )
	return cfundef

-- Returns all directly called functions
searchFunCalls :: (MonadIO m) => CExpr -> StateT [String] m CExpr
searchFunCalls ccall@(CCall funname_expr _ ni) = case funname_expr of
	CVar (Ident name _ _) _ -> do
		modify (name : )
		return ccall
	fne -> do
		let msg = show (posOfNode ni) ++ ": " ++ (render $ pretty ccall) ++ "\n"
		liftIO $ do
			putStr $ "searchFunCallsM: Strange funname_expr: " ++ msg
			appendFile ("calltree_out" </> "strangecalls.txt") msg 
		return ccall

searchFunCalls cexpr = return cexpr

