{-# LANGUAGE RecordWildCards,LambdaCase,OverloadedStrings,StandaloneDeriving,ExplicitForAll,DeriveGeneric,FlexibleInstances,TupleSections #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

{--
stack build :prevent-exe && stack exec prevent-exe -- prevent/test.c

http://hackage.haskell.org/package/language-c-0.8.2/docs/Language-C-Syntax-AST.html
--}

module Main where

import Prelude hiding ((<>))
import System.Environment
import Data.List
import Control.Monad
import Language.C
import Language.C.System.GCC
import Language.C.Analysis.AstAnalysis
import Language.C.Analysis.TravMonad
import Language.C.Analysis.SemRep
import Language.C.Data.Ident
import Language.C.Data.Position
import qualified GHC.Generics as GHCG
import System.FilePath
import qualified Data.Map.Strict as Map
import Data.Data
import Data.Generics
import Language.C.Pretty
import Text.PrettyPrint hiding ((<+>))
import Text.Printf

import DataTree

gcc = newGCC "gcc"

main = getArgs >>= maini

maini args = do
	let preprocess_args = filter (\ arg -> any (`isPrefixOf` arg) ["-I","-D"]) args
	forM (filter (".c" `isSuffixOf`) args) (handleSrcFile preprocess_args)

deriving instance GHCG.Generic IdentDecl
deriving instance GHCG.Generic Decl
deriving instance GHCG.Generic ObjDef
deriving instance GHCG.Generic FunDef
deriving instance GHCG.Generic Enumerator
deriving instance GHCG.Generic VarDecl
deriving instance GHCG.Generic VarName
deriving instance GHCG.Generic DeclAttrs
deriving instance GHCG.Generic FunctionAttrs
deriving instance GHCG.Generic Storage
deriving instance GHCG.Generic Linkage
deriving instance GHCG.Generic Attr
deriving instance GHCG.Generic Type
deriving instance GHCG.Generic TypeName
deriving instance GHCG.Generic IntType
deriving instance GHCG.Generic TagDef
deriving instance GHCG.Generic ParamDecl
deriving instance GHCG.Generic MemberDecl
deriving instance GHCG.Generic TypeDef
deriving instance GHCG.Generic FunType
deriving instance GHCG.Generic ArraySize
deriving instance GHCG.Generic TypeDefRef
deriving instance GHCG.Generic BuiltinType
deriving instance GHCG.Generic FloatType
deriving instance GHCG.Generic CompTypeRef
deriving instance GHCG.Generic CompType
deriving instance GHCG.Generic CompTyKind
deriving instance GHCG.Generic EnumTypeRef
deriving instance GHCG.Generic EnumType
deriving instance GHCG.Generic TypeQuals

handleSrcFile preprocess_args srcfilename = do
	mb_ast <- parseCFile gcc Nothing preprocess_args srcfilename
	case mb_ast of
		Left err -> error $ show err
		Right ctranslunit -> do
			let
				gobjs_map = case runTrav_ $ analyseAST ctranslunit of
					Left errmsgs -> error $ unlines $ map show errmsgs
					Right (GlobalDecls gobjs_map _ _,[]) -> gobjs_map
				globobjs = Map.elems gobjs_map
			writeFile (srcfilename <.> "ast" <.> "html") $ genericToHTMLString globobjs

			mapM_ putStrLn $ myFilter globobjs

type CFilter a b = a -> [b]

(>>>) :: (Typeable a,Typeable b,Typeable c) => CFilter a b -> CFilter b c -> CFilter a c
f >>> g = \ t -> concat [ g t' | t' <- f t ]

(<+>) :: (Typeable a,Typeable b) => CFilter a b -> CFilter a b -> CFilter a b
f <+> g = \ t -> f t ++ g t

findAll :: (Typeable a,Data a,Typeable b,Data b) => CFilter b b -> CFilter a b
findAll filt = everything (++) (mkQ [] filt)

findOne :: (Typeable a,Data a,Typeable b,Data b) => CFilter b b -> CFilter a b
findOne filt =  take 1 . findAll filt

isA :: (Typeable a,Data a,Typeable b,Data b) => CFilter a b -> CFilter a b
isA = id

funCall :: CFilter CExpr CExpr
funCall ccall@(CCall fun args _) = [ccall]
funCall _ = []

ternaryIf :: CFilter CExpr CExpr
ternaryIf ccond@(CCond _ _ _ _) = [ccond]
ternaryIf _ = []

binaryOp :: CFilter CExpr CExpr
binaryOp cbinary@(CBinary _ _ _ _) = [cbinary]
binaryOp _ = []

incOrDecOp :: CFilter CExpr CExpr
incOrDecOp cunary@(CUnary unaryop _ _) = if unaryop `elem` [CPostIncOp,CPostDecOp,CPreIncOp,CPreDecOp] then [cunary] else []
incOrDecOp _ = []

showPretty :: (Pretty a,CNode a) => CFilter a String
showPretty a = [ printf "%s%s" (showPos $ nodeInfo a) (render $ pretty a) ]

showPos :: NodeInfo -> String
showPos nodeinfo = printf "%s, line %i, col %i  :  " (posFile p) (posRow p) (posColumn p) where
	p = posOfNode nodeinfo

toString :: (Show a) => CFilter a String
toString a = [ show a ]

cDecl :: CFilter CDecl CDecl
cDecl cdecl@(CDecl _ _ _) = [cdecl]
cDecl _ = []

type ASTRoot = [IdentDecl]

arrayDecl :: CFilter CDecl ([CArrSize],CInit)
arrayDecl = arrayDeclDimsInits

arrayDeclDimsInits :: CFilter CDecl ([CArrSize],CInit)
arrayDeclDimsInits (CDecl _ l ni) = concatMap arraydecl l where
	arraydecl (Just (CDeclr _ deriveddeclrs _ _ _),Just cinit,_) = case toarrsize deriveddeclrs of
		[] -> []
		arrsizes -> [ (arrsizes,cinit) ]
	arraydecl _ = []
	toarrsize [] = []
	toarrsize (CArrDeclr _ arrsize _ : rest) = arrsize : toarrsize rest
	toarrsize (_:rest) = toarrsize rest

checkArrayDecl :: CFilter ([CArrSize],CInit) String
checkArrayDecl ( [] , CInitExpr _ _ ) = []
checkArrayDecl ( [] , CInitList _ ni ) = [ showPos ni ++ "Initializers nested deeper than array" ]
checkArrayDecl ( CNoArrSize _ : rest , CInitList initlist _ ) = concatMap (checkArrayDecl.(rest,)) (map snd initlist)
checkArrayDecl ( CArrSize _ (CConst (CIntConst (CInteger n _ _) _)) : rest , CInitList initlist _ ) | fromIntegral n == length initlist = concatMap (checkArrayDecl.(rest,)) (map snd initlist)
checkArrayDecl ( CArrSize _ (CConst (CIntConst (CInteger n _ _) _)) : rest , CInitList initlist ni ) = [ showPos ni ++ "length of initializer list should be " ++ show n ++", but is " ++ show (length initlist) ]
checkArrayDecl ( _:_ , CInitExpr _ ni ) = [ showPos ni ++ "Expected one more dimension in initializers" ]
checkArrayDecl ( CArrSize _ sizeexpr : _ , CInitList _ _ ) = [ head (showPretty sizeexpr) ++ " : Array size is not an integer constant" ]

-- Prevent DSL -----------------

{-
complexExpr = ternaryIf <+> binaryOp

myFilter =
	findAll complexExpr >>> findAll incOrDecOp >>> showPretty
-}

myFilter =
	findAll cDecl >>> isA arrayDecl >>> checkArrayDecl >>> toString

