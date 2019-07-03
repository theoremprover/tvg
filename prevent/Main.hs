{-# LANGUAGE RecordWildCards,LambdaCase,OverloadedStrings,StandaloneDeriving,ExplicitForAll,DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

{--
stack build :prevent-exe
stack exec prevent-exe -- test.c
stack build :prevent-exe && stack exec prevent-exe -- prevent/test.c

https://wiki.haskell.org/HXT#The_concept_of_filters
http://hackage.haskell.org/package/language-c-0.8.2/docs/Language-C-Syntax-AST.html
--}

module Main where

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
import Language.C.Pretty (pretty)
import Text.PrettyPrint (render)
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

deriving instance Typeable IdentDecl
deriving instance Typeable Decl
deriving instance Typeable ObjDef
deriving instance Typeable FunDef
deriving instance Typeable Enumerator
deriving instance Typeable VarDecl
deriving instance Typeable VarName
deriving instance Typeable DeclAttrs
deriving instance Typeable FunctionAttrs
deriving instance Typeable Storage
deriving instance Typeable Linkage
deriving instance Typeable Attr
deriving instance Typeable Type
deriving instance Typeable TypeName
deriving instance Typeable IntType
deriving instance Typeable TagDef
deriving instance Typeable ParamDecl
deriving instance Typeable MemberDecl
deriving instance Typeable TypeDef
deriving instance Typeable FunType
deriving instance Typeable ArraySize
deriving instance Typeable TypeDefRef
deriving instance Typeable BuiltinType
deriving instance Typeable FloatType
deriving instance Typeable CompTypeRef
deriving instance Typeable CompType
deriving instance Typeable CompTyKind
deriving instance Typeable EnumTypeRef
deriving instance Typeable EnumType
deriving instance Typeable TypeQuals

handleSrcFile preprocess_args srcfilename = do
	mb_ast <- parseCFile gcc Nothing preprocess_args srcfilename
	case mb_ast of
		Left err -> error $ show err
		Right ctranslunit -> do
			let
				Right (GlobalDecls gobjs_map _ _,[]) = runTrav_ $ analyseAST ctranslunit
				globobjs = Map.elems gobjs_map
			writeFile (srcfilename <.> "ast" <.> "html") $ genericToHTMLString globobjs

			mapM_ print $ myFilter globobjs

type CFilter a b = a -> [b]

(>>>) :: (Typeable a,Typeable b,Typeable c) => CFilter a b -> CFilter b c -> CFilter a c
f >>> g = \ t -> concat [ g t' | t' <- f t ]

(<+>) :: (Typeable a,Typeable b) => CFilter a b -> CFilter a b -> CFilter a b
f <+> g = \ t -> f t ++ g t

isA :: (Typeable a,Data a,Typeable b,Data b) => CFilter b b -> CFilter a b
isA filt = \ t -> everything (++) (mkQ [] filt) t

funCall :: CFilter CExpr CExpr
funCall ccall@(CCall fun args _) = [ccall]
funCall _ = []

ternaryIf :: CFilter CExpr CExpr
ternaryIf ccond@(CCond _ _ _ _) = [ccond]
ternaryIf _ = []

binaryOp :: CFilter CExpr CExpr
binaryOp cbinary@(CBinary _ _ _ _) = [cbinary]
binaryOp _ = []

postfixOp :: CFilter CExpr CExpr
postfixOp cunary@(CUnary unaryop _ _) = if unaryop `elem` [CPostIncOp,CPostDecOp] then [cunary] else []
postfixOp _ = []

toPretty :: (Pretty a,Pos a) => CFilter a String
toPretty a = let p = posOf a in
	[ printf "%s, line %i, col %i  :  %s" (posFile p) (posRow p) (posColumn p) (render $ pretty a ) ]

toString :: (Show a) => CFilter a String
toString a = [ show a ]

cDecl :: CFilter CDecl CDecl
cDecl cdecl@(CDecl _ _ _) = [cdecl]
cDecl _ = []

-- deriveddeclrs :: [ CArrDeclr [CTypeQualifier a] (CArraySize a) a ]
-- initlist a :: [([CPartDesignator a], CInitializer a)] 
notFullyInitializedArray :: CFilter CDecl String
notFullyInitializedArray cdecl@(CDecl _ l nodeinfo) = map notfullyinitialized l where
	notfullyinitialized (Just (CDeclr _ deriveddeclrs _ _ _),Just (CInitList initlist _),_) = match deriveddeclrs initlist
	match [] [] = False
	match () ()
	match _ = True
notFullyInitializedArray _ = []

-------------------

complexExpr = ternaryIf <+> binaryOp

--myFilter = isA complexExpr >>> isA postfixOp >>> toPretty
myFilter = isA cDecl >>> 
