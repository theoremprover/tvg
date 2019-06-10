{-# LANGUAGE RecordWildCards,LambdaCase,OverloadedStrings,StandaloneDeriving,ExplicitForAll #-}
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
--import GHC.Generics
import System.FilePath
import qualified Data.Map.Strict as Map
import Data.Data
import Data.Generics

import DataTree

gcc = newGCC "gcc"

main = getArgs >>= maini

maini args = do
	let preprocess_args = filter (\ arg -> any (`isPrefixOf` arg) ["-I","-D"]) args
	forM (filter (".c" `isSuffixOf`) args) (handleSrcFile preprocess_args)

{-
deriving instance Generic IdentDecl
deriving instance Generic Decl
deriving instance Generic ObjDef
deriving instance Generic FunDef
deriving instance Generic Enumerator
deriving instance Generic VarDecl
deriving instance Generic VarName
deriving instance Generic DeclAttrs
deriving instance Generic FunctionAttrs
deriving instance Generic Storage
deriving instance Generic Linkage
deriving instance Generic Attr
deriving instance Generic Type
deriving instance Generic TypeName
deriving instance Generic IntType
deriving instance Generic TagDef
deriving instance Generic ParamDecl
deriving instance Generic MemberDecl
deriving instance Generic TypeDef
deriving instance Generic FunType
deriving instance Generic ArraySize
deriving instance Generic TypeDefRef
deriving instance Generic BuiltinType
deriving instance Generic FloatType
deriving instance Generic CompTypeRef
deriving instance Generic CompType
deriving instance Generic CompTyKind
deriving instance Generic EnumTypeRef
deriving instance Generic EnumType
deriving instance Generic TypeQuals

data CTree =
	CTranslUnit_CTranslUnit [CTree] NodeInfo |
	CExternalDeclaration_CDeclExt CTree | 
	CExternalDeclaration_CFDefExt CTree | 
	CExternalDeclaration_CAsmExt CTree NodeInfo | 
	CFunDef_CFunDef [CTree] CTree [CTree] CTree NodeInfo |
	CDeclaration_CDecl [CTree] [(Maybe CTree,Maybe CTree,Maybe CTree)] NodeInfo |
	CStructTag_T
	CStructureUnion_T
	CEnumeration_T
	CFunctionSpecifier_T
	CDeclarationSpecifier_T
	CStorageSpecifier_T
	CTypeSpecifier_T
	CAlignmentSpecifier_T
	CTypeQualifier_T
	CAttribute_T
	CDeclarator_T
	CDerivedDeclarator_T
	CArraySize_T
	CInitializer_T
	CPartDesignator_T
	CStatement_T
	CCompoundBlockItem_T
	CAssemblyStatement_T
	CAssemblyOperand_T
	CExpression_T
	CAssignOp_T
	CBinaryOp_T
	CUnaryOp_T
	CBuiltinThing_T
	CConstant_T
	CStringLiteral
	deriving (Typeable,Data)
-}

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
--			writeFile (srcfilename <.> "ast" <.> "html") $ genericToHTMLString globobjs

			mapM_ print $ (getFunCalls >>> getFunName) globobjs

type CFilter a b = a -> [b]

{-
orElse :: (Data a) => CFilter a a -> CFilter a a -> CFilter a a
orElse f g = \ t -> if null (f t) then g t else f t

deep :: (Data a) => CFilter a a -> CFilter a a
deep f = f `orElse` (getChildren >>> deep f)
-}

(>>>) :: (Typeable a,Typeable b,Typeable c) => CFilter a b -> CFilter b c -> CFilter a c
f >>> g = \ t -> concat [ g t' | t' <- f t ]

getFunCalls :: (Typeable a,Data a) => CFilter a CExpr
getFunCalls = \ t -> everything (++) (mkQ [] funCall) t

{-
isA :: (Data a) => (a -> Bool) -> CFilter a a
isA predicate x = case G.cast predicate of
	Just x -> [x]
	_ -> []
-}
funCall :: CFilter CExpr CExpr
funCall ccall@(CCall fun args _) = [ccall]
funCall _ = []

getFunName :: CFilter CExpr CExpr
getFunName (CCall fun _ _) = [fun]
getFunName _ = []


