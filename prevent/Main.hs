{-# LANGUAGE RecordWildCards,LambdaCase,OverloadedStrings,DeriveGeneric,StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

{--
stack build :prevent-exe
stack exec prevent-exe -- test.c
stack build :prevent-exe && stack exec prevent-exe -- prevent/test.c
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
import GHC.Generics
import System.FilePath
import qualified Data.Map.Strict as Map

import DataTree

gcc = newGCC "gcc"

main = getArgs >>= maini

maini args = do
	let preprocess_args = filter (\ arg -> any (`isPrefixOf` arg) ["-I","-D"]) args
	forM (filter (".c" `isSuffixOf`) args) (handleSrcFile preprocess_args)

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

handleSrcFile preprocess_args srcfilename = do
	mb_ast <- parseCFile gcc Nothing preprocess_args srcfilename
	case mb_ast of
		Left err -> error $ show err
		Right ctranslunit -> do
			let Right (GlobalDecls globobjs _ _,[]) = runTrav_ $ analyseAST ctranslunit
			writeFile (srcfilename <.> "ast" <.> "html") $ genericToHTMLString (Map.toList globobjs)
