{-# LANGUAGE TemplateHaskell #-}

module CLenses where

import Control.Lens
import Language.C.Syntax.AST
import Language.C.Data.Node

makeLenses ''CTranslationUnit
makeLenses ''CExternalDeclaration
makeLenses ''CFunctionDef
makeLenses ''CDeclaration
makeLenses ''CStructTag
makeLenses ''CStructureUnion
makeLenses ''CEnumeration
makeLenses ''CFunctionSpecifier
makeLenses ''CDeclarationSpecifier
makeLenses ''CStorageSpecifier
makeLenses ''CTypeSpecifier
makeLenses ''CAlignmentSpecifier
makeLenses ''CTypeQualifier
makeLenses ''CAttribute
makeLenses ''CDeclarator
makeLenses ''CDerivedDeclarator
makeLenses ''CArraySize
makeLenses ''CInitializer
makeLenses ''CPartDesignator
makeLenses ''CStatement
makeLenses ''CCompoundBlockItem
makeLenses ''CAssemblyStatement
makeLenses ''CAssemblyOperand
makeLenses ''CExpression
makeLenses ''CAssignOp
makeLenses ''CBinaryOp
makeLenses ''CUnaryOp
makeLenses ''CBuiltinThing
makeLenses ''CConstant
makeLenses ''CStringLiteral
makeLenses ''NodeInfo
