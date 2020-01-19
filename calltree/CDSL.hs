{-# LANGUAGE RecordWildCards,LambdaCase,OverloadedStrings,StandaloneDeriving,ExplicitForAll,DeriveGeneric,FlexibleInstances,TupleSections #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module CDSL where

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

type CFilter a b = a -> [b]

(>>>) :: (Typeable a,Typeable b,Typeable c) => CFilter a b -> CFilter b c -> CFilter a c
f >>> g = \ t -> concat [ g t' | t' <- f t ]

(<+>) :: (Typeable a,Typeable b) => CFilter a b -> CFilter a b -> CFilter a b
f <+> g = \ t -> f t ++ g t

filterPred :: (a -> Bool) -> CFilter a a
filterPred p = \ t -> if p t then [t] else []

mapFilter :: (a -> b) -> CFilter a b
mapFilter f = \ t -> [ f t ]

filterFirst :: (Typeable a,Typeable b) => (a -> Bool) -> CFilter (a,b) b
filterFirst p = filterPred (p.fst) >>> mapFilter snd

filterSecondFirst :: (Typeable a,Typeable b,Typeable b,Data b,Typeable c,Data c) => (c -> Bool) -> CFilter (a,(c,b)) (a,b)
filterSecondFirst p = filterPred (p.fst.snd) >>> mapFilter (\ (a,(_,b)) -> (a,b))

findAll :: (Typeable a,Data a,Typeable b,Data b,Typeable c,Data c) => CFilter b c -> CFilter a c
findAll filt = everything (++) (mkQ [] filt)

rememberInProduct :: (Typeable a,Data a,Typeable b,Data b) => CFilter a b -> CFilter a (a,b)
rememberInProduct filt = \ t -> map ((t,)) (filt t)

findOne :: (Typeable a,Data a,Typeable b,Data b) => CFilter b b -> CFilter a b
findOne filt =  take 1 . findAll filt

isA :: (Typeable a,Data a,Typeable b,Data b) => CFilter a b -> CFilter a b
isA = id

isCVarWithIdent :: Ident -> CExpr -> Bool
isCVarWithIdent ident (CVar cvarident _) = ident==cvarident
isCVarWithIdent _ _ = False

funCall :: CFilter CExpr CExpr
funCall ccall@(CCall fun args _) = [ccall]
funCall _ = []

funCallInFunDef :: (Typeable a,Data a) => CFilter a (CFunDef,CExpr)
funCallInFunDef = findAll funDef >>> rememberInProduct (findAll funCall)

getCallArgs :: CFilter CExpr [CExpr]
getCallArgs (CCall _ args _) = [ args ]
getCallArgs _ = []

funDef :: CFilter CExtDecl CFunDef
funDef (CFDefExt cfundef) = [ cfundef ]
funDef _ = []

funDefName :: CFilter CFunDef Ident
funDefName fundef = [ getFunDefIdent fundef ]

-- funDecl finds both declarations and definitions
cDeclr :: CFilter CDeclr CDeclr
cDeclr cdeclr@(CDeclr _ _ _ _ _) = [ cdeclr ]

declrName :: CFilter CDeclr Ident
declrName (CDeclr (Just ident) _ _ _ _) = [ ident ]
declrName _ = []

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

cAssign :: CFilter CExpr (CExpr,CExpr)
cAssign (CAssign CAssignOp expr value _) = [(expr,value)]
cAssign _ = []

cAssignWithFunDef :: (Typeable a,Data a) => CFilter a (CFunDef,(CExpr,CExpr))
cAssignWithFunDef = findAll funDef >>> rememberInProduct (findAll cAssign)

varAssignment :: (Typeable a,Data a) => (CExpr -> Bool) -> CFilter a CExpr 
varAssignment pred = findAll cAssign >>> filterFirst pred

varAssignmentWithFunDef :: (Typeable a,Data a) => (CExpr -> Bool) -> CFilter a (CFunDef,CExpr)
varAssignmentWithFunDef pred = cAssignWithFunDef >>> filterSecondFirst pred

defFunName :: (Typeable a,Data a) => Ident -> CFilter a Ident
defFunName ident = findAll funDef >>> funDefName

declName :: (Typeable a,Data a) => Ident -> CFilter a Ident
declName ident = findAll cDeclr >>> declrName >>> filterPred (==ident)

getFunDefIdent :: CFunDef -> Ident
getFunDefIdent (CFunDef _ (CDeclr (Just ident) _ _ _ _) _ _ _) = ident
getFunDefIdent fundef = error $ "getFunDefIdent: " ++ (render.pretty) fundef ++ " has no name!"