{-# LANGUAGE TypeOperators,FlexibleInstances,FlexibleContexts,ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module DataTree (dataTreeToHTML) where

import GHC.Generics
import Language.C
import Language.C.Data.Ident

{-
data DataTree =
	Atomic |
	Constructor String [DataTree] |
	

class DataTreeNode f where
	DataTreeNode :: f p -> String

instance DataTreeNode V1 where
	DataTreeNode _ _ = error "DataTreeNode of empty type!"

instance DataTreeNode U1 where
	DataTreeNode _ _ = ""

instance {-# OVERLAPS #-} DataTreeNode (K1 i NodeInfo) where
	DataTreeNode i (K1 c) = ind i ++ show c ++ "\n"

instance {-# OVERLAPS #-} DataTreeNode (K1 i Ident) where
	DataTreeNode i (K1 (Ident name _ _)) = ind i ++ "Ident " ++ show name ++ "\n"

instance {-# OVERLAPS #-} DataTreeNode (K1 i Int) where
	DataTreeNode i (K1 c) = ind i ++ show c ++ "\n"
instance {-# OVERLAPS #-} DataTreeNode (K1 i Char) where
	DataTreeNode i (K1 c) = ind i ++ show c ++ "\n"
instance {-# OVERLAPS #-} DataTreeNode (K1 i String) where
	DataTreeNode i (K1 c) = ind i ++ show c ++ "\n"
instance {-# OVERLAPS #-} DataTreeNode (K1 i Integer) where
	DataTreeNode i (K1 c) = ind i ++ show c ++ "\n"

instance (Generic c,DataTreeNode (Rep c)) => DataTreeNode (K1 i c) where
	DataTreeNode i (K1 c) = DataTreeNode i (from c)

instance (DataTreeNode f,Constructor c) => DataTreeNode (M1 C c f) where
	DataTreeNode i (M1 x) = case conName (undefined :: M1 C c f p) of
		conname -> ind i ++ conname ++ "\n" ++ DataTreeNode (i+1) x

instance (DataTreeNode f,Selector s) => DataTreeNode (M1 S s f) where
	DataTreeNode i (M1 x) = DataTreeNode i x

instance (DataTreeNode f,Datatype d) => DataTreeNode (M1 D d f) where
	DataTreeNode i (M1 x) = DataTreeNode i x

instance (DataTreeNode f1,DataTreeNode f2) => DataTreeNode (f1 :*: f2) where
	DataTreeNode i (a :*: b) = DataTreeNode i a ++ DataTreeNode i b

instance (DataTreeNode f1,DataTreeNode f2) => DataTreeNode (f1 :+: f2) where
	DataTreeNode i (L1 x) = DataTreeNode i x
	DataTreeNode i (R1 x) = DataTreeNode i x
-}

--dataTreeToHTML :: (Generic a,DataTreeNode (Rep a)) => a -> String
dataTreeToHTML x = error "Not yet implemented"
