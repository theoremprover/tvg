{-# LANGUAGE TypeOperators,FlexibleInstances,FlexibleContexts,ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module DataTree (toDataTree) where

import GHC.Generics
import Language.C
import Language.C.Data.Ident
	

data DataTree = DataTree [DataTree] | Leaf (Show v) deriving (Show)

class DataTreeNode f where
	dataTree :: f p -> DataTree

instance DataTreeNode V1 where
	dataTree _ _ = error "DataTreeNode of empty type!"

instance DataTreeNode U1 where
	dataTree _ _ = Unit

instance {-# OVERLAPS #-} DataTreeNode (K1 i NodeInfo) where
	dataTree (K1 c) = ind i ++ show c ++ "\n"

instance {-# OVERLAPS #-} DataTreeNode (K1 i Ident) where
	dataTree (K1 (Ident name _ _)) = ind i ++ "Ident " ++ show name ++ "\n"

instance {-# OVERLAPS #-} DataTreeNode (K1 i Int) where
	dataTree (K1 c) = show c ++ "\n"
instance {-# OVERLAPS #-} DataTreeNode (K1 i Char) where
	dataTree (K1 c) = show c ++ "\n"
instance {-# OVERLAPS #-} DataTreeNode (K1 i String) where
	dataTree (K1 c) = show c ++ "\n"
instance {-# OVERLAPS #-} DataTreeNode (K1 i Integer) where
	dataTree (K1 c) = show c ++ "\n"

instance (Generic c,DataTreeNode (Rep c)) => DataTreeNode (K1 i c) where
	dataTree (K1 c) = DataTreeNode (from c)

instance (DataTreeNode f,Constructor c) => DataTreeNode (M1 C c f) where
	dataTree (M1 x) = case conName (undefined :: M1 C c f p) of
		conname -> conname ++ "\n" ++ DataTreeNode (i+1) x

instance (DataTreeNode f,Selector s) => DataTreeNode (M1 S s f) where
	dataTree (M1 x) = DataTreeNode x

instance (DataTreeNode f,Datatype d) => DataTreeNode (M1 D d f) where
	dataTree (M1 x) = DataTreeNode x

instance (DataTreeNode f1,DataTreeNode f2) => DataTreeNode (f1 :*: f2) where
	dataTree (a :*: b) = DataTreeNode a ++ DataTreeNode b

instance (DataTreeNode f1,DataTreeNode f2) => DataTreeNode (f1 :+: f2) where
	dataTree (L1 x) = DataTreeNode x
	dataTree (R1 x) = DataTreeNode x

toDataTree :: (Generic a,DataTreeNode (Rep a)) => a -> HTML
toDataTree x = dataTree (from x)
