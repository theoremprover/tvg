{-# LANGUAGE TypeOperators,FlexibleInstances,FlexibleContexts,ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module DataTree (toDataTree) where

import GHC.Generics
import Language.C
import Language.C.Data.Ident
import Text.Html


data DataTree = DataTree String [DataTree] | Leaf String deriving (Show)

class DataTreeNode f where
	dataTree :: f p -> DataTree

instance DataTreeNode V1 where
	dataTree _ = error "DataTreeNode of empty type!"

instance DataTreeNode U1 where
	dataTree _ = Leaf "()"

instance {-# OVERLAPS #-} DataTreeNode (K1 i NodeInfo) where
	dataTree (K1 c) = Leaf $ show c

instance {-# OVERLAPS #-} DataTreeNode (K1 i Ident) where
	dataTree (K1 (Ident name _ _)) = "Ident " ++ show name ++ "\n"

instance {-# OVERLAPS #-} (Show t) => DataTreeNode (K1 i t) where
	dataTree (K1 c) = Leaf $ show c

instance (Generic c,DataTreeNode (Rep c)) => DataTreeNode (K1 i c) where
	dataTree (K1 c) = dataTree (from c)

instance (DataTreeNode f,Constructor c) => DataTreeNode (M1 C c f) where
	dataTree (M1 x) = case conName (undefined :: M1 C c f p) of
		conname -> DataTree conname [dataTree x]

instance (DataTreeNode f,Selector s) => DataTreeNode (M1 S s f) where
	dataTree (M1 x) = dataTree x

instance (DataTreeNode f,Datatype d) => DataTreeNode (M1 D d f) where
	dataTree (M1 x) = dataTree x

instance (DataTreeNode f1,DataTreeNode f2) => DataTreeNode (f1 :*: f2) where
	dataTree (a :*: b) = DataTree "*" [dataTree a,dataTree b]

instance (DataTreeNode f1,DataTreeNode f2) => DataTreeNode (f1 :+: f2) where
	dataTree (L1 x) = DataTree "Left" [dataTree x]
	dataTree (R1 x) = DataTree "Right" [dataTree x]

toDataTree :: (Generic a,DataTreeNode (Rep a)) => a -> DataTree
toDataTree x = dataTree (from x)

dataTreeToHTML :: DataTree -> Html
dataTreeToHTML datatree = noHtml