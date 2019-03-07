{-# LANGUAGE TypeOperators,FlexibleInstances,FlexibleContexts,ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module DataTree (genericToHTMLString) where

import GHC.Generics
import Language.C
import Language.C.Data.Ident
import Text.Html


data DataTree = DataTree String [DataTree] | Leaf String deriving (Show)

class DataTreeNode f where
	dataTree :: f p -> [DataTree]

instance DataTreeNode V1 where
	dataTree _ = error "DataTreeNode of empty type!"

instance {-# OVERLAPS #-} DataTreeNode (K1 i NodeInfo) where
	dataTree (K1 c) = [Leaf $ show c]

instance {-# OVERLAPS #-} DataTreeNode (K1 i Ident) where
	dataTree (K1 (Ident name _ _)) = [Leaf $ "Ident " ++ show name]

instance {-# OVERLAPS #-} DataTreeNode (K1 i Int) where
	dataTree (K1 c) = [Leaf $ show c]
instance {-# OVERLAPS #-} DataTreeNode (K1 i String) where
	dataTree (K1 c) = [Leaf $ show c]
instance {-# OVERLAPS #-} DataTreeNode (K1 i Char) where
	dataTree (K1 c) = [Leaf $ show c]
instance {-# OVERLAPS #-} DataTreeNode (K1 i Integer) where
	dataTree (K1 c) = [Leaf $ show c]

instance (Generic c,DataTreeNode (Rep c)) => DataTreeNode (K1 i c) where
	dataTree (K1 c) = dataTree (from c)

instance (DataTreeNode f,Constructor c) => DataTreeNode (M1 C c f) where
	dataTree (M1 x) = case conName (undefined :: M1 C c f p) of
		conname -> [DataTree conname (dataTree x)]

instance (DataTreeNode f,Selector s) => DataTreeNode (M1 S s f) where
	dataTree (M1 x) = dataTree x

instance (DataTreeNode f,Datatype d) => DataTreeNode (M1 D d f) where
	dataTree (M1 x) = dataTree x

instance DataTreeNode U1 where
	dataTree U1 = [Leaf $ "()"]

instance (DataTreeNode f1,DataTreeNode f2) => DataTreeNode (f1 :*: f2) where
	dataTree (a :*: b) = dataTree a ++ dataTree b

instance (DataTreeNode f1,DataTreeNode f2) => DataTreeNode (f1 :+: f2) where
	dataTree (L1 x) = dataTree x
	dataTree (R1 x) = dataTree x

toDataTree :: (Generic a,DataTreeNode (Rep a)) => a -> [DataTree]
toDataTree x = dataTree (from x)

dataTreeToHtml (Leaf s) = li (stringToHtml s)
dataTreeToHtml (DataTree s subtrees) = li ((thespan ! [theclass "caret"]) (stringToHtml s) +++
	(ulist ! [theclass "nested"]) (concatHtml $ map dataTreeToHtml subtrees))

genericToHTMLString :: (Generic a,DataTreeNode (Rep a)) => a -> String
genericToHTMLString x = renderHtml $ pageframe $ map (! [identifier "myUL"]) $ map dataTreeToHtml $ toDataTree x
	where
	pageframe tree_htmls =
		header (style css) +++
		body ( concatHtml tree_htmls +++ tag "script" toggler )

css = primHtml "\
\ ul, #myUL {\
\  list-style-type: none;\
\ }\
\ \
\ #myUL {\
\  margin: 0;\
\  padding: 0;\
\ }\
\ \
\ .caret {\
\  cursor: pointer;\
\  user-select: none;\
\ }\
\ \
\ .caret::before {\
\  content: \"\\25B6\";\
\  color: black;\
\  display: inline-block;\
\  margin-right: 6px;\
\ }\
\ \
\ .caret-down::before {\
\  transform: rotate(90deg);\
\ }\
\ \
\ .nested {\
\  display: none;\
\ }\
\ \
\ .active {\
\  display: block;\
\ }\
\ "

toggler = primHtml "\
\ var toggler = document.getElementsByClassName(\"caret\");\
\ var i;\
\ \
\ for (i = 0; i < toggler.length; i++) {\
\  toggler[i].addEventListener(\"click\", function() {\
\    this.parentElement.querySelector(\".nested\").classList.toggle(\"active\");\
\    this.classList.toggle(\"caret-down\");\
\  });\
\ }\
\ "