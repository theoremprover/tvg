{-# LANGUAGE PackageImports,TypeOperators,FlexibleInstances,FlexibleContexts,ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module DataTree (genericToHTMLString) where

import GHC.Generics
import "language-c" Language.C
import Language.C.Data.Ident
import Text.Html


data DataTree = DataTree String [DataTree] | Leaf String deriving (Show)

data Mode = None | CollectList deriving (Show,Eq)

class DataTreeNode f where
	dataTree :: Mode -> f p -> [DataTree]

instance DataTreeNode V1 where
	dataTree _ _ = error "DataTreeNode of empty type!"

instance {-# OVERLAPS #-} DataTreeNode (K1 i NodeInfo) where
	dataTree _ (K1 c) = [Leaf $ show c]

instance {-# OVERLAPS #-} DataTreeNode (K1 i Ident) where
	dataTree _ (K1 (Ident name _ _)) = [Leaf $ "Ident " ++ show name]

instance {-# OVERLAPS #-} DataTreeNode (K1 i Int) where
	dataTree _ (K1 c) = [Leaf $ show c]
instance {-# OVERLAPS #-} DataTreeNode (K1 i String) where
	dataTree _ (K1 c) = [Leaf $ show c]
instance {-# OVERLAPS #-} DataTreeNode (K1 i Char) where
	dataTree _ (K1 c) = [Leaf $ show c]
instance {-# OVERLAPS #-} DataTreeNode (K1 i Integer) where
	dataTree _ (K1 c) = [Leaf $ show c]

instance (Generic c,DataTreeNode (Rep c)) => DataTreeNode (K1 i c) where
	dataTree mode (K1 c) = dataTree mode (from c)

instance (DataTreeNode f,Constructor c) => DataTreeNode (M1 C c f) where
	dataTree mode (M1 x) = case (mode,conName (undefined :: M1 C c f p)) of
		(None,       ":")  -> [DataTree "[]" (dataTree CollectList x)]
		(CollectList,":")  -> dataTree CollectList x
		(CollectList,"[]") -> []
		(_,conname)        -> [DataTree conname (dataTree None x)]

instance (DataTreeNode f,Selector s) => DataTreeNode (M1 S s f) where
	dataTree mode (M1 x) = dataTree mode x

instance (DataTreeNode f,Datatype d) => DataTreeNode (M1 D d f) where
	dataTree mode (M1 x) = dataTree mode x

instance DataTreeNode U1 where
	dataTree _ U1 = []

instance (DataTreeNode f1,DataTreeNode f2) => DataTreeNode (f1 :*: f2) where
	dataTree mode (a :*: b) = dataTree mode a ++ dataTree mode b

instance (DataTreeNode f1,DataTreeNode f2) => DataTreeNode (f1 :+: f2) where
	dataTree mode (L1 x) = dataTree mode x
	dataTree mode (R1 x) = dataTree mode x

toDataTree :: (Generic a,DataTreeNode (Rep a)) => a -> [DataTree]
toDataTree x = dataTree None (from x)

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
\  content: \"\\229E\";\
\  color: blue;\
\  display: inline-block;\
\  margin-right: 6px;\
\ }\
\ \
\ .caret-down::before {\
\  content: \"\\229F\";\
\  color: blue;\
\ }\
\ \
\ .nested {\
\  display: none;\
\  margin: 0;\
\ }\
\ \
\ .active {\
\  display: block;\
\  margin: 0;\
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