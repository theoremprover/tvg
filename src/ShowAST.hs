{-# LANGUAGE TypeOperators,FlexibleInstances,FlexibleContexts,ScopedTypeVariables,OverlappingInstances #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module ShowAST (showDataTree) where

import GHC.Generics
import Language.C


ind i = take (4*i) $ repeat ' '

class ShowData f where
	showData :: Int -> f p -> String

instance ShowData V1 where
	showData _ _ = error "showData of empty type!"

instance ShowData U1 where
	showData i _ = ind i ++ "()" ++ "\n"

instance ShowData (K1 i NodeInfo) where
	showData i (K1 c) = ind i ++ show c ++ "\n"

instance ShowData (K1 i CTranslUnit) where
	showData i (K1 (CTranslUnit extdecls nodeinfo)) = ind i ++ "CTranslUnit " ++ show nodeinfo ++ "\n" ++
		concatMap (showData (i+1) . from) (filter isinthisfile extdecls) where
			isinthisfile = (== (posFile (posOf nodeinfo))) . posFile . posOf

instance (Generic c,ShowData (Rep c)) => ShowData (K1 i c) where
	showData i (K1 c) = showData i (from c) ++ "\n"

instance ShowData (K1 i Int) where
	showData i (K1 c) = ind i ++ show c ++ "\n"
instance ShowData (K1 i Char) where
	showData i (K1 c) = ind i ++ show c ++ "\n"
instance ShowData (K1 i String) where
	showData i (K1 c) = ind i ++ show c ++ "\n"
instance ShowData (K1 i Integer) where
	showData i (K1 c) = ind i ++ show c ++ "\n"

instance (ShowData f,Constructor c) => ShowData (M1 C c f) where
	showData i (M1 x) = ind i ++ conName (undefined :: M1 C c f p) ++ "\n" ++ showData (i+1) x

instance (ShowData f,Selector s) => ShowData (M1 S s f) where
	showData i (M1 x) = showData i x

instance (ShowData f,Datatype d) => ShowData (M1 D d f) where
	showData i (M1 x) = showData i x

instance (ShowData f1,ShowData f2) => ShowData (f1 :*: f2) where
	showData i (a :*: b) = showData (i+1) a ++ showData (i+1) b

instance (ShowData f1,ShowData f2) => ShowData (f1 :+: f2) where
	showData i (L1 x) = showData i x
	showData i (R1 x) = showData i x

showDataTree :: (Generic a,ShowData (Rep a)) => a -> String
showDataTree x = showData 0 (from x)
