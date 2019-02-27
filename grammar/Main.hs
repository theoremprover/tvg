{-# OPTIONS_GHC -fno-warn-tabs #-}

module Main where

import Grammar
import ShowAST

{-
stack build :parsecpp-exe
-}

main = do
	let filename = "test.cpp"
	f <- readFile filename
	case parseTranslUnit filename f of
		Right err -> print err
		Left translunit -> do
			writeFile (filename++".ast") $ showDataTree translunit
