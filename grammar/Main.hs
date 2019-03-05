{-# OPTIONS_GHC -fno-warn-tabs #-}

module Main where

import Grammar
import ShowAST

{-
stack build :parsecpp-exe
stack repl :parsecpp-exe
stack exec parsecpp-exe
-}

main = do
	let filename = "test.cpp"
	f <- readFile filename
	case parseTranslUnit filename f of
		Left err -> print err
		Right translunit -> do
			writeFile (filename++".ast") $ showDataTree translunit
