{-# LANGUAGE RecordWildCards,LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Main where

import Prelude hiding (readFile)
import System.IO.Strict
import System.Environment
import System.Directory
import Text.Html hiding ((</>))
import Control.Monad
import System.FilePath
import Text.Printf
import Data.List


import CovDefs


coverage_dir = "coverage_report"
main = do
	cov_file:[] <- getArgs
	(_:cov) <- readCoverage cov_file

	removePathForcibly coverage_dir
	createDirectory coverage_dir

	indexlines <- forM (zip [1..] cov) $ \ (n,srcfile@SrcFile{..}) -> do
		putStrLn $ show n ++ " / " ++ show (length cov) ++ " : Creating " ++ sourceFilenameS ++ " .."
		srctext <- readFile sourceFilenameS
		let srcfilename = show n <.> "html"
		writeFile (coverage_dir </> srcfilename) $ renderHtml $ colourSrc srcfile srctext

		let (n_cov,n_stmts,cov_pct,_) = srcfile_covs srcfile
		return $ [ stringToHtml (show n), (anchor (stringToHtml sourceFilenameS)) ! [href srcfilename], stringToHtml (printf "%5.1f %%" cov_pct) ]

	writeFile (coverage_dir </> "index.html") $ renderHtml $ simpleTable [] [] indexlines

colourSrc SrcFile{..} srctext = (font ! [ face "courier" ]) $ pre $ concatHtml htmls 
	where
	htmls = colouring 1 "" (1,1) (sort countersS) srctext []
	colouring :: Int -> String -> (Int,Int) -> [Counter] -> String -> [Int] -> [Html]
	colouring ix buf pos cnts srctext (till:tills) | ix==till = stringToHtml buf : colouring ix "" pos cnts srctext tills
	colouring ix buf (l,c) (Counter{..}:cnts) srctext tills | l==lineC && c==columnC =
		(stringToHtml buf +++
		(font ! [ thestyle $ "background-color:" ++ if cntC==0 then "#ff0000" else "#00ff00" ]) sub) : rest
		where
		sub : rest = colouring ix "" (l,c) cnts srctext ((ix+lenC):tills)
	colouring ix buf (l,c) cnts ('\n':srctext) tills = colouring ix (buf++"\n") (l+1,1) cnts srctext tills
	colouring ix buf (l,c) cnts (ch:srctext) tills = colouring (ix+1) (buf++[ch]) (l,c+1) cnts srctext tills
	colouring _ _ _ [] "" [] = [ noHtml ]
	colouring ix buf pos cnts srctext tills = error $ "ix=" ++ show ix ++ "\nbuf=" ++ buf ++ "\npos=" ++ show pos ++
		"\ncnts=" ++ show cnts ++ "\nsrctext=" ++ take 10 srctext ++ "\ntills=" ++ show tills
{-
(if cntC==0 then "#ff0000" else "#00ff00")
data Counter = Counter { lineC :: Int, columnC :: Int, lenC :: Int, cntC :: Int } deriving (Eq,Ord,Show,Generic)
data SrcFile = SrcFile { sourceFilenameS :: String, outputFilenameS :: String, countersS :: [Counter] } deriving (Show,Generic)
-}