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
	cov <- readCoverage cov_file

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

data SrcPart = PlainText Int Int String | Colouring String SrcPart

colourSrc SrcFile{..} srctext = (font ! [ face "courier" ]) $ pre $ concatHtml $ map to_html $
	search_srcpart (sort countersS) $ map PlainText $ zip3 [1..] (repeat 1) (map (++"\n") $ lines srctext)

	to_html (_,_,txt) = toHtml txt
	to_html NewLine = toHtml "\n"
	to_html (Colouring colour srcparts) = (font ! [ thestyle $ "background-color:" ++ colour ]) $ concatHtml $ map to_html srcparts

	search_srcpart Counter{..} ((PlainText l c txt):srcparts) | l == lineC && c <= columnC = let
		(middles,last_part) = find_last (lineC,columnC) srcparts
	
	&& c + length txt > columnC =
		PlainText l c (take (columnC-c) txt) : Colouring (take lenC $ drop (columnC-c) txt) (if cntC==0 then "#ff0000" else "#00ff00") : PlainText l (columnC+lenC) (drop (columnC+lenC) txt) : srcparts
	mb_insert_colouring
	
{-
data Counter = Counter { lineC :: Int, columnC :: Int, lenC :: Int, cntC :: Int } deriving (Eq,Ord,Show,Generic)
data SrcFile = SrcFile { sourceFilenameS :: String, outputFilenameS :: String, countersS :: [Counter] } deriving (Show,Generic)
-}