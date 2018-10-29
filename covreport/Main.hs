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
		writeFile (coverage_dir </> srcfilename) $ renderHtml $ annotateSrc srcfile (lines srctext)

		let (n_cov,n_stmts,cov_pct,_) = srcfile_covs srcfile
		return $ [ stringToHtml (show n), (anchor (stringToHtml sourceFilenameS)) ! [href srcfilename], stringToHtml (printf "%5.1f %%" cov_pct) ]

	writeFile (coverage_dir </> "index.html") $ renderHtml $ simpleTable [] [] indexlines

annotateSrc SrcFile{..} srclines = (font ! [ face "courier" ]) (simpleTable [] [bgcolor "#f0f0f0"] (map annotate_line (zip [1..] srclines)))
	where
	replace_tabs txt = map (\case '\t' -> ' '; c -> c) txt
	annotate_line (line_no,line) = [ stringToHtml (show line_no),
		concatHtml $ set_bgcol (replace_tabs line) 1 (sort [ cnt | cnt@Counter{..} <- countersS, lineC==line_no ]) ]
	set_bgcol (' ':line) cursor rs = spaceHtml : set_bgcol line (cursor+1) rs
	set_bgcol line _ [] = [stringToHtml line]
	set_bgcol line cursor (cnt@Counter{..}:rs) | cursor==columnC = (cnt_font cnt) (stringToHtml (take lenC line)) : set_bgcol (drop lenC line) (cursor+lenC) rs
	set_bgcol line cursor rs@(Counter{..}:_) = stringToHtml (take (columnC-cursor) line) : set_bgcol (drop (columnC-cursor) line) columnC rs
	cnt_font cnt@Counter{..} = font ! [ thestyle $ "background-color:" ++ (if cntC==0 then "#ff0000" else "#00ff00"),
		title $ show cnt]
