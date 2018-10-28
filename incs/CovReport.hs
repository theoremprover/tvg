{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module CovReport where

import Prelude hiding (readFile)
import System.IO.Strict
import System.Environment
import System.Directory
import Text.Html

import CovDefs


--data SrcFile = SrcFile { sourceFilenameS :: String, outputFilenameS :: String, countersS :: [Counter] } deriving (Show,Generic)
--data Counter = Counter { lineC :: Int, columnC :: Int, cntC :: Int } deriving (Show,Generic)

coverage_dir = "coverage_report"
main = do
	cov_file:[] <- getArgs
	cov <- readCoverage cov_file

	removePathForcibly coverage_dir
	createDirectory coverage_dir

	indexlines <- forM (zip [1..] cov) $ \ (n,srcfile@SrcFile{..}) -> do
		putStrLn $ show n ++ " / " ++ show (length cov) ++ " : Creating " ++ sourceFilenameS ++ " .."
		srctext <- readFile sourceFilenameS
		let srcfilename = coverage_dir </> show n <.> "html"
		writeFile srcfilename $ renderHtml $ annotateSrc srcfile (lines srctext)

		let (n_cov,n_stmts,cov_pct,_) = srcfile_covs srcfile
		return $ [ stringToHtml (show n), (anchor (stringToHtml sourceFilenameS)) ! [href srcfilename], stringToHtml (printf "%5.1f %%" cov_pct) ]

	writeFile (coverage_dir </> "index.html") $ renderHtml $ simpleTable [] [] indexlines

{-
<font face="courier">
<table>
<tr bgcolor="#f0f0f0"><td>34</td><td>Test<font style="background-color:#ff0000"> Te</font>st</td></tr>
<tr bgcolor="#f0f0f0"><td>35</td><td>Test<font style="background-color:#ffff00"> Tes</font>st</td></tr>
<tr bgcolor="#f0f0f0"><td>36</td><td>Test<font style="background-color:#00ff00"> Testfvf</font>st</td></tr>
</table>
</font>
-}

annotateSrc SrcFile{..} srclines = font ! [ face "courier" ] (simpleTable [] [bgcolor "#f0f0f0"] (map annotate_line (zip [1..] srclines)))
	where
	annotate_line (line_no,line) = stringToHtml (show line_no) :
		set_bgcol line 1 (sort [ (columnC,lenC,cntC) | Counter{..} <- countersS, lineC==line_no ])
	set_bgcol line _ [] = [stringToHtml line]
	set_bgcol line cursor ((col,len,cnt):rs) | cursor==col = (cnt_font cnt) (stringToHtml (take len line)) : set_bgcol (drop len line) (cursor+len) rs
	set_bgcol line cursor rs@((col,_,_):_) = stringToHtml (take (col-cursor) line) : set_bgcol (drop (col-cursor) line) col rs
	cnt_font cnt = font ! [ thestyle $ "bgcolor=\"" ++ (if cnt==0 then "#ff0000" else "#00ff00") ++ "\"" ]
