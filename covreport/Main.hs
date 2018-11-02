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

colourSrc :: SrcFile -> String -> Html 
colourSrc SrcFile{..} srctext = (font ! [ face "courier" ]) $ simpleTable [] [] [ [
	pre $ (font ! [ thestyle $ "background-color:#f0f0f0" ]) (stringToHtml $ concatMap (printf "%6i\n") [1..(length (lines srctext))]),
	pre $ colouring (1,1,1) "" (sort countersS) Nothing Nothing srctext ] ]
	where
	colouring :: (Int,Int,Int) -> String -> [Counter] -> (Maybe Int) -> (Maybe Int) -> String -> Html
	colouring _ buf _ Nothing Nothing "" = stringToHtml buf
	colouring (_,_,i) buf _ (Just stop_at) Nothing _ | i==stop_at = stringToHtml buf
	colouring (l,c,i) buf (Counter{..}:cnts) stop_at skip srctext | lineC<l || (lineC==l && columnC<c) =
		colouring (l,c,i) buf cnts stop_at skip srctext
	colouring (l,c,i) buf (cnt@Counter{..}:cnts) stop_at Nothing srctext | lineC==l, columnC==c =
		stringToHtml buf +++
			(font ! [ thestyle $ "background-color:" ++ if cntC==0 then "#ff0000" else "#00ff00", title (show cnt) ])
				(colouring (l,c,i) "" cnts (Just $ i+lenC) Nothing srctext) +++
			colouring (l,c,i) "" cnts stop_at (Just lenC) srctext
	colouring (l,c,i) _ cnts stop_at (Just 0) srctext         = colouring (l,c,i) "" cnts stop_at Nothing srctext
	colouring (l,c,i) _ cnts stop_at (Just j) ('\n':srctext)  = colouring (l+1,1,i) "" cnts stop_at (Just j) srctext
	colouring (l,c,i) _ cnts stop_at (Just j) (_:srctext)     = colouring (l,c+1,i+1) "" cnts stop_at (Just $ j-1) srctext
	colouring (l,c,i) buf cnts stop_at Nothing ('\n':srctext) = colouring (l+1,1,i) (buf++"\n") cnts stop_at Nothing srctext
	colouring (l,c,i) buf cnts stop_at Nothing (char:srctext) = colouring (l,c+1,i+1) (buf++[char]) cnts stop_at Nothing srctext
	colouring lci buf cnts stop_at skip srctext = error $
		printf "lci=%s\n buf=%s\n cnts=%s\n stop_at=%s\n skip=%s\n srctext=%s\n" (show lci) buf (show cnts) (show stop_at) (show skip) (take 10 srctext)
{-
(if cntC==0 then "#ff0000" else "#00ff00")
data Counter = Counter { lineC :: Int, columnC :: Int, lenC :: Int, cntC :: Int } deriving (Eq,Ord,Show,Generic)
data SrcFile = SrcFile { sourceFilenameS :: String, outputFilenameS :: String, countersS :: [Counter] } deriving (Show,Generic)
-}