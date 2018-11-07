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

import Debug.Trace

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
		let
			srcfilename = show n <.> "html"
			(n_cov,n_stmts,cov_pct,_) = srcfile_covs srcfile
			cov_html = stringToHtml (printf "%5.1f %%" cov_pct)
			n_of_m = stringToHtml (printf "%6i of %6i statements covered" n_cov n_stmts)

		writeFile (coverage_dir </> srcfilename) $ renderHtml $
			h3 (stringToHtml sourceFilenameS) +++
			h4 (n_of_m +++ stringToHtml " -> " +++ cov_html) +++
			colourSrc srcfile srctext

		return $ [ stringToHtml (show n), (anchor (stringToHtml sourceFilenameS)) ! [href srcfilename], n_of_m , cov_html ]

	writeFile (coverage_dir </> "index.html") $ renderHtml $ simpleTable [] [] indexlines

data SrcPart = PlainText String | Coloured String Counter [((Int,Int),SrcPart)]

colourSrc :: SrcFile -> String -> Html 
colourSrc SrcFile{..} srctext = (font ! [ face "courier" ]) $ simpleTable [] [] [ [
	pre $ (font ! [ thestyle $ "background-color:#f0f0f0" ]) (stringToHtml $ concatMap (printf "%6i\n") [1..(length (lines srctext))]),
	pre $ colouring (sort $ drop 1 countersS) "" (srcchars (1,1,1) srctext) ] ]
	where

	srcchars _ "" = []
	srcchars (l,c,i) ('\n':srctext) = (l,c,i-1,'\n') : srcchars (l+1,1,i) srctext
	srcchars (l,c,i) (char:srctext) = (l,c,i,char) : srcchars (l,c+1,i+1) srctext

	colouring :: [Counter] -> String -> [(Int,Int,Int,Char)] -> Html
	colouring _ buf [] = trace (printf "3: stringToHtml %s..." (take 20 $ reverse buf)) $ stringToHtml $ reverse buf
	colouring ((cnt@Counter{..}):cnts) buf rest@((l,c,i,_):_) | lineC==l, columnC==c = trace (printf "1: cnt=%s, buf=%s, (l,c,i)=(%i,%i,%i)" (show cnt) (show $ reverse buf) l c i) $ 
		stringToHtml (reverse buf) +++
		(font ! [ thestyle $ "background-color:" ++ col, title (show cnt) ])
			(trace (printf "5: sub=%s..." (show $ take 3 sub)) $ colouring cnts "" sub) +++
		trace (printf "4: after_cnts=%s..., after=%s" (show $ take 1 after_cnts) (show $ take 1 after)) (colouring after_cnts "" after)
		where
		col = if cntC==0 then "#ff0000" else "#00ff00"
		(sub,after) = break (\(_,_,j,_) -> j >= i+lenC) rest
		after_cnts = case after of
			[] -> []
			((afterl,afterc,_,_):_) -> dropWhile (\ Counter{..} -> (lineC,columnC) < (afterl,afterc) ) cnts
	colouring ((cnt@Counter{..}):_) _ ((l,c,i,_):_) | (lineC,columnC)<(l,c) = error $ printf "Missed Counter %c" (show cnt)
	colouring cnt buf (a@(_,_,_,char):rest) = trace (printf "2: a=%s, cnt=%s, buf=%s" (show a) (show $ take 1 cnt) (show $ reverse buf)) $
		colouring cnt (char:buf) rest
