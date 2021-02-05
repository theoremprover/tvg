{-# LANGUAGE FlexibleContexts,ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Logging where

import Text.Html
import Data.List
import Data.Char

indentPrefix = "|   "

writeHTMLLog :: FilePath -> String -> IO ()
writeHTMLLog logFileHtml log =
	writeFile logFileHtml $ renderHtml $ pageframe $ makelist "Log" loghtml
	where
	([],loghtml) = log2html noHtml 0 $ lines log
	chkbox = primHtml "<input type=CHECKBOX id=cbtoggleexpand onclick=toggleexpand()>Expand all</input>"
	pageframe tree_html =
		header (style css) +++
		body ( chkbox +++ tree_html +++ tag "script" toggler +++ tag "script" expandall )

log2html :: Html -> Int -> [String] -> ([String],Html)
log2html cur_html _ [] = ([],cur_html)
log2html cur_html prev_indent (l:ls) | all isSpace l = log2html cur_html prev_indent ls
log2html cur_html prev_indent (l:ls)
	| current_indent == prev_indent = log2html (cur_html +++ makeli line) prev_indent ls
	| current_indent >  prev_indent = let (restlines,newhtml) = log2html noHtml (prev_indent+1) ls in
		log2html (cur_html +++ makelist line newhtml) prev_indent restlines
	| current_indent <  prev_indent = (l:ls,cur_html)
	where
	(current_indent,line) = parse_indented_line 0 l
	parse_indented_line :: Int -> String -> (Int,String)
	parse_indented_line indent l = case stripPrefix indentPrefix l of
		Nothing -> (indent,l)
		Just l' -> parse_indented_line (indent+1) l'

makelist headline content = (li ! [identifier "myUL"] $ (thespan ! [theclass "caret"]) (stringToHtml headline)) +++
	(ulist ! [theclass "nested"]) content

makeli s = li $ (thespan ! [theclass "leaf"]) (stringToHtml s)

{-
dataTreeToHtml (Leaf s) = li (stringToHtml s)
dataTreeToHtml (DataTree s subtrees) = (li ! [identifier "myUL"]) ((thespan ! [myclass]) (stringToHtml s) +++
	(ulist ! [theclass "nested"]) (concatHtml $ map dataTreeToHtml subtrees))
	where
	myclass = theclass $ if null subtrees then "leaf" else "caret"

genericToHTMLString :: (Generic a,DataTreeNode (Rep a)) => a -> String
genericToHTMLString x = dataTreeToHTMLString $ toDataTree x

dataTreeToHTMLString :: [DataTree] -> String
dataTreeToHTMLString datatrees = renderHtml $ pageframe $ map dataTreeToHtml datatrees
-}

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
\  color: #808080;\
\  display: inline-block;\
\  margin-right: 6px;\
\ }\
\ \
\ .caret-down::before {\
\  content: \"\\229F\";\
\  color: #808080;\
\ }\
\ \
\ .leaf {\
\  cursor: pointer;\
\  user-select: none;\
\ }\
\ \
\ .leaf::before {\
\  content: \"\\22A1\";\
\  visibility: hidden;\
\  color: #808080;\
\  display: inline-block;\
\  margin-right: 6px;\
\ }\
\ \
\ .leaf-down::before {\
\  content: \"\\22A1\";\
\  visibility: hidden;\
\  color: #808080;\
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
\ }"

expandall = primHtml "\
\ function toggleexpand() {\
\   var checkBox = document.getElementById(\"cbtoggleexpand\");\
\   if (checkBox.checked == true){\
\     document.querySelectorAll(\".nested\").forEach(function(elt) { elt.classList.add(\"active\"); } );\
\     document.querySelectorAll(\".caret\").forEach(function(elt) {elt.classList.add(\"caret-down\");} );\
\   } else {\
\     document.querySelectorAll(\".nested\").forEach(function(elt) { elt.classList.remove(\"active\"); } );\
\     document.querySelectorAll(\".caret\").forEach(function(elt) {elt.classList.remove(\"caret-down\");} );\
\   }\
\ }\
\ "