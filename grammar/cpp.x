{
module Lexer(lex) where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$eol   = [\n]
$idchar = [$alpha $digit \_]

tokens :-

$eol                          ;
$white+                       ;
[$alpha \_] $idchar*

{
lex :: String -> [Token]
lex = alexScanTokens
}

