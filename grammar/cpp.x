{
module Lexer(lex) where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$eol   = [\n]

tokens :-

$eol                          ;
$white+                       ;


{
lex :: String -> [Token]
lex = alexScanTokens
}

