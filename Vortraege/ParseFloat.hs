{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module ParseResults where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Data.Char


test :: IO ()
test = parseTest parseRational2 "-34.35"

{-
parseRational = do
	mb_sign          <- optionMaybe (char '+' <|> char '-')
	digits           <- many digit
	mb_decimalplaces <- optionMaybe (char '.' >> many digit)
	return (mb_sign,digits,mb_decimalplaces)
-}

parseRational2 :: (Monad m) => ParsecT String () m Double
parseRational2 = do
	sign          <- option 1.0 ((char '+' >> return 1.0) <|> (char '-' >> return (-1.0)))
	digits        <- many digit
	decimalplaces <- option [] (char '.' >> many digit)
	parserReturn $ sign * (toNum 10 digits 0 + toNum 0.1 decimalplaces 0)
	where
	toNum _ [] result = result
	toNum factor (d:rest) result = toNum factor rest (result*factor + (fromIntegral $ ord d - ord '0'))
