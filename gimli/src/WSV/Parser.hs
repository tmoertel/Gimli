-- CSV/Parser.hs Parse CSV (comma separated value) files.
-- Copyright (c) Thomas Moertel 2004.
-- $Id: Parser.hs,v 1.2 2004/10/05 15:41:05 thor Exp $

module WSV.Parser (wsvFile, wsvLine) where

import Text.ParserCombinators.Parsec

wsvFile   =  wsvLine `endBy` newline
wsvLine   =  do
             first <- quoted <|> (many $ noneOf " \t\n")
             hwhite
             rest  <- cell `sepEndBy` hwhite
             return (first:rest)
cell      =  quoted <|> (many1 $ noneOf " \t\n")
hwhite    =  many (oneOf " \t")
quoted    =  do
             left <- char '"' >> manyTill anyChar (char '"')
             option left (quoted >>= return . (left ++) . ("\"" ++))
