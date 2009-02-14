-- CSV/Parser.hs Parse CSV (comma separated value) files.
-- Copyright (c) Thomas Moertel 2004.
-- $Id: Parser.hs,v 1.2 2004/10/05 15:41:05 thor Exp $

module CSV.Parser (csvFile, csvLine) where

import Text.ParserCombinators.Parsec

csvFile   =  csvLine `endBy` newline
csvLine   =  cell `sepBy` char ','
cell      =  between hwhite hwhite (quoted <|> (many $ noneOf ",\n"))
hwhite    =  many (char ' ')
quoted    =  do
             left <- char '"' >> manyTill anyChar (char '"')
             option left (quoted >>= return . (left ++) . ("\"" ++))
