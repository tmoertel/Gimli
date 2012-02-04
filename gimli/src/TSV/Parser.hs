-- CSV/Parser.hs Parse CSV (comma separated value) files.
-- Copyright (c) Thomas Moertel 2004.
-- $Id: Parser.hs,v 1.2 2004/10/05 15:41:05 thor Exp $

module TSV.Parser (tsvFile, tsvLine) where

import Text.ParserCombinators.Parsec


tsvFile :: CharParser st [[String]]
tsvFile   =  tsvLine `endBy` newline
tsvLine   =  cell `sepEndBy` tabChar
cell      =  quoted <|> (many $ noneOf "\t\n")
tabChar   =  char '\t'
quoted    =  do
             left <- char '"' >> manyTill anyChar (char '"')
             option left (quoted >>= return . (left ++) . ("\"" ++))
