module Lexer where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P

gimlLexer :: P.TokenParser ()
gimlLexer =
    P.makeTokenParser
    ( haskellStyle
      { commentStart    = ""
      , commentEnd      = ""
      , commentLine     = "#"
      , reservedNames   = ["T", "TRUE", "F", "FALSE", "NA", "NULL", "c"]
      , reservedOpNames = ["*","/","+","-","==","!=","<-","->",":"]
      }
    )

runLex :: Show a => Parser a -> String -> IO ()
runLex p input =
    (`parseTest` input) $ do
        whiteSpace
        x <- p
        eof
        return x

lexNumber = do
    sign <- option id (try (char '-') >> return negate)
    naturalOrFloat >>= return . sign . either fromInteger id

lexString =
    stringLiteral >>= return

lexBool =
    ( ( reservedWords "T TRUE" >> return True )
      <|>
      ( reservedWords "F FALSE" >> return False )
    )

reservedWords = choice . map reserved . words


whiteSpace     = glex P.whiteSpace
lexeme         = glex P.lexeme
symbol         = glex P.symbol
natural        = glex P.natural
integer        = glex P.integer
float          = glex P.float
naturalOrFloat = glex P.naturalOrFloat
parens         = glex P.parens
semi           = glex P.semi
identifier     = glex P.identifier
reserved       = glex P.reserved
reservedOp     = glex P.reservedOp
stringLiteral  = glex P.stringLiteral
commaSep1      = glex P.commaSep1
brackets       = glex P.brackets
squares        = glex P.squares
semiSep        = glex P.semiSep
semiSep1       = glex P.semiSep1

glex f         = f gimlLexer