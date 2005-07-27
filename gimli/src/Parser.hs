module Parser (
    gimlParse
) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P

import Expr

gimlParse =
    parse gimlParser

gimlParser :: Parser Expr
gimlParser =
    literal

literal =
    integerExpr <|> stringExpr

integerExpr =
    integer >>= return . EVal . VInt

stringExpr =
    stringLiteral >>= return . EVal . VStr


gimlLexer :: P.TokenParser ()
gimlLexer =
    P.makeTokenParser
    ( javaStyle
      { commentStart    = "/*"
      , commentEnd      = "*/"
      , commentLine     = "#"
      , reservedNames   = []
      , reservedOpNames = ["*","/","+","-"]
      }
    )

runLex :: Show a => Parser a -> String -> IO ()
runLex p input =
    (`parseTest` input) $ do
        whiteSpace
        x <- p
        eof
        return x

whiteSpace = P.whiteSpace gimlLexer
lexeme     = P.lexeme gimlLexer
symbol     = P.symbol gimlLexer
natural    = P.natural gimlLexer
integer    = P.integer gimlLexer
parens     = P.parens gimlLexer
semi       = P.semi gimlLexer
identifier = P.identifier gimlLexer
reserved   = P.reserved gimlLexer
reservedOp = P.reservedOp gimlLexer
stringLiteral = P.stringLiteral gimlLexer
