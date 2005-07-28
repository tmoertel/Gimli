module Parser (
    gimlParse
) where

import Data.Either
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P

import Expr

gimlParse =
    parse expr

expr :: Parser Expr
expr =
    buildExpressionParser opTable factor

factor = parens expr <|> literalExpr <?> "simple expression"

literalExpr =
        numberLiteralExpr
    <|> stringLiteralExpr
    <|> boolLiteralExpr
    <|> naLiteralExpr

numberLiteralExpr = do
    sign <- option id (try (char '-') >> return negate)
    naturalOrFloat >>= return . EVal . VNum . sign . either fromInteger id

stringLiteralExpr =
    stringLiteral >>= return . EVal . VStr

boolLiteralExpr =
    ( ( reservedWords "T TRUE" >> return True )
      <|>
      ( reservedWords "F FALSE" >> return False )
    ) >>= return . EVal . VBool

naLiteralExpr =
    reserved "NA" >> return (EVal VNa)

reservedWords = choice . map reserved . words

opTable =
    [ [ lop "*" BinOpTimes
      , lop "/" BinOpDiv
      ]
    , [ lop "+" BinOpAdd
      , lop "-" BinOpSub
      ]
    , [ lop "==" BinOpEq
      , lop "!=" BinOpNeq
      ]
    ]
  where
    lop s ctor    = op s (bexp ctor) AssocLeft
    rop s ctor    = op s (bexp ctor) AssocRight
    bexp ctor l r = EBinOp ctor l r
    op a f assoc  = (`Infix` assoc) $
        ((reservedOp a >> return f) <?> "operator")

gimlLexer :: P.TokenParser ()
gimlLexer =
    P.makeTokenParser
    ( javaStyle
      { commentStart    = ""
      , commentEnd      = ""
      , commentLine     = "#"
      , reservedNames   = ["T", "TRUE", "F", "FALSE", "NA"]
      , reservedOpNames = ["*","/","+","-","==","!="]
      }
    )

runLex :: Show a => Parser a -> String -> IO ()
runLex p input =
    (`parseTest` input) $ do
        whiteSpace
        x <- p
        eof
        return x

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

glex f         = f gimlLexer

