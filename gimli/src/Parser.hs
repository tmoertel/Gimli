module Parser (
    gimlParse
) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P

import Expr

gimlParse =
    parse gimlParser

gimlParser :: Parser Expr
gimlParser =
    expr

expr :: Parser Expr
expr =
    buildExpressionParser opTable factor

factor = parens expr <|> literalExpr <?> "simple expression"

literalExpr =
    integerLiteralExpr <|> stringLiteralExpr <|> boolLiteralExpr

integerLiteralExpr =
    integer >>= return . EVal . VInt

stringLiteralExpr =
    stringLiteral >>= return . EVal . VStr

boolLiteralExpr =
    ( ( reservedWords "T TRUE" >> return True )
      <|>
      ( reservedWords "F FALSE" >> return False )
    ) >>= return . EVal . VBool

reservedWords = choice . map reserved . words

opTable =
    [ [ op "==" (cmpOp "==" (==)) l
      , op "!=" (cmpOp "!=" (/=)) l
      ]
    ]
  where
    l = AssocLeft
    r = AssocRight
    op a f assoc = (`Infix` assoc) $
        ((reservedOp a >> return f) <?> "operator")

cmpOp name op l r = EBinOp binop l r
  where
    binop    = BinOp name opfn
    opfn a b = VBool $ a `op` b

gimlLexer :: P.TokenParser ()
gimlLexer =
    P.makeTokenParser
    ( javaStyle
      { commentStart    = "/*"
      , commentEnd      = "*/"
      , commentLine     = "#"
      , reservedNames   = ["T", "TRUE", "F", "FALSE"]
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
