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
    parse $ do
        whiteSpace
        es <- semiSep1 expr
        eof
        return $ case es of
            [e] -> e
            _   -> ESeries es

expr :: Parser Expr
expr = 
    buildExpressionParser opTable factor

factor =
        nullExpr
    <|> parens expr
    <|> ( vectorExpr <?> "vector" )
    <|> varExpr

varExpr =
    identifier >>= return . EVar

nullExpr = do
    reserved "NULL" <|> try (brackets $ return ())
    return (EVal VNull)

vectorExpr = do
    (scalarLiteralExpr >>= return . v)
    <|>
    (bracketVectorExpr >>= return . v)

scalarLiteralExpr =
        numberLiteralExpr
    <|> stringLiteralExpr
    <|> boolLiteralExpr
    <|> naLiteralExpr

bracketVectorExpr =
    (brackets (commaSep1 scalarLiteralExpr))
    <|>
    (reserved "c" >> parens (commaSep1 scalarLiteralExpr))

v :: ToVector x => x -> Expr
v = EVal . VVector . toVector

numberLiteralExpr = do
    sign <- option id (try (char '-') >> return negate)
    naturalOrFloat >>= return . SNum . sign . either fromInteger id

stringLiteralExpr =
    stringLiteral >>= return . SStr

boolLiteralExpr =
    ( ( reservedWords "T TRUE" >> return True )
      <|>
      ( reservedWords "F FALSE" >> return False )
    ) >>= return . SLog

naLiteralExpr =
    reserved "NA" >> return SNa

reservedWords = choice . map reserved . words

opTable =
    [ [ vlop "*" BinOpTimes
      , vlop "/" BinOpDiv
      ]
    , [ vlop "+" BinOpAdd
      , vlop "-" BinOpSub
      ]
    , [ vlop "==" BinOpEq
      , vlop "!=" BinOpNeq
      ]
    , [ erop "<-" EBind
      , elop "->" (flip EBind)
      ]
    ]
  where
    vlop s ctor   = op s (bexp ctor) AssocLeft
    vrop s ctor   = op s (bexp ctor) AssocRight
    elop s ctor   = op s ctor AssocLeft
    erop s ctor   = op s ctor AssocRight
    bexp ctor l r = EBinOp ctor l r
    op a f assoc  = (`Infix` assoc) $
        ((reservedOp a >> return f) <?> "operator")

gimlLexer :: P.TokenParser ()
gimlLexer =
    P.makeTokenParser
    ( haskellDef
      { commentStart    = ""
      , commentEnd      = ""
      , commentLine     = "#"
      , reservedNames   = ["T", "TRUE", "F", "FALSE", "NA", "NULL", "c"]
      , reservedOpNames = ["*","/","+","-","==","!=","<-","->"]
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
commaSep1      = glex P.commaSep1
brackets       = glex P.brackets
squares        = glex P.squares
semiSep        = glex P.semiSep
semiSep1       = glex P.semiSep1

glex f         = f gimlLexer

