module Parser (
    gimlParse
) where

import Data.Either
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

import Expr
import Lexer

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
        try selectExpr
    <|> try projectExpr
    <|> infixExpr
    <?> "expression"

selectExpr = (<?> "selection") $ do
    target  <- infixExpr
    selects <- try $ many1 (brackets expr)
    return $ foldl1 ESelect (target:selects)

projectExpr = (<?> "projection") $ do
    target <- infixExpr
    reservedOp "$"
    pspecExpr >>= return . EProject target

infixExpr = 
    buildExpressionParser opTable factor

factor =
        nullExpr
    <|> parens expr
    <|> vectorExpr
    <|> varExpr
    <|> tableExpr
    <?> "simple expression"

tableExpr =
    reserved "table" >> parens (commaSep1 nvpair) >>= return . ETable

varExpr =
    identifier >>= return . EVar

nullExpr = do
    reserved "NULL" <|> try (brackets $ return ())
    return (EVal VNull)

vectorExpr = do
        (scalarLiteralExpr >>= return . v)
    <|> (bracketVectorExpr >>= return . v)
    <?> "vector"
  where
    v :: ToVector x => x -> Expr
    v = EVal . VVector . toVector

bracketVectorExpr =
        (brackets (commaSep1 scalarLiteralExpr))
    <|> (reserved "c" >> parens (commaSep1 scalarLiteralExpr))
    <?> "vector constructor"

scalarLiteralExpr =
        numberLiteralExpr
    <|> stringLiteralExpr
    <|> boolLiteralExpr
    <|> naLiteralExpr
    <?> "scalar literal"

numberLiteralExpr = lexNumber >>= return . SNum
stringLiteralExpr = lexString >>= return . SStr
boolLiteralExpr   = lexBool   >>= return . SLog
naLiteralExpr     = reserved "NA" >> return SNa


nvpair = do
    name <- identifier
    reservedOp "="
    val  <- expr
    return (name, val)

opTable =
    [ [ vopl ":" BinOpEllipses
      ]
    , [ vopl "*" BinOpTimes
      , vopl "/" BinOpDiv
      ]
    , [ vopl "+" BinOpAdd
      , vopl "-" BinOpSub
      ]
    , [ vopl "==" BinOpEq
      , vopl "!=" BinOpNeq
      ]
    , [ eopr "<-" EBind
      , eopl "->" (flip EBind)
      ]
    ]
  where
    vopl s ctor   = op s (bexp ctor) AssocLeft
    vopr s ctor   = op s (bexp ctor) AssocRight
    eopl s ctor   = op s ctor AssocLeft
    eopr s ctor   = op s ctor AssocRight
    bexp ctor l r = EBinOp ctor l r
    op a f assoc  = (reservedOp a >> return f) `Infix` assoc

-- projection specs

pspecExpr =
    pspecVector <|> parens pspecTable

pspecVector =
        (integer >>= return . PSVectorNum . fromInteger)
    <|> (identifier >>= return . PSVectorName)

pspecTable = do
    negated <- option False (char '-' >> return True)
    commaSep1 pspecElem >>= return . PSTable negated

pspecElem = 
        (integer >>= return . PSCNum . fromInteger)
    <|> pspecNameEqualsExpr
    <|> (reservedOp "*" >> return PSCStar)

pspecNameEqualsExpr = do
    name <- identifier
    option (PSCName name) $ do
        reservedOp "="
        expr >>= return . PSCExp name
