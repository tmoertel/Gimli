module Parser (
    gimlParse, gimlReadScalar, gimlParseTable
) where

import Control.Monad.Error
import Data.Either
import Data.List (transpose, filter)
import Text.ParserCombinators.Parsec
import ExprParser

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

gimlReadScalar =
    parse (whiteSpace >> scalarLiteralExpr) "string"

gimlParseTable :: MonadError e m => [[String]] -> m Table
gimlParseTable rows =
    return . mkTable . filter (not . null . fst) $ zip headings vectors
  where
    cols     = transpose rows
    headings = map head cols
    vectors  = map (mkVector . map parseTableElem . tail) cols
    parseTableElem s =
        either (const (SStr s)) id (gimlReadScalar s)
        
expr :: Parser Expr
expr =
        infixExpr
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
    <|> loadExpr
    <?> "simple expression"

tableExpr =
    reserved "table" >> parens (commaSep1 nvpair) >>= return . ETable

loadExpr =
    reserved "read.csv" >> parens stringLiteral >>= return . EReadCsv

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
    [ [ pfop  "-" UOpNegate
      ]
    , [ voplx ":" BinOpEllipses
      ]
    , [ sfop  "$" EProject pspecExpr
      , sfop  "[" ESelect (expr `followedBy` reservedOp "]")
      ]
    , [ vopl  "*" BinOpTimes
      , vopl  "/" BinOpDiv
      ]
    , [ vopl  "+" BinOpAdd
      , vopl  "-" BinOpSub
      ]
    , [ vopl  "==" BinOpEq
      , vopl  "!=" BinOpNeq
      , vopl  "<"  BinOpLt
      , vopl  "<=" BinOpLe
      , vopl  ">"  BinOpGt
      , vopl  ">=" BinOpGe
      ]
    , [ pfop  "!" UOpNot
      ]
    , [ eopr  "<-" EBind
      , eopl  "->" (flip EBind)
      ]
    ]
  where
    vopl s ctor   = op s (bexp ctor) AssocLeft
    voplx s ctor  = opx s (bexp ctor) AssocLeft
    vopr s ctor   = op s (bexp ctor) AssocRight
    eopl s ctor   = op s ctor AssocLeft
    eopr s ctor   = op s ctor AssocRight
    bexp ctor l r = EBinOp ctor l r
    pfop s ctor   = Prefix (reservedOp s >> return (EUOp ctor))
    sfop s ctor p = Postfix $ do
                        symbol s
                        x <- p
                        return $ \t -> ctor t x
    op a f assoc  = Infix (reservedOp a >> return f) assoc
    opx a f assoc = Infix (symbol a >> return f) assoc

-- projection specs

pspecExpr =
    pspecVector <|> pspecTable

pspecVector =
        (integer >>= return . PSVectorNum . fromInteger)
    <|> (identifier >>= return . PSVectorName)

pspecTable = parens $ do
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


-- helper parser combinators

p `followedBy` f = do
    v <- p
    f
    return v
