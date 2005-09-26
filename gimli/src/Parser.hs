{-# OPTIONS -fglasgow-exts #-}

module Parser (
    gimlParse, gimlReadScalar, gimlParseTable
) where

import Control.Monad.Error
import Data.Either
import Data.List (transpose, filter, group)
import Text.ParserCombinators.Parsec

import ExprParser
import Expr
import Lexer
import Primitives

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

gimlParseTable :: MonadError String m => [[String]] -> m Table
gimlParseTable rows = do
    unless (length (group (map length cols)) == 1) $
        throwError "table cannot be parsed: it is non-rectangular"
    return . mkTable . filter (not . null . fst) $ zip headings vectors
  where
    cols     = transpose (map (take $ length headings) rows)
    headings = head rows
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

infixExpr =
    buildExpressionParser opTable factor

factor =
        nullExpr
    <|> parens expr
    <|> vectorExpr
    <|> varExpr
    <|> tableExpr
    <|> ifThenElseExpr
    <|> forExpr
    <?> "simple expression"

tableExpr =
    reserved "table" >> parens (commaSep1 nvpair) >>= return . ETable

commaPair =
    sepPair comma

sepPair psep p1 p2 = do
    e1 <- p1
    psep
    e2 <- p2
    return (e1, e2)

varExpr = do
    s <- identifier
    return (primOrVar s)

primOrVar s =
    if isPrimitive s then EVal (VPrim $ Prim s []) else EVar s

nullExpr = do
    reserved "NULL" <|> try (brackets $ return ())
    return (EVal VNull)

vectorExpr = do
        (scalarLiteralExpr >>= return . EVal . VVector . toVector)
    <|> (bracketVectorExpr >>= return . EVector)
    <?> "vector"

bracketVectorExpr =
        (brackets (commaSep1 expr))
    <|> (reserved "c" >> parens (commaSep1 expr))
    <?> "vector constructor"

ifThenElseExpr = do
    kind <-  (reserved "if" >> return EIf)
         <|> (reserved "unless" >> return EUnless)
    test <- expr
    reserved "then"
    trueExpr <- expr
    maybeFalseExpr <- option Nothing $ do
        reserved "else"
        expr >>= return . Just
    return (kind test trueExpr maybeFalseExpr)

forExpr = do
    var <- forVarInFrag
    collection <- expr
    prog <- blockExpr
    return $ EFor var collection prog

forVarInFrag = do
    reserved "for"
    var <- identifier
    symbol "in"
    return var

blockExpr = do
    between (reserved "do") (reserved "end") (liftM EBlock $ many expr)

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
    [ [ sfop "("  EApp (commaSep expr `followedBy` symbol ")")
      ]
    , [ vopr  "^" BinOpPower
      ]
    , [ pfop  "-" UOpNegate
      ]
    , [ voplx ":" BinOpEllipses  -- use voplx so that -3:-1 parses
      ]
    , [ sfop  "$" (\t x -> x t) pspecExpr
      , sfop  "[" ESelect (expr `followedBy` reservedOp "]")
      ]
    , [ jopr  "-=-" $ \l r -> EJoin (JNatural JInner l r JInner)
      , jopr  "---" $ \l r -> EJoin (JNatural JInner l r JInner) -- shortcut
      , jopr  "-==" $ \l r -> EJoin (JNatural JInner l r JOuter)
      , jopr  "==-" $ \l r -> EJoin (JNatural JOuter l r JInner)
      , jopr  "===" $ \l r -> EJoin (JNatural JOuter l r JOuter)
      , eopr  "***" $ EJoin JCartesian
      ]
    , [ infixlFn
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
    , [ vopr  "|" BinOpVOr
      , vopr  "&" BinOpVAnd
      , vopr  "||" BinOpSOr
      , vopr  "&&" BinOpSAnd
      ]
    , [ eopr  "<-" EBind
      , eopl  "->" (flip EBind)
      ]
    , [ eoplx "if"     (\l r -> EIf r l Nothing)
      , eoplx "unless" (\l r -> EUnless r l Nothing)
      , infixFor
      ]
    ]
  where
    vopl s ctor   = op s (bexp ctor) AssocLeft
    voplx s ctor  = opx s (bexp ctor) AssocLeft
    vopr s ctor   = op s (bexp ctor) AssocRight
    eopl s ctor   = op s ctor AssocLeft
    eoplx s ctor  = opx s ctor AssocLeft
    eopr s ctor   = op s ctor AssocRight
    eoprx s ctor  = opx s ctor AssocRight
    bexp ctor l r = EBinOp ctor l r
    pfop s ctor   = Prefix (reservedOp s >> return (EUOp ctor))
    sfop s ctor p = Postfix $ do
                        symbol s
                        x <- p
                        return $ \t -> ctor t x
    infixlFn      = Infix ifn AssocLeft
      where
        ifn       = between pct pct identifier >>= return . apf
        apf s l r = EApp (primOrVar s) [l,r]
        pct       = symbol "%"
    op a f assoc  = Infix (reservedOp a >> return f) assoc
    opx a f assoc = Infix (symbol a >> return f) assoc
    jopr s ctor   = Infix (try joinExpr) AssocRight
      where
        joinExpr  = do
                    l <- bexprs
                    reservedOp s
                    r <- bexprs
                    return (ctor l r)
        bexprs    = option [] (braces (commaSep expr))
    infixFor      = (`Infix` AssocLeft) $ do
                        var <- forVarInFrag
                        return $ flip (EFor var)
-- projection specs

pspecExpr =
        (pspecVector >>= return . flip EProject)
    <|> pspecTableSeries

pspecVector =
        (integer >>= return . PSVectorNum . fromInteger)
    <|> (identifier >>= return . PSVectorName)

pspecTableSeries = parens $ do
    semiSep1 pspecTable >>= return . foldr (flip (.)) id

pspecTable =
      pspecTableAdditiveOverlay <|> pspecTableStraight

pspecTableAdditiveOverlay = do
    symbol "+"
    commaSep1 nvpair >>= return . flip EProject . PSTableOverlay

pspecTableStraight = do
    negated <- option False (symbol "-" >> return True)
    commaSep1 pspecElem >>= return . flip EProject . PSTable negated

pspecElem =
        try (do i <- integer
                notFollowedBy (noneOf ",;)")
                return . PSCNum $ fromInteger i)
    <|> try pspecNameEqualsExpr
    <|> try (do s <- identifier
                notFollowedBy (noneOf ",;)")
                return $ PSCName s)
    <|> (reservedOp "*" >> return PSCStar)
    <|> (expr >>= return . PSCExpr)

pspecNameEqualsExpr =
    liftM (uncurry PSCNExpr) nvpair


-- helper parser combinators

p `followedBy` f = do
    v <- p
    f
    return v
