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

import Debug.Trace

gimlParse =
    parse $ do
        whiteSpace
        e <- sourceExprParser exprs $ \es start end -> case es of
             [e] -> e
             es  -> Expr (EBlock es) start end
        whiteSpace
        eof
        return e

gimlReadScalar =
    (`parse` "string") $ do
        whiteSpace
        sc <- scalarLiteralExpr
        eof
        return sc

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

exprs :: Parser [Expr]
exprs = semiSep expr

expr :: Parser Expr
expr = 
    infixExpr <?> "expression"

sourceExprParser p ctor = do
    spos <- getPosition
    e    <- p
    epos <- getPosition
    return $ ctor e (toEpos spos) (toEpos epos)
  where
    toEpos pos = ExprPos
        { eposName = sourceName pos
        , eposLine = sourceLine pos
        , eposCol  = sourceColumn pos
        }

sEP p =
    sourceExprParser p $ \a b c -> (a, b, c)

sEPe p =
    sourceExprParser p Expr

infixExpr :: Parser Expr
infixExpr =
    buildExpressionParser opTable (sEPe factor)

factor =
        nullExpr
    <|> parensExpr
    <|> tableExpr
    <|> listExpr
    <|> functionExpr
    <|> vectorExpr
    <|> varExpr
    <|> ifThenElseExpr
    <|> forExpr
    <|> localExpr
    <?> "simple expression"

parensExpr =
    liftM EParens (parens expr)

tableExpr =
    reserved "table" >> parens (commaSep1 tspec) >>= return . ETable

tspec = do
    (liftM TCol anypair) <|> (liftM TSplice expr)

listExpr =
    reserved "list" >> parens (commaSep givenArg) >>= return . EList

localExpr =
    reserved "local" >> liftM ELocal blockOrExpr

functionExpr = do
    reserved "function" <|> reserved "func"
    args <- parens formalArgs
    body <- blockOrExpr
    return $ EFunc args body

formalArgs = do
    liftM mkArgList . commaSep $ do
        name <- identifier
        defaultExpr <- option Nothing $ do
            reservedOp "="
            liftM Just expr
        return $ Arg name defaultExpr

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
    if isPrimitive s then EVal (VPrim $ Prim s emptyArgList) else EVar s

nullExpr = do
    reserved "NULL" <|> try (brackets $ return ())
    return (EVal VNull)

vectorExpr = do
        (scalarLiteralExpr >>= return . EVal . VVector . toVector)
    <|> (bracketVectorExpr >>= return . EVector)
    <?> "vector"

bracketVectorExpr =
        (brackets (commaSep1 expr))
    <|> try (symbol "c" >> parens (commaSep1 expr))
    <?> "vector constructor"

ifThenElseExpr = do
    kind <-  (reserved "if" >> return EIf)
         <|> (reserved "unless" >> return EUnless)
    test <- expr
    reserved "then"
    trueExpr <- blockOrExpr
    maybeFalseExpr <- option Nothing $ do
        reserved "else"
        blockOrExpr >>= return . Just
    return (kind test trueExpr maybeFalseExpr)

forExpr = do
    reserved "for"
    var <- identifier
    symbol "in"
    collection <- expr
    prog <- blockExpr
    return $ EFor var collection prog

blockOrExpr =
        blockExpr
    <|> expr

blockExpr =
    (`sourceExprParser` Expr) $ do
        between (reserved "do") (reserved "end") blockContents
    <|> between (symbol "{") (symbol "}") blockContents
  where
    blockContents = liftM EBlock exprs

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
    return $ NVP name val

envpair = do
    name <- expr
    reservedOp "="
    val  <- expr
    return $ ENVP name val

anypair = do
    try nvpair <|> try envpair

givenArg = (<?> "function argument") $ do
    (try nvpair >>= \(NVP n e) -> return (GivenArg (Just n) e))
    <|>
    (liftM (GivenArg Nothing) expr)

opTable =
    [ [ sfop "("  EApp (commaSep givenArg `followedBy` symbol ")")
      ]
    , [ vopr  "^" BinOpPower
      ]
    , [ pfop  "-" UOpNegate
      ]
    , [ voplx ":" BinOpEllipses  -- use voplx so that -3:-1 parses
      ]
    , [ sfopx  "$" (\t x -> x t) pspecExpr
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
    , [ vopr  "++" BinOpConcat
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
      , eopr  "<<-" EBindOver
      , eopl  "->>" (flip EBindOver)
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
    pfop s ctor   = Prefix (reservedOp s >> eret1 (EUOp ctor))
    sfop s ctor p = Postfix $ do
                        symbol s
                        x <- p
                        eret1 $ \t -> ctor t x
    sfopx s ctor p = Postfix $ do
                        symbol s
                        x <- p
                        return $ \t -> ctor t x
    infixlFn      = Infix ifn AssocLeft
      where
        ifn       = do
                    (s, st, end) <- sEP (between pct pct identifier)
                    eret (apf $ Expr (primOrVar s) st end)
        apf o l r = EApp o $ map (GivenArg Nothing) [l,r]
        pct       = symbol "%"
    op a f assoc  = Infix (reservedOp a >> eret f) assoc
    opx a f assoc = Infix (symbol a >> eret f) assoc
    jopr s ctor   = Infix (try joinExpr) AssocRight
      where
        joinExpr  = do
                    l <- bexprs
                    reservedOp s
                    r <- bexprs
                    eret (ctor l r)
        bexprs    = option [] (braces (commaSep expr))
    infixFor      = (`Infix` AssocLeft) $ do
                        reserved "for"
                        var <- identifier
                        symbol "in"
                        eret $ flip (EFor var)
    eret f        = return (ef f)
    eret1 f       = return (ef1 f)
    ef f l r      = Expr (f l r) (exprStart l) (exprEnd r)
    ef1 f e       = Expr (f e) (exprStart e) (exprEnd e)


-- projection specs

pspecExpr :: Parser (Expr -> Expr)
pspecExpr =
        pspecTableSimple
    <|> pspecTableSeries

pspecTableSimple = do
    (f,l,r) <- sEP pspecVector
    return $ \te -> Expr (EProject te f) l r

pspecVector =
        (integer >>= return . PSVectorNum . fromInteger)
    <|> (identifier >>= return . PSVectorName)

pspecTableSeries = parens $ do
    psts <- semiSep1 (sEP pspecTable)
    return $ \te -> foldl (\ e (pst,l,r) -> Expr (pst e) l r) te psts

pspecTable =
      pspecTableAdditiveOverlay <|> pspecTableStraight

pspecTableAdditiveOverlay = do
    symbol "+"
    commaSep1 anypair >>= return . flip EProject . PSTableOverlay

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
    liftM PSCNExpr anypair


-- helper parser combinators

p `followedBy` f = do
    v <- p
    f
    return v
