{-# OPTIONS -fglasgow-exts #-}

module Expr (

    Expr(..),
    BinOp(..), UnaryOp(..),
    PSpec(..), PSCol(..),
    JoinOp(..), JoinInclusion(..),

    Value(..),
        vIsVector, vIsTable,
        toScalar,
        asVector, asNum, asTable, asString, asBool,
        mkVectorValue,

    module CoreTypes,
    module Scalar,
    module Vector,
    module Table,
)
where

import CoreTypes
import Scalar
import Vector
import Table
import PPrint

-- ============================================================================
-- core expressions
-- ============================================================================

data Expr
    = EVal Value
    | EBinOp !BinOp Expr Expr
    | EVector [Expr]
    | EUOp !UnaryOp Expr
    | ESeries [Expr]
    | EVar Identifier
    | EBind Expr Expr
    | ESelect Expr Expr
    | EProject Expr PSpec
    | ETable [(Identifier, Expr)]
    | EReadCsv Expr
    | EReadWsv Expr
    | EWriteWsv Expr Expr
    | EJoin JoinOp Expr Expr
    | EIf Expr Expr Expr
    deriving (Eq, Ord)

instance Show Expr where

    showsPrec p (EVal v)                   = showsPrec p v
    showsPrec _ (EReadCsv f)               = sFn "read.csv" f
    showsPrec _ (EReadWsv f)               = sFn "read.wsv" f
    showsPrec _ (EWriteWsv e f)            = sFn2 "write.wsv" e f
    showsPrec _ (EVector es)               = ss "[" . commajoin es . ss "]"
    showsPrec _ (EVar s)                   = ss s
    showsPrec _ (EIf e t f)                = ss "if " . shows e
                                           . ss " then " . shows t
                                           . ss " else " . shows f
    showsPrec _ (ETable nvps)              = ss "table" . showParen True nvps'
      where
        nvps' = xjoin "," $ map (\(i,e) -> ss i . ss "=" . shows e) nvps

    showsPrec p (EUOp UOpNegate x)         = sPfx 11 p (ss "-") x

    showsPrec p (EBinOp BinOpEllipses l r) = sIfx 10 p (ss ":") l r

    showsPrec p (EProject t ps)            = sIfx  9 p (ss "$") t ps
    showsPrec p (ESelect t e)              = sSfx  9 p (sBrackets e) t

    showsPrec p (EJoin op l r)             = sIfx  8 p (jOp op) l r

    showsPrec p (EBinOp BinOpTimes    l r) = sIfx  7 p (ss " * ") l r
    showsPrec p (EBinOp BinOpDiv      l r) = sIfx  7 p (ss " / ") l r

    showsPrec p (EBinOp BinOpAdd      l r) = sIfx  6 p (ss " + ") l r
    showsPrec p (EBinOp BinOpSub      l r) = sIfx  6 p (ss " - ") l r

    showsPrec p (EBinOp BinOpIn       l r) = sIfx  5 p (ss " %in% ") l r

    showsPrec p (EBinOp BinOpEq       l r) = sIfx  4 p (ss " == ") l r
    showsPrec p (EBinOp BinOpGe       l r) = sIfx  4 p (ss " >= ") l r
    showsPrec p (EBinOp BinOpGt       l r) = sIfx  4 p (ss " > ")  l r
    showsPrec p (EBinOp BinOpLe       l r) = sIfx  4 p (ss " <= ") l r
    showsPrec p (EBinOp BinOpLt       l r) = sIfx  4 p (ss " < ")  l r
    showsPrec p (EBinOp BinOpNeq      l r) = sIfx  4 p (ss " != ") l r

    showsPrec p (EUOp UOpNot    x)         = sPfx  3 p (ss "! ") x

    showsPrec p (EBinOp BinOpSAnd     l r) = sIfx  2 p (ss " && ") l r
    showsPrec p (EBinOp BinOpSOr      l r) = sIfx  2 p (ss " || ") l r
    showsPrec p (EBinOp BinOpVAnd     l r) = sIfx  2 p (ss " & ")  l r
    showsPrec p (EBinOp BinOpVOr      l r) = sIfx  2 p (ss " | ")  l r

    showsPrec p (EBind v e)                = sIfx  1 p (ss " <- ") v e

    showsPrec p (ESeries es)               = semijoin es

sFn fname x     = ss fname . sParens x
sFn2 fname x y  = ss fname . showParen True (commajoin [x,y])
sPfx q p op e   = showParen (p > q) $ op . showsPrec q e
sSfx q p op e   = showParen (p > q) $ showsPrec q e . op
sIfx q p op l r = showParen (p > q) $ showsPrec q l . op . showsPrec q r

commajoin, semijoin :: Show a => [a] -> ShowS
xjoin _ [] = id
xjoin x xs = foldr1 (\l r -> l . ss x . r) xs
commajoin  = xjoin ","  . map shows
semijoin   = xjoin "; " . map shows

ss = showString

sParens, sBrackets, sBraces :: Show a => a -> ShowS
sParens e       = ("("++) . shows e . (")"++)
sBrackets e     = ("["++) . shows e . ("]"++)
sBraces e       = ("{"++) . shows e . ("}"++)

jOp JCartesian = ss "***"
jOp (JNatural il l r ir) =
    ss " " . js l . ji il . ss "=" . ji ir . js r . ss " "
  where
    ji JInner = ss "="
    ji _      = ss "*"
    js []     = ss ""
    js x      = ss "{" . commajoin x . ss "}"



-- ============================================================================
-- operators
-- ============================================================================

data BinOp
    = BinOpTimes | BinOpDiv | BinOpAdd | BinOpSub | BinOpEq | BinOpNeq
    | BinOpEllipses
    | BinOpGt | BinOpGe | BinOpLt | BinOpLe
    | BinOpSOr | BinOpSAnd | BinOpVOr | BinOpVAnd
    | BinOpIn
    deriving  (Show, Read, Eq, Ord)

data UnaryOp
    = UOpNegate | UOpNot
    deriving (Show, Read, Eq, Ord)

instance PPrint Expr

-- ============================================================================
-- projection specs
-- ============================================================================

data PSpec
    = PSTable Bool [PSCol]    -- ^ build a new table by spec
    | PSVectorName Identifier -- ^ extract vector by column name
    | PSVectorNum Int         -- ^ extract vector by column number
    deriving (Eq, Ord)

instance Show PSpec where
    showsPrec _ (PSVectorName s)  = ss s
    showsPrec _ (PSVectorNum i)   = shows i
    showsPrec _ (PSTable b pscs)  = showParen True $ b' . commajoin pscs
      where
        b' = if b then ss "-" else id


data PSCol
    = PSCNum Int                -- ^ column number
    | PSCName Identifier        -- ^ column name
    | PSCNExpr Identifier Expr  -- ^ ident=expr
    | PSCStar                   -- ^ "star" for all columns
    | PSCExpr Expr              -- ^ expression
    deriving (Eq, Ord)

instance Show PSCol where
    showsPrec _ (PSCNum i)     = shows i
    showsPrec _ (PSCName s)    = ss s
    showsPrec _ (PSCNExpr s e) = ss s . ss "=" . shows e
    showsPrec _ (PSCStar)      = ss "*"
    showsPrec _ (PSCExpr e)    = shows e

-- ============================================================================
-- join operators
-- ============================================================================

data JoinOp
    = JCartesian
    | JNatural JoinInclusion [Expr] [Expr] JoinInclusion
    deriving (Eq, Ord, Show)

data JoinInclusion
    = JInner
    | JOuter
    deriving (Eq, Ord, Show)



-- ============================================================================
-- ============================================================================
-- values
-- ============================================================================
-- ============================================================================


data Value
  = VVector Vector
  | VTable Table
  | VNull
    deriving (Ord, Eq)

instance Show Value where
    showsPrec _ (VVector v) = shows v
    showsPrec _ (VTable t)  = shows t
    showsPrec _  VNull      = showString "NULL"

instance PPrint Value where
    toDoc (VVector v) = toDoc v
    toDoc VNull       = text "NULL"
    toDoc (VTable t)  = toDoc t
--    toDoc x           = error $ "don't know how to pp " ++ show x

vIsVector (VVector _) = True
vIsVector _           = False

vIsTable (VTable _)   = True
vIsTable _            = False

-- coercions

asVector :: Monad m => Value -> m Vector
asVector (VVector v) = return v
asVector _           = fail "not a vector"

asNum :: Monad m => Value -> m Double
asNum v = asVector v >>= vecNum

asTable :: Monad m => Value -> m Table
asTable (VTable t) = return t
asTable _          = fail "not a table"

asString :: Monad m => Value -> m String
asString v = asVector v >>= vecStr

asBool :: Monad m => Value -> m Bool
asBool v = asVector v >>= vecLog


-- vector values

instance ToVector [Value] where
    toVector = mkVector . map toScalar

toScalar (VVector (V _ _ (x:_))) = x
toScalar _                       = SNa

mkVectorValue :: [Scalar] -> Value
mkVectorValue xs =
    if vnull v then VNull else VVector v
  where
    v = mkVector xs
