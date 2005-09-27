{-# OPTIONS -fglasgow-exts #-}

module Expr (

    Expr(..),
    BinOp(..), UnaryOp(..),
    PSpec(..), PSCol(..), ENVPair(..), TableSpec(..),
    JoinOp(..), JoinInclusion(..),
    ArgList, Arg(..),
    Primitive(..),

    Value(..),
        vIsVector, vIsTable,
        toScalar,
        asVector, asVectorNull, asNum, asTable, asString, asBool,
        mkVectorValue,
        concatVals,
        test,

    module CoreTypes,
    module Scalar,
    module Vector,
    module Table,
)
where

import Control.Monad
import Data.List (intersperse)

import CoreTypes
import Scalar
import Vector
import Table
import PPrint

-- ============================================================================
-- core expressions
-- ============================================================================

type Prog = [Expr] -- ^ A program is a series of expressions

data Expr
    = EApp Expr [Expr]
    | EBinOp !BinOp Expr Expr
    | EBlock [Expr]
    | EBind Expr Expr
    | EIf Expr Expr (Maybe Expr)
    | EFor Identifier Expr Expr
    | EUnless Expr Expr (Maybe Expr)
    | EJoin JoinOp Expr Expr
    | EPrimitive Identifier
    | EProject Expr PSpec
    | ESelect Expr Expr
    | ESeries [Expr]
    | ETable [TableSpec]
    | EUOp !UnaryOp Expr
    | EVal Value
    | EVar Identifier
    | EVector [Expr]
    deriving (Eq, Ord)

instance Show Expr where

    showsPrec p (EVal v)            = showsPrec p v
    showsPrec _ (EVector es)        = ss "[" . commajoin es . ss "]"
    showsPrec _ (EVar s)            = ss s
    showsPrec _ (EIf e t f)         = ss "if " . shows e
                                    . ss " then " . shows t
                                    . maybe id  (\x -> ss " else " . shows x) f
    showsPrec _ (EUnless e t f)     = ss "unless " . shows e
                                    . ss " then " . shows t
                                    . maybe id  (\x -> ss " else " . shows x) f
    showsPrec _ (EFor s e1 e2)      = ss "for " . ss s . ss " in "
                                    . sParens e1 . ss " " . shows e2

    showsPrec _ (EBlock es)         = ss "do " . showParen True (semijoin es)
                                    . ss " end"

    showsPrec _ (ETable tspecs)     = ss "table" . showParen True tspecs'
      where
        tspecs' = commajoin tspecs

    showsPrec p (EApp e args)              = let q = 13 in
                                             showParen (p > q) $
                                               (showsPrec q e) .
                                               showParen True (commajoin args)

    showsPrec p (EUOp UOpNegate x)         = sPfx 12 p (ss "-") x

    showsPrec p (EBinOp BinOpEllipses l r) = sIfx 11 p (ss ":") l r

    showsPrec p (EProject t ps)            = sIfx 10 p (ss "$") t ps
    showsPrec p (ESelect t e)              = sSfx 10 p (sBrackets e) t

    showsPrec p (EJoin op l r)             = sIfx  9 p (jOp op) l r

    showsPrec p (EBinOp BinOpTimes    l r) = sIfx  8 p (ss " * ") l r
    showsPrec p (EBinOp BinOpDiv      l r) = sIfx  8 p (ss " / ") l r

    showsPrec p (EBinOp BinOpAdd      l r) = sIfx  7 p (ss " + ") l r
    showsPrec p (EBinOp BinOpSub      l r) = sIfx  7 p (ss " - ") l r

    showsPrec p (EBinOp BinOpConcat   l r) = sIfx  6 p (ss " ++ ") l r

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

data TableSpec
    = TCol ENVPair
    | TSplice Expr
    deriving (Eq, Ord)

instance Show TableSpec where
    showsPrec _ (TCol x)    = shows x
    showsPrec _ (TSplice e) = sParens e

sFn fname x     = ss fname . sParens x
sFn2 fname x y  = ss fname . showParen True (commajoin [x,y])
sPfx q p op e   = showParen (p > q) $ op . showsPrec q e
sSfx q p op e   = showParen (p > q) $ showsPrec q e . op
sIfx q p op l r = showParen (p > q) $ showsPrec q l . op . showsPrec q r

xjoin :: String -> [ShowS] -> ShowS
xjoin x xs = foldr (.) id $ intersperse (ss x) xs

commajoin, semijoin :: Show a => [a] -> ShowS
commajoin  = xjoin ","  . map shows
semijoin   = xjoin "; " . map shows

ss = showString

sParens, sBrackets, sBraces :: Show a => a -> ShowS
sParens e       = ("("++) . shows e . (")"++)
sBrackets e     = ("["++) . shows e . ("]"++)
sBraces e       = ("{"++) . shows e . ("}"++)

jOp JCartesian = ss " *** "
jOp (JNatural il l r ir) =
    ss " " . js l . ji il . ss "=" . ji ir . js r . ss " "
  where
    ji JInner = ss "-"
    ji _      = ss "="
    js []     = ss ""
    js x      = ss "{" . commajoin x . ss "}"



-- ============================================================================
-- operators
-- ============================================================================

data BinOp
    = BinOpPower
    | BinOpTimes | BinOpDiv | BinOpAdd | BinOpSub | BinOpEq | BinOpNeq
    | BinOpEllipses
    | BinOpGt | BinOpGe | BinOpLt | BinOpLe
    | BinOpSOr | BinOpSAnd | BinOpVOr | BinOpVAnd
    | BinOpConcat
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
    | PSTableOverlay [ENVPair]  -- ^ additively overlay columns
    | PSVectorName Identifier -- ^ extract vector by column name
    | PSVectorNum Int         -- ^ extract vector by column number
    deriving (Eq, Ord)

instance Show PSpec where
    showsPrec _ (PSVectorName s)  = ss s
    showsPrec _ (PSVectorNum i)   = shows i
    showsPrec _ (PSTable b pscs)  = showParen True $ b' . commajoin pscs
      where
        b' = if b then ss "-" else id
    showsPrec _ (PSTableOverlay nvps)
                                  = showParen True $ ss "+"
                                  . (xjoin "," $ map shows nvps)

data PSCol
    = PSCNum Int                -- ^ column number
    | PSCName Identifier        -- ^ column name
    | PSCNExpr ENVPair          -- ^ ident=expr
    | PSCStar                   -- ^ "star" for all columns
    | PSCExpr Expr              -- ^ expression
    deriving (Eq, Ord)

instance Show PSCol where
    showsPrec _ (PSCNum i)     = shows i
    showsPrec _ (PSCName s)    = ss s
    showsPrec _ (PSCNExpr nvp) = shows nvp
    showsPrec _ (PSCStar)      = ss "*"
    showsPrec _ (PSCExpr e)    = showParen True $ shows e

data ENVPair
    = NVP String Expr
    | ENVP Expr Expr
    deriving (Eq, Ord)

instance Show ENVPair where
    showsPrec _ (NVP s e)   = ss s . ss "=" . shows e
    showsPrec _ (ENVP es e) = sParens es . ss "=" . shows e

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
-- function/primitive types
-- ============================================================================
-- ============================================================================

type ArgList = [Arg]
data Arg     = Arg
    { argName :: String
    }
    deriving (Eq,Ord,Show,Read)

data Primitive = Prim
    { primName :: String
    , primArgs :: ArgList
    }
    deriving (Eq,Ord,Show,Read)


-- ============================================================================
-- ============================================================================
-- values
-- ============================================================================
-- ============================================================================


data Value
  = VVector Vector
  | VTable Table
  | VFunc ArgList Prog
  | VPrim Primitive
  | VNull
    deriving (Ord, Eq)

instance Show Value where
    showsPrec _ (VVector v)       = shows v
    showsPrec _ (VTable t)        = shows t
    showsPrec _  VNull            = showString "NULL"
    showsPrec _ (VFunc args prog) =
        ss "function"
        . showParen True (xjoin ", " (map (ss . argName) args))
        . ss " { " . (xjoin "; " (map shows prog)) . ss " }"
    showsPrec _ (VPrim prim)      = ss (primName prim)

instance PPrint Value where
    toDoc (VVector v) = toDoc v
    toDoc VNull       = text "NULL"
    toDoc (VTable t)  = toDoc t
    toDoc x           = text (show x)

vIsVector (VVector _) = True
vIsVector _           = False

vIsTable (VTable _)   = True
vIsTable _            = False

-- coercions

asVector :: Monad m => Value -> m Vector
asVector (VVector v) = return v
asVector _           = fail "not a vector"

asVectorNull :: Monad m => Value -> m Vector
asVectorNull VNull   = return nullVector
asVectorNull x       = asVector x

asNum :: Monad m => Value -> m Double
asNum v = asVector v >>= vecNum

asTable :: Monad m => Value -> m Table
asTable (VTable t) = return t
asTable _          = fail "not a table"

asString :: Monad m => Value -> m String
asString v = asVector v >>= vecStr

asBool :: Monad m => Value -> m Bool
asBool v = asVector v >>= vecLog


-- concatenation

concatVals :: Monad m => [Value] -> m Value
concatVals vs = do
    vecs <- mapM asVectorNull vs
    return . mkVectorValue . concat $ map vlist vecs


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

nullVector = V VTLog 0 []

-- true/false test

test :: Value -> Bool
test VNull       = False
test (VVector v) = case vlist v of
                       []           -> False
                       SLog False:_ -> False
                       SNa:_        -> False
                       _            -> True
test _           = True
