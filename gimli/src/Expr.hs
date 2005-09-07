{-# OPTIONS -fglasgow-exts #-}

module Expr (
    Expr(..),
    BinOp(..), UnaryOp(..),
    PSpec(..), PSCol(..),
    JoinOp(..), JoinInclusion(..),
    module Value
)
where

import Value
import PPrint

data Expr
  = EVal Value
  | EBinOp BinOp Expr Expr
  | EVector [Expr]
  | EUOp UnaryOp Expr
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
  deriving (Show, Read, Eq, Ord)

data BinOp = BinOpTimes | BinOpDiv | BinOpAdd | BinOpSub | BinOpEq | BinOpNeq
           | BinOpBindL | BinOpBindR | BinOpEllipses
           | BinOpGt | BinOpGe | BinOpLt | BinOpLe
           | BinOpSOr | BinOpSAnd | BinOpVOr | BinOpVAnd
    deriving  (Show, Read, Eq, Ord)

data UnaryOp = UOpNegate | UOpNot
    deriving (Show, Read, Eq, Ord)

instance PPrint Expr

-- ============================================================================
-- projection specs
-- ============================================================================

data PSpec  = PSTable Bool [PSCol]    -- ^ build a new table by spec
            | PSVectorName Identifier -- ^ extract vector by column name
            | PSVectorNum Int         -- ^ extract vector by column number
            deriving (Read, Show, Eq, Ord)
data PSCol  = PSCNum Int              -- ^ column number
            | PSCName Identifier      -- ^ column name
            | PSCExp Identifier Expr  -- ^ ident=expr
            | PSCStar                 -- ^ "star" for all columns
            | PSCExpr Expr            -- ^ expression
            deriving (Read, Show, Eq, Ord)

-- ============================================================================
-- join operators
-- ============================================================================

data JoinOp
    = JCartesian
    | JNatural JoinInclusion [Expr] [Expr] JoinInclusion
    deriving (Eq, Ord, Read, Show)

data JoinInclusion
    = JInner
    | JOuter
    deriving (Eq, Ord, Read, Show)

