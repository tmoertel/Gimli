{-# OPTIONS -fglasgow-exts #-}

module Expr (
    Expr(..),
    BinOp(..),
    PSpec(..), PSCol(..),
    module Value
)
where

import Value
import PPrint

data Expr
  = EVal Value
  | EBinOp BinOp Expr Expr
  | ESeries [Expr]
  | EVar Identifier
  | EBind Expr Expr
  | ESelect Expr Expr
  | EProject Expr PSpec
  | ETable [(Identifier, Expr)]
  deriving (Show, Read, Eq, Ord)

data BinOp = BinOpTimes | BinOpDiv | BinOpAdd | BinOpSub | BinOpEq | BinOpNeq
           | BinOpBindL | BinOpBindR | BinOpEllipses
    deriving  (Show, Read, Eq, Ord)

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
            deriving (Read, Show, Eq, Ord)
