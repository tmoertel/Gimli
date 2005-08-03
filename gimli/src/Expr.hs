{-# OPTIONS -fglasgow-exts #-}

module Expr (
    Expr(..),
    BinOp(..),
    Identifier,
    module Value
)
where

import Value
import PPrint

type Identifier = String

data Expr
  = EVal Value
  | EBinOp BinOp Expr Expr
  | ESeries [Expr]
  | EVar Identifier
  | EBind Expr Expr
  deriving (Show, Read, Eq, Ord)

data BinOp = BinOpTimes | BinOpDiv | BinOpAdd | BinOpSub | BinOpEq | BinOpNeq
           | BinOpBindL | BinOpBindR
    deriving  (Show, Read, Eq, Ord)

instance PPrint Expr

