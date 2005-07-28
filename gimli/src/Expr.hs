{-# OPTIONS -fglasgow-exts #-}

module Expr (
    Expr(..),
    BinOp(..),
    module Value
)
where

import Value
import PPrint

data Expr = EVal Value | EBinOp BinOp Expr Expr
    deriving (Show, Read, Eq, Ord)

data BinOp = BinOpTimes | BinOpDiv | BinOpAdd | BinOpSub | BinOpEq | BinOpNeq
    deriving  (Show, Read, Eq, Ord)

instance PPrint Expr

