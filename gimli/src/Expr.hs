{-# OPTIONS -fglasgow-exts #-}

module Expr (
    Expr(..),
    BinOp(..),
    module Value
)
where

import Value

type BinFn = Value -> Value -> Value
data BinOp = BinOp { boName :: String, boOp :: BinFn } 
    deriving (Show)

instance Show BinFn where show _ = "BinaryFn"

data Expr = EVal Value | EBinOp BinOp Expr Expr
    deriving (Show)
