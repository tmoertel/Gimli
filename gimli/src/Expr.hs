module Expr (
    Expr(..),
    module Value
)
where

import Value

data Expr = EVal Value
    deriving (Show, Read, Eq, Ord)
