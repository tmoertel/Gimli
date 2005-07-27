module Eval where

import Expr

eval :: Expr -> Value
eval (EVal v) = v
eval e        = error $ "eval error; could not eval (" ++ show e ++ ")"
