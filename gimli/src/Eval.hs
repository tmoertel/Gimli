module Eval where

import Expr

eval :: Expr -> Value

eval (EVal v)
    = v

eval (EBinOp op l r)
    = boOp op (eval l) (eval r)

eval e
    = error $ "eval error; could not eval (" ++ show e ++ ")"
