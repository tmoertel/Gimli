module Eval (
    eval
) where

import Expr

eval :: Expr -> Value

eval (EVal v)
    = v

eval (EBinOp op l r)
    = binOp op (eval l) (eval r)

eval e
    = error $ "eval error; could not eval (" ++ show e ++ ")"

binOp :: BinOp -> Value -> Value -> Value
binOp BinOpTimes = numOp (*)
binOp BinOpDiv   = numOp div
binOp BinOpAdd   = numOp (+)
binOp BinOpSub   = numOp (-)
binOp BinOpEq    = cmpOp (==)
binOp BinOpNeq   = cmpOp (/=)

cmpOp op = propNa (binWrap VBool id op)
numOp op = propNa (binWrap VInt valInt op)

binWrap wrapper argfn op l r = wrapper (argfn l `op` argfn r)

propNa _ VNa _   = VNa
propNa _ _   VNa = VNa
propNa f a   b   = f a b

