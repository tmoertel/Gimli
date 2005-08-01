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
binOp BinOpDiv   = numOp (/)
binOp BinOpAdd   = numOp (+)
binOp BinOpSub   = numOp (-)
binOp BinOpEq    = cmpOp (==)
binOp BinOpNeq   = cmpOp (/=)

cmpOp op x y = VVector $ vectorize (propNa (binWrap SLog id op)) x y
numOp op x y = VVector $ vectorize (propNa (binWrap SNum valNum op)) x y

valNum x =
    case toSNum x of
        SNum v -> v
        _      -> error $ "cannot coerce into numeric: (" ++ show x ++ ")"

binWrap wrapper argfn op l r = wrapper (argfn l `op` argfn r)

propNa _ SNa _   = SNa
propNa _ _   SNa = SNa
propNa f a   b   = f a b

vectorize op (VVector vx) (VVector vy) =
    toVector . take len $ zipWith op vx' vy'
  where
    len = maximum (map vlen [vx, vy])
    vx' = cycle (vlist vx)
    vy' = cycle (vlist vy)

vectorize _ _ _ = error $ "vector operation requires two vectors"
