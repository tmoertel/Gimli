{-# OPTIONS -fglasgow-exts #-}

module Eval (
    eval, evalTop, run, emptyEnv,
    EvalState
) where

import Control.Monad.Cont
import Control.Monad.State
import qualified Data.Map as Map

import PPrint
import Expr

type Closure    = (Env, Value, Maybe Expr)

clEnv (e,_,_) = e
clVal (_,v,_) = v
clExp (_,v,x) = x

nullClosure = (emptyEnv, VNull, Nothing)

type EnvMap     = Map.Map Identifier Closure
data Env        = Env EnvMap

emptyEnv :: Env
emptyEnv  = Env Map.empty

modifyEnv :: (EnvMap -> EnvMap) -> Env -> Env
modifyEnv f (Env emap) = Env (f emap)

envMap (Env emap) = emap

-- Eval monad

type EvalState   = Env
type Eval r a    = ContT r (StateT EvalState IO) a

stEnv = id

bind ident valExpr = do
    env <- gets stEnv
    val <- eval valExpr
    modify $ modifyEnv $ Map.insert ident (env, val, Just valExpr)
    return val


evalTop :: Expr -> IO Value
evalTop =
    (`evalStateT` emptyEnv) . (`runContT` return) . eval

run :: EvalState -> Expr -> IO (Value, EvalState)
run st =
    (`runStateT` st) . (`runContT` return) . eval

eval (EVal v)
    = return v

eval (EBind lvalue ev)
    | EVar ident <- lvalue = bind ident ev
    | otherwise            = error $ "cannot bind to non-lvalue: " ++ pp lvalue

eval (EVar ident)
    = gets stEnv >>=
      return . clVal . Map.findWithDefault nullClosure ident . envMap

eval (EBinOp op l r)
    = do
      lval <- eval l
      rval <- eval r
      return $ binOp op lval rval

eval (ESeries es)
    | es == []  = return VNull
    | otherwise = foldr1 (>>) $ map eval es

{-
eval e
    = error $ "eval error; could not eval (" ++ show e ++ ")"
-}

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
