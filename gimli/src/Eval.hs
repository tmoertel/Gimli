{-# OPTIONS -fglasgow-exts #-}

module Eval (
    eval, evalTop, run, emptyEnv,
    EvalState
) where

import Control.Monad.Cont
import Control.Monad.State
import Data.Array
import qualified Data.Map as Map
import Data.List (group)
import Data.Maybe

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

eval (ESelect etarget eselect) = do
    target <- eval etarget
    case target of
        VVector vec -> eval eselect >>= return . VVector . selectVector vec
        _ -> return . VError $ "selection applies only to tables and vectors"

eval (EProject etarget pspec) = do
    target <- eval etarget
    case target of
        VTable table -> return $ project table pspec
        _            -> return . VError $ "first operand of $ must be a table"

eval (ETable ecolspecs) = do
    vvecs <- mapM eval evecs
    let vecs = [v | VVector v <- vvecs]
    let vlens = map vlen vecs
    if all vIsVector vvecs && length (group vlens) == 1
        then return . VTable $ mkTable (zip names vecs)
        else return . VError $ "table columns must be vectors of equal length"
  where
    names = map fst ecolspecs
    evecs = map snd ecolspecs

-- select elements _es_ from vector _vec_:

selectVector (V tvtype _ txs) (VVector (V VTNum _ sxs)) =
    V tvtype (length xs) xs
  where
    xs   = map (pull txs') sxs'
    txs' = txs ++ repeat SNa
    sxs' = if sxs == [ sx | sx@(SNum n) <- sxs, n < 0 ]
           then [ SNum n
                  | n <- map fromIntegral [1..length txs]
                  , negate n `notElem` nsxs ]
           else sxs
    nsxs = [ n | SNum n <- sxs ]
selectVector (V tvtype _ txs) (VVector (V VTLog _ sxs)) =
    V tvtype (length xs) xs
  where
    xs   = catMaybes $ zipWith takeLogical txs sxs
selectVector _ _ = error "vector-selection criteria must be a vector"


pull xs (SNum n) = xs !! (round n - 1)
pull _  _        = SNa

takeLogical tx (SLog True) = Just tx
takeLogical _  SNa         = Just SNa
takeLogical _  _           = Nothing


-- projection of table

project table (PSVectorNum n) =
    VVector $ tvecs table ! n

project table (PSVectorName s) =
    project table (PSVectorNum . fromJust . Map.lookup s $ tlookup table)


-- ============================================================================
-- binary operations
-- ============================================================================

binOp :: BinOp -> Value -> Value -> Value
binOp BinOpTimes    = numOp (*)
binOp BinOpDiv      = numOp (/)
binOp BinOpAdd      = numOp (+)
binOp BinOpSub      = numOp (-)
binOp BinOpEllipses = doEllipses

binOp BinOpEq       = cmpOp (==)
binOp BinOpNeq      = cmpOp (/=)

cmpOp op x y = VVector $ vectorize (propNa (binWrap SLog id op)) x y
numOp op x y = VVector $ vectorize (propNa (binWrap SNum valNum op)) x y

doEllipses (VVector start) (VVector end) =
    VVector $ mkVector $ map SNum [ (vecNum start) .. (vecNum end) ]
doEllipses _ _ =
    VError $ "both operands to the (:) operator must be vectors"

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
