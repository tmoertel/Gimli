{-# OPTIONS -fglasgow-exts #-}

module Eval (
    eval, evalTop, run, emptyEnv,
    clExp, clVal,
    envMap,
    EvalState
) where

import Control.Exception
import Control.Monad.Cont
import Control.Monad.Error
import Control.Monad.State
import Data.Array
import Data.Either
import Data.List (group, transpose, (\\))
import qualified Data.Map as Map
import Data.Maybe

import Expr
import Join
import LoadData
import PPrint

type Closure    = (Env, Value, Maybe Expr)

clEnv (e,_,_) = e
clVal (_,v,_) = v
clExp (_,v,x) = x

nullClosure = (emptyEnv, VNull, Nothing)

type EnvMap     = Map.Map Identifier Closure
data Env        = Env EnvMap deriving (Read, Show, Eq, Ord)

emptyEnv :: Env
emptyEnv  = Env Map.empty

modifyEnv :: (EnvMap -> EnvMap) -> Env -> Env
modifyEnv f (Env emap) = Env (f emap)

envMap (Env emap) = emap

-- Eval monad

type EvalError   = String
type EvalState   = Env
type Eval r a    = ErrorT EvalError (ContT r (StateT EvalState IO)) a

stEnv = id

bind ident valExpr = do
    val <- eval valExpr
    env <- gets stEnv
    modify $ modifyEnv $ Map.insert ident (env, val, Just valExpr)
    return val

evalTop :: Expr -> IO (Either EvalError Value)
evalTop =
    (`evalStateT` emptyEnv) . (`runContT` return) . runErrorT . evalL

run :: EvalState -> Expr -> IO (Either EvalError Value, EvalState)
run st =
    (`runStateT` st) . (`runContT` return) . runErrorT . evalL

evalL x = eval x >>= bind "LAST" . EVal

evalString e = eval e >>= asString
evalTable e  = eval e >>= asTable
evalVector e = eval e >>= asVector


{- | Evaluate an expression to result in a Value -}

eval :: Expr -> Eval r Value

eval (EVal v) =
    return v

eval (EVector es) = do
    vecs <- argof "vector constructor" $ mapM evalVector es
    return . VVector . mkVector $ concatMap vlist vecs

eval (EBind lvalue ev)
    | EVar ident <- lvalue = bind ident ev
    | otherwise            = throwError $
                             "cannot bind to non-lvalue: " ++ pp lvalue

eval (EVar ident) =
    gets stEnv >>=
    return . clVal . Map.findWithDefault nullClosure ident . envMap

eval (EUOp op x) =
    eval x >>= uOp op

eval (EBinOp op el er) = do
    l <- eval el
    r <- eval er
    binOp op l r

eval (ESeries es)
    | es == []  = return VNull
    | otherwise = foldr1 (>>) $ map evalL es

eval (ESelect etarget eselect) = do
    target <- eval etarget
    case target of
        VVector vec  -> eval eselect >>= return . VVector . selectVector vec
        VTable table -> select table eselect
        _ -> throwError $ "selection applies only to tables and vectors"

eval (EJoin joinType eltarg ertarg) = do
    ltable <- arg1of nm (evalTable eltarg)
    rtable <- arg2of nm (evalTable ertarg)
    liftM VTable (tableJoin joinType ltable rtable)
  where
    nm = "join"
    notTableError loc =
        throwError $ loc ++ " argument of join must be a table"

eval (EProject etarget pspec) = do
    table <- arg1of "$" (evalTable etarget)
    project table pspec

eval (EReadCsv efile) =
    liftM VTable $ loadCsvTable =<< argof "read.csv" (evalString efile)

eval (EReadWsv efile) =
    liftM VTable $ loadWsvTable =<< argof "read.csv" (evalString efile)

eval (EWriteWsv etable efile) = do
    table <- arg1of nm $ evalTable etable
    file  <- arg2of nm $ evalString efile
    during "write.wsv" $ do
        result <- liftIO . try $ writeFile file (pp table ++ "\n")
        case result of
            Left err -> throwError (show err)
            Right x  -> return (mkVectorValue [SStr file])
  where
    nm = "write.csv"

eval (ETable ecolspecs) = do
    vecs <- argof nm $ mapM evalVector evecs
    let vlens = map vlen vecs
    if length (group vlens) == 1
        then return . VTable $ mkTable (zip names vecs)
        else throwError $ "table columns must be vectors of equal length"
  where
    nm    = "table constructor"
    names = map fst ecolspecs
    evecs = map snd ecolspecs


-- error-reporting helpers

during :: String -> Eval r a -> Eval r a
during fname m =
    catchError m describe
  where
    describe err =
        throwError $ "during " ++ fname ++ ": " ++ err

argof  fname m = argxof fname ""   m
arg1of fname m = argxof fname " 1" m
arg2of fname m = argxof fname " 2" m
argxof fname argstr m =
    catchError m describe
  where
    describe err =
        throwError ("argument" ++ argstr  ++ " of " ++ fname ++ ": " ++ err)


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


-- selection of table

select table expr = do
    env <- gets stEnv
    result <- (`catchError` \e -> put env >> throwError e) $ do
        selections <- evalRows table [expr] >>= return . map head
        vecs' <- mapM (liftM catMaybes . zipWithM sel1 selections . vlist) vecs
        return . VTable $
               table { tvecs = listArray (bounds tvts) $
                       zipWith mkVectorOfType origTypes vecs' }
    put env
    return result
  where
    tvts      = tvecs table
    vecs      = elems tvts
    origTypes = map vtype vecs

sel1 val x =
    return $ case keepNAs toSLog $ toScalar val of
        SLog True  -> Just x
        _          -> Nothing


-- projection of table

project table (PSVectorNum n) =
    liftM (VVector . (tvecs table !)) $ tableColumnIndexCheck table n

project table (PSVectorName s) =
    (project table . PSVectorNum) =<< tableColumnLookupIndex table s

project table (PSTable True pscols) = do
    colIndexes <- mapM getIndex (removeStars pscols)
    project table . PSTable False . map PSCNum $
        range (bounds $ tcols table) \\ colIndexes
  where
    getIndex (PSCNum n)   = tableColumnIndexCheck table n
    getIndex (PSCName s)  = tableColumnLookupIndex table s
    getIndex (PSCExp s _) = tableColumnLookupIndex table s

project table (PSTable False pscols) = do
    env <- gets stEnv -- remember pre-eval environemnt
    result <- (`catchError` (\e -> put env >> throwError e)) $ do
        colNames <- mapM getName pscols'
        colExps  <- mapM getExp pscols'
        rows <- evalRows table colExps
        return $ VTable $ mkTable $
               zip colNames (map toVector $ transpose rows)
    put env -- restore environment
    return result
  where
    pscols'                = expandStars table pscols
    getName                = pscol id fst
    getExp                 = pscol EVar snd
    pscol f g (PSCNum n)   = tableColumnIndexCheck table n >>=
                             return . f . (tcols table !)
    pscol f g (PSCName s)  = tableColumnLookupIndex table s >>=
                             pscol f g . PSCNum
    pscol f g (PSCExp s e) = return $ g (s,e)

evalRows table colExps = do
    mapM projectRow (trows table)
  where
    projectRow r = bindCols r >> mapM eval colExps
    bindCols     = zipWithM bindScalar (tcnames table)

bindScalar ident x =
    bind ident (EVal $ mkVectorValue [x])

removeStars =
    filter (not . isStar)
  where
    isStar PSCStar = True
    isStar _       = False

expandStars table =
    concatMap starExpand
  where
    starExpand PSCStar = map PSCNum (range . bounds $ tcols table)
    starExpand x       = [x]


-- ============================================================================
-- unary operations
-- ============================================================================

uOp :: UnaryOp -> Value -> Eval r Value
uOp UOpNegate x  = binOp BinOpTimes (VVector negOne) x
uOp UOpNot v     = asVector v >>=
                   return . VVector . vmap logNot . vectorCoerce VTLog

logNot (SLog x) = SLog (not x)
logNot _        = SNa

negOne = V VTNum 1 [SNum (-1.0)]


-- ============================================================================
-- binary operations
-- ============================================================================

binOp :: BinOp -> Value -> Value -> Eval r Value

binOp BinOpEllipses = doEllipses

binOp BinOpTimes    = numOp (*)
binOp BinOpDiv      = numOp (/)
binOp BinOpAdd      = numOp (+)
binOp BinOpSub      = numOp (-)

binOp BinOpEq       = cmpOp (==)
binOp BinOpNeq      = cmpOp (/=)
binOp BinOpLt       = cmpOp (<)
binOp BinOpLe       = cmpOp (<=)
binOp BinOpGt       = cmpOp (>)
binOp BinOpGe       = cmpOp (>=)

binOp BinOpVOr      = logOp (||)
binOp BinOpVAnd     = logOp (&&)
binOp BinOpSOr      = scalarize logOp (||)
binOp BinOpSAnd     = scalarize logOp (&&)


numOp op x y = vectorize (propNa (binWrap SNum valNum op)) x y
logOp op x y = vectorize (propNa (binWrap SLog valBool op)) x y
cmpOp op x y = vectorize (propNa f) x y
  where
    f x y = return $ SLog (withBestType op x y)

scalarize f op x y =
    return . mkVectorValue . (:[]) . toScalar =<< f op x y

doEllipses start end = do
    s <- arg1of nm $ asNum start
    e <- arg2of nm $ asNum end
    return $ mkVectorValue $ map SNum [ s .. e ]
  where
    nm = "(:) operator"

valNum x =
    case toSNum x of
        SNum v -> return v
        _      -> throwError $ "cannot coerce into numeric: (" ++ show x ++ ")"

valBool x =
    case toSLog x of
        SLog b -> return b
        _      -> throwError $ "cannot coerce into logical: (" ++ show x ++ ")"

binWrap wrapper argfn op l r = do
    al <- argfn l
    ar <- argfn r
    return . wrapper $ al `op` ar

withBestType :: (Scalar -> Scalar -> a) -> Scalar -> Scalar -> a
withBestType f l r =
    f l' r'
  where
    [l', r'] = vlist $ toVector [l, r]

propNa _ SNa _   = return SNa
propNa _ _   SNa = return SNa
propNa f a   b   = f a b

vectorize :: (Scalar -> Scalar -> Eval r Scalar)
             -> Value -> Value -> Eval r Value
vectorize op x y =
    return . VVector =<< vectorize' op x y

vectorize' op (VVector vx) (VVector vy) =
    return . toVector =<< zipWithM op (take len vx') (take len vy')
  where
    len = maximum (map vlen [vx, vy])
    vx' = cycle (vlist vx)
    vy' = cycle (vlist vy)

vectorize' _ _ _ = throwError "vector operation requires two vectors"
