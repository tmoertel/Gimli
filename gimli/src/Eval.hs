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

type EvalState   = Env
type Eval r a    = ContT r (StateT EvalState IO) a

stEnv = id

bind ident valExpr = do
    val <- eval valExpr
    env <- gets stEnv
    modify $ modifyEnv $ Map.insert ident (env, val, Just valExpr)
    return val

evalTop :: Expr -> IO Value
evalTop =
    (`evalStateT` emptyEnv) . (`runContT` return) . evalL

run :: EvalState -> Expr -> IO (Value, EvalState)
run st =
    (`runStateT` st) . (`runContT` return) . evalL

evalL x = eval x >>= bind "LAST" . EVal

errorWrapT fsuccess m =
    runErrorT m >>= return . either VError fsuccess

evalString e = lift (eval e) >>= asString
evalTable e  = lift (eval e) >>= asTable


{- | Evaluate an expression to result in a Value -}

eval :: Expr -> Eval r Value

eval (EVal v)
    = return v

eval (EBind lvalue ev)
    | EVar ident <- lvalue = bind ident ev
    | otherwise            = return . VError $
                             "cannot bind to non-lvalue: " ++ pp lvalue

eval (EVar ident)
    = gets stEnv >>=
      return . clVal . Map.findWithDefault nullClosure ident . envMap

eval (EUOp op x)
    = eval x >>= return . uOp op

eval (EBinOp op l r)
    = do
      lval <- eval l
      rval <- eval r
      return $ binOp op lval rval

eval (ESeries es)
    | es == []  = return VNull
    | otherwise = foldr1 (>>) $ map evalL es

eval (ESelect etarget eselect) = do
    target <- eval etarget
    case target of
        VVector vec  -> eval eselect >>= return . VVector . selectVector vec
        VTable table -> select table eselect
        _ -> return . VError $ "selection applies only to tables and vectors"

eval (EJoin joinType eltarg ertarg) =
    return . (VError `either` VTable) =<< runErrorT ( do
        ltable <- lift (eval eltarg)
        unless (vIsTable ltable) (notTableError "left")
        rtable <- lift (eval ertarg)
        unless (vIsTable rtable) (notTableError "right")
        lt <- asTable ltable
        rt <- asTable rtable
        tableJoin joinType lt rt
        )
  where
    notTableError loc =
        throwError $ loc ++ " argument of join must be a table"

eval (EProject etarget pspec) = do
    target <- eval etarget
    case target of
        VTable table -> project table pspec
        _            -> return . VError $ "first operand of $ must be a table"

eval (EReadCsv efile) =
    errorWrapT VTable $ loadCsvTable =<< argof "read.csv" (evalString efile)

eval (EReadWsv efile) =
    errorWrapT VTable $ loadWsvTable =<< argof "read.csv" (evalString efile)

eval (EWriteWsv etable efile) =
    errorWrapT (const VNull) $ do
        table <- arg1of nm $ evalTable etable
        file  <- arg2of nm $ evalString efile
        r <- liftIO $ try (writeFile file (pp table ++ "\n"))
        case r of
            Left err -> throwError (show err)
            Right _  -> return ()
  where
    nm = "write.csv"

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
    result <- (return . (VError `either` VTable)) =<< runErrorT ( do
        selections <- lift $ evalRows table [expr] >>= return . map head
        vecs' <- mapM (liftM catMaybes . zipWithM sel1 selections . vlist) vecs
        return $ table { tvecs = listArray (bounds tvts) $
                                 zipWith mkVectorOfType origTypes vecs' }
        )
    put env
    return result
  where
    tvts      = tvecs table
    vecs      = elems tvts
    origTypes = map vtype vecs

sel1 val x =
    return $ case keepNAs toSLog $ toScalar val of
        SLog False -> Nothing
        SLog True  -> Just x
        _          -> Just SNa


-- projection of table

project table (PSVectorNum n) =
    return . (VError `either` (VVector . (tvecs table !))) $
    tableColumnIndexCheck table n

project table (PSVectorName s) =
    ((return . VError) `either` (project table . PSVectorNum)) $
    tableColumnLookupIndex table s

project table (PSTable True pscols) = do
    ((return . VError) `either` (project table . PSTable False)) =<<
        runErrorT ( do
            colIndexes <- mapM getIndex (removeStars pscols)
            return . map PSCNum $ range (bounds $ tcols table) \\ colIndexes
            )
  where
    getIndex (PSCNum n)   = tableColumnIndexCheck table n
    getIndex (PSCName s)  = tableColumnLookupIndex table s
    getIndex (PSCExp s _) = tableColumnLookupIndex table s

project table (PSTable False pscols) = do
    env <- gets stEnv -- remember pre-eval environemnt
    result <- return . (VError `either` VTable) =<< runErrorT ( do
        colNames <- mapM getName pscols'
        colExps  <- mapM getExp pscols'
        rows <- lift $ evalRows table colExps
        return $ mkTable $ zip colNames (map toVector $ transpose rows)
        )
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

uOp :: UnaryOp -> Value -> Value
uOp UOpNegate x        = binOp BinOpTimes (VVector negOne) x
uOp UOpNot (VVector x) = VVector . vmap logNot $ vectorCoerce VTLog x
uOp UOpNot _           = VError $ "operand of (!) must be a vector"

logNot (SLog x) = SLog (not x)
logNot _        = SNa

negOne = V VTNum 1 [SNum (-1.0)]

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
binOp BinOpLt       = cmpOp (<)
binOp BinOpLe       = cmpOp (<=)
binOp BinOpGt       = cmpOp (>)
binOp BinOpGe       = cmpOp (>=)

binOp BinOpSOr      = scalarize logOp (||)
binOp BinOpSAnd     = scalarize logOp (&&)
binOp BinOpVOr      = logOp (||)
binOp BinOpVAnd     = logOp (&&)

cmpOp op x y = vectorize (propNa ((SLog.) . withBestType op)) x y
numOp op x y = vectorize (propNa (binWrap SNum valNum op)) x y
logOp op x y = vectorize (propNa (binWrap SLog valLog op)) x y

scalarize f op x y =
    mkVectorValue [toScalar (f op x y)]

doEllipses start end =
    fromMaybe err $ do
        s <- asNum start
        e <- asNum end
        return $ mkVectorValue $ map SNum [ s .. e ]
  where
    err = VError $ "both operands to the (:) operator must be vectors"

valNum x =
    case toSNum x of
        SNum v -> v
        _      -> error $ "cannot coerce into numeric: (" ++ show x ++ ")"

valLog x =
    case toSLog x of
        SLog b -> b
        _      -> error $ "cannot coerce into logical: (" ++ show x ++ ")"

binWrap wrapper argfn op l r = wrapper (argfn l `op` argfn r)

withBestType :: (Scalar -> Scalar -> a) -> Scalar -> Scalar -> a
withBestType f l r =
    f l' r'
  where
    [l', r'] = vlist $ toVector [l, r]

propNa _ SNa _   = SNa
propNa _ _   SNa = SNa
propNa f a   b   = f a b

vectorize op x y =
    either VError VVector $ vectorize' op x y

vectorize' op (VVector vx) (VVector vy) =
    return . toVector . take len $ zipWith op vx' vy'
  where
    len = maximum (map vlen [vx, vy])
    vx' = cycle (vlist vx)
    vy' = cycle (vlist vy)

vectorize' _ _ _ = throwError "vector operation requires two vectors"
