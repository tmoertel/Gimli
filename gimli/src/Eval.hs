{-# OPTIONS -fglasgow-exts #-}

module Eval (
    eval, run, emptyEnv,
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
import Data.List (group, intersperse, transpose, (\\))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe

import EvalMonad
import Expr
import LoadData
import PPrint
import Utils

type Closure    = (Value, Maybe Expr)

clEnv _     = error "no environment in closure"
clVal (v,_) = v
clExp (v,x) = x

nullClosure = (VNull, Nothing)

type EnvMap     = Map.Map Identifier Closure
data Env        = Env EnvMap deriving (Show, Eq, Ord)

emptyEnv :: Env
emptyEnv  = Env Map.empty

modifyEnv :: (EnvMap -> EnvMap) -> Env -> Env
modifyEnv f (Env emap) = Env (f emap)

envMap (Env emap) = emap

-- ===========================================================================
-- Eval monad
-- ===========================================================================

type EvalError   = String
type EvalState   = Env
type LogS        = [String] -> [String]
type Eval r a    = EvalM EvalError EnvMap EvalState LogS r a

stEnv = id

bind ident valExpr = do
    val <- eval valExpr
    env <- gets stEnv
    modify $ modifyEnv $ Map.insert ident (val, Just valExpr)
    return val

run :: EvalState -> Expr -> IO (Either EvalError Value, EvalState, LogS)
run st =
    runEval Map.empty st . evalL

evalL x = eval x >>= bind "LAST" . EVal

evalString e = eval e >>= asString
evalTable e  = eval e >>= asTable
evalBool e   = eval e >>= asBool
evalVector e = eval e >>= asVector


-- ===========================================================================
{- | Evaluate an expression to result in a Value -}
-- ===========================================================================

eval :: Expr -> Eval r Value

eval (EVal v) =
    return v

eval (EApp (EVal (VVector (V _ _ [SStr fname]))) args) =
    eval (EApp (EVar fname) args)

eval (EApp fnExp argExps) = do
    fn <- eval fnExp
    case fn of
        VPrim prim      -> doPrim prim argExps
        VFunc args prog -> throwError "non-primitive functions not implemented"
        x               -> throwError "cannot apply non-function"

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

eval (EIf etest etrue maybeEfalse) = do
    doIfBody etest etrue maybeEfalse id

eval (EUnless etest etrue maybeEfalse) = do
    doIfBody etest etrue maybeEfalse not

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


-- if/unless helpers

doIfBody etest etrue maybeEfalse trueTest = do
    testResult <- eval etest
    if trueTest (test testResult)
        then eval etrue
        else maybe (return testResult) eval maybeEfalse




-- ============================================================================
-- vector operations
-- ============================================================================

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


-- ============================================================================
-- table operations
-- ============================================================================

savingEnv m = do
    env <- gets stEnv
    result <- m `catchError` \e -> put env >> throwError e
    put env
    return result


-- joins

tableJoin JCartesian tl tr =
    return $ mkTable (zip colnames colvecs)
  where
    (lrs, rrs) = both trows (tl, tr)
    colvecs    = zipWith mkVectorOfType coltypes (transpose rows)
    rows       = [ lr ++ rr | lr <- lrs, rr <- rrs ]
    colnames   = uniqify . uncurry (++) $ both tcnames (tl, tr)
    coltypes   = uncurry (++) (both tctypes (tl, tr))

tableJoin (JNatural il [] [] ir) tl tr =
    tableJoin (JNatural il sharedColumns sharedColumns ir) tl tr
  where
    sharedColumns =
        map EVar . withDefault ["ROW.ID"] $
        sharedStrings (tcnames tl) (tcnames tr)

tableJoin (JNatural il lexps [] ir) tl tr =
    tableJoin (JNatural il lexps lexps ir) tl tr

tableJoin (JNatural il [] rexps ir) tl tr =
    tableJoin (JNatural il rexps rexps ir) tl tr

tableJoin (JNatural il lexps rexps ir) tl tr = savingEnv $ do
    lassocs <- liftM (`zip` trows tl) (evalRows tl lexps)
    rmap    <- buildRowMap tr rexps
    return $
        naturalJoin (tl, lassocs, il==JOuter) (tr, rmap, rexps, ir==JOuter)

naturalJoin (tl, lassocs, outL) (tr, rmap, rexps, outR) =
    mkTable (zip colnames colvecs)
  where
    colnames     = uniqify (keepers (tcnames tl ++ tcnames tr))
    colvecs      = zipWith mkVectorOfType coltypes (keepers $ transpose rows)
    coltypes     = keepers (tctypes tl ++ tctypes tr)
    keepers xs   = [ x | (True, x) <- zip wantedCols xs ]
    wantedCols   = [ True | _ <- tcnames tl ] ++
                   map (\c -> outR || Set.member c wantedRCols) (tcnames tr)
    wantedRCols  = Set.fromList (dropJoinVars rexps tr)
    rows         = naturalRows (tl, lassocs, outL) (tr, rmap, outR)

naturalRows (tl, lassocs, outL) (tr, rmap, outR) =
    rowsl ++ if outR then rowsr else []
  where
    rowsl = [ l ++ r | (k,l) <- lassocs
                     , r <- Map.findWithDefault defL k rmap ]
    (nasL,nasR) = both ((`replicate` SNa) . rangeSize . bounds . tcols) (tl,tr)
    defL  = if outL then [nasR] else []
    rowsr = if outR then [nasL ++ r | r <- rdiff] else []
    rdiff = concat $ Map.elems (rmap `Map.difference` Map.fromList lassocs)

dropJoinVars es table =
    tcnames table \\ joinVars es table

joinVars es table =
    sharedStrings [s | EVar s <- es] (tcnames table)

buildRowMap table exps = do
    rowResults <- evalRows table exps
    return $ (Map.fromListWith (flip (++)))
             (rowResults `zip` map (:[]) (trows table))

withDefault l [] = l
withDefault _ r  = r

sharedStrings xs ys =
    Set.toList $
    uncurry Set.intersection $
    both Set.fromList (xs, ys)


-- selection of table

select table expr = savingEnv $ do
    selections <- evalRows table [expr] >>= return . map head
    vecs' <- mapM (liftM catMaybes . zipWithM sel1 selections . vlist) vecs
    return . VTable $
           table { tvecs = listArray (bounds tvts) $
                   zipWith mkVectorOfType origTypes vecs' }
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

project table (PSTableOverlay nvps) =
    project table (PSTable False pscols)
  where
    pscols      = map expForCol (colNames ++ newNvpCols)
    colNames    = tcnames table
    colNamesMap = Map.fromList [(n,EVar n) | n <- colNames]
    overlayMap  = Map.fromList nvps
    newNvpCols  = filter (not . flip Map.member colNamesMap) $ map fst nvps
    expForCol c = PSCNExpr c . fromJust $
                  Map.lookup c overlayMap `mplus` Map.lookup c colNamesMap

project table (PSTable True pscols) = do
    colIndexes <- mapM getIndex =<< expandSpecials table (removeStars pscols)
    project table . PSTable False . map PSCNum $
        range (bounds $ tcols table) \\ colIndexes
  where
    getIndex (PSCNum n)     = tableColumnIndexCheck table n
    getIndex (PSCName s)    = tableColumnLookupIndex table s
    getIndex (PSCNExpr s _) = tableColumnLookupIndex table s

project table (PSTable False pscols) = savingEnv $ do
    pscols'  <- expandSpecials table pscols
    colNames <- mapM getName pscols'
    colExps  <- mapM getExp pscols'
    rows <- evalRows table colExps
    return $ VTable $ mkTable $
           zip colNames (map toVector $ transpose rows)
  where
    getName                  = pscol id fst
    getExp                   = pscol EVar snd
    pscol f g (PSCNum n)     = tableColumnIndexCheck table n >>=
                               return . f . (tcols table !)
    pscol f g (PSCName s)    = tableColumnLookupIndex table s >>=
                               pscol f g . PSCNum
    pscol f g (PSCNExpr s e) = return $ g (s,e)

expandSpecials table pscs =
    liftM concat (mapM evalPS (expandStars table pscs))

expandStars table =
    concatMap starExpand
  where
    starExpand PSCStar = map PSCNum (range . bounds $ tcols table)
    starExpand x       = [x]

evalPS :: PSCol -> Eval r [PSCol]
evalPS (PSCExpr e) = do
    vec <- argof nm (evalVector e)
    return $ mapMaybe toCol (vlist vec)
  where
    nm = "projection-specification subexpression"
    toCol x = case x of
        SNum n -> Just $ PSCNum (round n)
        SStr s -> Just $ PSCName s
        _      -> Nothing


evalPS psc = return [psc]


evalRows :: Table -> [Expr] -> Eval r [[Value]]
evalRows table colExps = do
    zipWithM projectRow [1..] (trows table)
  where
    projectRow rid row = do
        bindCols row
        bindScalar "ROW.ID" (SNum (fromIntegral rid))
        mapM eval colExps
    bindCols = zipWithM bindScalar (tcnames table)

bindScalar ident x =
    bind ident (EVal $ mkVectorValue [x])

removeStars =
    filter (not . isStar)
  where
    isStar PSCStar = True
    isStar _       = False


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

binOp BinOpPower    = numOp (**)

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



-- ============================================================================
-- primitive functions
-- ============================================================================

doPrim :: Primitive -> [Expr] -> Eval r Value
doPrim (prim@Prim { primName=name }) args =
    case name of
    "in"        -> prim2 primIn
    "is.na"     -> prim1 primIsNa
    "read.csv"  -> prim1 primReadCsv
    "read.tsv"  -> prim1 primReadTsv
    "read.wsv"  -> prim1 primReadWsv
    "write.csv" -> prim2 primWriteCsv
    "write.tsv" -> prim2 primWriteTsv
    "write.wsv" -> prim2 primWriteWsv
  where
    prim1 f = case args of
        [x] -> f name x
        _   -> argErr 1
    prim2 f = case args of
        [x,y] -> f name x y
        _     -> argErr 2
    argErr n = throwError $ name ++ " requires " ++ show n
                                 ++ " argument(s), not " ++ show (length args)

primIn nm velems vset = do
    es  <- arg1of nm $ liftM vlist (evalVector velems)
    set <- arg2of nm $ liftM (Set.fromList . vlist) (evalVector vset)
    return . mkVectorValue $ map (SLog . (`Set.member` set)) es

primIsNa nm arg = do
    argVal <- eval arg
    case argVal of
        VVector v -> vectorize (binWrap SLog return (==)) naVecVal argVal
        _         -> return $ VVector falseVector
  where
    naVecVal = VVector naVector

primReadCsv = primReadX loadCsvTable
primReadTsv = primReadX loadTsvTable
primReadWsv = primReadX loadWsvTable

primReadX parser nm efile =
    liftM VTable $ parser =<< argof nm (evalString efile)

primWriteCsv = primWriteX (printXsv ",")
primWriteTsv = primWriteX (printXsv "\t")
primWriteWsv = primWriteX (\v -> pp v ++ "\n")

printXsv sep table =
    unlines . map (concat . intersperse sep) . (headings:) $ map (map pp) rows
  where
    headings = tcnames table
    rows     = trows table

primWriteX printer nm etable efile = do
    table <- arg1of nm $ evalTable etable
    file  <- arg2of nm $ evalString efile
    during nm $ do
        result <- liftIO . try $ writeFile file (printer table)
        case result of
            Left err -> throwError (show err)
            Right x  -> return (mkVectorValue [SStr file])
