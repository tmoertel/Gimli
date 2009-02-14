{-# LANGUAGE ScopedTypeVariables, PatternGuards, FlexibleContexts #-}

module Eval (
    Eval, evaluate
) where

import Control.Exception (IOException, try)
import Control.Monad.Error
import Data.Array
import Data.Either
import Data.IORef
import Data.List ( group, intersperse, mapAccumL, sort, transpose, (\\)
                 , genericLength )
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Text.Regex

import EvalKernel
import Expr
import HasNames
import LoadData
import PPrint
import Utils
import Glob

import SourcePos

import Debug.Trace

-- ============================================================================
-- Type for expressing Gimli-evaluatable actions
-- ============================================================================

type Eval r a = EvalG r Value Expr a


-- ============================================================================
-- top-level evaluation
-- ============================================================================

evaluate :: EvalCtx Value Expr -- ^ context of evaluation
         -> Expr               -- ^ expression to evaluate
         -> IO (Either EvalError Value, EvalCtx Value Expr, LogS)
            -- ^ action carrying out the evaluation
evaluate st expr = do
    (errOrVal, _, log) <- runEval st () (evalL expr)
    return (errOrVal, st, log)


-- ===========================================================================
{- | Evaluate an expression to result in a Value -}
-- ===========================================================================

-- eval and bind to LAST variable

evalL x = evalAndBind "LAST" x


-- eval

eval :: Expr -> Eval r Value

eval (Expr e st ed) =
    catchError (eval' e) describe
  where
    describe err = throwError $ "from " ++ posn ++ ": " ++ err
    posn = sposName st ++ ":" ++ show (sposLine st) ++ ":" ++ show (sposCol st)

eval' (EVal v) =
    return v

eval' (EFunc args body) =
    liftM (VFunc args body) getScope

eval' (EApp (Expr (EVal (VVector (V _ _ [SStr fname]))) st ed) args) =
    eval' (EApp (Expr (EVar fname) st ed) args)

eval' (EApp fnExp givenArgs) =
    during "function application" $ do
    fn <- eval fnExp
    case fn of
        VPrim prim          -> doPrim prim givenArgs
        VFunc args prog ctx -> doFnCall (args, prog, ctx) givenArgs
        x                   -> throwError "cannot call a non-function"

eval' (EVector es) = do
    vecs <- argof "vector constructor" $ mapM evalVectorNull es
    return . vectorOrNull . mkVector $ concatMap vlist vecs

eval' (EBind lvalue ev)
    = doBind "<-" evalAndBind lvalue ev

eval' (EBindOver lvalue ev)
    = doBind "<<-" evalAndBindOver lvalue ev

eval' (ELocal e) = do
    enterNewScope (eval e)

eval' (EVar ident) =
    getBindingValue ident

eval' (EUOp op x) =
    eval x >>= uOp op

eval' (EBinOp op el er) = do
    l <- eval el
    r <- eval er
    binOp op l r

eval' (ESeries es) =
    foldM (\_ e -> evalL e) VNull es

eval' (EBlock es) =
    foldM (\_ e -> evalL e) VNull es

eval' (EIf etest etrue maybeEfalse) = do
    doIfBody etest etrue maybeEfalse id

eval' (EUnless etest etrue maybeEfalse) = do
    doIfBody etest etrue maybeEfalse not

eval' (EFor var ecoll eblk) = do
    coll <- arg1of nm (evalCollection ecoll)
    during nm $ foldM bindAndEval VNull coll
  where
    nm = "for " ++ var ++ " in ..."
    bindAndEval :: Value -> Value -> Eval r Value
    bindAndEval _ val = do
        bindVal var val
        eval eblk

eval' (EList args) =
    during "list constructor" $ do
    vals  <- mapM (eval . gaExpr) args
    return . VList . mkGList $
        zipWith (\a v -> (gaName a, v)) args vals

eval' (EParens e) =
    eval e

eval' (ESelect etarget eselect) =
    during "table selection" $ do
    target <- eval etarget
    case target of
        VNull        -> getVectorCriteria >>= selectVector emptyVector
        VVector vec  -> getVectorCriteria >>= selectVector vec
        VList gl     -> getVectorCriteria >>= selectList gl
        VTable table -> select table eselect
        _ -> throwError $
             "selection applies only to vectors, lists, and tables"
  where
    getVectorCriteria = eval eselect >>= asVectorNull

eval' (EJoin joinType eltarg ertarg) = do
    ltable <- arg1of nm (evalTable eltarg)
    rtable <- arg2of nm (evalTable ertarg)
    liftM VTable (tableJoin joinType ltable rtable)
  where
    nm = "join"
    notTableError loc =
        throwError $ loc ++ " argument of join must be a table"

eval' (EProject etarget pspec) = do
    target <- arg1of "$" $ eval etarget
    case target of
        VList gl     -> during "projection on list" $ projectList gl pspec
        VTable table -> during "projection on table" $ project table pspec
        _            -> throwError $
                        "projection applies only to lists and tables"

eval' (ETable tspecs) =
    during "table constructor" (constructTable tspecs)

-- variable-lookup helper

getBindingValue ident = do
    b <- lookupBinding ident
    case b of
        Just cl -> return (clVal cl)
        _       -> throwError $ "variable \"" ++ ident ++ "\" not found"


-- binding helper

doBind nm binder lvalue ev
    | (Expr (EVar ident) _ _) <- lvalue
    = binder ident ev
    | otherwise
    = during nm $ do
          ident <- evalString lvalue
          binder ident ev

-- error-reporting helpers

during :: MonadError String m => String -> m a -> m a
during operation m =
    catchError m describe
  where
    describe err =
        throwError $ "during " ++ operation ++ ": " ++ err

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


-- typed-value helpers

evalString e     = eval e >>= asString
evalTable e      = eval e >>= asTable
evalBool e       = eval e >>= asBool
evalVector e     = eval e >>= asVector
evalVectorNull e = eval e >>= asVectorNull
evalCollection e = do
    v <- eval e
    case v of
        VNull       -> return []
        VVector vec -> return [mkVectorValue [x] | x <- vlist vec]
        VList gl    -> return $ elems (glvals gl)
        x           -> throwError $ "expecting vector or list"

-- binding helper

evalAndBind :: Identifier -> Expr -> Eval r Value
evalAndBind ident valExpr = do
    val <- eval valExpr
    bindValExpr ident val (Just valExpr)

evalAndBindOver :: Identifier -> Expr -> Eval r Value
evalAndBindOver ident valExpr = do
    val <- eval valExpr
    bindOverValExpr ident val (Just valExpr)


-- function-call helper

doFnCall :: (ArgList, Expr, EvalCtx Value Expr) -> [GivenArg] -> Eval r Value
doFnCall (formalArgs, body, ctx) givenArgs = do

    (givenBinds, defaultBinds) <- computeFnBindings formalArgs givenArgs

    -- evaluate given expressions in the current (calling) context

    givenValBindings <- toValueBindings givenBinds

    -- enter function's context and then create a new scope for local bindings

    withinScope ctx . enterNewScope $ do

        -- bind values to formal arguments

        mapM_ (uncurry bindVal) givenValBindings
        mapM_ (uncurry bindVal) =<< toValueBindings defaultBinds

        -- finally, evaluate the function body

        eval body

toValueBindings neps = mapM (\(n,e) -> liftM ((,) n) (eval e)) neps



-- functional-call bindings helper

computeFnBindings :: ArgList    -- ^ formal arguments
           -> [GivenArg] -- ^ arguments passed to function
           -> Eval r ([(Identifier, Expr)],[(Identifier, Expr)]) -- ^ bindings
computeFnBindings formalArgs givenArgs = do

    -- match args first by name

    inexactBinds <- liftM Map.fromList $
                    mapM inexactMatch (Map.toList namedRest)

    -- and then by position

    let formalUnmatched = formalRest `Map.difference` inexactBinds
    let formalByPos = [(n,e) | arg <- argList formalArgs
                             , let (n,e) = pair (argName, argDefault) arg
                             , n `Map.member` formalUnmatched]
    when (length formalByPos < length orderGivens) $
        throwError $ "too many arguments given"
    let posBinds = Map.fromList $ zip (map fst formalByPos) orderGivens

    -- summarize all formals bound to given arguments

    let givenBinds = foldl1 Map.union [exactBinds, inexactBinds, posBinds]

    -- the remaining unbound formals must have defaults

    let unboundFormals = argMap formalArgs `Map.difference` givenBinds
    defaultBinds <- (`mapM` Map.toList unboundFormals) $ \(nm, def) ->
        case def of
            Just e -> return (nm, e)
            _      -> throwError $
                      "no value provided for required argument \"" ++ nm++ "\""

    return (Map.toList givenBinds, defaultBinds)

  where
    namedGivens = Map.fromList [ (n,e) | GivenArg (Just n) e <- givenArgs ]
    orderGivens = [ e | GivenArg Nothing e <- givenArgs ]

    exactBinds  = namedGivens `Map.intersection` formals
    formalRest  = formals `Map.difference` exactBinds
    namedRest   = namedGivens `Map.difference` exactBinds

    formals     = argMap formalArgs

    inexactMatch :: (String, Expr) -> Eval r (String, Expr)
    inexactMatch (nm, e) = case Map.keys matches of
        []    -> throwError $ "no formal argument matches \"" ++nm++ "\""
        [fnm] -> return (fnm, e)
        fnms  -> throwError $ "argument \"" ++ nm ++ "\" is ambiguous; "
                          ++ "matches " ++ show fnms
      where
        (_, greater) = Map.split nm formalRest
        (matches,_)  = Map.split (nm++"~") greater

    toValueBindings neps = mapM (\(n,e) -> liftM ((,) n) (eval e)) neps


-- ============================================================================
-- vector/list selection
-- ============================================================================

-- select elements _es_ from vector _vec_:

selectVector (V tvtype _ txs) (V VTNum _ sxs) =
    return (vectorOrNull $ V tvtype (length xs) xs)
  where
    xs   = map pull (selectIndices [1..length txs] sxs)
    txs' = txs ++ repeat SNa
    pull (SNum n)
        | n > 0   = txs' !! (round n - 1)
    pull _        = SNa

selectVector (V tvtype _ txs) (V VTLog _ sxs) =
    return (vectorOrNull $ V tvtype (length xs) xs)
  where
    xs   = catMaybes $ zipWith (takeLogical SNa) txs (ncycle sxs)

selectVector _ _ =
    throwError "vector-selection criteria must be a vector"


-- select elements _es_ from list _gl_:

selectList gl (V VTNum _ sxs) =
    liftM (VList . mkGListNamed) $ mapM pull (selectIndices (range bnds) sxs)
  where
    vals   = glvals gl
    names  = glnames gl
    bnds   = bounds vals
    pull (SNum i) = do
             let i' = round i
             when (not $ inRange bnds i') $
                 throwError $ "list index " ++ show i ++ " is out of range "
                              ++ show bnds
             return (names ! i', vals ! i')
    pull _ = return naListElem

selectList gl (V VTStr _ sxs) =
    liftM (VList . mkGListNamed) $  mapM pull sxs
  where
    pull (SStr s) = glistLookupIndex gl s >>= \n -> return (names!n, vals!n)
    pull _        = return naListElem
    (names, vals) = pair (glnames, glvals) gl

selectList gl (V VTLog _ sxs) =
    return . VList . mkGListNamed . catMaybes $
        zipWith (takeLogical naListElem) (glpairs gl) (ncycle sxs)


naListElem = ("", mkVectorValue [SNa])

-- helpers

selectIndices indices sxs =
    case [ sx | sx@(SNum n) <- sxs, n < 0 ] of
    _:_ -> [ SNum (fromIntegral n)
             | n <- indices
             , not (negate n `Set.member` nsxs) ]
    _   -> sxs
  where
    nsxs = Set.fromList [ round n | SNum n <- sxs ]


takeLogical _  tx (SLog True) = Just tx
takeLogical na _  SNa         = Just na
takeLogical _  _  _           = Nothing


-- ============================================================================
-- list projection
-- ============================================================================

projectList gl pspec =
    liftM (head . elems . glvals . unVList) $ case pspec of
        PSVectorName nm ->
            selectList gl (V VTStr 1 [SStr nm])
        PSVectorNum  i  ->
            selectList gl (V VTNum 1 [SNum $ fromIntegral i])
        _ ->
            throwError "lists do not support complex projection"
  where
    unVList (VList gl) = gl

-- ============================================================================
-- table operations
-- ============================================================================

-- construction

constructTable tspecs = do
    ecolspecs <- during "argument evaluation" $ do
        toNvps . concat =<< mapM splice tspecs
    let names = map fst ecolspecs
    let evecs = map snd ecolspecs
    vecs <- argof nm $ mapM evalVector evecs
    let vlens = map vlen vecs
    if length (group vlens) == 1
        then return . VTable $ mkTable (zip names vecs)
        else throwError $
             "table columns must be non-empty vectors of equal length"
  where
    nm    = "table(...) constructor"
    splice (TCol envp)  = return [envp]
    splice (TSplice e)  = do
        val <- eval e
        case val of
            VTable t -> return $ zipWith mkNVP (tcnames t) (elems (tvecs t))
            VList gl -> liftM (zipWith mkNVP (map name . elems $ glnames gl)) $
                        mapM asVectorNull (elems $ glvals gl)
            _        -> throwError $
                "can't construct table columns from (" ++ show val ++ ")"
    mkNVP n vec = NVP n (mkNoPosExpr . EVal $ VVector vec)
    name ""     = "NA"
    name n      = n


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
        map (mkNoPosExpr . EVar) . withDefault ["ROW.ID"] $
        sharedStrings (tcnames tl) (tcnames tr)

tableJoin (JNatural il lexps [] ir) tl tr =
    tableJoin (JNatural il lexps lexps ir) tl tr

tableJoin (JNatural il [] rexps ir) tl tr =
    tableJoin (JNatural il rexps rexps ir) tl tr

tableJoin (JNatural il lexps rexps ir) tl tr = enterNewScope $ do
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
    sharedStrings [s | Expr (EVar s) _ _ <- es] (tcnames table)

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

select table expr = enterNewScope $ do
    selections <- evalRows table [expr] >>= return . map head
    let vecs' = map (catMaybes . zipWith sel1 selections . vlist) vecs
    return . VTable . mkTable . zip (tcnames table) $
        zipWith mkVectorOfType origTypes vecs'
  where
    tvts       = tvecs table
    vecs       = elems tvts
    origTypes  = map vtype vecs
    sel1 val x = if test val then Just x else Nothing

-- projection of table

toNvp :: ENVPair -> Eval r (String, Expr)
toNvp (ENVP ne e) = liftM (flip (,) e) (evalString ne)
toNvp (NVP n e)   = return (n, e)

toNvps = mapM toNvp

project table (PSVectorNum n) =
    liftM (VVector . (tvecs table !)) $ tableColumnIndexCheck table n

project table (PSVectorName s) =
    (project table . PSVectorNum) =<< tableColumnLookupIndex table s

project table (PSTableOverlay envps) = do
    nvps <- toNvps envps
    project table (PSTable False (pscols nvps))
  where
    pscols nvps = map expForCol (colNames ++ newNvpCols)
      where
        colNames    = tcnames table
        colNamesMap = Map.fromList [(n, mkNoPosExpr $ EVar n) | n <- colNames]
        overlayMap  = Map.fromList nvps
        newNvpCols  = filter (not . flip Map.member colNamesMap) $ map fst nvps
        expForCol c = PSCNExpr . NVP c . fromJust $
                      Map.lookup c overlayMap `mplus` Map.lookup c colNamesMap

project table (PSTable True pscols) = do
    colIndexes <- mapM getIndex =<< expandSpecials table (removeStars pscols)
    project table . PSTable False . map PSCNum $
        range (bounds $ tcols table) \\ colIndexes
  where
    getIndex (PSCNum n)      = tableColumnIndexCheck table n
    getIndex (PSCName s)     = tableColumnLookupIndex table s
    getIndex (PSCNExpr envp) = toNvp envp >>=
                               tableColumnLookupIndex table . fst

project table (PSTable False pscols) = enterNewScope $ do
    pscols'  <- expandSpecials table pscols
    colNames <- mapM getName pscols'
    colExps  <- mapM getExp pscols'
    rows <- evalRows table colExps
    return $ VTable $ mkTable $
           zip colNames (map toVector $ transpose rows)
  where
    getName                   = pscol id fst
    getExp                    = pscol (mkNoPosExpr . EVar) snd
    pscol f g (PSCNum n)      = tableColumnIndexCheck table n >>=
                                return . f . (tcols table !)
    pscol f g (PSCName s)     = tableColumnLookupIndex table s >>=
                                pscol f g . PSCNum
    pscol f g (PSCNExpr envp) = liftM g (toNvp envp)

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
    rowRef <- liftIO . newIORef $ mkRowArray (repeat undefined)
    let getVal n = liftM (\a -> mkVectorValue [a!n]) (readIORef rowRef)
    zipWithM (\c n -> bindIOVal c (getVal n)) ("ROW.ID" : tcnames table) [0..]
    zipWithM (projectRow rowRef) [1..] (trows table)
  where
    mkRowArray xs = listArray (0, length (tcnames table)) xs
    projectRow rowRef rid row = do
        liftIO . writeIORef rowRef $ mkRowArray (SNum (fromIntegral rid) : row)
        mapM eval colExps

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
uOp UOpNot v     = asVectorNull v >>=
                   return . vectorOrNull . vmap logNot . vectorCoerce VTLog

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

binOp BinOpConcat   = strOp (++)

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
strOp op x y = vectorize (propNa (binWrap SStr valStr op)) x y
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

valStr x =
    case toSStr x of
        SStr s -> return s
        _      -> throwError $ "cannot coerce into string: (" ++ show x ++ ")"

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
    return . vectorOrNull =<< vectorize' op x y

vectorize' op (VVector vx) (VVector vy) =
    liftM toVector $ zipWithM op (take len vx') (take len vy')
  where
    len = maximum (map vlen [vx, vy])
    vx' = ncycle (vlist vx)
    vy' = ncycle (vlist vy)

vectorize' _ _ _ = throwError "vector operation requires two vectors"



-- ============================================================================
-- primitive functions
-- ============================================================================

doPrim :: Primitive -> [GivenArg] -> Eval r Value
doPrim prim args = doPrim' prim (map gaExpr args) args

doPrim' (prim@Prim { primName=name }) args givenArgs =
    case name of
    "as.list"   -> primAsList name args
    "as.table"  -> primAsTable name args
    "in"        -> args2 primIn
    "glob"      -> argsFlatten primGlob
    "inspect"   -> primInspect args
    "is.na"     -> args1 primIsNa
    "length"    -> argsFlatten primLength
    "match"     -> primMatch name args
    "names"     -> args1 primNames
    "print"     -> primPrint args
    "read.csv"  -> argsR primReadCsv
    "read.tsv"  -> argsR primReadTsv
    "read.wsv"  -> argsR primReadWsv
    "sort"      -> argsFlatten primSort
    "write.csv" -> argsW primWriteCsv
    "write.tsv" -> argsW primWriteTsv
    "write.wsv" -> argsW primWriteWsv
    "uniq"      -> argsFlatten primUniq
    "var"       -> args1 primVar
  where
    args1 f = case args of
        [x] -> f name x
        _   -> argErr 1
    args2 f = case args of
        [x,y] -> f name x y
        _     -> argErr 2
    argsFlatten f = (f name . vlist) =<<
               argof name (mapM eval args >>= concatVals >>= asVectorNull)
    argErr n = throwError $ name ++ " requires " ++ show n
                                 ++ " argument(s), not " ++ show (length args)

    argsR f = doFnStylePrim f' formalsForRead givenArgs where
        f' = do file   <- getBindingValue "file"      >>= asString
                header <- getBindingValue "header"    >>= asBool
                tr     <- getBindingValue "transpose" >>= asBool
                f name file header tr

    formalsForRead = mkArgList $
        [ Arg "file"      $ Nothing
        , Arg "header"    $ Just (mkNoPosExpr $ EVal trueValue)
        , Arg "transpose" $ Just (mkNoPosExpr $ EVal falseValue)
        ]

    argsW f = doFnStylePrim f' formalsForWrite givenArgs where
        f' = do table  <- getBindingValue "table"     >>= asTable
                file   <- getBindingValue "file"      >>= asString
                header <- getBindingValue "header"    >>= asBool
                f name table file header

    formalsForWrite = mkArgList $
        [ Arg "table"     $ Nothing
        , Arg "file"      $ Nothing
        , Arg "header"    $ Just (mkNoPosExpr $ EVal trueValue)
        ]

doFnStylePrim body formalArgs givenArgs = do
    (givenBinds, defaultBinds) <- computeFnBindings formalArgs givenArgs
    givenValBindings <- toValueBindings givenBinds
    enterNewScope $ do
        mapM_ (uncurry bindVal) givenValBindings
        mapM_ (uncurry bindVal) =<< toValueBindings defaultBinds
        body

primAsList nm ethings =
    during nm $ do
    lists <- mapM ((coerceToGList =<<) . eval) ethings
    return . VList . mkGListNamed $ concatMap glpairs lists

coerceToGList val =
    return $ case val of
        VNull     -> mkGListVals []
        VList  gl -> gl
        VTable t  -> glmap VVector (tglist t)
        VVector v -> mkGListVals . (map (\x -> mkVectorValue [x])) $ vlist v
        x         -> mkGListVals [x]

primAsTable nm ethings =
    during nm $ do
    lists <- mapM ((coerceToGList =<<) . eval) ethings
    during nm $
        constructTable (map (TSplice . mkNoPosExpr . EVal . VList) lists)

primIn nm velems vset = do
    es  <- arg1of nm $ liftM vlist (evalVector velems)
    set <- arg2of nm $ liftM (Set.fromList . vlist) (evalVector vset)
    return . mkVectorValue $ map (SLog . (`Set.member` set)) es

primInspect args = do
    foldM (\_ expr -> inspect expr) VNull args
  where
    inspect expr = do
        val <- eval expr
        liftIO . putStrLn . ((pp expr ++ " => ") ++) . pp $ val
        return val

primPrint args = do
    foldM (\_ expr -> pr expr) VNull args
  where
    pr expr = do
        val <- eval expr
        liftIO . putStrLn . pp $ val
        return val

primIsNa nm arg = do
    argVal <- eval arg
    case argVal of
        VVector v -> vectorize (binWrap SLog return (==)) naVecVal argVal
        _         -> return $ VVector falseVector
  where
    naVecVal = VVector naVector

primLength _ xs = do
    return $ mkVectorValue [SNum (genericLength xs)]

primMatch nm args =
    during "match" $ do
    ss <- during "arguments" $ mapM evalString args
    case ss of
        [s,re]       -> doRegexMatch s re (True, True)
        [s,re,flags] -> doRegexMatch s re ( 's' `notElem` flags
                                          , 'i' `notElem` flags)
        _            -> throwError "2 or 3 arguments are required"
  where
    doRegexMatch s re (multiline, nocase) = return $
        case matchRegexAll (mkRegexWithOpts re multiline nocase) s of
            Nothing -> VNull
            Just (before, match, after, []) -> svec [before, match, after]
            Just (_     , _    , _    , xs) -> svec xs
    svec xs = mkVectorValue (map SStr xs)

primNames nm evalue =
    during "names" $ do
    val <- eval evalue
    names <- case val of
        VTable t -> return $ getNames t
        VList gl -> return $ getNames gl
        _        -> throwError "only lists and tables have names"
    return . mkVectorValue . map SStr $ names
  where

primReadCsv n f h tr = primReadX loadCsvTable n f h tr
primReadTsv n f h tr = primReadX loadTsvTable n f h tr
primReadWsv n f h tr = primReadX loadWsvTable n f h tr

primReadX parser nm file header tr =
    liftM VTable (parser file header tr)

primWriteCsv, primWriteTsv, primWriteWsv
    :: String -> Table -> String -> Bool -> Eval r Value
primWriteCsv n t f h = primWriteX (printXsv "," h)  n t f
primWriteTsv n t f h = primWriteX (printXsv "\t" h) n t f
primWriteWsv n t f h = primWriteX (printTable h)  n t f

printTable :: Bool -> Table -> String
printTable header table =
    (if header then id else unlines . tail . lines) (pp table ++ "\n")

printXsv sep header table =
    unlines . (if header then id else tail) .
    map (concat . intersperse sep) . (headings:) $ map (map pp) rows
  where
    headings = tcnames table
    rows     = trows table

primWriteX printer nm table file = do
    during nm $ do
        result <- liftIO . try $ writeFile file (printer table)
        case result of
            Left (err :: IOException) -> throwError (show err)
            Right x  -> return (mkVectorValue [SStr file])

primGlob nm ss = do
    pats <- argof nm $ do
        (`mapM` ss) $ \val -> case val of
            SStr s -> return s
            _      -> throwError "not a string"
    liftIO (mapM glob pats >>= return . mkVectorValue . map SStr . concat)


primSort nm xs = do
    return . mkVectorValue $ sort xs

primUniq nm xs = do
    return . mkVectorValue . concat . snd $ mapAccumL f Set.empty xs
  where
    f set x = (Set.insert x set, if Set.member x set then [] else [x])

primVar nm eVarName = do
    during nm $ do
        var <- during "argument" (evalString eVarName)
        getBindingValue var

-- null-handling cycle

ncycle [] = []
ncycle xs = cycle xs
