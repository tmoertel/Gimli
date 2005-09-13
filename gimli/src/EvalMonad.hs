{-# OPTIONS -fglasgow-exts #-}

module EvalMonad (
    EvalM,
    runEval, shiftEval, resetEval
)
where

import Control.Monad
import Control.Monad.RWS
import Control.Monad.Cont
import Control.Monad.Error

type EvalM err env state log prompt a =
    ErrorT err (ContT prompt (RWST env log state IO)) a

runEval :: (Monoid log) =>
           env    -- ^ Local environment
        -> state  -- ^ Global state
        -> EvalM err env state log (Either err a) a -- ^ Action to run
        -> IO (Either err a, state, log) -- ^ Result of running action
runEval env state m =
    (\m' -> runRWST m' env state) . (`runContT` return) $ runErrorT m


-- shift/reset for the ContT monad transformer and Eval monad

shift e     = ContT $ \k -> runContT (e $ \v -> ContT $ \c -> k v >>= c) return
reset e     = ContT $ \k -> runContT e return >>= k
shiftEval e = ErrorT $ shift $ \c -> runErrorT (e (ErrorT . c . Right))
resetEval m = ErrorT $ reset (runErrorT m)


{-

-- testing / debugging

type EvalR r a    = EvalM String Ctx String ([String]->[String]) r a
type Eval a       = EvalR a a
type EvalCont     = String -> Eval String



data Ctx = Ctx
    { ctxReturn   :: Maybe EvalCont
    , ctxYield    :: Maybe EvalCont
    , ctxDepth    :: Int
    , ctxBindings :: [(String, String)]
    }

emptyCtx = Ctx
    { ctxReturn   = Nothing
    , ctxYield    = Nothing
    , ctxDepth    = 0
    , ctxBindings = []
    }


printEval m = do
    (a, s, w) <- runEval emptyCtx "" m
    print (a, s)
    putStrLn "-- log --"
    putStr $ unlines (w [])

liftRWST m = (lift . lift) m
liftCont m = lift m

logMsg msg = liftRWST $ tell ([msg]++)
lcl  m     = local (const "local") m
st m       = put "state" >> m
ccc m      = callCC (\esc -> m >>= esc)

ccl m      = callCC $ \esc -> lcl (m >>= esc)


enterScope m = do
    resetEval $ local (\ctx -> ctx { ctxDepth = ctxDepth ctx + 1 }) m

modifyLocalBindings f ctx =
    ctx { ctxBindings = f (ctxBindings ctx) }

logBindings msg =
    asks ctxBindings >>= logMsg . ((msg ++ " bindings: ") ++) . show

lb m = logBindings m

bindLocal var val =
    shiftEval $ \cont -> local (modifyLocalBindings ((var,val):)) (cont ())

var s =
    asks ctxBindings >>=
    maybe (throwError $ "no variable " ++ s) return  . lookup s

vprint s = var s >>= logMsg . ((s ++ " = ") ++)

nop :: EvalR r ()
nop = return ()

test1 = testx nop nop nop

testx m0 m1 m2 = do
    logBindings "top level"
    m0
    enterScope $ do
        logBindings "> before bind x"
        x =:= "one"
        vprint x
        logBindings "> after bind x"
        x =:= "one (again)"
        m1
        logBindings "> after bind x again"
        enterScope $ do
            "y" =:= "two"
            m2
            logBindings ">> after bind y"
        logBindings "> (exited nested scope)"
    logBindings "(back to top level)"
  where
    name =:= val = bindLocal name val
    x = "x"
-}

    
{- output

*EvalMonad> printEval test1
(Right (),"s")
-- log --
top level bindings: []
> before bind x bindings: []
> after bind x bindings: [("x","one")]
> after bind x again bindings: [("x","one (again)"),("x","one")]
>> after bind y bindings: [("y","two"),("x","one (again)"),("x","one")]
> (exited nested scope) bindings: [("x","one (again)"),("x","one")]
(back to top level) bindings: []

-}
