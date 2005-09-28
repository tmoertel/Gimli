{-# OPTIONS -fglasgow-exts #-}

module EvalKernel (
    EvalState, EvalError, LogS,
    Eval, runEval,
    enterNewScope,
    lookupBinding, bindVal, bindValExpr,
    newTopLevel,
    clExp, clVal
)
where

import Control.Monad
import Control.Monad.RWS
import Control.Monad.Cont
import Control.Monad.Error
import Data.IORef

import qualified Data.Map as Map

import Expr



-- ============================================================================
-- core monad
-- ============================================================================

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


-- ===========================================================================
-- Eval monad
-- ===========================================================================

type EvalError   = String
type EvalState   = FrameStack
type LogS        = [String] -> [String]
type Eval r a    = EvalM EvalError FrameStack () LogS r a

type FrameStack  = [Frame]
type Env         = Map.Map Identifier Closure
data Frame       = Frame
    { frameEnvRef :: IORef Env
    }

type Closure = (Value, Maybe Expr)

clVal = fst
clExp = snd

nullClosure = (VNull, Nothing)

newFrame :: IO Frame
newFrame = do
    env <- newIORef emptyEnv
    return $ Frame { frameEnvRef = env }

newTopLevel :: IO FrameStack
newTopLevel = do
    fr <- newFrame
    return [fr]

getLocalFrame :: Eval r Frame
getLocalFrame =
    asks $ head

modifyFrameEnv :: (Env -> Env) -> Frame -> Eval r Env
modifyFrameEnv f frame = liftIO $ do
    let envRef = frameEnvRef frame
    env <- readIORef envRef
    let env' = f env
    writeIORef envRef env'
    return env'

modifyLocalEnv :: (Env -> Env) -> Eval r Env
modifyLocalEnv f =
    modifyFrameEnv f =<< getLocalFrame

enterNewScope :: Eval r a -> Eval r a
enterNewScope m = do
    fr <- liftIO newFrame
    local (fr:) m

emptyEnv :: Env
emptyEnv  = Map.empty

bindVal :: Identifier -> Value -> Eval r Value
bindVal ident val =
    bindValExpr ident val Nothing

bindValExpr :: Identifier -> Value -> Maybe Expr -> Eval r Value
bindValExpr ident val valExpr = do
    modifyLocalEnv $ Map.insert ident (val, valExpr)
    return val

lookupBinding :: Identifier -> Eval r (Maybe Closure)
lookupBinding s = do
    envRefStack <- asks (map frameEnvRef)
    callCC $ \esc -> do
        mapM_ (search esc s) envRefStack
        return Nothing
  where
    search esc s envRef = do
       env <- liftIO $ readIORef envRef
       case Map.lookup s env of
           Just cl -> esc (Just cl)
           _       -> return Nothing

