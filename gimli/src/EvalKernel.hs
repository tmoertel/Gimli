{-# OPTIONS -fglasgow-exts #-}

module EvalKernel (
    EvalCtx(..), EvalError, LogS,
    EvalG, runEval,
    getScope, enterNewScope, withinScope,
    lookupBinding, bindVal, bindValExpr, bindOverValExpr,
    newTopLevel,
    clExp, clVal
)
where

import Control.Monad
import Control.Monad.RWS
import Control.Monad.Cont
import Control.Monad.Error
import Data.IORef

import CoreTypes

import qualified Data.Map as Map



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
-- | Generic evaluation kernel parameterized over prompt type 'r',
--   value type 'v', expression type 'e', and result type 'a'.
-- ===========================================================================

type EvalError    = String
type LogS         = [String] -> [String]
type EvalG r v e a = EvalM EvalError (EvalCtx v e) () LogS r a

data EvalCtx v e = EvalCtx
                   { ctxFrames   :: FrameStack v e
                   , ctxTopLevel :: Frame v e
                   }
type Env v e     = Map.Map Identifier (Closure v e)

type FrameStack v e  = [Frame v e]
data Frame v e   = Frame
    { frameEnvRef :: IORef (Env v e)
    , frameNext   :: Maybe (Frame v e)
    }

type Closure v e = (v, Maybe e)

clVal = fst
clExp = snd

newFrame :: (Maybe (Frame v e)) -> IO (Frame v e)
newFrame nextFrame = do
    env <- newIORef emptyEnv
    return $ Frame { frameEnvRef = env, frameNext = nextFrame }

newTopLevel :: IO (EvalCtx v e)
newTopLevel = do
    fr <- newFrame Nothing
    return $ EvalCtx { ctxFrames = [fr], ctxTopLevel = fr }

modifyCtxFrames f ctx =
    ctx { ctxFrames = f (ctxFrames ctx) }

getLocalFrame :: EvalG r v e (Frame v e)
getLocalFrame =
    asks $ head . ctxFrames

modifyFrameEnv :: (Env v e -> Env v e) -> Frame v e -> EvalG r v e (Env v e)
modifyFrameEnv f frame = liftIO $ do
    let envRef = frameEnvRef frame
    env <- readIORef envRef
    let env' = f env
    writeIORef envRef env'
    return env'

modifyLocalEnv :: (Env v e -> Env v e) -> EvalG r v e (Env v e)
modifyLocalEnv f =
    modifyFrameEnv f =<< getLocalFrame

getScope :: EvalG r v e (EvalCtx v e)
getScope = ask

enterNewScope :: EvalG r v e a -> EvalG r v e a
enterNewScope m = do
    lf <- getLocalFrame
    fr <- liftIO $ newFrame (Just lf)
    local (modifyCtxFrames (fr:)) m

withinScope :: EvalCtx v e -> EvalG r v e a -> EvalG r v e a
withinScope ctx m =
    local (const ctx) m

emptyEnv :: Env v e
emptyEnv  = Map.empty

bindVal :: Identifier -> v -> EvalG r v e v
bindVal ident val =
    bindValExpr ident val Nothing

bindValExpr :: Identifier -> v -> Maybe e -> EvalG r v e v
bindValExpr ident val valExpr = do
    modifyLocalEnv $ Map.insert ident (val, valExpr)
    return val

bindOverValExpr :: Identifier -> v -> Maybe e -> EvalG r v e v
bindOverValExpr ident val valExpr = do
    frame <- findBindingFrame ident
    Map.insert ident (val, valExpr) `modifyFrameEnv` frame
    return val

lookupBinding :: Identifier -> EvalG r v e (Maybe (Closure v e))
lookupBinding s = do
    liftM (maybe Nothing (Just . snd)) (lookupBindingAndFrame s)

lookupBindingAndFrame :: Identifier
                      -> EvalG r v e (Maybe (Frame v e, Closure v e))
lookupBindingAndFrame s = do
    frames <- asks ctxFrames
    callCC $ \esc -> do
        mapM_ (search esc s) frames
        return Nothing
  where
    search esc s frame = do
       env <- liftIO $ readIORef (frameEnvRef frame)
       case Map.lookup s env of
           Just cl -> esc $ Just (frame, cl)
           _       -> return Nothing


-- | Find the frame that contains the given binding or, if not found,
--   the top-level frame.

findBindingFrame :: Identifier -> EvalG r v e (Frame v e)
findBindingFrame s = do
    baf <- lookupBindingAndFrame s
    case baf of
        Just (frame, _) -> return frame
        _               -> asks ctxTopLevel
