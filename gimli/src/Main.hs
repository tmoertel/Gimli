{-# OPTIONS -fglasgow-exts #-}

module Main (main) where

import Control.Exception
import Control.Monad.Cont
import Control.Monad.State
import Data.Char
import qualified Data.Map as Map
import Data.Maybe
import System.Environment (getEnv)

import qualified System.Console.Readline as LE
import qualified System.Posix.Terminal as Terminal

import qualified Version as Version
import qualified Name as Name
import qualified Parser as Parser
import qualified Eval as Eval
import qualified Value as Value
import PPrint
import Utils

-- Config

dotGimli = ".gimli"     -- ^ Name of GIMLI


-- Main entry point

main = do
    term <- Terminal.queryTerminal 0
    when term welcome
    enterRepl =<< initialState term
    when term $ putStrLn "Exiting."
    

initialState term =
    (`execStateT` s0) . (`runContT` return) $ do
        userconf <- liftIO $ handle (\_ -> return []) $ do
            home <- getEnv "HOME"
            liftM lines $ readFile (home ++ "/" ++ dotGimli)
        mapM_ eval $ defaults ++ userconf
  where
    defaults =
        if term then [ "SYS.ROWS <- 10; SYS.COLS <- 80" ]
        else         [ "SYS.ROWS <- 1e9; SYS.COLS <- 1e9" ]
    s0 = ReplState { stExit = undefined
                   , stTerminal = term
                   , stEvalState = Eval.emptyEnv
                   }

welcome =
    putStrLn . unlines $
    [ "      _       _ _"
    , " __ _(_)_ __ | (_)    " ++ Name.name
    , "/ _` | | '  \\| | |    Version " ++ Version.version
    , "\\__, |_|_|_|_|_|_|    Enter :? for help."
    , "|___/"
    ]


-- REPL monad

data ReplState r a =
    ReplState { stExit      :: a -> ContT r (StateT (ReplState r a) IO) a
              , stTerminal  :: Bool
              , stEvalState :: Eval.EvalState
              }

exit val =
    gets stExit >>= ($ val)

putEvalState s = modify $ \rs -> rs { stEvalState = s }

-- Read-Eval-Print Loop

enterRepl :: ReplState () () -> IO ()
enterRepl s0 = do
    (`evalStateT` s0) . (`runContT` return) $ do
        initializeTerminal
        callCC $ \exitCont -> do
            modify $ \s -> s { stExit = exitCont }
            repl
        restoreTerminal

repl =
    getCommand >>=
    return () `maybe` (\cmd -> eval cmd >>= liftIO . putStr >> repl)

eval cmd@(':':_)
    | Just cmdFn <- let c = head (words cmd) in
                    lookup c $ mapFst (take (length c)) sysCommands
    = cmdFn cmd
    | otherwise
    = return $ "Unknown command: \"" ++ cmd ++ "\"\n"
eval cmd = do
    result <- case parse cmd of
        Left err   -> return (Value.VError $ show err)
        Right expr -> doEval expr
    formatter <- getFormatter
    return . unlines . formatter . lines $ pp result

doEval expr = do
    st <- gets stEvalState
    (val, st') <-  liftIO $
        handle (\e -> return (Value.VError $ show e, st)) $
        Eval.run st expr
    putEvalState st'
    return val

mapFst f = map (cross (f, id))
pair (f, g) x = (f x, g x)
cross (f, g)  = pair (f . fst, g . snd)

getFormatter = do
    rows <- getBindingValue "SYS.ROWS" >>= asNumWithDefault 10
    cols <- getBindingValue "SYS.COLS" >>= asNumWithDefault 80
    return (mkFormatter rows cols)
  where
    asNumWithDefault d v = return . maybe d round $ v >>= Value.asNum

getBinding varname =
    gets (Eval.envMap . stEvalState) >>= return . Map.lookup varname

getBindingValue varname = do
    v <- getBinding varname
    return $ v >>= return . Eval.clVal

mkFormatter :: Int -> Int -> [String] -> [String]
mkFormatter rows cols ss =
    trunc rows 0 [rmsg] $ map (trunc cols 3 "...") ss
  where
    rmsg = "(" ++ show rows ++ " of " ++ show (length ss) ++ " lines)"

trunc limit cut add xs =
    if length xs <= limit then xs
    else take (limit - cut) xs ++ add


-- System commands

handleStd = handle (\e -> return $ "an error occurred: " ++ show e ++ "\n")

sysCommands =
    [ (":quit",    sysQuit)
    , (":?",       sysHelp)
    , (":explain", sysExplain)
    , (":inspect", sysInspect)
    , (":freeze",  sysFreeze)
    , (":thaw",    sysThaw)
    ]

sysQuit _ = do
    exit ()
    return ""

sysHelp _ =
    return . unlines $
    "Commands I know:" : map (("  " ++) . fst) sysCommands

sysExplain cmd = do
    est <- gets (Eval.envMap . stEvalState)
    return . (++"\n") . maybe nsvar pp $ Map.lookup varname est >>= Eval.clExp
  where
    varname = concat . tail $ words cmd
    nsvar   = "the variable \"" ++ varname
              ++ "\" does not exist or has no binding history"

sysInspect =
    return . (++"\n") . either show pp . parse . skipToArgs

sysFreeze cmd = do
    stRep <- gets stEvalState >>= return . show
    liftIO . handleStd $ do
        writeFile (head . words . skipToArgs $ cmd) (stRep ++ "\n")
        return $ "wrote state " ++ bytes stRep ++ "\n"

sysThaw cmd = do
    result <- liftIO . try $ do
        stRep <- readFile (head . words . skipToArgs $ cmd)
        st    <- evaluate (read stRep)
        return (stRep, st)
    case result of
        Left e -> liftIO . handleStd $ throw e
        Right (stRep, st) -> do
            putEvalState st
            return $"read state " ++ bytes stRep ++ "\n"

bytes s = "(" ++ show (length s) ++ " bytes)"

parse =
    Parser.gimlParse "input"

skipToArgs =
    dropWhile (' ' /=)

-- Terminal helpers

initializeTerminal =
    gets stTerminal >>= (`when` liftIO LE.initialize)

restoreTerminal =
    return ()
    -- gets stTerminal >>= (`when` liftIO LE.restore)

getCommand = do
    term <- gets stTerminal
    liftIO $
        handle (\_ -> return Nothing) $
        if term then prompt
        else getLine >>= return . Just

prompt = do
    LE.readline "gimli> " >>= preparse

preparse Nothing = return Nothing
preparse cmd@(Just line)
    | all isSpace line = prompt
    | otherwise        = LE.addHistory line >> return cmd
