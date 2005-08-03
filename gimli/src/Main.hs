{-# OPTIONS -fglasgow-exts #-}

module Main (main) where

import Control.Exception
import Control.Monad.Cont
import Control.Monad.State
import Data.Char
import Data.Maybe

import qualified System.Console.Readline as LE
import qualified System.Posix.Terminal as Terminal

import qualified Version as Version
import qualified Name as Name
import qualified Parser as Parser
import qualified Eval as Eval
import PPrint


-- Main entry point

main = do
    term <- Terminal.queryTerminal 0
    when term welcome
    enterRepl $ initialState term
    when term $ putStrLn "Exiting."
    

initialState term =
    ReplState { stExit = undefined
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

type ReplCtx r = ContT r (StateT (ReplState r ()) IO) ()

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
    return () `maybe` \cmd -> eval cmd >> repl

eval cmd@(':':_)
    | Just cmdFn <- let c = head (words cmd) in
                    lookup c $ mapFst (take (length c)) sysCommands
    = cmdFn cmd
    | otherwise
    = liftIO . putStrLn $ "Unknown command: \"" ++ cmd ++ "\""
eval cmd = do
    case parse cmd of
        Left err   -> liftIO $ putStrLn $ "error: " ++ show err
        Right expr -> doEval expr >>= liftIO . putStrLn . pp

doEval expr = do
    st <- gets stEvalState
    (val, st') <- liftIO $ Eval.run st expr
    putEvalState st'
    return val

mapFst f = map (cross (f, id))
pair (f, g) x = (f x, g x)
cross (f, g)  = pair (f . fst, g . snd)


-- System commands

sysCommands =
    [ (":quit",    sysQuit)
    , (":?",       sysHelp)
    , (":inspect", sysInspect)
    ]

sysQuit _ =
    exit ()

sysHelp _ =
    liftIO . putStrLn . unlines $
    "Commands I know:" : map (("  " ++) . fst) sysCommands

sysInspect =
    liftIO . putStrLn . either show pp . parse . skipToArgs

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
    LE.readline "gimli> " >>= doCommand

doCommand Nothing = return Nothing
doCommand cmd@(Just line)
    | all isSpace line = prompt
    | otherwise        = LE.addHistory line >> return cmd
