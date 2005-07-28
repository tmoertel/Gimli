{-# OPTIONS -fglasgow-exts #-}

module Main (main) where

import Control.Exception
import Control.Monad.Cont
import Control.Monad.State
import Data.Maybe

import qualified System.Console.SimpleLineEditor as LE
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
    enterRepl $ ReplState { stExit = undefined, stTerminal = term }
    when term $ putStrLn "Exiting."

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
    ReplState { stExit :: a -> ContT r (StateT (ReplState r a) IO) a
              , stTerminal :: Bool
              }

exit val =
    gets stExit >>= ($ val)


-- Read-Eval-Print Loop

enterRepl :: ReplState () () -> IO ()
enterRepl s0 = do
    (`evalStateT` s0) . (`runContT` return) $ do
        initializeTerminal
        callCC $ \exitCont -> do
            modify $ \s -> s { stExit = exitCont }
            repl

repl =
    promptTerminal >>=
    return () `maybe` \cmd -> eval cmd >> repl

eval cmd@(':':_)
    | Just cmdFn <- lookup (head $ words cmd) sysCommands
    = cmdFn cmd
    | otherwise
    = liftIO . putStrLn $ "Unknown command: \"" ++ cmd ++ "\""
eval cmd = do
    liftIO $ case parse cmd of
        Left err   -> putStrLn $ "error: " ++ show err
        Right expr -> putStrLn . pp $ Eval.eval expr


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
    gets stTerminal >>= (`when` liftIO LE.initialise)

promptTerminal = do
    term <- gets stTerminal
    liftIO $
        handle (\_ -> return Nothing) $
        if term then LE.getLineEdited "gimli> "
        else getLine >>= return . Just
