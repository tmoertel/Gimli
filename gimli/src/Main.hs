{-# OPTIONS -fglasgow-exts #-}

module Main (main) where

import Control.Monad.Cont
import Control.Monad.State

import qualified System.Console.SimpleLineEditor as LE

import qualified Version as Version
import qualified Name as Name


-- Main entry point

main = do
    welcome
    enterRepl
    putStrLn "Exiting."

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
              }

exit val =
    gets stExit >>= ($ val)


-- Read-Eval-Print Loop

enterRepl :: IO ()
enterRepl =
    (`evalStateT` ReplState undefined) . (`runContT` return) $ do
        liftIO LE.initialise
        callCC $ \exitCont -> do
            modify $ \s -> s { stExit = exitCont }
            repl

repl :: ReplCtx r
repl = do
    input <- liftIO $ LE.getLineEdited "gimli> "
    return ()
    case input of
        Just cmd -> eval cmd >> repl
        _        -> return ()

eval cmd@(':':_)
    | Just cmdFn <- lookup (head $ words cmd) sysCommands
    = cmdFn cmd
eval cmd = do
    liftIO $ putStrLn $ "Unknown command \"" ++ cmd ++ "\"."


-- System commands

sysCommands =
    [ (":quit", sysQuit)
    , (":?",    sysHelp)
    ]

sysQuit _ =
    exit ()

sysHelp _ =
    liftIO . putStrLn . unlines $
    "Commands I know:" : map (("  " ++) . fst) sysCommands
