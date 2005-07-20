{-# OPTIONS -fglasgow-exts #-}

module Main (main) where

import Control.Monad.Cont
import Control.Monad.State

import qualified System.Console.SimpleLineEditor as LE

import qualified Version as Version
import qualified Name as Name

data ReplCtx r a =
    ReplCtx { cxtExit :: a -> ContT r (StateT (ReplCtx r a) IO) a
            }

exit val =
    gets cxtExit >>= ($ val)

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

enterRepl :: IO ()
enterRepl =
    (`evalStateT` ReplCtx undefined) . (`runContT` return) $ do
        liftIO LE.initialise
        callCC $ \exitCont -> do
            modify $ \ctx -> ctx { cxtExit = exitCont }
            repl

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


sysCommands =
    [ (":quit", sysQuit)
    , (":?",    sysHelp)
    ]

sysQuit _ =
    exit ()

sysHelp _ =
    liftIO . putStrLn . unlines $
    "Commands I know:" : map (("  " ++) . fst) sysCommands

