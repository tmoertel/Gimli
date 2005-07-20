{-# OPTIONS -fglasgow-exts #-}

module Main (main) where

import Control.Monad.Cont
import Control.Monad.State
import qualified System.Console.SimpleLineEditor as LE

import Version (version)

data ReplMonad r a = ReplMonad {
    rmExit :: a -> ContT r (StateT (ReplMonad r a) IO) a
}

exit val =
    gets rmExit >>= ($ val)


newtype C m b = C { runC :: C m b -> m b }

main = do
    welcome
    enterRepl

welcome =
    putStrLn . unlines $
    [ "      _       _ _"
    , " __ _(_)_ __ | (_)"
    , "/ _` | | '  \\| | |    " ++ name
    , "\\__, |_|_|_|_|_|_|    Version " ++ version
    , "|___/"
    ]

name =
    "Genetics Information Manipulation Language Interface"

enterRepl :: IO ()
enterRepl =
    (`evalStateT` undefined) . (`runContT` return) $ do
        liftIO LE.initialise
        callCC $ \exit -> do
            put (ReplMonad exit)
            repl
        return ()

repl = do
    input <- liftIO $ LE.getLineEdited "gimli> "
    return ()
    case input of
        Just cmd -> do
            liftIO $ putStrLn $ "command = \"" ++ cmd ++ "\""
            eval cmd
            repl
        _ -> return ()

eval (':':cmd)
    | Just cmdFn <- lookup (':':cmd) sysCommands = cmdFn
eval cmd = do
    liftIO $ putStrLn $ "Unknown command \"" ++ cmd ++ "\""


sysCommands = 
    [ (":quit", sysQuit)
    ]

sysQuit =
    exit ()

