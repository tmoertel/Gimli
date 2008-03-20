{-# OPTIONS -fglasgow-exts #-}

module Main (main) where

import Control.Exception
import Control.Monad.Cont
import Control.Monad.State
import Data.Char
import qualified Data.Map as Map
import Data.Maybe
import System.Environment (getEnv)
import Text.ParserCombinators.Parsec.Error

import qualified System.Console.Readline as LE
import qualified System.Posix.Terminal as Terminal

import qualified Version as Version
import qualified Name as Name
import qualified Parser as Parser
import Expr (asNum, Value, Expr)
import qualified Eval as Eval
import PPrint
import Utils
import qualified EvalKernel as EV

-- Config

dotGimli = ".gimli"     -- ^ Name of GIMLI


-- Main entry point

main = do
    term <- Terminal.queryTerminal 0
    when term welcome
    enterRepl =<< initialState term
    when term $ putStrLn "Exiting."


initialState term = do
    topLevel <- EV.newTopLevel
    (`execStateT` s0 {stEvalState = topLevel}) . (`runContT` return) $ do
        userconf <- liftIO $ handle (\_ -> return []) $ do
            home <- getEnv "HOME"
            liftM (:[]) $ readFile (home ++ "/" ++ dotGimli)
        mapM_ eval $ defaults ++ userconf
  where
    defaults =
        if term then [ "SYS.ROWS <- 10; SYS.COLS <- 80" ]
        else         [ "SYS.ROWS <- 1e9; SYS.COLS <- 1e9" ]
    s0 = ReplState { stExit      = undefined
                   , stTerminal  = term
                   , stEvalState = undefined
                   , stContinue  = Nothing
                   }

welcome =
    putStrLn . unlines $
    [ "      _       _ _"
    , " __ _(_)_ __ | (_)"
    , "/ _` | | '  \\| | |"
    , "\\__, |_|_|_|_|_|_|"
    , "|___/"
    , ""
    , Name.name
    , "Version " ++ Version.version
    , "Enter :? for help."
    ]


-- REPL monad

data ReplState r a =
    ReplState { stExit      :: a -> ContT r (StateT (ReplState r a) IO) a
              , stTerminal  :: Bool
              , stEvalState :: EV.EvalCtx Value Expr
              , stContinue  :: Maybe String
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
    (return () `maybe` \cmd -> evalPrint cmd >> repl)

evalPrint cmd = do
    (quiet, result) <- eval cmd
    when (not quiet) . liftIO . putStr $ result

eval cmd@(':':_)
    | Just cmdFn <- let c = head (words cmd) in
                    lookup c $ mapFst (take (length c)) sysCommands
    = cmdFn cmd >>= return . ((,) False)
    | otherwise
    = return (False, "Unknown command: \"" ++ cmd ++ "\"\n")

eval cmd = do
    noContinue
    case parse cmd'' of
        Left err   -> do
            if null [e | e@(SysUnExpect "") <- errorMessages err]
               then parseError
               else evalContinue parseError cmd' -- continue line if EOF error
            where
              parseError = do
                  return (False, "syntax error: " ++ show err ++ "\n")
        Right expr -> do
            doEval expr >>= either showError ppResult
  where
    quiet = not (null cmd') && last cmd' == ';'
    cmd'  = reverse (dropWhile isSpace (reverse cmd))
    cmd'' = if quiet then init cmd' else cmd'
    noContinue = modify $ \st -> st { stContinue = Nothing }
    showError e = return (False, "error: " ++ e ++ "\n")
    ppResult r  = do
        formatter <- getFormatter
        return . ((,) quiet) . unlines . formatter . lines . pp $ r

evalContinue err cmd = do
    modify $ \st -> st { stContinue = Just $ cmd }
    getCommand >>= maybe err (eval . ((cmd++"\n")++))

doEval expr = do
    st <- gets stEvalState
    (val, st', logS) <-  liftIO $ Eval.evaluate st expr
    let msgs = logS []
    when (not (null msgs)) (liftIO $ putStr (unlines msgs))
    putEvalState st'
    return val

mapFst f = map (cross (f, id))

getFormatter = do
    rows <- getBindingValue "SYS.ROWS" >>= asNumWithDefault 10
    cols <- getBindingValue "SYS.COLS" >>= asNumWithDefault 80
    return (mkFormatter rows cols)
  where
    asNumWithDefault d v = return . maybe d round $ v >>= asNum

getBinding varname = do
    evalState <- gets stEvalState
    (result, _, _) <- liftIO $
        EV.runEval evalState () (EV.lookupBinding varname)
    return $ case result of
        Right x -> x
        _       -> Nothing

getBindingValue varname = do
    v <- getBinding varname
    return $ v >>= return . EV.clVal

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
    ]

sysQuit _ = do
    exit ()
    return ""

sysHelp _ =
    return . unlines $
    "Commands I know:" : map (("  " ++) . fst) sysCommands

sysExplain cmd = do
    binding <- getBinding varname
    return . (++"\n") $ maybe nsvar (maybe nsvar pp . EV.clExp) binding
  where
    varname = concat . tail $ words cmd
    nsvar   = "the variable \"" ++ varname
              ++ "\" does not exist or has no binding history"

sysInspect =
    return . (++"\n") . either show pp . parse . skipToArgs

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
    contCmd <- gets stContinue
    liftIO $
        handle (\_ -> return Nothing) $
        if term then prompt (isJust contCmd)
        else getContents >>= return . Just

prompt continue =
    LE.readline ("gimli" ++ if continue then "+ " else "> ") >>=
    preparse continue

preparse continue Nothing = return Nothing
preparse continue cmd@(Just line)
    | all isSpace line = prompt continue
    | otherwise        = LE.addHistory line >> return cmd
