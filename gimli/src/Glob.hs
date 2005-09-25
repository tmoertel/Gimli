{-

File "globbing."

Tom Moertel <tom@moertel.com>
24 Sep 2005

-}

module Glob (glob) where

import Control.Exception as Ex
import Control.Monad
import Control.Monad.List
import Data.List (intersperse, sort)
import Data.Maybe (mapMaybe, listToMaybe)
import System.Directory (getDirectoryContents)
import Text.ParserCombinators.ReadP

glob :: String -> IO [String]
glob pat =
    runListT $ foldM search root restDirs
  where
    (root, restDirs) = case dirs pat of
        "":ds -> ("/", ds)
        ds    -> (".", ds)

search :: String -> String -> ListT IO String
search p d = do
--  liftIO $ putStrLn $ "getDirectoryContents " ++ p ++ " >>= match " ++ d
    fs <- liftIO $ Ex.handle (const (return [])) (getDirectoryContents p) >>=
        return . sort . mapMaybe (match d)
    ListT . return $ map ((p' ++ "/") ++) fs
      where
        p' = if p == "/" then "" else p


match :: String -> String -> Maybe String
match ('*':_) ('.':_) =
    mzero -- star cannot match leading dot
match p s =
    liftM fst . listToMaybe $ readP_to_S parser s
  where
    parser = (foldr (.) id $ map toParser p) $ do
        -- force parser to match whole string by failing unless nothing is left
        rest <- look
        when (not (null rest)) pfail
        return ""
    toParser '*' = liftM2 (++) (many get)
    toParser '?' = liftM2 (++) (liftM (:[]) get)
    toParser c   = liftM2 (:)  (char c)

{-

Hand-rolled parser

match ('*':_) ('.':_) =
    mzero -- star cannot match leading dot
match p s     =
    matchParse p ([], s) >>= return . fst
  where
    matchParse [] (s, [])  = return (reverse s, [])
    matchParse "*" (s, []) = return (reverse s, [])
    matchParse (x:xs) (s, y:ys)
        | x == '*'         = matchParse xs (s, y:ys) `mplus`
                             matchParse (x:xs) (y:s, ys)
        | x == y           = matchParse xs (y:s, ys)
    matchParse _ _         = mzero

-}



-- Break a string upon slashes (like 'lines' for slashes instead of newlines)

dirs :: String -> [String]
dirs s = lines . map (\c -> if c == '/' then '\n' else c) $ s
