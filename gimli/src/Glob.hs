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

-- | Search for directory entries that match the given shell-globbing
--   pattern 'pat', in which "?" matches any character, and "*" matches
--   any series of characters.

glob :: MonadIO m => String -> m [String]
glob pat =
    runListT $ uncurry (foldM search) $ case dirs pat of
        "":ds -> ("/", ds)
        ds    -> (".", ds)


-- | Search the directory 'd' for entries that match the
--   simple (slashless) shell-globbing pattern 'p'.

search :: MonadIO m => String -> String -> ListT m String
search d p = do
    fs <- liftIO $ Ex.handle (const (return [])) $
          (sort . mapMaybe (match p)) `liftM` getDirectoryContents d
    ListT . return $ map ((d' ++ "/") ++) fs
      where
        d' = if d == "/" then "" else d


-- | Attempts to match the pattern 'p' against the string 's' and
--   returns 'Just s' if they can be matched; 'Nothing', otherwise.

match :: String -> String -> Maybe String
match ('*':_) ('.':_) =
    mzero -- star cannot match leading dot
match p s =
    liftM fst . listToMaybe $ readP_to_S parser s
  where
    parser = (foldr (.) id $ map toParser p) $ do
        -- force parser to match whole string
        -- by failing unless nothing is left
        rest <- look
        when (not (null rest)) pfail
        return ""
    toParser '*' = liftM2 (++) (many get)
    toParser '?' = liftM2 (:)  get
    toParser c   = liftM2 (:)  (char c)


-- | Breaks a string upon slashes (like 'lines' for slashes instead of
--   newlines)

dirs :: String -> [String]
dirs =
    lines . map (\c -> if c == '/' then '\n' else c)
