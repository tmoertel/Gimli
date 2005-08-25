-- $Id: Export.hs,v 1.2 2004/10/05 15:41:05 thor Exp $
-- TGM 2004-08-03


module CSV.Export 
    ( toCsvFile
    , toCsvLine
    )
where

import Data.List (intersperse)

toCsvFile               :: [[String]] -> String
toCsvFile xss           =  unlines . map toCsvLine $ xss

toCsvLine               :: [String] -> String
toCsvLine xs            =  concat . intersperse "," . map toCsvCell $ xs

toCsvCell               :: String -> String
toCsvCell x
    | simple            =  x
    | otherwise         =  "\"" ++ escape x ++ "\""
  where
    simple              =  all (`notElem` x) specials
    specials            = "\" \n"
    escape cs           =  concatMap escapeChar cs
    escapeChar '"'      =  "\"\""
    escapeChar x        =  [x]
