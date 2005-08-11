module DataMapRead where

import qualified Data.Map as Map
import Text.ParserCombinators.ReadPrec
import qualified Text.ParserCombinators.ReadP as ReadP
import Text.Read

instance (Ord k, Read k, Read v) => Read (Map.Map k v) where
    readPrec = readMap


readMap :: (Ord k, Read k, Read v) => ReadPrec (Map.Map k v)
readMap = do
    lift (ReadP.string "{")
    nvps <- nvpairs
    lift (ReadP.string "}")
    return (Map.fromList nvps)

nvpairs :: (Read k, Read v) => ReadPrec [(k,v)]
nvpairs =
    nvpairs1 <++ return []

nvpairs1 :: (Read k, Read v) => ReadPrec [(k,v)]
nvpairs1 = do
    nvp <- nvpair
    look >>= \s -> case s of
      ',':_ -> get >> nvpairs >>= return . (nvp:)
      _     -> return [nvp]

nvpair :: (Read k, Read v) => ReadPrec (k,v)
nvpair = do
    k <- readPrec
    lift $ ReadP.string ":="
    v <- readPrec
    return (k,v)
