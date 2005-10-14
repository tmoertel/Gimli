{-# OPTIONS -fglasgow-exts #-}

module GList (
    GList(..),
        mkGList,
        glistIndexCheck, glistLookupIndex
) where

import Control.Monad
import Control.Monad.Error
import Data.Array
import qualified Data.Map as Map
import Data.List (intersperse, transpose, mapAccumL)
import Data.Maybe

import CoreTypes
import PPrint
import Utils (uniqify)
import Scalar
import Vector

-- ============================================================================
-- general lists
-- ============================================================================

data (Show a, Eq a, Ord a) => GList a
    = GList
      { glnames   :: Array Int Identifier
      , glvals    :: Array Int a
      , gllookup  :: Map.Map Identifier Int
      }
             deriving (Show, Eq, Ord)



mkGList :: (Show a, Ord a) => [(Maybe Identifier, a)] -> GList a
mkGList ispecs =
    GList names vals lookup
  where
    names  = listArray (1, len) . snd $
             mapAccumL f unames (map fst ispecs)
    vals   = listArray (1, len) (map snd ispecs)
    lookup = Map.fromList [(n,i) | (i,n) <- assocs names, n /= ""]
    len    = length ispecs
    unames = uniqify (mapMaybe fst ispecs)
    f (n:r) (Just _) = (r, n)
    f ns    _        = (ns, "")

glistIndexCheck glist n =
    if inRange bnds n then return n else throwError msg
  where
    bnds = bounds (glvals glist)
    msg  = "index " ++ show n
                    ++ " is out of range " ++ showRange bnds

showRange (l,h) = "[" ++ show l ++ "," ++ show h ++ "]"

glistLookupIndex glist s =
    (throwError msg `maybe` return) $
    Map.lookup s (gllookup glist)
  where
    msg = "index name \"" ++ s ++ "\" does not exist"
