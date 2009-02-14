{-# LANGUAGE ParallelListComp #-}

module GList (
    GList(..),
        mkGList, mkGListNamed, mkGListVals,
        glpairs, glbounds, gllength,
        glmap, glmapM,
        glistIndexCheck, glistLookupIndex
) where

import Control.Monad
import Control.Monad.Error
import Data.Array
import qualified Data.Map as Map
import Data.List (intersperse, transpose, mapAccumL)
import Data.Maybe

import CoreTypes
import HasNames
import PPrint
import Utils
import Scalar
import Vector

-- ============================================================================
-- general lists
-- ============================================================================

data (Show a, Ord a) => GList a
    = GList
      { glnames   :: Array Int Identifier
      , glvals    :: Array Int a
      , gllookup  :: Map.Map Identifier Int
      }
      deriving (Show, Eq, Ord)

glbounds gl = bounds (glvals gl)
gllength gl = rangeSize (glbounds gl)

glmap :: (Show a, Ord a, Show b, Ord b) => (a -> b) -> GList a -> GList b
glmap f gl = gl { glvals = fmap f (glvals gl) }

glmapM :: (Show a, Ord a, Show b, Ord b, Monad m) =>
          (a -> m b) -> GList a -> m (GList b)
glmapM f gl = do
    glelems' <- mapM f . elems . glvals $ gl
    return $ gl { glvals = listArray (1, gllength gl) glelems' }

glpairs gl =
    uncurry zip . pair (elems . glnames, elems . glvals) $ gl

mkGList :: (Show a, Ord a) => [(Maybe Identifier, a)] -> GList a
mkGList ispecs =
    GList names vals lookup
  where
    inames = map (\(n,_) -> case n of { Just "" -> Nothing; _ -> n }) ispecs
    names  = listArray (1, len) . snd $ mapAccumL blankify unames inames
    vals   = listArray (1, len) (map snd ispecs)
    lookup = Map.fromList [(n,i) | (i,n) <- assocs names, n /= ""]
    len    = length ispecs
    unames = uniqify (mapMaybe fst ispecs)
    blankify (n:r) (Just _) = (r, n)
    blankify ns    _        = (ns, "")

mkGListNamed :: (Show a, Ord a) => [(Identifier, a)] -> GList a
mkGListNamed ispecs =
    mkGList $ map (cross (Just, id)) ispecs

mkGListVals :: (Show a, Ord a) => [a] -> GList a
mkGListVals vals =
    mkGListNamed $ map (pair (const "", id)) vals

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

instance (Show a, Ord a) => HasNames (GList a) where
    getNames gl    = elems (glnames gl)
    setNames gl ns =
        do
        when (length ns /= nameCount) $
            fail ("you must supply exactly " ++ show nameCount
                  ++ "names")
        return $ mkGList [ (Just n, val)
                           | n   <- elems (glnames gl)
                           | val <- elems (glvals gl)  ]
      where
        nameCount = rangeSize (bounds (glnames gl))

instance (PPrint a, Ord a) => PPrint (GList a) where
    toDoc gl
        | 0 == rangeSize (bounds (glvals gl))
        = text "list()"
        | otherwise
        = vcat . zipWith assocDoc [1..] $ glpairs gl

assocDoc n (nm,val) = hang (indexDoc n nm) 2 (toDoc val)

indexDoc n "" = hcat . map text $ ["[", show n, "] =>"]
indexDoc _ nm = hcat . map text $ ["$", nm, " =>"]

