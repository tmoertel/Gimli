{-# OPTIONS -fglasgow-exts #-}

module Table (
    Table(..),
        mkTable,
        tableColumnIndexCheck, tableColumnLookupIndex,
        trows, tcnames, tctypes
) where

import Control.Monad
import Control.Monad.Error
import Data.Array
import qualified Data.Map as Map
import Data.List (intersperse, transpose)
import Data.Maybe

import CoreTypes
import PPrint
import Utils (uniqify)
import Scalar
import Vector

-- ============================================================================
-- tables
-- ============================================================================

data Table = T { tcols   :: Array Int Identifier
               , tvecs   :: Array Int Vector
               , tlookup :: Map.Map Identifier Int
               }
             deriving (Show, Eq, Ord)

trows = transpose . map vlist . elems . tvecs

tcnames = elems . tcols
tctypes = map vtype . elems . tvecs


mkTable :: [(Identifier, Vector)] -> Table
mkTable colspecs =
    T cols vecs lookup
  where
    cols   = listArray (1, len) (uniqify $ map fst colspecs)
    vecs   = listArray (1, len) (map snd colspecs)
    lookup = Map.fromList (zip (elems cols) [1..])
    len    = length colspecs

class ColumnIdentifier a where
    toIndex :: Table -> a -> Int
instance ColumnIdentifier Double where
    toIndex _ n = round n
instance ColumnIdentifier String where
    toIndex (T _ _ lut) s = fromJust (Map.lookup s lut)

instance PPrint Table where
    toDoc = vcat . map text . lines . ppTable

ppTable (T ns vs _)
    | rangeSize (bounds vs) == 0
    = "(empty table)"

ppTable (T ns vs _) =
    unlines $
    foldr (zipWith (++)) (repeat "") $
    intersperse (repeat " ") $
    map fillRight $
    ("" : take rowlen (map show [1..])) : zipWith (:) ordns ppvecs
  where
    ordns  = elems ns
    ppvecs = map (map pp . vlist) $ elems vs
    rowlen = vlen (vs ! 1)

sndOrd a b = snd a `compare` snd b

fillRight :: [String] -> [String]
fillRight ss =
    zipWith (++) pads ss
  where
    lens  = map length ss
    fill  = maximum lens
    pads  = map pad lens
    pad l = replicate (fill - l) ' '

tableColumnIndexCheck table n =
    if inRange bnds n then return n else throwError msg
  where
    bnds = bounds (tvecs table)
    msg  = "column index " ++ show n
                           ++ " is out of range " ++ showRange bnds

showRange (l,h) = "[" ++ show l ++ "," ++ show h ++ "]"

tableColumnLookupIndex table s =
    (throwError msg `maybe` return) $
    Map.lookup s (tlookup table)
  where
    msg = "column name \"" ++ s ++ "\" does not exist"
