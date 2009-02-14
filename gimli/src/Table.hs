module Table (
    Table,
        mkTable, tcols, tvecs,
        tableColumnIndexCheck, tableColumnLookupIndex,
        trows, tcnames, tctypes, tglist
) where

import Control.Monad
import Control.Monad.Error
import Data.Array
import qualified Data.Map as Map
import Data.List (intersperse, transpose)
import Data.Maybe

import CoreTypes
import GList
import HasNames
import PPrint
import Utils (uniqify, cross)
import Scalar
import Vector

-- ============================================================================
-- tables
-- ============================================================================

newtype Table = T (GList Vector)
    deriving (Show, Eq, Ord)

tglist (T gl)  = gl
tcols (T gl)   = glnames gl
tvecs (T gl)   = glvals gl
tlookup (T gl) = gllookup gl

trows = transpose . map vlist . elems . tvecs

tcnames t = elems . tcols $ t
tctypes t = map vtype . elems . tvecs $ t


mkTable :: [(Identifier, Vector)] -> Table
mkTable colspecs =
    T . mkGList $ map (cross (Just, id)) colspecs

instance PPrint Table where
    toDoc = vcat . map text . lines . ppTable

ppTable (T gl)
    | rangeSize (bounds (glvals gl)) == 0
    = "(empty table)"

ppTable (T gl) =
    unlines $
    foldr (zipWith (++)) (repeat "") $
    intersperse (repeat " ") $
    map fillRight $
    ("" : take rowlen (map show [1..])) : zipWith (:) ordns ppvecs
  where
    ordns  = elems (glnames gl)
    ppvecs = map (map pp . vlist) $ elems (glvals gl)
    rowlen = vlen (glvals gl ! 1)

sndOrd a b = snd a `compare` snd b

fillRight :: [String] -> [String]
fillRight ss =
    zipWith (++) pads ss
  where
    lens  = map length ss
    fill  = maximum lens
    pads  = map pad lens
    pad l = replicate (fill - l) ' '

tableColumnIndexCheck (T gl) n  = glistIndexCheck gl n
tableColumnLookupIndex (T gl) s = glistLookupIndex gl s

instance HasNames Table where
    getNames (T gl)    = getNames gl
    setNames (T gl) ns = liftM T (setNames gl ns)
