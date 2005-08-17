{-# OPTIONS -fglasgow-exts #-}

module Value (
    Identifier,
    Value(..),
    vIsVector, vIsTable,
    Scalar(..), toSNum,
    Vector(..), ToVector(..),
        vlen, vtype, vlist, vectorCoerce, vecNum, mkVector,
        VecType(..),
    Table(..),
        mkTable, tableColumnIndexCheck, tableColumnLookupIndex, trows, tcnames
) where

import Control.Monad
import Control.Monad.Error
import Data.Array
import qualified Data.Map as Map
import Data.List (intersperse, sortBy, transpose)
import Data.Maybe
import Text.ParserCombinators.Parsec (parse)

import DataMapRead
import Lexer  -- for number parsing
import PPrint

type Identifier = String

data Value
  = VVector Vector
  | VTable Table
  | VNull   
  | VError String
    deriving (Read, Show, Ord, Eq)

instance PPrint Value where
    toDoc (VVector v) = toDoc v
    toDoc VNull       = text "NULL"
    toDoc (VError s)  = text $ "error: " ++ s
    toDoc (VTable t)  = toDoc t
--    toDoc x           = error $ "don't know how to pp " ++ show x

vIsVector (VVector _) = True
vIsVector _           = False

vIsTable (VTable _)   = True
vIsTable _            = False

-- ============================================================================
-- scalars
-- ============================================================================

data Scalar     -- ^ scalar value
  = SStr String -- ^ string
  | SNum Double -- ^ numeric
  | SLog Bool   -- ^ logical
  | SNa         -- ^ NA
  | SNull       -- ^ NULL
    deriving (Read, Show, Ord, Eq)


instance PPrint Scalar where
    toDoc (SNum x)  = toDoc x
    toDoc (SStr x)  = toDoc x
    toDoc (SLog b)  = text $ if b then "TRUE" else "FALSE"
    toDoc  SNa      = text "NA"


-- coercions

toSStr ss@(SStr _)   = ss
toSStr x             = SStr (pp x)

toSNum sn@(SNum _)   = sn
toSNum (SLog True)   = SNum 1.0
toSNum (SStr s)
    | Just n <- strToNum s
                     = SNum n
toSNum _             = SNum 0.0

toSLog sl@(SLog _)   = sl
toSLog (SNum 1.0)    = SLog True
toSLog (SStr "TRUE") = SLog True
toSLog (SStr "T")    = SLog True
toSLog _             = SLog False

strToNum s =
    case parse (whiteSpace >> lexNumber) "" s of
        Right n -> Just n
        _       -> Nothing


-- ============================================================================
-- vectors
-- ============================================================================

data Vector  = V VecType Int [Scalar]
               deriving (Read, Show, Ord, Eq)

data VecType = VTNum | VTStr | VTLog
               deriving (Read, Show, Ord, Eq)

instance PPrint Vector where
    toDoc (V _ _ [x]) = toDoc x
    toDoc (V _ _ xs)  = listToDoc xs

vlen :: Vector -> Int
vlen (V _ l _) = l

vtype :: Vector -> VecType
vtype (V t _ _) = t

vlist :: Vector -> [Scalar]
vlist (V _ _ xs) = xs

class    ToVector a        where toVector :: a -> Vector
instance ToVector Scalar   where toVector x = mkVector [x]
instance ToVector [Scalar] where toVector   = mkVector
instance ToVector [Value]  where toVector   = mkVector . map toScalar

toScalar (VVector (V _ _ (x:_))) = x
toScalar _                       = SNa

mkVector :: [Scalar] -> Vector
mkVector xs = mkVectorOfType (foldl vtPromote VTLog xs) xs

mkVectorOfType :: VecType -> [Scalar] -> Vector
mkVectorOfType vtype xs = V vtype (length vl) vl
  where
    vl = mapMaybe (vtCoerce vtype) xs

vectorCoerce vtype vec =
    mkVectorOfType vtype (vlist vec)

vecNum (V _ _ (x:_)) =
    case toSNum x of (SNum n) -> n

vtPromote VTStr _         = VTStr
vtPromote VTNum (SLog _)  = VTNum
vtPromote _     (SStr _)  = VTStr
vtPromote _     (SNum _)  = VTNum
vtPromote _     (SLog _)  = VTLog
vtPromote t     _         = t

vtCoerce _ SNull  = Nothing
vtCoerce VTStr x  = Just . keepNas toSStr $ x
vtCoerce VTNum x  = Just . keepNas toSNum $ x
vtCoerce VTLog x  = Just . keepNas toSLog $ x

keepNas f SNa = SNa
keepNas f s   = f s


-- ============================================================================
-- tables
-- ============================================================================

data Table = T { tcols   :: Array Int Identifier
               , tvecs   :: Array Int Vector
               , tlookup :: Map.Map Identifier Int
               }
             deriving (Read, Show, Eq, Ord)

trows = transpose . map vlist . elems . tvecs

tcnames = elems . tcols

mkTable :: [(Identifier, Vector)] -> Table
mkTable colspecs =
    T cols vecs lookup
  where
    cols   = listArray (1, len) (map fst colspecs)
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

