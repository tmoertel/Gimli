{-# OPTIONS -fglasgow-exts #-}

module Value (
    Identifier,
    Value(..),
        vIsVector, vIsTable,
        asVector, asNum, asTable,
    Scalar(..), toScalar, toSNum, toSLog, keepNAs,
    Vector(..), ToVector(..),
        vlen, vtype, vlist, vmap, vnull,
        vectorCoerce, vecNum, mkVector, mkVectorOfType, mkVectorValue,
        bestType,
        VecType(..),
    Table(..),
        mkTable,
        tableColumnIndexCheck, tableColumnLookupIndex,
        trows, tcnames, tctypes
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
import Utils (uniqify)

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

data Scalar      -- ^ scalar value
  = SStr String  -- ^ string
  | SNum !Double -- ^ numeric
  | SLog !Bool   -- ^ logical
  | SNa          -- ^ NA
  | SNull        -- ^ NULL
    deriving (Read, Show, Ord, Eq)


instance PPrint Scalar where
    toDoc (SNum x)  = toDoc x
    toDoc (SStr x)  = toDoc x
    toDoc (SLog b)  = text $ if b then "T" else "F"
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
toSLog (SNum 0.0)    = SLog False
toSLog (SNum _)      = SLog True
toSLog (SStr "TRUE") = SLog True
toSLog (SStr "T")    = SLog True
toSLog _             = SLog False

strToNum s =
    case parse (whiteSpace >> lexNumber) "" s of
        Right n -> Just n
        _       -> Nothing

asVector :: Monad m => Value -> m Vector
asVector (VVector v) = return v
asVector _           = fail "not a vector"

asNum :: Monad m => Value -> m Double
asNum v = asVector v >>= vecNum

asTable :: Monad m => Value -> m Table
asTable (VTable t) = return t
asTable _          = fail "not a table"


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

vmap :: (Scalar -> Scalar) -> Vector -> Vector
vmap f v = mkVector $ map f (vlist v)

vnull = null . vlist

class    ToVector a        where toVector :: a -> Vector
instance ToVector Scalar   where toVector x = mkVector [x]
instance ToVector [Scalar] where toVector   = mkVector
instance ToVector [Value]  where toVector   = mkVector . map toScalar

toScalar (VVector (V _ _ (x:_))) = x
toScalar _                       = SNa

mkVectorValue :: [Scalar] -> Value
mkVectorValue xs =
    if vnull v then VNull else VVector v
  where
    v = mkVector xs

mkVector :: [Scalar] -> Vector
mkVector xs = mkVectorOfType (bestType xs) xs

mkVectorOfType :: VecType -> [Scalar] -> Vector
mkVectorOfType vtype xs = V vtype (length vl) vl
  where
    vl = mapMaybe (vtCoerce vtype) xs

bestType xs =
    foldl vtPromote VTLog xs

vectorCoerce vtype vec =
    mkVectorOfType vtype (vlist vec)

vecNum :: Monad m => Vector -> m Double
vecNum (V _ _ (x:_)) =
    case toSNum x of
        SNum n -> return n
        _      -> fail "not a number"
           
vtPromote VTStr _         = VTStr
vtPromote VTNum (SLog _)  = VTNum
vtPromote _     (SStr _)  = VTStr
vtPromote _     (SNum _)  = VTNum
vtPromote _     (SLog _)  = VTLog
vtPromote t     _         = t

vtCoerce _ SNull  = Nothing
vtCoerce VTStr x  = Just . keepNAs toSStr $ x
vtCoerce VTNum x  = Just . keepNAs toSNum $ x
vtCoerce VTLog x  = Just . keepNAs toSLog $ x

keepNAs f SNa = SNa
keepNAs f s   = f s


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

