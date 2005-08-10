{-# OPTIONS -fglasgow-exts #-}

module Value (
    Value(..),
    Scalar(..), toSNum,
    Vector(..), ToVector(..), vlen, vtype, vlist,
    VecType(..)
) where

import Control.Monad
import Data.Maybe

import PPrint

data Value
  = VVector Vector
  | VNull   
  | VError String
    deriving (Read, Show, Ord, Eq)

instance PPrint Value where
    toDoc (VVector v) = toDoc v
    toDoc VNull       = text "NULL"
    toDoc (VError s)  = text $ "error: " ++ s
--    toDoc x           = error $ "don't know how to pp " ++ show x

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

mkVector :: [Scalar] -> Vector
mkVector xs = mkVectorOfType (foldl vtPromote VTLog xs) xs

mkVectorOfType :: VecType -> [Scalar] -> Vector
mkVectorOfType vtype xs = V vtype (length vl) vl
  where
    vl = mapMaybe (vtCoerce vtype) xs

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

toSStr ss@(SStr _)   = ss
toSStr x             = SStr (pp x)

toSNum sn@(SNum _)   = sn
toSNum (SLog True)   = SNum 1.0
toSNum _             = SNum 0.0

toSLog sl@(SLog _)   = sl
toSLog (SNum 1.0)    = SLog True
toSLog (SStr "TRUE") = SLog True
toSLog (SStr "T")    = SLog True
toSLog _             = SLog False
