{-# LANGUAGE FlexibleInstances #-}

module Vector (
    Vector(..), ToVector(..),
        naVector, falseVector, trueVector, emptyVector,
        vlen, vtype, vlist, vmap, vnull,
        vecNum, vecLog, vecStr,
        vectorCoerce, mkVector, mkVectorOfType,
        bestType,
        VecType(..)
) where

import Control.Monad
import Data.Maybe
import Text.Show

import CoreTypes
import PPrint
import Scalar

-- ============================================================================
-- vectors
-- ============================================================================

data Vector  = V VecType Int [Scalar]
               deriving (Ord, Eq)

data VecType = VTNum | VTStr | VTLog
               deriving (Read, Show, Ord, Eq)

instance PPrint Vector where
    toDoc (V _ _ [x]) = toDoc x
    toDoc (V _ _ xs)  = listToDoc xs

instance Show Vector where
    showsPrec _ (V _ _ [x]) = shows x
    showsPrec _ (V _ _ xs)  = showListWith shows xs

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

vecLog :: Monad m => Vector -> m Bool
vecLog (V _ _ (x:_)) =
    case toSLog x of
        SLog b -> return b
        _      -> fail "not a logical"

vecStr :: Monad m => Vector -> m String
vecStr (V _ _ (x:_)) =
    case toSStr x of
        SStr s -> return s
        _      -> fail "not a string"

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

naVector    = mkVector [SNa]
falseVector = mkVector [SLog False]
trueVector  = mkVector [SLog True]
emptyVector = V VTLog 0 []
