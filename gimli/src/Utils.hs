module Utils where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (mapAccumL)

combinations :: [[a]] -> [[a]]
combinations xs =
    foldr outer [[]] xs
  where
    outer xs yss = [ x:ys | x <- xs, ys <- yss ]

-- todo:  make uniqify fast when there are many duplicates
-- uniqify (replicate 1000 "x")

uniqify :: [String] -> [String]
uniqify ss =
    snd (mapAccumL ins S.empty ss)
  where
    given = S.fromList ss
    ins taken s = (taken', s') 
      where
        taken' = S.insert s' taken
        s'     = if S.member s taken then next s taken else s
    next s taken = head [ sn | n <- [1..]
                             , let sn = s ++ "." ++ show n
                             , not $ S.member sn given
                             , not $ S.member sn taken ]

-- helpers for pairs

pair (f, g) x    = (f x, g x)
cross (f, g)     = pair (f . fst, g . snd)
both f           = pair (f . fst, f . snd)
