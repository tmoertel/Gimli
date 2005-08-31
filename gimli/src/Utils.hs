module Utils where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (mapAccumL)

combinations :: [[a]] -> [[a]]
combinations xs =
    foldr outer [[]] xs
  where
    outer xs yss = [ x:ys | x <- xs, ys <- yss ]

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
