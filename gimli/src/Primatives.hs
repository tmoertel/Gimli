module Primatives (
    isPrimative
)
where

import qualified Data.Set as Set
import qualified Utils as U

isPrimative :: String -> Bool
isPrimative s = Set.member s primativesSet

primatives :: [String]
primatives = pfile ++ words "in"
  where
    pfile = map concat $
            U.combinations [ words "read write"
                           , words "."
                           , words "csv tsv wsv"
                           ]

primativesSet = Set.fromList primatives
