module Primatives (
    isPrimative
)
where

import qualified Data.Set as Set
import qualified Utils as U

isPrimative :: String -> Bool
isPrimative s = Set.member s primativesSet

primatives :: [String]
primatives = pfile ++ words "in" ++ ises
  where
    pfile = map concat $
            U.combinations [ words "read write"
                           , words "."
                           , words "csv tsv wsv"
                           ]
    ises = map ("is." ++) $ words "na"

primativesSet = Set.fromList primatives
