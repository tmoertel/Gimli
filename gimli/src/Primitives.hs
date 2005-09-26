module Primitives (
    isPrimitive
)
where

import qualified Data.Set as Set
import qualified Utils as U

isPrimitive :: String -> Bool
isPrimitive s = Set.member s primitivesSet

primitives :: [String]
primitives = pfile ++ words "in" ++ ises ++ sys
  where
    pfile = map concat $
            U.combinations [ words "read write"
                           , words "."
                           , words "csv tsv wsv"
                           ]
    ises = map ("is." ++) $ words "na"
    sys  = words "glob uniq"

primitivesSet = Set.fromList primitives
