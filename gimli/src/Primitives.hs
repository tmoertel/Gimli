module Primitives (
    isPrimitive
)
where

import qualified Data.Set as Set
import qualified Utils as U

isPrimitive :: String -> Bool
isPrimitive s = Set.member s primitivesSet

primitives :: [String]
primitives = pfile ++ words "in" ++ ises ++ kernel ++ table
  where
    pfile = map concat $
            U.combinations [ words "read write"
                           , words "."
                           , words "csv tsv wsv"
                           ]
    ises   = map ("is." ++) $ words "na"
    kernel = words "glob uniq length match"
    table  = words "names"

primitivesSet = Set.fromList primitives
