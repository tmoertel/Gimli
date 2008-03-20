module Primitives (
    isPrimitive
)
where

import qualified Data.Set as Set

isPrimitive :: String -> Bool
isPrimitive s = Set.member s primitivesSet

primitives :: [String]
primitives = pfile ++ words "in" ++ ases ++ ises ++ kernel ++ table
  where
    pfile = map concat $
            sequence [ words "read write"
                     , words "."
                     , words "csv tsv wsv"
                     ]
    ases   = map ("as." ++) $ words "list table"
    ises   = map ("is." ++) $ words "na"
    kernel = words "glob inspect length match print sort uniq var"
    table  = words "names"

primitivesSet = Set.fromList primitives
