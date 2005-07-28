module Value where

import PPrint

data Value = VNum Double | VStr String | VBool Bool | VNa
    deriving (Read, Show, Ord, Eq)

valNum :: Value -> Double
valNum (VNum x) = x
valNum _        = 0.0


instance PPrint Value where
    toDoc (VNum x)  = toDoc x
    toDoc (VStr x)  = toDoc x
    toDoc (VBool b) = text $ if b then "TRUE" else "FALSE"
    toDoc  VNa      = text "NA"

