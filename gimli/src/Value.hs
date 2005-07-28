module Value where

import PPrint

data Value = VInt Integer | VStr String | VBool Bool | VNa
    deriving (Read, Show, Ord, Eq)

valInt :: Value -> Integer
valInt (VInt x) = x
valInt _        = 0


instance PPrint Value where
    toDoc (VInt x)  = toDoc x
    toDoc (VStr x)  = toDoc x
    toDoc (VBool b) = text $ if b then "TRUE" else "FALSE"
    toDoc  VNa      = text "NA"

