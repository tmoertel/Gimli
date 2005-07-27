module Value where

import PPrint

data Value = VInt Integer | VStr String | VBool Bool
    deriving (Read, Show, Ord, Eq)

instance PPrint Value where
    toDoc (VInt x)  = toDoc x
    toDoc (VStr x)  = toDoc x
    toDoc (VBool b) = text $ if b then "TRUE" else "FALSE"
