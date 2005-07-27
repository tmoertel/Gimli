module Value where

import PPrint

data Value = VInt Integer | VStr String
    deriving (Read, Show, Ord, Eq)

instance PPrint Value where
    toDoc (VInt x) = toDoc x
    toDoc (VStr x) = toDoc x
