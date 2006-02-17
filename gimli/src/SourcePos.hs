module SourcePos where

data SourcePos = SourcePos
    {
      sposName :: String
    , sposLine :: Int
    , sposCol  :: Int
    }
    deriving (Eq, Ord)

emptySourcePos = SourcePos "empty-SourcePos" (-1) (-1)
