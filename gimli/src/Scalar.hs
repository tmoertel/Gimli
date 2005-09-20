{-# OPTIONS -fglasgow-exts #-}

module Scalar (
    Scalar(..),
    toSStr, toSNum, toSLog,
    keepNAs
)
where

import Text.ParserCombinators.Parsec (parse, eof)

import Lexer
import PPrint

-- ============================================================================
-- scalars
-- ============================================================================

data Scalar      -- ^ scalar value
  = SStr String  -- ^ string
  | SNum !Double -- ^ numeric
  | SLog !Bool   -- ^ logical
  | SNa          -- ^ NA
  | SNull        -- ^ NULL
    deriving (Ord, Eq)

instance Show Scalar where
    showsPrec _ (SStr s) = shows s
    showsPrec _ (SNum n) = showString . trimPointZero $ show n
    showsPrec _ (SLog b) = showString (if b then "T" else "F")
    showsPrec _  SNa     = showString "NA"
    showsPrec _  SNull   = showString "NULL"

instance PPrint Scalar where
    toDoc (SNum x)  = toDoc x
    toDoc (SStr x)  = toDoc x
    toDoc (SLog b)  = text $ if b then "T" else "F"
    toDoc  SNa      = text "NA"


-- coercions

toSStr ss@(SStr _)   = ss
toSStr x             = SStr (pp x)

toSNum sn@(SNum _)   = sn
toSNum (SLog True)   = SNum 1.0
toSNum (SStr s)
    | Just n <- strToNum s
                     = SNum n
toSNum _             = SNum 0.0

toSLog sl@(SLog _)   = sl
toSLog (SNum 0.0)    = SLog False
toSLog (SNum _)      = SLog True
toSLog (SStr "TRUE") = SLog True
toSLog (SStr "T")    = SLog True
toSLog _             = SLog False

strToNum s =
    case parse (do whiteSpace; n <- lexNumber;  eof; return n) "" s of
        Right n -> Just n
        _       -> Nothing

keepNAs f SNa = SNa
keepNAs f s   = f s

