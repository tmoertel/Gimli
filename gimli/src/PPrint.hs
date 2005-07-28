{-# OPTIONS -fglasgow-exts #-}

-- Pretty printing support

module PPrint (
    pp,
    module Text.PrettyPrint.HughesPJ,
    PPrint(..)
) where

import Text.PrettyPrint.HughesPJ

pp :: PPrint a => a -> String
pp = render . toDoc

class Show a => PPrint a where
    toDoc :: a -> Doc
    toDoc = text . show

instance PPrint Int
instance PPrint Integer
instance PPrint String
instance PPrint Double where toDoc = text . trimPointZero . show

trimPointZero = reverse . dropZeroPoint . reverse
dropZeroPoint ('0':'.':xs) = xs
dropZeroPoint xs           = xs
