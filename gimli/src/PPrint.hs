{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances #-}

-- Pretty printing support

module PPrint (
    listToDoc,
    module Text.PrettyPrint.HughesPJ,
    PPrint(..)
) where

import Text.PrettyPrint.HughesPJ

class Show a => PPrint a where
    toDoc :: a -> Doc
    toDoc = text . show
    pp :: a -> String
    pp = render . toDoc

listToDoc :: PPrint a => [a] -> Doc
listToDoc = brackets . cat . punctuate comma . map toDoc

instance PPrint Int
instance PPrint Integer
instance PPrint String
instance PPrint Double where toDoc = text . trimPointZero . show

trimPointZero = reverse . dropZeroPoint . reverse
dropZeroPoint ('0':'.':xs) = xs
dropZeroPoint xs           = xs
