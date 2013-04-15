{-# LANGUAGE FlexibleInstances, OverlappingInstances, TypeSynonymInstances #-}

-- Pretty printing support

module PPrint (
    listToDoc, trimPointZero,
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
listToDoc = brackets . hcat . punctuate comma . map toDoc

instance PPrint Int
instance PPrint Integer
instance PPrint String
instance PPrint Double where toDoc = text . trimPointZero . show

trimPointZero = reverse . dropZeroPoint . reverse
dropZeroPoint ('0':'.':xs) = xs
dropZeroPoint xs           = xs
