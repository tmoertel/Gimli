{-# OPTIONS -fglasgow-exts #-}

-- Pretty printing support

module PPrint where

import Text.PrettyPrint.HughesPJ

pp :: PPrint a => a -> String
pp = render . toDoc

class Show a => PPrint a where
    toDoc :: a -> Doc
    toDoc = text . show

instance PPrint Int
instance PPrint Integer
instance PPrint String
