module HasNames where

import Control.Monad.Error

class HasNames a where
    getNames :: a -> [String]
    setNames :: Monad m => a -> [String] -> m a
