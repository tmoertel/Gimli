module Join where

import Control.Monad.Error
import Data.List (transpose)


import Utils
import Value

data JoinOp
    = JCartesian
    | JEquijoin JoinInclusion JoinInclusion
    deriving (Eq, Ord, Read, Show)

data JoinInclusion
    = JInner
    | JOuter
    deriving (Eq, Ord, Read, Show)

tableJoin JCartesian tl tr =
    return $ mkTable (zip colnames colvecs)
  where
    (lrs, rrs) = both trows (tl, tr)
    colvecs    = zipWith mkVectorOfType coltypes (transpose rows)
    rows       = [ lr ++ rr | lr <- lrs, rr <- rrs ]
    colnames   = uniqify . uncurry (++) $ both tcnames (tl, tr)
    coltypes   = uncurry (++) (both tctypes (tl, tr))

tableJoin op _ _ =
    throwError $ "this type of join (" ++ show op ++ ") is not implemented yet"
