module Median ( median ) where

import           Control.Monad.Primitive      (PrimMonad, PrimState)
import qualified Data.Vector.Algorithms.Merge as Merge
import           Data.Vector.Generic.Mutable  (MVector)
import qualified Data.Vector.Generic.Mutable  as MV

{-# INLINABLE median #-}
median :: (PrimMonad m, MVector v e, Ord e, Fractional e) => v (PrimState m) e -> m e
median v = do
    Merge.sort v
    let l = MV.length v
    if odd l
        then v `MV.read` ((l - 1) `div` 2)
        else do
            x0 <- v `MV.read` ((l `div` 2) - 1)
            x1 <- v `MV.read` (l `div` 2)
            pure $ (x0 + x1) / 2
